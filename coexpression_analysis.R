# Co-Expression Analysis Module for Anatomic RNA Atlas
# This module finds genes with similar expression patterns to user-selected genes

library(corrr)
library(dplyr)
library(plotly)
library(DT)
library(tidyr)
library(parallel)
library(foreach)
library(doParallel)

# 1. Calculate gene co-expression similarity (Parallelized Streaming Only)
find_coexpressed_genes <- function(expression_data, query_genes, n_similar = 25, 
                                 min_correlation = 0.6, data_type = "log2_cpm", 
                                 max_genes_batch = 1000, use_parallel = TRUE, log_func = NULL) {
    
    # Helper function for logging
    log_message <- function(msg) {
        message(msg)  # Still print to console
        if (!is.null(log_func)) {
            log_func(msg)
        }
    }
    
    # Filter expression data for the specified data type
    expr_filtered <- expression_data %>%
        filter(data_type == !!data_type)
    
    # Check if we have enough query genes
    available_query_genes <- intersect(query_genes, unique(expr_filtered$gene))
    
    if (length(available_query_genes) < 3) {
        log_message(paste("Need at least 3 genes. Found only", length(available_query_genes), "of", length(query_genes)))
        return(list(
            similar_genes = data.frame(),
            query_genes_used = available_query_genes,
            correlation_matrix = NULL,
            message = paste("Need at least 3 genes. Found only", length(available_query_genes), "of", length(query_genes))
        ))
    }
    
    # Get all unique genes and check dataset size
    all_genes <- unique(expr_filtered$gene)
    n_genes <- length(all_genes)
    n_samples <- length(unique(expr_filtered$sample))
    
    log_message(paste("Dataset:", n_genes, "genes,", n_samples, "samples. Using streaming approach."))
    
    # Always use streaming approach with optional parallelization
    if (use_parallel) {
        log_message("Using parallel streaming approach")
        return(find_coexpressed_genes_streaming_parallel(expr_filtered, available_query_genes, 
                                                       n_similar, min_correlation, max_genes_batch, log_func))
    } else {
        log_message("Using single-threaded streaming approach")
        return(find_coexpressed_genes_streaming(expr_filtered, available_query_genes, 
                                              n_similar, min_correlation, max_genes_batch, log_func))
    }

    
    # For smaller datasets, use the original matrix approach
    # Create wide format expression matrix
    expr_matrix <- expr_filtered %>%
        select(sample, gene, expression) %>%
        pivot_wider(names_from = gene, values_from = expression, 
                   values_fill = 0, values_fn = mean)
    
    # Convert to matrix with genes as columns
    sample_names <- expr_matrix$sample
    expr_matrix <- as.matrix(expr_matrix[, -1])
    rownames(expr_matrix) <- sample_names
    
    # Calculate correlation matrix between all genes
    gene_cor_matrix <- cor(expr_matrix, use = "complete.obs")
    
    # For each query gene, find the most correlated genes
    similar_genes_list <- list()
    
    for (query_gene in available_query_genes) {
        if (query_gene %in% colnames(gene_cor_matrix)) {
            # Get correlations for this query gene
            gene_correlations <- gene_cor_matrix[query_gene, ]
            
            # Remove self-correlation and other query genes
            gene_correlations <- gene_correlations[!names(gene_correlations) %in% available_query_genes]
            
            # Filter by minimum correlation and sort
            high_cor_genes <- gene_correlations[abs(gene_correlations) >= min_correlation]
            high_cor_genes <- sort(high_cor_genes, decreasing = TRUE)
            
            # Create data frame for this query gene
            if (length(high_cor_genes) > 0) {
                similar_genes_list[[query_gene]] <- data.frame(
                    query_gene = query_gene,
                    similar_gene = names(high_cor_genes),
                    correlation = as.numeric(high_cor_genes),
                    stringsAsFactors = FALSE
                )
            }
        }
    }
    
    if (length(similar_genes_list) == 0) {
        return(list(
            similar_genes = data.frame(),
            query_genes_used = available_query_genes,
            correlation_matrix = gene_cor_matrix,
            message = paste("No genes found with correlation >=", min_correlation)
        ))
    }
    
    # Combine results and rank by average correlation across query genes
    all_similar <- do.call(rbind, similar_genes_list)
    
    # Calculate average correlation for each similar gene across all query genes
    gene_summary <- all_similar %>%
        group_by(similar_gene) %>%
        summarise(
            avg_correlation = mean(abs(correlation)),
            max_correlation = max(abs(correlation)),
            min_correlation = min(abs(correlation)),
            n_query_genes = n(),
            correlations = paste(round(correlation, 3), collapse = ", "),
            query_genes = paste(query_gene, collapse = ", "),
            .groups = "drop"
        ) %>%
        arrange(desc(avg_correlation)) %>%
        head(n_similar)
    
    return(list(
        similar_genes = gene_summary,
        query_genes_used = available_query_genes,
        correlation_matrix = gene_cor_matrix,
        detailed_correlations = all_similar,
        message = paste("Found", nrow(gene_summary), "co-expressed genes")
    ))
}

# Memory-efficient streaming correlation for large datasets
find_coexpressed_genes_streaming <- function(expr_filtered, available_query_genes, 
                                           n_similar = 25, min_correlation = 0.6, 
                                           max_genes_batch = 1000, log_func = NULL) {
    
    # Helper function for logging
    log_message <- function(msg) {
        message(msg)  # Still print to console
        if (!is.null(log_func)) {
            log_func(msg)
        }
    }
    
    # Get all genes and create batches to process
    all_genes <- unique(expr_filtered$gene)
    other_genes <- setdiff(all_genes, available_query_genes)
    
    # Adjust batch size based on dataset size to avoid memory issues
    n_genes <- length(other_genes)
    
    # Create batches of genes to avoid memory issues
    n_batches <- ceiling(length(other_genes) / max_genes_batch)
    gene_batches <- split(other_genes, ceiling(seq_along(other_genes) / max_genes_batch))
    
    log_message(paste("Processing", length(other_genes), "genes in", n_batches, "batches"))
    
    # Store results for each query gene
    similar_genes_list <- list()
    
    # Process each query gene
    for (query_idx in seq_along(available_query_genes)) {
        query_gene <- available_query_genes[query_idx]
        
        log_message(paste("Processing query gene", query_idx, "of", length(available_query_genes), ":", query_gene))
        
        # Get expression data for the query gene
        query_expr <- expr_filtered %>%
            filter(gene == query_gene) %>%
            select(sample, expression) %>%
            arrange(sample)
        
        if (nrow(query_expr) == 0) next
        
        gene_correlations <- numeric()
        correlation_names <- character()
        
        # Process each batch of genes
        for (batch_idx in seq_along(gene_batches)) {
            batch_genes <- gene_batches[[batch_idx]]
            
            if (query_idx == 1) {  # Only show batch progress for first query gene
                log_message(paste("  Processing batch", batch_idx, "of", n_batches, 
                             "(", length(batch_genes), "genes)"))
            }
            
            # Get expression matrix for this batch
            tryCatch({
                batch_expr <- expr_filtered %>%
                    filter(gene %in% batch_genes) %>%
                    select(sample, gene, expression) %>%
                    pivot_wider(names_from = gene, values_from = expression, 
                               values_fill = 0, values_fn = mean) %>%
                    arrange(sample)
                
                # Ensure samples match between query and batch
                common_samples <- intersect(query_expr$sample, batch_expr$sample)
                if (length(common_samples) < 3) {
                    rm(batch_expr)
                    next  # Need at least 3 samples for correlation
                }
                
                query_values <- query_expr %>%
                    filter(sample %in% common_samples) %>%
                    arrange(sample) %>%
                    pull(expression)
                
                batch_matrix <- batch_expr %>%
                    filter(sample %in% common_samples) %>%
                    arrange(sample) %>%
                    select(-sample) %>%
                    as.matrix()
                
                # Calculate correlations for this batch
                if (length(query_values) > 2 && ncol(batch_matrix) > 0) {
                    batch_correlations <- cor(query_values, batch_matrix, use = "complete.obs")
                    
                    # Store results (only non-NA correlations)
                    valid_cors <- !is.na(batch_correlations)
                    if (any(valid_cors)) {
                        gene_correlations <- c(gene_correlations, as.numeric(batch_correlations[valid_cors]))
                        correlation_names <- c(correlation_names, colnames(batch_matrix)[valid_cors])
                    }
                }
                
                # Clean up memory immediately
                rm(batch_expr, batch_matrix, batch_correlations)
                gc(verbose = FALSE)
                
            }, error = function(e) {
                warning(paste("Error processing batch", batch_idx, "for gene", query_gene, ":", e$message))
                log_message(paste("ERROR in batch", batch_idx, ":", e$message))
                # Force cleanup even on error
                suppressWarnings({
                    try(rm(batch_expr, batch_matrix, batch_correlations), silent = TRUE)
                    gc(verbose = FALSE)
                })
            })
        }
        
        # Name the correlation vector
        names(gene_correlations) <- correlation_names
        
        # Filter by minimum correlation and sort
        high_cor_genes <- gene_correlations[abs(gene_correlations) >= min_correlation]
        high_cor_genes <- sort(high_cor_genes, decreasing = TRUE)
        
        log_message(paste("Found", length(high_cor_genes), "correlated genes for", query_gene))
        
        # Store results for this query gene
        if (length(high_cor_genes) > 0) {
            similar_genes_list[[query_gene]] <- data.frame(
                query_gene = query_gene,
                similar_gene = names(high_cor_genes),
                correlation = as.numeric(high_cor_genes),
                stringsAsFactors = FALSE
            )
        }
    }
    
    if (length(similar_genes_list) == 0) {
        log_message("No co-expressed genes found meeting the criteria")
        return(list(
            similar_genes = data.frame(),
            query_genes_used = available_query_genes,
            correlation_matrix = NULL,
            message = paste("No genes found with correlation >=", min_correlation, "(streaming mode)")
        ))
    }
    
    # Combine results and rank by average correlation across query genes
    all_similar <- do.call(rbind, similar_genes_list)
    
    # Calculate average correlation for each similar gene across all query genes
    gene_summary <- all_similar %>%
        group_by(similar_gene) %>%
        summarise(
            avg_correlation = mean(abs(correlation)),
            max_correlation = max(abs(correlation)),
            min_correlation = min(abs(correlation)),
            n_query_genes = n(),
            correlations = paste(round(correlation, 3), collapse = ", "),
            query_genes = paste(query_gene, collapse = ", "),
            .groups = "drop"
        ) %>%
        arrange(desc(avg_correlation)) %>%
        head(n_similar)
    
    log_message(paste("Final results: top", nrow(gene_summary), "co-expressed genes identified"))
    
    # Create a minimal correlation matrix for network visualization
    # Only include query genes and top similar genes
    top_genes <- c(available_query_genes, gene_summary$similar_gene)
    
    # Build correlation matrix only for the genes we need
    correlation_matrix <- matrix(NA, nrow = length(top_genes), ncol = length(top_genes))
    rownames(correlation_matrix) <- top_genes
    colnames(correlation_matrix) <- top_genes
    
    # Fill in correlations we calculated
    for (i in 1:nrow(all_similar)) {
        row_gene <- all_similar$query_gene[i]
        col_gene <- all_similar$similar_gene[i]
        correlation_val <- all_similar$correlation[i]
        
        if (row_gene %in% top_genes && col_gene %in% top_genes) {
            row_idx <- which(rownames(correlation_matrix) == row_gene)
            col_idx <- which(colnames(correlation_matrix) == col_gene)
            correlation_matrix[row_idx, col_idx] <- correlation_val
            correlation_matrix[col_idx, row_idx] <- correlation_val
        }
    }
    
    # Set diagonal to 1
    diag(correlation_matrix) <- 1
    
    return(list(
        similar_genes = gene_summary,
        query_genes_used = available_query_genes,
        correlation_matrix = correlation_matrix,
        detailed_correlations = all_similar,
        message = paste("Found", nrow(gene_summary), "co-expressed genes (streaming mode)")
    ))
}

# Memory-efficient PARALLEL streaming correlation for large datasets
find_coexpressed_genes_streaming_parallel <- function(expr_filtered, available_query_genes, 
                                                    n_similar = 5, min_correlation = 0.6, 
                                                    max_genes_batch = 1000, log_func = NULL) {
    
    # Helper function for logging
    log_message <- function(msg) {
        message(msg)  # Still print to console
        if (!is.null(log_func)) {
            log_func(msg)
        }
    }
    
    # Auto-detect number of cores (but limit to reasonable number)
    n_cores <- min(8, max(1, parallel::detectCores()))  # max 8
    log_message(paste("Using", n_cores, "cores for parallel processing"))
    
    # Get all genes and create batches to process
    all_genes <- unique(expr_filtered$gene)
    other_genes <- setdiff(all_genes, available_query_genes)
    
    # Adjust batch size based on dataset size to avoid memory issues
    n_genes <- length(other_genes)

    # Create batches of genes to avoid memory issues
    n_batches <- ceiling(length(other_genes) / max_genes_batch)
    gene_batches <- split(other_genes, ceiling(seq_along(other_genes) / max_genes_batch))
    
    log_message(paste("Processing", length(other_genes), "genes in", n_batches, "batches across", n_cores, "cores"))
    
    # Helper function to process one query gene across all batches (sequential batches)
    process_query_gene <- function(query_idx, query_gene) {
        
        # Get expression data for the query gene from the parent environment
        query_expr <- expr_filtered %>%
            filter(gene == query_gene) %>%
            select(sample, expression) %>%
            arrange(sample)
        
        if (nrow(query_expr) == 0) return(NULL)
        
        gene_correlations <- numeric()
        correlation_names <- character()
        
        # Process each batch of genes sequentially within each query gene process
        for (batch_idx in seq_along(gene_batches)) {
            batch_genes <- gene_batches[[batch_idx]]
            
            # Get expression matrix for this batch
            tryCatch({
                batch_expr <- expr_filtered %>%
                    filter(gene %in% batch_genes) %>%
                    select(sample, gene, expression) %>%
                    pivot_wider(names_from = gene, values_from = expression, 
                               values_fill = 0, values_fn = mean) %>%
                    arrange(sample)
                
                # Ensure samples match between query and batch
                common_samples <- intersect(query_expr$sample, batch_expr$sample)
                if (length(common_samples) < 3) {
                    rm(batch_expr)
                    next  # Need at least 3 samples for correlation
                }
                
                query_values <- query_expr %>%
                    filter(sample %in% common_samples) %>%
                    arrange(sample) %>%
                    pull(expression)
                
                batch_matrix <- batch_expr %>%
                    filter(sample %in% common_samples) %>%
                    arrange(sample) %>%
                    select(-sample) %>%
                    as.matrix()
                
                # Calculate correlations for this batch
                if (length(query_values) > 2 && ncol(batch_matrix) > 0) {
                    batch_correlations <- cor(query_values, batch_matrix, use = "complete.obs")
                    
                    # Store results (only non-NA correlations)
                    valid_cors <- !is.na(batch_correlations)
                    if (any(valid_cors)) {
                        gene_correlations <- c(gene_correlations, as.numeric(batch_correlations[valid_cors]))
                        correlation_names <- c(correlation_names, colnames(batch_matrix)[valid_cors])
                    }
                }
                
                # Clean up memory immediately
                rm(batch_expr, batch_matrix, batch_correlations)
                gc(verbose = FALSE)
                
            }, error = function(e) {
                # Silent error handling in parallel workers
                return(NULL)
            })
        }
        
        # Name the correlation vector
        names(gene_correlations) <- correlation_names
        
        # Filter by minimum correlation and sort
        high_cor_genes <- gene_correlations[abs(gene_correlations) >= min_correlation]
        high_cor_genes <- sort(high_cor_genes, decreasing = TRUE)
        
        # Return results for this query gene
        if (length(high_cor_genes) > 0) {
            return(data.frame(
                query_gene = query_gene,
                similar_gene = names(high_cor_genes),
                correlation = as.numeric(high_cor_genes),
                stringsAsFactors = FALSE
            ))
        }
        return(NULL)
    }
    
    # For small numbers of query genes, don't use parallel processing
    if (length(available_query_genes) <= 2) {
        log_message("Small number of query genes - using sequential processing")
        similar_genes_list <- list()
        for (i in seq_along(available_query_genes)) {
            query_gene <- available_query_genes[i]
            log_message(paste("Processing query gene", i, "of", length(available_query_genes), ":", query_gene))
            result <- process_query_gene(i, query_gene)
            similar_genes_list[[query_gene]] <- result
        }
    } else {
        # Use parallel processing for multiple query genes
        tryCatch({
            # Set up parallel cluster
            cl <- parallel::makeCluster(n_cores)
            on.exit(parallel::stopCluster(cl), add = TRUE)
            
            # Export necessary objects to workers
            parallel::clusterEvalQ(cl, {
                library(dplyr)
                library(tidyr)
            })
            
            # Export the data and parameters (this may use significant memory)
            parallel::clusterExport(cl, c("expr_filtered", "gene_batches", "min_correlation"), 
                                   envir = environment())
            
            log_message("Starting parallel processing of query genes...")
            
            # Process query genes in parallel
            results_list <- parallel::clusterMap(cl, process_query_gene, 
                                                seq_along(available_query_genes), 
                                                available_query_genes,
                                                SIMPLIFY = FALSE)
            
            # Name the results
            names(results_list) <- available_query_genes
            similar_genes_list <- results_list
            
            log_message("Parallel processing completed")
            
        }, error = function(e) {
            log_message(paste("Parallel processing failed, falling back to sequential:", e$message))
            # Fallback to sequential processing
            similar_genes_list <- list()
            for (i in seq_along(available_query_genes)) {
                query_gene <- available_query_genes[i]
                log_message(paste("Processing query gene", i, "of", length(available_query_genes), ":", query_gene))
                result <- process_query_gene(i, query_gene)
                similar_genes_list[[query_gene]] <- result
            }
        })
    }
    
    # Filter out NULL results
    similar_genes_list <- similar_genes_list[!sapply(similar_genes_list, is.null)]
    
    if (length(similar_genes_list) == 0) {
        log_message("No co-expressed genes found meeting the criteria")
        return(list(
            similar_genes = data.frame(),
            query_genes_used = available_query_genes,
            correlation_matrix = NULL,
            message = paste("No genes found with correlation >=", min_correlation, "(parallel streaming mode)")
        ))
    }
    
    # Combine results and rank by average correlation across query genes
    all_similar <- do.call(rbind, similar_genes_list)
    
    # Calculate average correlation for each similar gene across all query genes
    gene_summary <- all_similar %>%
        group_by(similar_gene) %>%
        summarise(
            avg_correlation = mean(abs(correlation)),
            max_correlation = max(abs(correlation)),
            min_correlation = min(abs(correlation)),
            n_query_genes = n(),
            correlations = paste(round(correlation, 3), collapse = ", "),
            query_genes = paste(query_gene, collapse = ", "),
            .groups = "drop"
        ) %>%
        arrange(desc(avg_correlation)) %>%
        head(n_similar)
    
    log_message(paste("Final results: top", nrow(gene_summary), "co-expressed genes identified (parallel mode)"))
    
    # Create a minimal correlation matrix for network visualization
    # Only include query genes and top similar genes
    top_genes <- c(available_query_genes, gene_summary$similar_gene)
    
    # Build correlation matrix only for the genes we need
    correlation_matrix <- matrix(NA, nrow = length(top_genes), ncol = length(top_genes))
    rownames(correlation_matrix) <- top_genes 
    colnames(correlation_matrix) <- top_genes
    
    # Fill in correlations we calculated
    for (i in 1:nrow(all_similar)) {
        row_gene <- all_similar$query_gene[i]
        col_gene <- all_similar$similar_gene[i]
        correlation_val <- all_similar$correlation[i]
        
        if (row_gene %in% top_genes && col_gene %in% top_genes) {
            row_idx <- which(rownames(correlation_matrix) == row_gene)
            col_idx <- which(colnames(correlation_matrix) == col_gene)
            correlation_matrix[row_idx, col_idx] <- correlation_val
            correlation_matrix[col_idx, row_idx] <- correlation_val
        }
    }
    
    # Set diagonal to 1
    diag(correlation_matrix) <- 1
    
    return(list(
        similar_genes = gene_summary,
        query_genes_used = available_query_genes,
        correlation_matrix = correlation_matrix,
        detailed_correlations = all_similar,
        message = paste("Found", nrow(gene_summary), "co-expressed genes (parallel streaming mode)")
    ))
}

# 2. Create expression heatmap for query + similar genes
create_coexpression_heatmap <- function(expression_data, query_genes, similar_genes, 
                                      data_type = "log2_cpm", sample_metadata = NULL) {
    
    # Combine query and similar genes
    all_genes <- c(query_genes, similar_genes)
    
    # Filter expression data
    expr_data <- expression_data %>%
        filter(gene %in% all_genes, data_type == !!data_type)
    
    if (nrow(expr_data) == 0) {
        return(NULL)
    }
    
    # Add sample group information if available
    if (!is.null(sample_metadata)) {
        celltype_col <- if ("PubCelltype" %in% colnames(sample_metadata)) {
            "PubCelltype"
        } else if ("Celltype" %in% colnames(sample_metadata)) {
            "Celltype"
        } else {
            NULL
        }
        
        sample_col <- if ("PubName" %in% colnames(sample_metadata)) {
            "PubName"
        } else if ("Name" %in% colnames(sample_metadata)) {
            "Name"
        } else {
            NULL
        }
        
        if (!is.null(celltype_col) && !is.null(sample_col)) {
            sample_info <- sample_metadata %>%
                select(!!sample_col, !!celltype_col) %>%
                rename(sample = !!sample_col, celltype = !!celltype_col)
            
            expr_data <- expr_data %>%
                left_join(sample_info, by = "sample") %>%
                mutate(celltype = ifelse(is.na(celltype), "Unknown", celltype))
        }
    }
    
    # Calculate mean expression by cell type (or overall if no metadata)
    if ("celltype" %in% colnames(expr_data)) {
        heatmap_data <- expr_data %>%
            group_by(gene, celltype) %>%
            summarise(mean_expr = mean(expression, na.rm = TRUE), .groups = "drop") %>%
            pivot_wider(names_from = celltype, values_from = mean_expr, values_fill = 0)
    } else {
        # If no cell type info, create a single column
        heatmap_data <- expr_data %>%
            group_by(gene) %>%
            summarise(mean_expr = mean(expression, na.rm = TRUE), .groups = "drop") %>%
            mutate(All_Samples = mean_expr) %>%
            select(gene, All_Samples)
    }
    
    # Convert to matrix
    gene_names <- heatmap_data$gene
    expr_matrix <- as.matrix(heatmap_data[, -1])
    rownames(expr_matrix) <- gene_names
    
    # Create gene type annotations (query vs similar)
    gene_types <- ifelse(gene_names %in% query_genes, "Query Gene", "Similar Gene")
    
    # Create data type label
    data_type_label <- if (data_type == "log2_cpm") "log2(CPM + 1)" else "VST"
    
    # Create plotly heatmap
    plot_ly(
        z = expr_matrix,
        x = colnames(expr_matrix),
        y = paste0(rownames(expr_matrix), " (", gene_types, ")"),
        type = "heatmap",
        colorscale = "Viridis",
        showscale = TRUE,
        hovertemplate = paste0("Gene: %{y}<br>Group: %{x}<br>", data_type_label, ": %{z:.2f}<extra></extra>")
    ) %>%
        layout(
            title = paste("Co-Expression Heatmap:", length(query_genes), "Query +", 
                         length(similar_genes), "Similar Genes"),
            xaxis = list(title = "Cell Types", side = "bottom"),
            yaxis = list(title = "Genes"),
            margin = list(l = 200, r = 50, t = 80, b = 80),
            font = list(size = 10)
        )
}

# 3. Create correlation network plot for co-expressed genes (Updated for streaming)
create_coexpression_network <- function(correlation_matrix, query_genes, similar_genes, 
                                      min_correlation = 0.6) {
    
    # Handle case where correlation_matrix might be NULL (from streaming mode)
    if (is.null(correlation_matrix)) {
        return(NULL)
    }
    
    # Combine all genes
    all_genes <- c(query_genes, similar_genes)
    
    # Filter correlation matrix
    if (!all(all_genes %in% rownames(correlation_matrix))) {
        missing_genes <- all_genes[!all_genes %in% rownames(correlation_matrix)]
        all_genes <- all_genes[all_genes %in% rownames(correlation_matrix)]
    }
    
    if (length(all_genes) < 2) {
        return(NULL)
    }
    
    cor_subset <- correlation_matrix[all_genes, all_genes]
    
    # Create edges data (only correlations above threshold)
    edges_list <- list()
    edge_count <- 0
    
    for (i in 1:(nrow(cor_subset) - 1)) {
        for (j in (i + 1):nrow(cor_subset)) {
            correlation <- cor_subset[i, j]
            
            # Handle NA values from streaming correlation matrix
            if (!is.na(correlation) && abs(correlation) >= min_correlation) {
                edge_count <- edge_count + 1
                edges_list[[edge_count]] <- data.frame(
                    from = rownames(cor_subset)[i],
                    to = rownames(cor_subset)[j],
                    weight = correlation,
                    abs_weight = abs(correlation),
                    stringsAsFactors = FALSE
                )
            }
        }
    }
    
    if (length(edges_list) == 0) {
        return(NULL)
    }
    
    edges <- do.call(rbind, edges_list)
    
    # Create nodes data
    nodes <- data.frame(
        name = all_genes,
        group = ifelse(all_genes %in% query_genes, "Query", "Similar"),
        size = 15,
        stringsAsFactors = FALSE
    )
    
    # Convert to indices for networkD3
    edges$source <- match(edges$from, nodes$name) - 1
    edges$target <- match(edges$to, nodes$name) - 1
    edges$value <- edges$abs_weight * 10
    
    # Create network plot
    forceNetwork(
        Links = edges,
        Nodes = nodes,
        Source = "source",
        Target = "target",
        Value = "value",
        NodeID = "name",
        Group = "group",
        fontSize = 12,
        fontFamily = "Arial",
        linkDistance = 100,
        linkWidth = JS("function(d){return Math.sqrt(d.value);}"),
        radiusCalculation = "15",
        charge = -400,
        opacity = 0.8,
        zoom = TRUE,
        legend = TRUE,
        arrows = FALSE,
        bounded = FALSE,
        opacityNoHover = 0.1
    )
}

# 4. Create detailed correlation table
create_correlation_table <- function(coexpression_results) {
    
    if (nrow(coexpression_results$similar_genes) == 0) {
        return(data.frame(Message = "No co-expressed genes found"))
    }
    
    # Format the results table
    results_table <- coexpression_results$similar_genes %>%
        select(
            Gene = similar_gene,
            `Avg Correlation` = avg_correlation,
            `Max Correlation` = max_correlation,
            `Min Correlation` = min_correlation,
            `Query Genes Matched` = n_query_genes,
            `Individual Correlations` = correlations
        ) %>%
        mutate(
            `Avg Correlation` = round(`Avg Correlation`, 3),
            `Max Correlation` = round(`Max Correlation`, 3),
            `Min Correlation` = round(`Min Correlation`, 3)
        )
    
    return(results_table)
}

# 5. UI Tab for Co-Expression Analysis
create_coexpression_tab_ui <- function() {
    tabPanel(
        "Co-Expression Analysis",
        fluidRow(
            column(
                width = 3,
                wellPanel(
                    h5("Co-Expression Controls", style = "margin-top: 0;"),
                    
                    helpText("Select at least 3 genes to find co-expressed genes across the entire dataset.",
                           class = "help-text"),
                    
                    numericInput("min_correlation_coexpr",
                        "Minimum Correlation:",
                        value = 0.6,
                        min = 0.1,
                        max = 1.0,
                        step = 0.1
                    ),
                    
                    numericInput("n_similar_genes",
                        "Number of Similar Genes:",
                        value = 25,
                        min = 1,
                        max = 250,
                        step = 1
                    ),
                    
                    numericInput("max_genes_batch",
                        "Batch Size (for large datasets):",
                        value = 1000,
                        min = 100,
                        max = 5000,
                        step = 100,
                        help = "Smaller values use less memory but take longer"
                    ),
                    
                    checkboxInput("force_streaming",
                        "Force streaming mode (recommended)",
                        value = TRUE
                    ),
                    
                    helpText("Streaming mode processes genes in batches to avoid memory issues.",
                           class = "help-text"),
                    
                    br(),
                    
                    actionButton("run_coexpression", 
                               "Find Co-expressed Genes", 
                               class = "btn-primary btn-block"),
                    
                    br(),
                    
                    h6("Analysis Status", style = "font-weight: bold;"),
                    verbatimTextOutput("coexpression_status"),
                    
                    br(),
                    
                    conditionalPanel(
                        condition = "output.coexpression_status",
                        h6("Found Genes", style = "font-weight: bold;"),
                        DT::dataTableOutput("coexpressed_genes_summary", height = "200px")
                    )
                )
            ),
            column(
                width = 9,
                tabsetPanel(
                    tabPanel(
                        "Expression Heatmap",
                        plotlyOutput("coexpression_heatmap", height = "500px") %>% 
                            shinycssloaders::withSpinner()
                    ),
                    tabPanel(
                        "Correlation Network",
                        networkD3::forceNetworkOutput("coexpression_network", height = "500px") %>%
                            shinycssloaders::withSpinner()
                    ),
                    tabPanel(
                        "Detailed Results",
                        h5("Co-expressed Genes Details"),
                        DT::dataTableOutput("coexpression_detailed_table") %>%
                            shinycssloaders::withSpinner()
                    )
                )
            )
        )
    )
}

# 6. Server Logic for Co-Expression Analysis
# Add these reactive expressions and outputs to your server function:

# NOTE: The following server logic has been moved to app.R and integrated
# into the main server function. This commented code is kept for reference.
