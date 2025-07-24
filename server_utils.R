# =============================================================================
# Server Utilities Module for Anatomic RNA Atlas
# Contains server helper functions and reactive utilities
# =============================================================================

# Required libraries for this module
suppressPackageStartupMessages({
    library(dplyr)
    library(DT)
})

# =============================================================================
# Data Processing Functions for Server
# =============================================================================

# Process gene statistics for display
process_gene_statistics <- function(contrast_expression, selected_genes, group1, group2, data_type) {
    if (is.null(contrast_expression) || nrow(contrast_expression) == 0) {
        return(data.frame(Message = "No expression data available"))
    }
    
    if (is.null(selected_genes) || length(selected_genes) == 0) {
        return(data.frame(Message = "No genes selected"))
    }
    
    # Filter for selected genes
    contrast_expression <- contrast_expression %>%
        filter(gene %in% selected_genes)
    
    if (nrow(contrast_expression) == 0) {
        return(data.frame(Message = "No expression data available for selected genes"))
    }
    
    data_type_label <- if (data_type == "log2_cpm") "log2(CPM+1)" else "VST"
    
    # Calculate statistics for each gene
    gene_stats_list <- list()
    
    for (gene in selected_genes) {
        # Get expression data for this specific gene
        gene_expr <- contrast_expression %>%
            filter(gene == !!gene)
        
        if (nrow(gene_expr) == 0) {
            gene_stats_list[[gene]] <- data.frame(
                Gene = gene,
                Group = c(group1, group2),
                Samples = c(0, 0),
                Mean = c(NA, NA),
                Median = c(NA, NA),
                Q25 = c(NA, NA),
                Q75 = c(NA, NA),
                Range_Q5_Q95 = c("No data", "No data")
            )
            next
        }
        
        # Calculate statistics by group
        group_stats <- gene_expr %>%
            group_by(celltype) %>%
            summarise(
                n = n(),
                mean_expr = mean(expression, na.rm = TRUE),
                median_expr = median(expression, na.rm = TRUE),
                q25 = quantile(expression, 0.25, na.rm = TRUE),
                q75 = quantile(expression, 0.75, na.rm = TRUE),
                .groups = "drop"
            )
        
        # Overall statistics for this gene
        all_expr <- gene_expr$expression
        overall_quantiles <- quantile(all_expr, probs = c(0.05, 0.95), na.rm = TRUE)
        range_text <- paste(round(overall_quantiles[1], 3), "-", round(overall_quantiles[2], 3))
        
        # Create a row for each group
        gene_data <- data.frame(
            Gene = gene,
            Group = group_stats$celltype,
            Samples = group_stats$n,
            Mean = round(group_stats$mean_expr, 3),
            Median = round(group_stats$median_expr, 3),
            Q25 = round(group_stats$q25, 3),
            Q75 = round(group_stats$q75, 3),
            Range_Q5_Q95 = range_text
        )
        
        gene_stats_list[[gene]] <- gene_data
    }
    
    # Combine all gene statistics
    all_stats <- do.call(rbind, gene_stats_list)
    
    # Create column names with data type
    colnames(all_stats) <- c(
        "Gene",
        "Group",
        "Samples",
        paste0("Mean (", data_type_label, ")"),
        paste0("Median (", data_type_label, ")"),
        paste0("Q25 (", data_type_label, ")"),
        paste0("Q75 (", data_type_label, ")"),
        paste0("Range Q5-Q95 (", data_type_label, ")")
    )
    
    return(all_stats)
}

# Calculate overall dataset statistics
calculate_overall_statistics <- function(contrast_expression, data_type) {
    if (is.null(contrast_expression) || nrow(contrast_expression) == 0) {
        return(list(
            valid = FALSE,
            message = "No expression data available"
        ))
    }
    
    data_type_label <- if (data_type == "log2_cpm") "log2(CPM+1)" else "VST"
    
    # Calculate statistics
    expr_values <- contrast_expression$expression
    quantiles <- quantile(expr_values, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
    mean_expr <- mean(expr_values, na.rm = TRUE)
    zero_count <- sum(expr_values <= 0.1, na.rm = TRUE)
    total_measurements <- length(expr_values)
    zero_percent <- round((zero_count / total_measurements) * 100, 1)
    
    return(list(
        valid = TRUE,
        data_type_label = data_type_label,
        mean_expr = mean_expr,
        quantiles = quantiles,
        zero_count = zero_count,
        total_measurements = total_measurements,
        zero_percent = zero_percent
    ))
}

# Process expression data for table display
process_expression_table_data <- function(contrast_expression, selected_genes, data_type, group1, group2) {
    if (is.null(contrast_expression) || nrow(contrast_expression) == 0) {
        return(data.frame(Message = "Unable to load data for selected groups"))
    }
    
    # Use selected genes or default to first gene alphabetically
    genes_to_show <- if (!is.null(selected_genes) && length(selected_genes) > 0) {
        selected_genes
    } else {
        # This should be handled by the calling function with available genes
        character(0)
    }
    
    if (length(genes_to_show) == 0) {
        return(data.frame(Message = "No genes available for display"))
    }
    
    # Filter for selected genes and prepare for table
    expression_subset <- contrast_expression %>%
        filter(gene %in% genes_to_show) %>%
        select(sample, celltype, gene, expression) %>%
        # Rename celltype to group for consistency with table display
        rename(group = celltype) %>%
        # Pivot wider to have genes as columns
        pivot_wider(
            names_from = gene,
            values_from = expression,
            values_fill = 0
        ) %>%
        # Round expression values to 3 decimal places
        mutate(across(where(is.numeric), ~ round(.x, 3)))
    
    if (nrow(expression_subset) == 0) {
        message_text <- if (length(selected_genes) > 0) {
            "No expression data found for selected genes in contrast groups"
        } else {
            "No expression data available"
        }
        return(data.frame(Message = message_text))
    }
    
    # Create data type label for column names
    data_type_label <- if (data_type == "log2_cpm") " (log2 CPM)" else " (VST)"
    
    # Safely rename columns to handle special characters
    original_colnames <- colnames(expression_subset)
    gene_columns <- original_colnames[-(1:2)]  # Exclude Sample and Group columns
    
    # Create safe column names by cleaning special characters and adding data type
    safe_colnames <- c(
        "Sample", 
        "Group",
        paste0(make.names(gene_columns), data_type_label)
    )
    
    # Apply the safe column names
    colnames(expression_subset) <- safe_colnames
    
    return(expression_subset)
}

# =============================================================================
# UI Generation Helper Functions
# =============================================================================

# Generate gene-specific stat cards
generate_gene_stat_cards <- function(selected_genes, contrast_expression, data_type, app_theme) {
    if (is.null(selected_genes) || length(selected_genes) == 0) {
        return(list())
    }
    
    if (is.null(contrast_expression) || nrow(contrast_expression) == 0) {
        return(list(div(
            class = "stat-card",
            div("No expression data available", class = "stat-card-content")
        )))
    }
    
    data_type_label <- if (data_type == "log2_cpm") "log2(CPM+1)" else "VST"
    ui_elements <- list()
    
    # Show gene-specific statistics
    for (i in seq_along(selected_genes)) {
        gene <- selected_genes[i]
        
        # Get expression data for this specific gene
        gene_expr <- contrast_expression %>%
            filter(gene == !!gene)
        
        if (nrow(gene_expr) == 0) {
            ui_elements[[i]] <- div(
                class = "stat-card",
                div(gene, class = "stat-card-header"),
                div("No data available", class = "stat-card-content")
            )
            next
        }
        
        # Calculate statistics by group
        group_stats <- gene_expr %>%
            group_by(celltype) %>%
            summarise(
                n = n(),
                mean_expr = mean(expression, na.rm = TRUE),
                median_expr = median(expression, na.rm = TRUE),
                q25 = quantile(expression, 0.25, na.rm = TRUE),
                q75 = quantile(expression, 0.75, na.rm = TRUE),
                .groups = "drop"
            )
        
        # Overall statistics for this gene
        all_expr <- gene_expr$expression
        overall_quantiles <- quantile(all_expr, probs = c(0.05, 0.95), na.rm = TRUE)
        
        # Create gene-specific card
        gene_card_content <- list()
        
        # Overall range
        gene_card_content[[1]] <- div(
            class = "stat-item",
            span("Range (Q5-Q95)", class = "stat-label"),
            span(paste(round(overall_quantiles[1], 3), "-", round(overall_quantiles[2], 3)), class = "stat-value")
        )
        
        # Group-specific stats
        for (j in 1:nrow(group_stats)) {
            group_name <- group_stats$celltype[j]
            gene_card_content[[j + 1]] <- div(
                style = "margin-top: 8px; padding-top: 8px; border-top: 1px solid #dee2e6;",
                div(style = "font-weight: bold; margin-bottom: 4px;", paste("", group_name)),
                div(
                    class = "stat-item",
                    span("Samples", class = "stat-label"),
                    span(group_stats$n[j], class = "stat-value")
                ),
                div(
                    class = "stat-item",
                    span("Mean", class = "stat-label"),
                    span(round(group_stats$mean_expr[j], 3), class = "stat-value")
                ),
                div(
                    class = "stat-item",
                    span("Median", class = "stat-label"),
                    span(round(group_stats$median_expr[j], 3), class = "stat-value")
                ),
                div(
                    class = "stat-item",
                    span("Q25-Q75", class = "stat-label"),
                    span(paste(round(group_stats$q25[j], 3), "-", round(group_stats$q75[j], 3)), class = "stat-value")
                )
            )
        }
        
        ui_elements[[i]] <- div(
            class = "stat-card",
            div(paste(gene, "(", data_type_label, ")"), class = "stat-card-header"),
            div(class = "stat-card-content", gene_card_content)
        )
    }
    
    return(ui_elements)
}

# Generate sample counts card
generate_sample_counts_card <- function(sample_data, group1, group2, total_genes) {
    if (is.null(sample_data)) {
        return(div(
            class = "stat-card",
            div("No data loaded", class = "stat-card-content")
        ))
    }
    
    # Use helper function to determine the cell type column
    celltype_col <- get_celltype_column(sample_data)
    
    if (is.null(celltype_col)) {
        return(div(
            class = "stat-card",
            div("Cell type information not available", class = "stat-card-content")
        ))
    }
    
    # Basic counts
    group1_count <- sum(sample_data[[celltype_col]] == group1, na.rm = TRUE)
    group2_count <- sum(sample_data[[celltype_col]] == group2, na.rm = TRUE)
    
    # Basic sample counts card
    div(
        class = "stat-card",
        div("Sample Counts", class = "stat-card-header"),
        div(
            class = "stat-card-content",
            div(
                class = "stat-item",
                span(paste("Group 1 (", group1, ")"), class = "stat-label"),
                span(paste(group1_count, "samples"), class = "stat-value")
            ),
            div(
                class = "stat-item",
                span(paste("Group 2 (", group2, ")"), class = "stat-label"),
                span(paste(group2_count, "samples"), class = "stat-value")
            ),
            div(
                class = "stat-item",
                span("Total genes", class = "stat-label"),
                span(format(total_genes, big.mark = ","), class = "stat-value")
            )
        )
    )
}

# =============================================================================
# Pagination Helper Functions
# =============================================================================

# Calculate pagination information
calculate_pagination_info <- function(all_genes, current_page, genes_per_page = 10) {
    if (is.null(all_genes) || length(all_genes) == 0) {
        return(list(
            valid = FALSE,
            message = "No genes available"
        ))
    }
    
    total_genes <- length(all_genes)
    max_pages <- ceiling(total_genes / genes_per_page)
    start_idx <- (current_page - 1) * genes_per_page + 1
    end_idx <- min(current_page * genes_per_page, total_genes)
    
    # Validate current page
    if (current_page > max_pages) {
        return(list(
            valid = FALSE,
            reset_to_page = 1,
            message = "Page out of bounds"
        ))
    }
    
    return(list(
        valid = TRUE,
        total_genes = total_genes,
        max_pages = max_pages,
        start_idx = start_idx,
        end_idx = end_idx,
        current_genes = all_genes[start_idx:end_idx],
        pagination_text = paste0("Showing genes ", start_idx, "-", end_idx, " of ", total_genes, 
                                " (Page ", current_page, " of ", max_pages, ")")
    ))
}

# =============================================================================
# File Processing Functions
# =============================================================================

# Validate and process uploaded gene file
validate_uploaded_gene_file <- function(file_path, file_ext) {
    tryCatch({
        # Read the file based on extension
        if (file_ext %in% c("csv")) {
            uploaded_data <- readr::read_csv(file_path, show_col_types = FALSE)
        } else if (file_ext %in% c("tsv", "txt")) {
            uploaded_data <- readr::read_tsv(file_path, show_col_types = FALSE)
        } else {
            # Try to read as generic delimited file
            uploaded_data <- readr::read_delim(file_path, show_col_types = FALSE)
        }
        
        # Extract gene names - assume first column contains gene symbols
        if (ncol(uploaded_data) >= 1) {
            gene_names <- uploaded_data[[1]]
            # Remove any NA values and convert to character
            gene_names <- as.character(gene_names[!is.na(gene_names)])
            # Remove empty strings
            gene_names <- gene_names[gene_names != ""]
            
            if (length(gene_names) > 0) {
                return(list(
                    success = TRUE,
                    genes = gene_names,
                    message = paste("Successfully loaded", length(gene_names), "genes from uploaded file")
                ))
            } else {
                return(list(
                    success = FALSE,
                    message = "No valid gene names found in uploaded file"
                ))
            }
        } else {
            return(list(
                success = FALSE,
                message = "File appears to be empty or invalid"
            ))
        }
    }, error = function(e) {
        return(list(
            success = FALSE,
            message = paste("Error reading file:", e$message)
        ))
    })
}

# =============================================================================
# Co-expression Analysis Helper Functions
# =============================================================================

# Format co-expression results for display
format_coexpression_summary <- function(results) {
    if (is.null(results) || is.null(results$similar_genes) || nrow(results$similar_genes) == 0) {
        return(data.frame(Message = "No co-expressed genes found"))
    }
    
    summary_table <- results$similar_genes %>%
        select(
            Gene = similar_gene,
            `Avg Correlation` = avg_correlation,
            `Max Correlation` = max_correlation,
            `Query Genes` = n_correlations
        ) %>%
        mutate(
            `Avg Correlation` = round(`Avg Correlation`, 3),
            `Max Correlation` = round(`Max Correlation`, 3)
        )
    
    return(summary_table)
}

# Format detailed co-expression results
format_coexpression_detailed <- function(results) {
    if (is.null(results) || is.null(results$detailed_correlations) || nrow(results$detailed_correlations) == 0) {
        return(data.frame(Message = "No detailed results available"))
    }
    
    detailed_table <- results$detailed_correlations %>%
        select(
            `Query Gene` = query_gene,
            `Similar Gene` = similar_gene,
            Correlation = correlation
        ) %>%
        mutate(
            Correlation = round(Correlation, 3)
        ) %>%
        arrange(desc(abs(Correlation)))
    
    return(detailed_table)
}
