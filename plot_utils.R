# =============================================================================
# Plot Utilities Module for Anatomic RNA Atlas
# Contains plotting functions and visualization helpers
# =============================================================================

# Suppress package startup messages
suppressPackageStartupMessages({
    library(ggplot2)
    library(plotly)
    library(dplyr)
    library(tidyr)
    library(viridis)
    library(RColorBrewer)
    library(tibble)
})

# =============================================================================
# Core Plotting Functions
# =============================================================================

# Create expression boxplot/violin plot
create_expression_plot <- function(plot_data, genes_to_plot, groups, data_type = "log2_cpm", 
                                 show_points = TRUE, plot_theme = NULL) {
    
    if (is.null(plot_data) || nrow(plot_data) == 0) {
        return(NULL)
    }
    
    if (is.null(genes_to_plot) || length(genes_to_plot) == 0) {
        return(NULL)
    }
    
    # Create Y-axis label based on data type
    y_label <- if (data_type == "log2_cpm") {
        "Expression Level (log2(CPM + 1))"
    } else {
        "Expression Level (VST)"
    }
    
    # Filter for selected genes
    filtered_data <- plot_data %>%
        filter(gene %in% genes_to_plot)
    
    if (nrow(filtered_data) == 0) {
        return(NULL)
    }
    
    # Set default theme if none provided
    if (is.null(plot_theme)) {
        plot_theme <- list(
            colors = c("#DC143C", "#1f62a5", "#95A5A6"),
            background = "#FFFFFF",
            text_color = "#000000"
        )
    }
    
    # Create the plot
    p <- ggplot(filtered_data, aes(x = violin_group, y = expression, fill = violin_group, text = sample)) +
        geom_boxplot(alpha = 0.7, outlier.shape = NA) +
        scale_fill_manual(values = plot_theme$colors) +
        labs(
            x = "Cell Type",
            y = y_label,
            fill = "Group"
        ) +
        theme_minimal() +
        theme(
            strip.text = element_text(size = 10, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
            axis.text.y = element_text(size = 9),
            axis.title = element_text(size = 11, face = "bold"),
            legend.position = "bottom",
            legend.title = element_text(size = 10, face = "bold"),
            legend.text = element_text(size = 9),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = plot_theme$background, color = NA),
            panel.background = element_rect(fill = plot_theme$background, color = NA)
        ) +
        facet_wrap(~gene, scales = "free_y", ncol = 2)
    
    # Add points if requested
    if (show_points) {
        p <- p + geom_jitter(width = 0.2, alpha = 0.6, size = 1)
    }
    
    return(p)
}

# Create gene set heatmap
create_gene_heatmap <- function(expression_data, selected_genes, sample_data = NULL, 
                              data_type = "log2_cpm", plot_theme = NULL) {
    
    if (is.null(selected_genes) || length(selected_genes) == 0) {
        return(NULL)
    }
    
    # Filter expression data
    expr_data <- expression_data %>%
        filter(gene %in% selected_genes, data_type == !!data_type)
    
    if (nrow(expr_data) == 0) {
        return(NULL)
    }
    
    # Add sample group information if available
    if (!is.null(sample_data)) {
        # Determine column names
        celltype_col <- if ("PubCelltype" %in% colnames(sample_data)) {
            "PubCelltype"
        } else if ("Celltype" %in% colnames(sample_data)) {
            "Celltype"
        } else if ("celltype" %in% colnames(sample_data)) {
            "celltype"
        } else {
            NULL
        }
        
        sample_col <- if ("PubName" %in% colnames(sample_data)) {
            "PubName"
        } else if ("Name" %in% colnames(sample_data)) {
            "Name"
        } else {
            NULL
        }
        
        if (!is.null(celltype_col) && !is.null(sample_col)) {
            sample_info <- sample_data %>%
                select(all_of(c(sample_col, celltype_col))) %>%
                rename(sample = !!sample_col, celltype = !!celltype_col)
            
            expr_data <- expr_data %>%
                left_join(sample_info, by = "sample")
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
            mutate(Overall = mean_expr) %>%
            select(gene, Overall)
    }
    
    # Convert to matrix
    gene_names <- heatmap_data$gene
    expr_matrix <- as.matrix(heatmap_data[, -1])
    rownames(expr_matrix) <- gene_names
    
    # Create data type label
    data_type_label <- if (data_type == "log2_cpm") "log2(CPM + 1)" else "VST"
    
    # Create plotly heatmap
    plot_ly(
        z = expr_matrix,
        x = colnames(expr_matrix),
        y = rownames(expr_matrix),
        type = "heatmap",
        colorscale = "Viridis",
        showscale = TRUE,
        hovertemplate = paste0("Gene: %{y}<br>Group: %{x}<br>", data_type_label, ": %{z:.2f}<extra></extra>")
    ) %>%
        layout(
            title = list(
                text = paste("Gene Expression Heatmap -", data_type_label),
                font = list(size = 14)
            ),
            xaxis = list(title = "Cell Type", tickangle = 45),
            yaxis = list(title = "Gene"),
            margin = list(l = 100, r = 50, t = 50, b = 100)
        )
}

# Create group expression ranking barplot
create_group_barplot <- function(contrast_data, group1, group2, plot_theme = NULL) {
    
    if (is.null(contrast_data) || nrow(contrast_data) == 0) {
        return(NULL)
    }
    
    # Set default theme if none provided
    if (is.null(plot_theme)) {
        plot_theme <- list(
            colors = c("#DC143C", "#1f62a5"),
            background = "#FFFFFF",
            text_color = "#000000"
        )
    }
    
    # Calculate mean expression by group
    group_means <- contrast_data %>%
        filter(violin_group %in% c(group1, group2)) %>%
        group_by(gene, violin_group) %>%
        summarise(mean_expr = mean(expression, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = violin_group, values_from = mean_expr, values_fill = 0) %>%
        mutate(
            log2fc = get(group2) - get(group1),
            direction = ifelse(log2fc > 0, paste("Higher in", group2), paste("Higher in", group1))
        ) %>%
        arrange(desc(abs(log2fc))) %>%
        head(20)  # Top 20 most different genes
    
    if (nrow(group_means) == 0) {
        return(NULL)
    }
    
    # Create barplot
    p <- ggplot(group_means, aes(x = reorder(gene, abs(log2fc)), y = log2fc, fill = direction)) +
        geom_col(alpha = 0.8) +
        scale_fill_manual(values = plot_theme$colors) +
        coord_flip() +
        labs(
            title = paste("Top Differential Genes:", group1, "vs", group2),
            x = "Gene",
            y = "Mean Expression Difference",
            fill = "Direction"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 9),
            axis.title = element_text(size = 10, face = "bold"),
            legend.position = "bottom",
            legend.title = element_text(size = 9, face = "bold"),
            legend.text = element_text(size = 8),
            plot.background = element_rect(fill = plot_theme$background, color = NA),
            panel.background = element_rect(fill = plot_theme$background, color = NA)
        )
    
    return(ggplotly(p, tooltip = c("x", "y", "fill")))
}

# =============================================================================
# Target Mode Specific Plot Functions
# =============================================================================

# Create portfolio ranking plot for target mode
create_portfolio_ranking_plot <- function(expression_data, sample_data, selected_genes, 
                                        data_type = "log2_cpm", plot_theme = NULL, 
                                        enabled_cell_types = NULL) {
    
    if (is.null(selected_genes) || length(selected_genes) == 0) {
        return(create_empty_plot("Select genes to display portfolio ranking", plot_theme))
    }
    
    # Set default theme if none provided
    if (is.null(plot_theme)) {
        plot_theme <- list(
            primary_color = "#DC143C",
            text_secondary = "#666666",
            background = "#FFFFFF",
            text_color = "#000000"
        )
    }
    
    # Get column names
    celltype_col <- if ("PubCelltype" %in% colnames(sample_data)) {
        "PubCelltype"
    } else if ("Celltype" %in% colnames(sample_data)) {
        "Celltype"
    } else if ("celltype" %in% colnames(sample_data)) {
        "celltype"
    } else {
        return(create_empty_plot("Cannot find cell type column in sample data", plot_theme))
    }
    
    sample_col <- if ("PubName" %in% colnames(sample_data)) {
        "PubName"
    } else if ("Name" %in% colnames(sample_data)) {
        "Name"
    } else {
        return(create_empty_plot("Cannot find sample column in sample data", plot_theme))
    }
    
    # Filter sample data by enabled cell types first if provided
    filtered_sample_data <- if (!is.null(enabled_cell_types) && length(enabled_cell_types) > 0) {
        sample_data %>% filter(!!sym(celltype_col) %in% enabled_cell_types)
    } else {
        sample_data
    }
    
    # Filter expression data and join with filtered sample metadata
    plot_data <- expression_data %>%
        filter(
            gene %in% selected_genes,
            data_type == !!data_type
        ) %>%
        left_join(
            filtered_sample_data %>%
                select(all_of(c(sample_col, celltype_col))) %>%
                rename(sample = !!sample_col, celltype = !!celltype_col),
            by = "sample"
        ) %>%
        filter(!is.na(celltype))
    
    if (nrow(plot_data) == 0) {
        return(create_empty_plot("No expression data available for selected genes", plot_theme))
    }
    
    # Define product categories for visual separation and color coding
    real_products <- c("RealDRGx", "RealDRG", "RealMoto", "RealMelo", "RealDHN", "RealSCP")
    hipsc_products <- c("hiPSCMN", "hiPSCMelo_1", "hiPSCMelo_2")
    primary_products <- c("hDRG", "hMelo_1", "hSCP")
    
    # Calculate mean expression by cell type and add category information
    celltype_means <- plot_data %>%
        group_by(celltype) %>%
        summarise(mean_expression = mean(expression, na.rm = TRUE), .groups = "drop") %>%
        mutate(
            product_category = case_when(
                celltype %in% real_products ~ "Real* Products",
                celltype %in% hipsc_products ~ "Human iPSC",
                celltype %in% primary_products ~ "Primary Cells",
                TRUE ~ "Other"
            ),
            # Assign colors based on category
            bar_color = case_when(
                product_category == "Real* Products" ~ "#DC143C",
                product_category == "Human iPSC" ~ "#FF8C00",
                product_category == "Primary Cells" ~ "#228B22",
                TRUE ~ "#808080"
            )
        ) %>%
        arrange(desc(mean_expression))
    
    # Calculate individual gene means by cell type for overlay points
    gene_means <- plot_data %>%
        group_by(celltype, gene) %>%
        summarise(gene_mean_expression = mean(expression, na.rm = TRUE), .groups = "drop")
    
    # Create y-axis label
    y_label <- if (data_type == "log2_cpm") "Mean log2(CPM + 1)" else "Mean VST"
    
    # Create title based on number of selected genes
    title_text <- if (length(selected_genes) == 1) {
        paste("Portfolio Expression Ranking:", selected_genes[1])
    } else {
        paste("Portfolio Expression Ranking (", length(selected_genes), "genes)")
    }
    
    # Create the plot with category-based colors
    p <- plot_ly() %>%
        add_bars(
            data = celltype_means,
            x = ~mean_expression,
            y = ~reorder(celltype, mean_expression),
            orientation = 'h',
            name = "Product Average",
            marker = list(
                color = ~bar_color,
                opacity = 0.8,
                line = list(color = "white", width = 1)
            ),
            hovertemplate = paste0(
                "<b>Product:</b> %{y}<br>",
                "<b>Category:</b> %{customdata}<br>",
                "<b>", y_label, ":</b> %{x:.3f}<br>",
                "<extra></extra>"
            ),
            customdata = ~product_category
        ) %>%
        add_markers(
            data = gene_means,
            x = ~gene_mean_expression,
            y = ~celltype,
            name = "Individual Genes",
            marker = list(
                color = "#666666",
                size = 6,
                opacity = 0.7,
                symbol = "diamond"
            ),
            hovertemplate = paste0(
                "<b>Gene:</b> %{text}<br>",
                "<b>Product:</b> %{y}<br>",
                "<b>", y_label, ":</b> %{x:.3f}<br>",
                "<extra></extra>"
            ),
            text = ~gene
        ) %>%
        layout(
            title = list(
                text = title_text,
                font = list(size = 14, color = plot_theme$text_color)
            ),
            xaxis = list(
                title = y_label,
                titlefont = list(size = 12, color = plot_theme$text_color),
                tickfont = list(size = 10, color = plot_theme$text_color)
            ),
            yaxis = list(
                title = "Anatomic Product",
                titlefont = list(size = 12, color = plot_theme$text_color),
                tickfont = list(size = 10, color = plot_theme$text_color)
            ),
            showlegend = TRUE,
            legend = list(
                x = 0.7, y = 0.1,
                font = list(size = 10, color = plot_theme$text_color)
            ),
            plot_bgcolor = plot_theme$background,
            paper_bgcolor = plot_theme$background,
            margin = list(l = 150, r = 50, t = 50, b = 50)
        ) %>%
        config(displayModeBar = FALSE)
    
    return(p)
}

# Create target heatmap with genes on X-axis and products on Y-axis
create_target_heatmap <- function(expression_data, sample_data, selected_genes, 
                                 data_type = "log2_cpm", plot_theme = NULL,
                                 enabled_cell_types = NULL) {
    
    if (is.null(selected_genes) || length(selected_genes) == 0) {
        return(create_empty_plot("Select genes to display target heatmap", plot_theme))
    }
    
    # Set default theme if none provided
    if (is.null(plot_theme)) {
        plot_theme <- list(
            background = "#FFFFFF",
            text_color = "#000000"
        )
    }
    
    # Get column names
    celltype_col <- if ("PubCelltype" %in% colnames(sample_data)) {
        "PubCelltype"
    } else if ("Celltype" %in% colnames(sample_data)) {
        "Celltype"
    } else if ("celltype" %in% colnames(sample_data)) {
        "celltype"
    } else {
        return(create_empty_plot("Cannot find cell type column in sample data", plot_theme))
    }
    
    sample_col <- if ("PubName" %in% colnames(sample_data)) {
        "PubName"
    } else if ("Name" %in% colnames(sample_data)) {
        "Name"
    } else {
        return(create_empty_plot("Cannot find sample column in sample data", plot_theme))
    }
    
    # Filter sample data by enabled cell types first if provided
    filtered_sample_data <- if (!is.null(enabled_cell_types) && length(enabled_cell_types) > 0) {
        sample_data %>% filter(!!sym(celltype_col) %in% enabled_cell_types)
    } else {
        sample_data
    }
    
    # Filter expression data and join with filtered sample metadata
    plot_data <- expression_data %>%
        filter(
            gene %in% selected_genes,
            data_type == !!data_type
        ) %>%
        left_join(
            filtered_sample_data %>%
                select(all_of(c(sample_col, celltype_col))) %>%
                rename(sample = !!sample_col, celltype = !!celltype_col),
            by = "sample"
        ) %>%
        filter(!is.na(celltype))
    
    if (nrow(plot_data) == 0) {
        return(create_empty_plot("No expression data available for selected genes", plot_theme))
    }
    
    # Calculate mean expression by gene and cell type
    heatmap_data <- plot_data %>%
        group_by(gene, celltype) %>%
        summarise(mean_expr = mean(expression, na.rm = TRUE), .groups = "drop")
    
    # Create matrix for heatmap (genes as columns, cell types as rows)
    heatmap_matrix <- heatmap_data %>%
        pivot_wider(names_from = gene, values_from = mean_expr, values_fill = 0) %>%
        column_to_rownames("celltype") %>%
        as.matrix()
    
    # Create data type label for title and hover
    data_type_label <- if (data_type == "log2_cpm") "log2(CPM + 1)" else "VST"
    
    # Create title
    title_text <- paste("Target Heatmap:", data_type_label)
    
    # Create the heatmap
    p <- plot_ly(
        z = ~heatmap_matrix,
        x = colnames(heatmap_matrix),  # Genes on X-axis
        y = rownames(heatmap_matrix),  # Products (cell types) on Y-axis
        type = "heatmap",
        colorscale = "Viridis",
        hovertemplate = paste0(
            "<b>Gene:</b> %{x}<br>",
            "<b>Product:</b> %{y}<br>",
            "<b>", data_type_label, ":</b> %{z:.3f}<br>",
            "<extra></extra>"
        )
    ) %>%
        layout(
            title = list(
                text = title_text,
                font = list(size = 14, color = plot_theme$text_color)
            ),
            xaxis = list(
                title = "Gene",
                titlefont = list(size = 12, color = plot_theme$text_color),
                tickfont = list(size = 10, color = plot_theme$text_color),
                tickangle = -45
            ),
            yaxis = list(
                title = "Anatomic Product",
                titlefont = list(size = 12, color = plot_theme$text_color),
                tickfont = list(size = 10, color = plot_theme$text_color)
            ),
            plot_bgcolor = plot_theme$background,
            paper_bgcolor = plot_theme$background,
            margin = list(l = 150, r = 50, t = 80, b = 100)
        ) %>%
        config(displayModeBar = TRUE)
    
    return(p)
}

# Create portfolio summary table data
create_portfolio_summary_table <- function(expression_data, sample_data, selected_genes, 
                                          data_type = "log2_cpm", enabled_cell_types = NULL) {
    
    if (is.null(selected_genes) || length(selected_genes) == 0) {
        return(data.frame(Message = "Select genes to display portfolio summary"))
    }
    
    # Get column names
    celltype_col <- if ("PubCelltype" %in% colnames(sample_data)) {
        "PubCelltype"
    } else if ("Celltype" %in% colnames(sample_data)) {
        "Celltype"
    } else if ("celltype" %in% colnames(sample_data)) {
        "celltype"
    } else {
        return(data.frame(Message = "Cannot find cell type column in sample data"))
    }
    
    sample_col <- if ("PubName" %in% colnames(sample_data)) {
        "PubName"
    } else if ("Name" %in% colnames(sample_data)) {
        "Name"
    } else {
        return(data.frame(Message = "Cannot find sample column in sample data"))
    }
    
    # Filter sample data by enabled cell types first if provided
    filtered_sample_data <- if (!is.null(enabled_cell_types) && length(enabled_cell_types) > 0) {
        sample_data %>% filter(!!sym(celltype_col) %in% enabled_cell_types)
    } else {
        sample_data
    }
    
    # Filter expression data and join with filtered sample metadata
    plot_data <- expression_data %>%
        filter(
            gene %in% selected_genes,
            data_type == !!data_type
        ) %>%
        left_join(
            filtered_sample_data %>%
                select(all_of(c(sample_col, celltype_col))) %>%
                rename(sample = !!sample_col, celltype = !!celltype_col),
            by = "sample"
        ) %>%
        filter(!is.na(celltype))
    
    if (nrow(plot_data) == 0) {
        return(data.frame(Message = "No expression data available for selected genes"))
    }
    
    # Define product categories for visual separation
    real_products <- c("RealDRGx", "RealDRG", "RealMoto", "RealMelo", "RealDHN", "RealSCP")
    hipsc_products <- c("hiPSCMN", "hiPSCMelo_1", "hiPSCMelo_2")
    primary_products <- c("hDRG", "hMelo_1", "hSCP")
    
    # Calculate statistics by product
    product_stats <- plot_data %>%
        group_by(celltype) %>%
        summarise(
            n_samples = n_distinct(sample),
            mean_expression = mean(expression, na.rm = TRUE),
            median_expression = median(expression, na.rm = TRUE),
            max_expression = max(expression, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        mutate(
            # Categorize products
            product_category = case_when(
                celltype %in% real_products ~ "Real* Products",
                celltype %in% hipsc_products ~ "Human iPSC", 
                celltype %in% primary_products ~ "Primary Cells",
                TRUE ~ "Other"
            )
        ) %>%
        # Sort by category (Real* first), then by mean expression within category
        arrange(
            factor(product_category, levels = c("Real* Products", "Human iPSC", "Primary Cells", "Other")),
            desc(mean_expression)
        ) %>%
        mutate(
            ranking = row_number(),
            expression_level = case_when(
                mean_expression >= quantile(mean_expression, 0.8) ~ "Very High",
                mean_expression >= quantile(mean_expression, 0.6) ~ "High",
                mean_expression >= quantile(mean_expression, 0.4) ~ "Medium",
                mean_expression >= quantile(mean_expression, 0.2) ~ "Low",
                TRUE ~ "Very Low"
            )
        )
    
    # Create data type label for column names
    data_type_label <- if (data_type == "log2_cpm") "log2(CPM + 1)" else "VST"
    
    # Format the table with improved column names
    result_table <- product_stats %>%
        select(
            Product = celltype,
            Category = product_category,
            Rank = ranking,
            Level = expression_level,
            Samples = n_samples,
            Mean = mean_expression,
            Median = median_expression,
            Max = max_expression
        ) %>%
        mutate(
            Mean = round(Mean, 3),
            Median = round(Median, 3),
            Max = round(Max, 3)
        )
    
    # Update column names with data type - more concise headers
    colnames(result_table)[6:8] <- paste0(c("Mean", "Median", "Max"), " (", data_type_label, ")")
    
    # Return the raw data table - let app.R handle DT formatting
    if (nrow(result_table) > 0) {
        return(result_table)
    } else {
        return(data.frame(Message = "No data available for selected genes and cell types"))
    }
}

# =============================================================================
# Plot Helper Functions
# =============================================================================

# Standardize plot appearance
apply_plot_theme <- function(plot, plot_theme) {
    if (is.null(plot) || is.null(plot_theme)) {
        return(plot)
    }
    
    plot %>%
        layout(
            plot_bgcolor = plot_theme$background,
            paper_bgcolor = plot_theme$background,
            font = list(
                color = plot_theme$text_color,
                size = plot_theme$axis_text_size
            )
        )
}

# Create empty plot with message
create_empty_plot <- function(message = "No data available", plot_theme = NULL) {
    
    if (is.null(plot_theme)) {
        plot_theme <- list(
            background = "#FFFFFF",
            text_color = "#000000"
        )
    }
    
    plot_ly() %>%
        add_annotations(
            text = message,
            x = 0.5,
            y = 0.5,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 16, color = plot_theme$text_color)
        ) %>%
        layout(
            plot_bgcolor = plot_theme$background,
            paper_bgcolor = plot_theme$background,
            showlegend = FALSE,
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
        )
}

# Generate plot subtitle with context information
generate_plot_subtitle <- function(data_type, total_genes, current_genes = NULL, page_info = NULL) {
    
    data_type_label <- if (data_type == "log2_cpm") "log2(CPM + 1)" else "VST"
    
    subtitle_parts <- c(
        paste("Data:", data_type_label),
        paste("Total genes:", total_genes)
    )
    
    if (!is.null(current_genes)) {
        subtitle_parts <- c(subtitle_parts, paste("Showing:", length(current_genes), "genes"))
    }
    
    if (!is.null(page_info)) {
        subtitle_parts <- c(subtitle_parts, page_info)
    }
    
    return(paste(subtitle_parts, collapse = " | "))
}

# =============================================================================
# Plot Validation Functions
# =============================================================================

# Validate plot data structure
validate_plot_data <- function(plot_data, required_columns = c("gene", "expression", "sample")) {
    
    if (is.null(plot_data)) {
        return(list(valid = FALSE, message = "Plot data is NULL"))
    }
    
    if (nrow(plot_data) == 0) {
        return(list(valid = FALSE, message = "Plot data is empty"))
    }
    
    missing_cols <- required_columns[!required_columns %in% colnames(plot_data)]
    
    if (length(missing_cols) > 0) {
        return(list(
            valid = FALSE, 
            message = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
        ))
    }
    
    return(list(valid = TRUE, message = "Plot data is valid"))
}

# Check if genes exist in dataset
validate_gene_selection <- function(selected_genes, available_genes) {
    
    if (is.null(selected_genes) || length(selected_genes) == 0) {
        return(list(valid = FALSE, message = "No genes selected"))
    }
    
    missing_genes <- selected_genes[!selected_genes %in% available_genes]
    available_selected <- selected_genes[selected_genes %in% available_genes]
    
    if (length(available_selected) == 0) {
        return(list(
            valid = FALSE, 
            message = "None of the selected genes are available in the dataset"
        ))
    }
    
    if (length(missing_genes) > 0) {
        warning(paste("Genes not found in dataset:", paste(missing_genes, collapse = ", ")))
    }
    
    return(list(
        valid = TRUE, 
        message = paste("Found", length(available_selected), "of", length(selected_genes), "selected genes"),
        available_genes = available_selected,
        missing_genes = missing_genes
    ))
}
