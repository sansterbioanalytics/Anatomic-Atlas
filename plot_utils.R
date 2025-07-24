# =============================================================================
# Plot Utilities Module for Anatomic RNA Atlas
# Contains plotting functions and visualization helpers
# =============================================================================

# Required libraries for this module
suppressPackageStartupMessages({
    library(ggplot2)
    library(plotly)
    library(dplyr)
    library(tidyr)
    library(viridis)
    library(RColorBrewer)
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
