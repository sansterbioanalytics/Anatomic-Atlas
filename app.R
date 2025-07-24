# =============================================================================
# Anatomic RNA Atlas - Interactive Visualization Application
# Modular Shiny Application for RNA Expression Analysis
# =============================================================================
# TODO: title of the web nav is <div> logo class something, should be Anatomic Atlas
# TODO: small visual tweaks like 
# TODO: error on startup RNA89s

# Load required libraries
suppressPackageStartupMessages({
    library(shiny)
    library(shinydashboard)
    library(DT)
    library(plotly)
    library(ggplot2)
    library(dplyr)
    library(rlang)
    library(readr)
    library(tidyr)
    library(viridis)
    library(RColorBrewer)
    library(shinycssloaders)
    library(corrr)
    library(networkD3)
})

# Source modular components
source("data_utils.R")        # Data loading and processing functions
source("theme_config.R")      # Theme configuration and CSS generation
source("plot_utils.R")        # Plotting and visualization functions
source("ui_components.R")     # Reusable UI components
source("server_utils.R")      # Server helper functions
source("coexpression_analysis.R") # Co-expression analysis functions

# =============================================================================
# UI Definition
# =============================================================================

ui <- dashboardPage(
    skin = app_theme$dashboard_skin,
    create_app_header(app_theme),
    create_app_sidebar(app_theme),
    dashboardBody(
        # Apply custom theme CSS
        tags$head(
            tags$style(generate_app_css(app_theme)),
            tags$script(generate_app_javascript())
        ),

        # Loading overlay (initially visible)
        create_loading_overlay(app_theme),

        # Main content layout
        create_main_layout(app_theme),
        
        # Bottom section layout
        create_bottom_layout()
    )
)

# =============================================================================
# Server Logic
# =============================================================================

server <- function(input, output, session) {
    # Load data on startup with progress
    atlas_data <- NULL
    gene_sets <- load_gene_sets()

    # Reactive values
    values <- reactiveValues(
        sample_data = NULL,
        expression_data = NULL,
        contrast_data = NULL,
        uploaded_genes = NULL,
        data_loaded = FALSE,
        gene_page = 1  # Current page for gene pagination (always 10 genes per page)
    )

    # Load data with progress indication
    observe({
        tryCatch(
            {
                # Use JavaScript to show loading progress
                session$sendCustomMessage("updateProgress", list(percent = 10, message = "Starting data load..."))

                atlas_data <- load_atlas_data(progress = list(
                    set = function(message = "", value = 0) {
                        session$sendCustomMessage("updateProgress", list(
                            percent = value * 100,
                            message = message
                        ))
                    }
                ))

                if (is.null(atlas_data$sample_data) || is.null(atlas_data$expression_data)) {
                    session$sendCustomMessage("updateProgress", list(
                        percent = 100,
                        message = "Error: Could not load data files"
                    ))
                    showNotification("Failed to load atlas data. Please check data files.", type = "error", duration = NULL)
                    return()
                }

                values$sample_data <- atlas_data$sample_data
                values$expression_data <- atlas_data$expression_data
                values$data_loaded <- TRUE

                # Hide loading overlay
                session$sendCustomMessage("hideLoading", list())
            },
            error = function(e) {
                session$sendCustomMessage("updateProgress", list(
                    percent = 100,
                    message = paste("Error loading data:", e$message)
                ))
                showNotification(paste("Error loading data:", e$message), type = "error", duration = NULL)
            }
        )
    })

    # Add JavaScript handlers for loading progress
    observe({
        session$sendCustomMessage("addProgressHandlers", list())
    })

    # Reactive expression for pre-filtered contrast data
    contrast_data <- reactive({
        req(values$expression_data, values$sample_data, input$group1, input$group2, input$data_type)

        create_contrast_data(
            values$expression_data,
            values$sample_data,
            input$group1,
            input$group2,
            input$data_type
        )
    })

    # Update choices when data is loaded
    observe({
        if (!is.null(values$sample_data)) {
            # Use helper function to determine the cell type column name
            celltype_col <- get_celltype_column(values$sample_data)

            if (!is.null(celltype_col)) {
                cell_types <- unique(values$sample_data[[celltype_col]])

                updateSelectInput(session, "group1",
                    choices = cell_types,
                    selected = cell_types[1]
                )

                updateSelectInput(session, "group2",
                    choices = cell_types,
                    selected = if (length(cell_types) > 1) cell_types[2] else cell_types[1]
                )
            }
        }

        # Update gene choices
        if (!is.null(values$expression_data)) {
            genes <- unique(values$expression_data$gene)
            # Set default to "SCN11A" if present, otherwise NULL
            default_gene <- if ("SCN11A" %in% genes) "SCN11A" else NULL
            updateSelectizeInput(session, "selected_genes",
                choices = genes,
                selected = default_gene,
                server = TRUE
            )
        }

        # Update gene set choices
        if (length(gene_sets) > 0) {
            gene_set_choices <- c("Custom Genes" = "Custom Genes", names(gene_sets))
            updateSelectInput(session, "gene_set_selection",
                choices = gene_set_choices,
                selected = "Custom Genes"
            )
        }
    })

    # Sync co-expression data type with main data type selector
    observe({
        if (!is.null(input$data_type)) {
            updateSelectInput(session, "coexpression_data_type",
                selected = input$data_type
            )
        }
    })

    # Reactive expression for currently selected genes (unified across all plots)
    current_genes <- reactive({
        if (input$gene_set_selection == "Custom Genes") {
            return(input$selected_genes)
        } else if (input$gene_set_selection == "Uploaded Gene Set") {
            return(values$uploaded_genes)
        } else {
            # Using a predefined gene set
            return(gene_sets[[input$gene_set_selection]])
        }
    })
    
    # Reactive expression for genes on current page (for expression plots)
    current_page_genes <- reactive({
        all_genes <- current_genes()
        if (is.null(all_genes) || length(all_genes) == 0) {
            return(NULL)
        }
        
        # Calculate pagination with fixed 10 genes per page
        genes_per_page <- 10
        total_genes <- length(all_genes)
        start_idx <- (values$gene_page - 1) * genes_per_page + 1
        end_idx <- min(values$gene_page * genes_per_page, total_genes)
        
        if (start_idx > total_genes) {
            # Reset to page 1 if current page is out of bounds
            values$gene_page <- 1
            start_idx <- 1
            end_idx <- min(genes_per_page, total_genes)
        }
        
        return(all_genes[start_idx:end_idx])
    })
    
    # Observer to reset page when gene selection changes
    observe({
        current_genes()  # Trigger when genes change
        values$gene_page <- 1  # Reset to first page
    })
    
    # Simple increment/decrement controls
    observeEvent(input$next_genes, {
        all_genes <- current_genes()
        if (!is.null(all_genes) && length(all_genes) > 0) {
            genes_per_page <- 10
            max_pages <- ceiling(length(all_genes) / genes_per_page)
            if (values$gene_page < max_pages) {
                values$gene_page <- values$gene_page + 1
            }
        }
    })
    
    observeEvent(input$prev_genes, {
        if (values$gene_page > 1) {
            values$gene_page <- values$gene_page - 1
        }
    })
    
    # Simple pagination info UI
    output$gene_pagination_info <- renderUI({
        all_genes <- current_genes()
        if (is.null(all_genes) || length(all_genes) <= 10) {
            return(span("", style = "color: #666;"))
        }
        
        genes_per_page <- 10
        total_genes <- length(all_genes)
        max_pages <- ceiling(total_genes / genes_per_page)
        start_idx <- (values$gene_page - 1) * genes_per_page + 1
        end_idx <- min(values$gene_page * genes_per_page, total_genes)
        
        span(
            paste0("Showing genes ", start_idx, "-", end_idx, " of ", total_genes, 
                   " (Page ", values$gene_page, " of ", max_pages, ")"),
            style = "color: #666; font-size: 12px;"
        )
    })
    
    # Output to control conditional panel for gene navigation
    output$show_gene_pagination <- reactive({
        all_genes <- current_genes()
        !is.null(all_genes) && length(all_genes) > 10
    })
    outputOptions(output, "show_gene_pagination", suspendWhenHidden = FALSE)
    
    # Display selected genes information
    output$selected_genes_display <- renderUI({
        genes <- current_genes()
        
        if (is.null(genes) || length(genes) == 0) {
            return(div(
                style = paste0("color: ", app_theme$text_light, "; font-size: ", app_theme$font_size_small, ";"),
                "No genes selected"
            ))
        }
        
        gene_source <- if (input$gene_set_selection == "Custom Genes") {
            "Custom selection"
        } else if (input$gene_set_selection == "Uploaded Gene Set") {
            "Uploaded gene set"
        } else {
            input$gene_set_selection
        }
        
        div(
            style = paste0("color: ", app_theme$text_white, "; font-size: ", app_theme$font_size_small, "; margin-bottom: ", app_theme$spacing_sm, ";"),
            tags$strong(paste(length(genes), "genes selected")),
            br(),
            tags$em(paste("Source:", gene_source))
        )
    })

    # Handle file upload for custom gene sets
    observeEvent(input$gene_file_upload, {
        req(input$gene_file_upload)

        file_path <- input$gene_file_upload$datapath
        file_ext <- tools::file_ext(input$gene_file_upload$name)

        tryCatch(
            {
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
                        values$uploaded_genes <- gene_names

                        # Update gene set choices to include uploaded set
                        current_choices <- c("Custom Genes" = "Custom Genes", names(gene_sets))
                        if (!is.null(values$uploaded_genes)) {
                            current_choices <- c(current_choices, "Uploaded Gene Set" = "Uploaded Gene Set")
                        }

                        updateSelectInput(session, "gene_set_selection",
                            choices = current_choices,
                            selected = "Uploaded Gene Set"
                        )

                        showNotification(
                            paste("Successfully loaded", length(gene_names), "genes from uploaded file"),
                            type = "success"
                        )
                    } else {
                        showNotification("No valid gene names found in uploaded file", type = "error")
                    }
                } else {
                    showNotification("File appears to be empty or invalid", type = "error")
                }
            },
            error = function(e) {
                showNotification(paste("Error reading file:", e$message), type = "error")
            }
        )
    })

    # Expression boxplot for selected genes, split into three groups
    output$expression_histogram <- renderPlotly({
        # Get pre-filtered contrast data
        plot_data_base <- contrast_data()

        if (is.null(plot_data_base)) {
            return(plotly_empty(type = "scatter", mode = "markers") %>%
                add_annotations(
                    text = "Unable to load data for selected groups",
                    showarrow = FALSE
                ))
        }

        # Use current page genes for plotting (paginated)
        genes_to_plot <- current_page_genes()
        
        # Only plot if genes are selected
        if (is.null(genes_to_plot) || length(genes_to_plot) == 0) {
            return(plotly_empty(type = "scatter", mode = "markers") %>%
                add_annotations(
                    text = "Select genes to display expression plots",
                    showarrow = FALSE
                ))
        }

        # Create Y-axis label based on data type
        y_label <- if (input$data_type == "log2_cpm") {
            "Expression Level (log2(CPM + 1))"
        } else {
            "Expression Level (VST)"
        }

        # Filter for selected genes from pre-filtered data
        plot_data <- plot_data_base %>%
            filter(gene %in% genes_to_plot)

        # Check if we have data
        if (nrow(plot_data) == 0) {
            return(plotly_empty(type = "scatter", mode = "markers") %>%
                add_annotations(
                    text = "No expression data available for selected genes",
                    showarrow = FALSE
                ))
        }

        # Create subtitle with data type information and pagination
        data_type_label <- if (input$data_type == "log2_cpm") "log2(CPM + 1)" else "VST"
        all_selected_genes <- current_genes()
        genes_per_page <- 10  # Fixed at 10 genes per page
        
        subtitle_parts <- c(
            paste0("Data: ", data_type_label)
        )
        
        # Add pagination info if we have multiple pages
        if (!is.null(all_selected_genes) && length(all_selected_genes) > genes_per_page) {
            total_genes <- length(all_selected_genes)
            max_pages <- ceiling(total_genes / genes_per_page)
            start_idx <- (values$gene_page - 1) * genes_per_page + 1
            end_idx <- min(values$gene_page * genes_per_page, total_genes)
            
            subtitle_parts <- c(
                subtitle_parts,
                paste0("Showing ", length(genes_to_plot), " of ", total_genes, " genes (Page ", values$gene_page, "/", max_pages, ")")
            )
        } else if (!is.null(genes_to_plot)) {
            subtitle_parts <- c(
                subtitle_parts,
                paste0("Genes: ", paste(genes_to_plot, collapse = ", "))
            )
        }
        
        subtitle_text <- paste(subtitle_parts, collapse = " | ")

        # Create boxplot, faceted by gene
        plot_theme <- get_plot_theme(app_theme)

        p <- ggplot(plot_data, aes(x = violin_group, y = expression, fill = violin_group, text = sample)) +
            geom_boxplot(alpha = 0.7, outlier.shape = NA) +
            {
                if (input$show_points) {
                    geom_jitter(
                        width = 0.15, height = 0,
                        size = 1.2, alpha = 0.6, color = app_theme$text_secondary
                    )
                }
            } +
            scale_fill_manual(
                name = "Sample Group",
                values = setNames(
                    plot_theme$colors,
                    c(input$group1, input$group2, "Other")
                )
            ) +
            facet_wrap(~gene, scales = "free_y", nrow = 2) +
            labs(
                title = "Expression Distribution of Selected Genes",
                subtitle = subtitle_text,
                x = NULL,
                y = y_label
            ) +
            theme_minimal() +
            theme(
                legend.position = "bottom",
                plot.title = element_text(size = plot_theme$title_size, face = "bold", color = plot_theme$text_color),
                plot.subtitle = element_text(size = plot_theme$subtitle_size, color = app_theme$text_light),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = plot_theme$axis_text_size, color = plot_theme$text_color),
                axis.text.y = element_text(color = plot_theme$text_color),
                axis.title = element_text(color = plot_theme$text_color),
                strip.text = element_text(color = plot_theme$text_color, face = "bold"),
                panel.background = element_rect(fill = plot_theme$background, color = NA),
                plot.background = element_rect(fill = plot_theme$background, color = NA),
                panel.grid.major = element_line(color = plot_theme$grid_color, linewidth = 0.3),
                panel.grid.minor = element_line(color = plot_theme$grid_color, linewidth = 0.1),
                legend.text = element_text(size = plot_theme$legend_text_size, color = plot_theme$text_color)
            )

        # Convert to plotly with better performance settings - include text in tooltip
        tryCatch(
            {
                ggplotly(p, tooltip = c("x", "y", "text")) %>%
                    layout(showlegend = TRUE) %>%
                    config(displayModeBar = TRUE, displaylogo = FALSE)
            },
            error = function(e) {
                # If plot fails, return error message
                plotly_empty() %>%
                    add_annotations(
                        text = paste("Plot rendering failed. Please try selecting different genes or groups.\nError:", e$message),
                        showarrow = FALSE
                    )
            }
        )
    })

    # Single gene set heatmap
    output$gene_set_heatmap <- renderPlotly({
        # Use unified gene selection
        selected_genes <- current_genes()
        
        if (is.null(selected_genes) || length(selected_genes) == 0) {
            return(plotly_empty(type = "scatter", mode = "markers") %>%
                add_annotations(
                    text = "Select genes or upload a gene set to display heatmap",
                    showarrow = FALSE
                ))
        }

        # Filter for genes that exist in the dataset
        available_genes <- intersect(selected_genes, unique(values$expression_data$gene))

        if (length(available_genes) == 0) {
            return(plotly_empty(type = "scatter", mode = "markers") %>%
                add_annotations(
                    text = paste("No genes from selection found in dataset"),
                    showarrow = FALSE
                ))
        }

        # Get expression data for selected groups and genes, filtered by data type
        plot_data_base <- contrast_data()

        if (is.null(plot_data_base)) {
            return(plotly_empty(type = "scatter", mode = "markers") %>%
                add_annotations(
                    text = "Unable to load data for selected groups",
                    showarrow = FALSE
                ))
        }

        heatmap_data <- plot_data_base %>%
            filter(gene %in% available_genes) %>%
            group_by(gene, celltype) %>%
            summarise(mean_expr = mean(expression, na.rm = TRUE), .groups = "drop") %>%
            pivot_wider(names_from = celltype, values_from = mean_expr, values_fill = 0)

        if (nrow(heatmap_data) == 0) {
            return(plotly_empty(type = "scatter", mode = "markers") %>%
                add_annotations(
                    text = "No expression data available for selected groups",
                    showarrow = FALSE
                ))
        }

        # Prepare matrix for heatmap
        gene_names <- heatmap_data$gene
        expr_matrix <- as.matrix(heatmap_data[, -1])
        rownames(expr_matrix) <- gene_names

        # Create title based on gene source
        gene_source <- if (input$gene_set_selection == "Custom Genes") {
            "Custom Gene Selection"
        } else if (input$gene_set_selection == "Uploaded Gene Set") {
            "Uploaded Gene Set"
        } else {
            input$gene_set_selection
        }
        
        data_type_label <- if (input$data_type == "log2_cpm") "log2(CPM + 1)" else "VST"
        heatmap_title <- paste0(gene_source, " Heatmap (", data_type_label, ")")

        # Create y-axis label based on data type
        y_axis_subtitle <- if (input$data_type == "log2_cpm") {
            "Mean log2(CPM + 1)"
        } else {
            "Mean VST"
        }

        # Create plotly heatmap
        plot_ly(
            z = expr_matrix,
            x = colnames(expr_matrix),
            y = rownames(expr_matrix),
            type = "heatmap",
            colorscale = "Viridis",
            showscale = TRUE,
            hovertemplate = paste0("Gene: %{y}<br>Group: %{x}<br>", y_axis_subtitle, ": %{z:.2f}<extra></extra>")
        ) %>%
            layout(
                title = heatmap_title,
                xaxis = list(title = "Cell Type", side = "bottom"),
                yaxis = list(title = "Genes"),
                margin = list(l = 100, r = 50, t = 80, b = 50)
            )
    })

    # Render dynamic comparison title
    output$contrast_title <- renderText({
        if (is.null(input$group1) || is.null(input$group2)) {
            return("Select groups for comparison")
        }
        paste("Comparing:", input$group1, "vs", input$group2)
    })

    # Add comprehensive data summary output as UI components
    output$data_summary_ui <- renderUI({
        if (is.null(values$sample_data) || is.null(values$expression_data)) {
            return(div(
                class = "stat-card",
                div("No data loaded", class = "stat-card-content")
            ))
        }

        req(input$group1, input$group2, input$data_type)

        # Determine celltype column
        celltype_col <- NULL
        if ("PubCelltype" %in% colnames(values$sample_data)) {
            celltype_col <- "PubCelltype"
        } else if ("Celltype" %in% colnames(values$sample_data)) {
            celltype_col <- "Celltype"
        } else if ("celltype" %in% colnames(values$sample_data)) {
            celltype_col <- "celltype"
        }

        if (is.null(celltype_col)) {
            return(div(
                class = "stat-card",
                div("Cell type information not available", class = "stat-card-content")
            ))
        }

        sample_col <- if ("PubName" %in% colnames(values$sample_data)) "PubName" else "Name"

        # Basic counts
        group1_count <- sum(values$sample_data[[celltype_col]] == input$group1, na.rm = TRUE)
        group2_count <- sum(values$sample_data[[celltype_col]] == input$group2, na.rm = TRUE)
        total_genes <- length(unique(values$expression_data$gene))

        data_type_label <- if (input$data_type == "log2_cpm") "log2(CPM+1)" else "VST"

        # Get samples for the two contrast groups
        group1_samples <- values$sample_data[values$sample_data[[celltype_col]] == input$group1, ][[sample_col]]
        group2_samples <- values$sample_data[values$sample_data[[celltype_col]] == input$group2, ][[sample_col]]

        # Get expression data for contrast samples and current data type
        contrast_expression <- values$expression_data %>%
            filter(
                sample %in% c(group1_samples, group2_samples),
                data_type == input$data_type
            )

        if (nrow(contrast_expression) == 0) {
            return(div(
                class = "stat-card",
                div("No expression data available for selected groups", class = "stat-card-content")
            ))
        }

        # Create UI elements
        ui_elements <- list()

        # Basic sample counts card
        ui_elements[[1]] <- div(
            class = "stat-card",
            div("Sample Counts", class = "stat-card-header"),
            div(
                class = "stat-card-content",
                div(
                    class = "stat-item",
                    span(paste("Group 1 (", input$group1, ")"), class = "stat-label"),
                    span(paste(group1_count, "samples"), class = "stat-value")
                ),
                div(
                    class = "stat-item",
                    span(paste("Group 2 (", input$group2, ")"), class = "stat-label"),
                    span(paste(group2_count, "samples"), class = "stat-value")
                ),
                div(
                    class = "stat-item",
                    span("Total genes", class = "stat-label"),
                    span(format(total_genes, big.mark = ","), class = "stat-value")
                )
                # Could add some more things here
            )
        )

        # Gene-specific or overall statistics
        if (length(input$selected_genes) > 0) {
            # Show gene-specific statistics
            for (i in seq_along(input$selected_genes)) {
                gene <- input$selected_genes[i]

                # Get expression data for this specific gene
                gene_expr <- contrast_expression %>%
                    filter(gene == !!gene) %>%
                    left_join(
                        values$sample_data %>%
                            select(all_of(c(sample_col, celltype_col))) %>%
                            rename(sample = !!sample_col, group = !!celltype_col),
                        by = "sample"
                    )

                if (nrow(gene_expr) == 0) {
                    ui_elements[[i + 1]] <- div(
                        class = "stat-card",
                        div(gene, class = "stat-card-header"),
                        div("No data available", class = "stat-card-content")
                    )
                    next
                }

                # Calculate statistics by group
                group_stats <- gene_expr %>%
                    group_by(group) %>%
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
                    group_name <- group_stats$group[j]
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

                ui_elements[[i + 1]] <- div(
                    class = "stat-card",
                    div(paste(gene, "(", data_type_label, ")"), class = "stat-card-header"),
                    div(class = "stat-card-content", gene_card_content)
                )
            }
        } else {
            # Show overall dataset statistics for the contrast groups
            expr_values <- contrast_expression$expression
            quantiles <- quantile(expr_values, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
            mean_expr <- mean(expr_values, na.rm = TRUE)
            zero_count <- sum(expr_values <= 0.1, na.rm = TRUE)
            total_measurements <- length(expr_values)
            zero_percent <- round((zero_count / total_measurements) * 100, 1)

            ui_elements[[2]] <- div(
                class = "stat-card",
                div(paste("Expression Distribution (", data_type_label, ")"), class = "stat-card-header"),
                div(
                    class = "stat-card-content",
                    div(
                        class = "stat-item",
                        span("Mean expression", class = "stat-label"),
                        span(round(mean_expr, 3), class = "stat-value")
                    ),
                    div(
                        class = "stat-item",
                        span("Q5 (5th percentile)", class = "stat-label"),
                        span(round(quantiles[1], 3), class = "stat-value")
                    ),
                    div(
                        class = "stat-item",
                        span("Q25 (25th percentile)", class = "stat-label"),
                        span(round(quantiles[2], 3), class = "stat-value")
                    ),
                    div(
                        class = "stat-item",
                        span("Q50 (median)", class = "stat-label"),
                        span(round(quantiles[3], 3), class = "stat-value")
                    ),
                    div(
                        class = "stat-item",
                        span("Q75 (75th percentile)", class = "stat-label"),
                        span(round(quantiles[4], 3), class = "stat-value")
                    ),
                    div(
                        class = "stat-item",
                        span("Q95 (95th percentile)", class = "stat-label"),
                        span(round(quantiles[5], 3), class = "stat-value")
                    ),
                    div(
                        class = "stat-item",
                        span("Low expression (<= 0.1)", class = "stat-label"),
                        span(paste0(format(zero_count, big.mark = ","), " (", zero_percent, "%)"), class = "stat-value")
                    )
                )
            )
        }

        return(do.call(tagList, ui_elements))
    })

    # Sample counts UI component
    output$sample_counts_ui <- renderUI({
        if (is.null(values$sample_data) || is.null(values$expression_data)) {
            return(div(
                class = "stat-card",
                div("No data loaded", class = "stat-card-content")
            ))
        }

        req(input$group1, input$group2)

        # Use helper function to determine the cell type column
        celltype_col <- get_celltype_column(values$sample_data)

        if (is.null(celltype_col)) {
            return(div(
                class = "stat-card",
                div("Cell type information not available", class = "stat-card-content")
            ))
        }

        # Basic counts
        group1_count <- sum(values$sample_data[[celltype_col]] == input$group1, na.rm = TRUE)
        group2_count <- sum(values$sample_data[[celltype_col]] == input$group2, na.rm = TRUE)
        total_genes <- length(unique(values$expression_data$gene))

        # Basic sample counts card
        div(
            class = "stat-card",
            div("Sample Counts", class = "stat-card-header"),
            div(
                class = "stat-card-content",
                div(
                    class = "stat-item",
                    span(paste("Group 1 (", input$group1, ")"), class = "stat-label"),
                    span(paste(group1_count, "samples"), class = "stat-value")
                ),
                div(
                    class = "stat-item",
                    span(paste("Group 2 (", input$group2, ")"), class = "stat-label"),
                    span(paste(group2_count, "samples"), class = "stat-value")
                ),
                div(
                    class = "stat-item",
                    span("Total genes", class = "stat-label"),
                    span(format(total_genes, big.mark = ","), class = "stat-value")
                )
            )
        )
    })

    # Gene statistics datatable
    output$gene_stats_table <- DT::renderDataTable({
        # Use unified gene selection
        selected_genes <- current_genes()
        
        if (is.null(selected_genes) || length(selected_genes) == 0) {
            return(data.frame(Message = "No genes selected"))
        }

        # Get pre-filtered contrast data
        contrast_expression <- contrast_data()

        if (is.null(contrast_expression)) {
            return(data.frame(Message = "Unable to load data for selected groups"))
        }

        # Filter for selected genes
        contrast_expression <- contrast_expression %>%
            filter(gene %in% selected_genes)

        if (nrow(contrast_expression) == 0) {
            return(data.frame(Message = "No expression data available for selected genes"))
        }

        data_type_label <- if (input$data_type == "log2_cpm") "log2(CPM+1)" else "VST"

        # Calculate statistics for each gene
        gene_stats_list <- list()

        for (gene in selected_genes) {
            # Get expression data for this specific gene
            gene_expr <- contrast_expression %>%
                filter(gene == !!gene)

            if (nrow(gene_expr) == 0) {
                gene_stats_list[[gene]] <- data.frame(
                    Gene = gene,
                    Group = c(input$group1, input$group2),
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

        # Create DT with formatting
        DT::datatable(
            all_stats,
            options = list(
                pageLength = 10,
                scrollY = "150px",
                scrollCollapse = TRUE,
                dom = "tip",
                ordering = TRUE,
                order = list(list(0, "asc"), list(1, "asc"))
            ),
            rownames = FALSE,
            class = "compact stripe hover",
            selection = "none"
        ) %>%
            DT::formatStyle(
                columns = 1:ncol(all_stats),
                fontSize = "11px"
            ) %>%
            DT::formatStyle(
                "Gene",
                fontWeight = "bold",
                color = app_theme$primary_color
            ) %>%
            DT::formatStyle(
                "Group",
                backgroundColor = DT::styleEqual(
                    c(input$group1, input$group2),
                    c("rgba(220, 20, 60, 0.1)", "rgba(31, 98, 165, 0.1)")
                )
            )
    })

    # Overall statistics UI component (when no genes selected)
    output$overall_stats_ui <- renderUI({
        # Get pre-filtered contrast data
        contrast_expression <- contrast_data()

        if (is.null(contrast_expression)) {
            return(div(
                class = "stat-card",
                div("Unable to load data for selected groups", class = "stat-card-content")
            ))
        }

        data_type_label <- if (input$data_type == "log2_cpm") "log2(CPM+1)" else "VST"

        # Show overall dataset statistics for the contrast groups
        expr_values <- contrast_expression$expression
        quantiles <- quantile(expr_values, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
        mean_expr <- mean(expr_values, na.rm = TRUE)
        zero_count <- sum(expr_values <= 0.1, na.rm = TRUE)
        total_measurements <- length(expr_values)
        zero_percent <- round((zero_count / total_measurements) * 100, 1)

        div(
            class = "stat-card",
            div(paste("Expression Distribution (", data_type_label, ")"), class = "stat-card-header"),
            div(
                class = "stat-card-content",
                div(
                    class = "stat-item",
                    span("Mean expression", class = "stat-label"),
                    span(round(mean_expr, 3), class = "stat-value")
                ),
                div(
                    class = "stat-item",
                    span("Q5 (5th percentile)", class = "stat-label"),
                    span(round(quantiles[1], 3), class = "stat-value")
                ),
                div(
                    class = "stat-item",
                    span("Q25 (25th percentile)", class = "stat-label"),
                    span(round(quantiles[2], 3), class = "stat-value")
                ),
                div(
                    class = "stat-item",
                    span("Q50 (median)", class = "stat-label"),
                    span(round(quantiles[3], 3), class = "stat-value")
                ),
                div(
                    class = "stat-item",
                    span("Q75 (75th percentile)", class = "stat-label"),
                    span(round(quantiles[4], 3), class = "stat-value")
                ),
                div(
                    class = "stat-item",
                    span("Q95 (95th percentile)", class = "stat-label"),
                    span(round(quantiles[5], 3), class = "stat-value")
                ),
                div(
                    class = "stat-item",
                    span("Low expression (<= 0.1)", class = "stat-label"),
                    span(paste0(format(zero_count, big.mark = ","), " (", zero_percent, "%)"), class = "stat-value")
                )
            )
        )
    })

    # Add group expression ranking barplot
    output$group_expression_barplot <- renderPlotly({
        req(values$expression_data, values$sample_data, input$data_type)
        
        # Use unified gene selection
        selected_genes <- current_genes()
        
        if (is.null(selected_genes) || length(selected_genes) == 0) {
            return(plotly_empty(type = "scatter", mode = "markers") %>%
                add_annotations(
                    text = "Select genes to display group expression plot",
                    showarrow = FALSE
                ))
        }

        # Use helper functions to determine column names
        celltype_col <- get_celltype_column(values$sample_data)
        sample_col <- get_sample_column(values$sample_data)

        if (is.null(celltype_col) || is.null(sample_col)) {
            return(plotly_empty() %>%
                add_annotations(
                    text = "Unable to determine cell type or sample columns",
                    showarrow = FALSE
                ))
        }

        # Join and filter for selected genes and data type
        plot_data <- values$expression_data %>%
            filter(
                gene %in% selected_genes,
                data_type == input$data_type
            ) %>%
            left_join(
                values$sample_data %>%
                    select(all_of(c(sample_col, celltype_col))) %>%
                    rename(sample = !!sample_col, celltype = !!celltype_col),
                by = "sample"
            )

        # Calculate mean expression by group (celltype)
        group_means <- plot_data %>%
            group_by(celltype) %>%
            summarise(mean_expression = mean(expression, na.rm = TRUE), .groups = "drop") %>%
            arrange(desc(mean_expression))

        # Calculate individual gene means by group for points overlay
        gene_means <- plot_data %>%
            group_by(celltype, gene) %>%
            summarise(gene_mean_expression = mean(expression, na.rm = TRUE), .groups = "drop")

        # Create dynamic title based on selected genes
        title_text <- if (length(selected_genes) == 1) {
            paste("Mean Expression by Group:", selected_genes[1])
        } else {
            paste("Mean Expression by Group (", length(selected_genes), "genes)")
        }

        # Create y-axis label
        y_label <- if (input$data_type == "log2_cpm") "Mean log2(CPM + 1)" else "Mean VST"

        # Create the base bar plot
        p <- plot_ly() %>%
            add_bars(
                x = group_means$mean_expression,
                y = reorder(group_means$celltype, group_means$mean_expression),
                orientation = 'h',
                name = "Group Average",
                marker = list(color = app_theme$primary_color, opacity = 0.7),
                hovertemplate = paste0(
                    "<b>Group:</b> %{y}<br>",
                    "<b>", y_label, ":</b> %{x:.3f}<br>",
                    "<extra></extra>"
                )
            ) %>%
            add_markers(
                data = gene_means,
                x = ~gene_mean_expression,
                y = ~celltype,
                name = "Individual Genes",
                marker = list(
                    color = app_theme$text_secondary,
                    size = 6,
                    opacity = 0.8
                ),
                hovertemplate = paste0(
                    "<b>Gene:</b> %{text}<br>",
                    "<b>Group:</b> %{y}<br>",
                    "<b>", y_label, ":</b> %{x:.3f}<br>",
                    "<extra></extra>"
                ),
                text = ~gene
            ) %>%
            layout(
                title = list(
                    text = title_text,
                    font = list(size = 14, color = app_theme$text_primary)
                ),
                xaxis = list(
                    title = y_label,
                    titlefont = list(size = 12, color = app_theme$text_primary),
                    tickfont = list(size = 10, color = app_theme$text_primary)
                ),
                yaxis = list(
                    title = "Group",
                    titlefont = list(size = 12, color = app_theme$text_primary),
                    tickfont = list(size = 10, color = app_theme$text_primary)
                ),
                showlegend = TRUE,
                legend = list(
                    x = 0.7, y = 0.1,
                    font = list(size = 10, color = app_theme$text_primary)
                ),
                plot_bgcolor = app_theme$background_white,
                paper_bgcolor = app_theme$background_white,
                margin = list(l = 120, r = 50, t = 50, b = 50)
            ) %>%
            config(displayModeBar = FALSE)

        return(p)
    })

    # Dynamic title for expression table
    output$expression_table_title <- renderText({
        # Use unified gene selection
        selected_genes <- current_genes()
        
        if (!is.null(selected_genes) && length(selected_genes) > 0) {
            if (length(selected_genes) == 1) {
                paste("Expression Data for", selected_genes[1])
            } else {
                paste("Expression Data for", length(selected_genes), "Selected Genes")
            }
        } else {
            "Expression Data (First Gene Alphabetically)"
        }
    })

    # Expression data table
    output$expression_table <- DT::renderDataTable({
        # Use unified gene selection
        selected_genes <- current_genes()
        
        # If no genes selected, show first gene alphabetically
        genes_to_show <- if (!is.null(selected_genes) && length(selected_genes) > 0) {
            selected_genes
        } else {
            # Get first gene alphabetically as default
            all_genes <- sort(unique(values$expression_data$gene))
            head(all_genes, 1)
        }

        # Get pre-filtered contrast data
        contrast_expression <- contrast_data()

        if (is.null(contrast_expression)) {
            return(data.frame(Message = "Unable to load data for selected groups"))
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
            message_text <- if (length(input$selected_genes) > 0) {
                "No expression data found for selected genes in contrast groups"
            } else {
                "No expression data available"
            }
            return(data.frame(Message = message_text))
        }

        # Create data type label for column names
        data_type_label <- if (input$data_type == "log2_cpm") " (log2 CPM)" else " (VST)"

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

        # Create DT with formatting and error handling
        tryCatch({
            dt <- DT::datatable(
                expression_subset,
                options = list(
                    pageLength = 25,
                    scrollX = TRUE,
                    scrollY = "600px",
                    scrollCollapse = TRUE,
                    dom = "Bfrtip",
                    buttons = c("copy", "csv", "excel"),
                    columnDefs = list(
                        list(className = "dt-center", targets = "_all"),
                        list(width = "120px", targets = c(0, 1)) # Fixed width for Sample and Group columns
                    )
                ),
                rownames = FALSE,
                class = "compact stripe hover",
                extensions = "Buttons"
            )
            # Only apply formatStyle if 'Group' column exists
            if ("Group" %in% colnames(expression_subset)) {
                dt <- dt %>% DT::formatStyle(
                    "Group",
                    backgroundColor = DT::styleEqual(
                        c(input$group1, input$group2),
                        c(
                            paste0("rgba(", paste(col2rgb(app_theme$plot_group1), collapse = ","), ",0.1)"),
                            paste0("rgba(", paste(col2rgb(app_theme$plot_group2), collapse = ","), ",0.1)")
                        )
                    )
                )
            }
            dt
        }, error = function(e) {
            # If DataTable creation fails, return a simple data frame
            warning(paste("DataTable creation failed:", e$message))
            return(data.frame(
                Message = paste("Table display error. Data available but formatting failed:", e$message)
            ))
        })
    })

    # Download handler
    output$download_plot <- downloadHandler(
        filename = function() {
            data_type_label <- if (input$data_type == "log2_cpm") "CPM" else "VST"
            paste0("anatomic_atlas_expression_", input$group1, "_vs_", input$group2, "_", data_type_label, "_", Sys.Date(), ".png")
        },
        content = function(file) {
            req(input$data_type, input$group1, input$group2)

            # Use unified gene selection
            selected_genes <- current_genes()
            
            if (is.null(selected_genes) || length(selected_genes) == 0) {
                # Create a simple informational plot if no genes selected
                p <- ggplot() +
                    annotate("text", x = 0.5, y = 0.5, label = "No genes selected for visualization", size = 8) +
                    theme_void() +
                    labs(title = paste("Expression Comparison:", input$group1, "vs", input$group2))

                ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
                return()
            }

            # Recreate the violin plot for download
            data_type_label <- if (input$data_type == "log2_cpm") "log2(CPM + 1)" else "VST"

            # Use helper functions to determine column names
            celltype_col <- get_celltype_column(values$sample_data)
            sample_col <- get_sample_column(values$sample_data)

            # Create Y-axis label based on data type
            y_label <- if (input$data_type == "log2_cpm") {
                "Expression Level (log2(CPM + 1))"
            } else {
                "Expression Level (VST)"
            }

            # Join and filter for selected genes and data type (includes all celltypes for "Other" category)
            plot_data <- values$expression_data %>%
                filter(
                    gene %in% selected_genes,
                    data_type == input$data_type
                ) %>%
                left_join(
                    values$sample_data %>%
                        select(all_of(c(sample_col, celltype_col))) %>%
                        rename(sample = !!sample_col, celltype = !!celltype_col),
                    by = "sample"
                ) %>%
                mutate(
                    violin_group = case_when(
                        celltype == input$group1 ~ input$group1,
                        celltype == input$group2 ~ input$group2,
                        TRUE ~ "Other"
                    )
                )

            # Create violin plot
            plot_theme <- get_plot_theme(app_theme)

            p <- ggplot(plot_data, aes(x = violin_group, y = expression, fill = violin_group)) +
                geom_violin(trim = FALSE, alpha = 0.7) +
                geom_jitter(width = 0.15, height = 0, size = 1.2, alpha = 0.6, color = app_theme$text_secondary) +
                scale_fill_manual(
                    name = "Sample Group",
                    values = setNames(
                        plot_theme$colors,
                        c(input$group1, input$group2, "Other")
                    )
                ) +
                facet_wrap(~gene, scales = "free_y", nrow = 1) +
                labs(
                    title = "Expression Distribution of Selected Genes",
                    subtitle = paste0("Genes: ", paste(selected_genes, collapse = ", "), " (Data: ", data_type_label, ")"),
                    x = NULL, # Remove x-axis label
                    y = y_label
                ) +
                theme_minimal() +
                theme(
                    legend.position = "bottom",
                    plot.title = element_text(size = plot_theme$title_size, face = "bold", color = plot_theme$text_color),
                    plot.subtitle = element_text(size = plot_theme$subtitle_size, color = app_theme$text_light),
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = plot_theme$axis_text_size, color = plot_theme$text_color),
                    axis.text.y = element_text(color = plot_theme$text_color),
                    axis.title = element_text(color = plot_theme$text_color),
                    strip.text = element_text(color = plot_theme$text_color, face = "bold"),
                    panel.background = element_rect(fill = plot_theme$background, color = NA),
                    plot.background = element_rect(fill = plot_theme$background, color = NA),
                    panel.grid.major = element_line(color = plot_theme$grid_color, linewidth = 0.3),
                    panel.grid.minor = element_line(color = plot_theme$grid_color, linewidth = 0.1),
                    legend.text = element_text(size = plot_theme$legend_text_size, color = plot_theme$text_color)
                )

            ggsave(file, plot = p, width = 12, height = 8, dpi = 300)
        }
    )
    
    # =============================================================================
    # Co-Expression Analysis Server Logic
    # =============================================================================
    
    # Reactive values for co-expression analysis
    coexpression_log <- reactiveVal(character(0))
    coexpression_running <- reactiveVal(FALSE)
    
    # Function to add messages to the live log
    add_coexpression_log <- function(message) {
        timestamp <- format(Sys.time(), "%H:%M:%S")
        log_entry <- paste0("[", timestamp, "] ", message)
        current_log <- coexpression_log()
        # Keep only last 50 entries to prevent memory issues
        if (length(current_log) >= 50) {
            current_log <- current_log[-1]
        }
        coexpression_log(c(current_log, log_entry))
    }
    
    # Reactive for co-expression analysis
    coexpression_results <- eventReactive(input$run_coexpression, {
        # Use unified gene selection
        selected_genes <- current_genes()
        
        req(selected_genes, values$expression_data)
        
        if (length(selected_genes) < 3) {
            add_coexpression_log("ERROR: Please select at least 3 genes for co-expression analysis")
            showNotification("Please select at least 3 genes for co-expression analysis", 
                            type = "warning", duration = 5)
            return(NULL)
        }
        
        # Set running status and clear previous log
        coexpression_running(TRUE)
        coexpression_log(character(0))
        
        add_coexpression_log(paste("Starting co-expression analysis with", length(selected_genes), "query genes"))
        add_coexpression_log(paste("Parameters: min_correlation =", input$min_correlation_coexpr, 
                                  ", n_similar =", input$n_similar_genes, 
                                  ", batch_size =", input$max_genes_batch))
        add_coexpression_log(paste("Using parallel processing:", input$use_parallel))
        add_coexpression_log(paste("Data type:", input$coexpression_data_type))
        
        # Get dataset info for progress message
        total_genes <- length(unique(values$expression_data$gene))
        add_coexpression_log(paste("Dataset contains", format(total_genes, big.mark = ","), "genes"))
        
        add_coexpression_log("Starting analysis...")
        
        # Use live log for progress instead of withProgress modal
        add_coexpression_log("Step 1/3: Calculating correlations...")
        
        results <- tryCatch({
            find_coexpressed_genes(
                expression_data = values$expression_data,
                query_genes = selected_genes,
                n_similar = input$n_similar_genes %||% 5,
                min_correlation = input$min_correlation_coexpr %||% 0.6,
                data_type = input$coexpression_data_type,
                max_genes_batch = input$max_genes_batch %||% 1000,
                use_parallel = input$use_parallel %||% FALSE,
                log_func = add_coexpression_log
            )
        }, error = function(e) {
            add_coexpression_log(paste("ERROR:", e$message))
            coexpression_running(FALSE)
            return(NULL)
        })
        
        add_coexpression_log("Step 2/3: Processing results...")
        
        if (!is.null(results) && !is.null(results$similar_genes) && nrow(results$similar_genes) > 0) {
            add_coexpression_log(paste("Step 3/3: Analysis completed successfully -", nrow(results$similar_genes), "co-expressed genes found"))
        } else {
            add_coexpression_log("Step 3/3: Analysis completed - no co-expressed genes found meeting the criteria")
        }
        
        # Reset running status
        coexpression_running(FALSE)
        
        return(results)
    })
    
    # Observer to update button state based on analysis status
    observe({
        if (coexpression_running()) {
            updateActionButton(session, "run_coexpression", 
                             label = "Analysis Running...", 
                             icon = icon("spinner", class = "fa-spin"))
        } else {
            updateActionButton(session, "run_coexpression", 
                             label = "Find Co-expressed Genes",
                             icon = NULL)
        }
    })
    
    # Live log output
    output$coexpression_live_log <- renderText({
        log_entries <- coexpression_log()
        if (length(log_entries) == 0) {
            if (coexpression_running()) {
                return("Analysis starting...")
            } else {
                return("Ready to run analysis...")
            }
        }
        paste(log_entries, collapse = "\n")
    })
    
    # Summary table of found genes
    output$coexpressed_genes_summary <- DT::renderDataTable({
        results <- coexpression_results()
        
        if (is.null(results) || nrow(results$similar_genes) == 0) {
            return(data.frame(Message = "No results"))
        }
        
        summary_table <- results$similar_genes %>%
            select(Gene = similar_gene, `Avg Corr` = avg_correlation) %>%
            mutate(`Avg Corr` = round(`Avg Corr`, 3))
        
        DT::datatable(
            summary_table,
            options = list(
                pageLength = 10,
                dom = "t",
                ordering = FALSE,
                scrollY = "200px",
                scrollCollapse = TRUE
            ),
            rownames = FALSE,
            selection = "none"
        ) %>%
            DT::formatStyle("Gene", fontWeight = "bold")
    })
    
    # Co-expression heatmap
    output$coexpression_heatmap <- renderPlotly({
        results <- coexpression_results()
        
        if (is.null(results) || nrow(results$similar_genes) == 0) {
            return(plotly_empty(type = "scatter", mode = "markers") %>%
                add_annotations(
                    text = "Run co-expression analysis to see heatmap",
                    showarrow = FALSE
                ))
        }
        
        create_coexpression_heatmap(
            expression_data = values$expression_data,
            query_genes = results$query_genes_used,
            similar_genes = results$similar_genes$similar_gene,
            data_type = input$data_type,
            sample_metadata = values$sample_data
        )
    })
    
    # Co-expression network
    output$coexpression_network <- networkD3::renderForceNetwork({
        results <- coexpression_results()
        
        if (is.null(results) || nrow(results$similar_genes) == 0) {
            return(NULL)
        }
        
        create_coexpression_network(
            correlation_matrix = results$correlation_matrix,
            query_genes = results$query_genes_used,
            similar_genes = results$similar_genes$similar_gene,
            min_correlation = input$min_correlation_coexpr %||% 0.6
        )
    })
    
    # Detailed results table
    output$coexpression_detailed_table <- DT::renderDataTable({
        results <- coexpression_results()
        
        if (is.null(results)) {
            return(data.frame(Message = "No analysis run yet"))
        }
        
        detailed_table <- create_correlation_table(results)
        
        DT::datatable(
            detailed_table,
            options = list(
                pageLength = 15,
                scrollX = TRUE,
                dom = "Bfrtip",
                buttons = c("copy", "csv", "excel")
            ),
            rownames = FALSE,
            extensions = "Buttons",
            selection = "none"
        ) %>%
            DT::formatStyle("Gene", fontWeight = "bold") %>%
            DT::formatStyle(
                c("Avg Correlation", "Max Correlation", "Min Correlation"),
                backgroundColor = DT::styleInterval(
                    c(0.7, 0.8, 0.9), 
                    c("white", "#e8f5e8", "#d4edda", "#c3e6cb")
                )
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
