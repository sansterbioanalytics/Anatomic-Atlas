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

# Suppress warnings for DT column issues
options(DT.warn.size = FALSE)

# Helper operator for null-coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a

# Source modular components
source("data_utils.R")        # Data loading and processing functions
source("theme_config.R")      # Theme configuration and CSS generation
source("plot_utils.R")        # Plotting and visualization functions
source("ui_components.R")     # Reusable UI components
source("server_utils.R")      # Server helper functions
source("coexpression_analysis.R") # Co-expression analysis functions
source("product_groupings.R") # Product portfolio groupings configuration

# =============================================================================
# PRE-LOAD DATA (before UI creation)
# =============================================================================
cat("=== PRE-LOADING DATA ===\n")
atlas_data_global <- load_atlas_data(progress = list(
    set = function(message = "", value = 0) {
        cat("Progress:", message, "(", round(value * 100), "%)\n")
    }
))

# Load gene sets directly
load_gene_sets <- function() {
    gene_set_path <- "./"
    gene_sets <- list()

    # Define the gene set files
    gene_set_files <- c(
        "TRP_channels.csv",
        "Sodium_Channels.csv",
        "Calcium_Channels.csv",
        "Ion_Channels.csv",
        "Price_Neuropeptides.csv",
        "Price_GPCRs.csv",
        "Cell_Adhesion_Molecules.csv"
    )

    for (file in gene_set_files) {
        file_path <- file.path(gene_set_path, file)
        if (file.exists(file_path)) {
            name <- gsub("\\.csv$", "", file)
            name <- gsub("_", " ", name) # Replace underscores with spaces
            gene_sets[[name]] <- readr::read_csv(file_path, show_col_types = FALSE)$Symbol
        }
    }

    return(gene_sets)
}

gene_sets_global <- load_gene_sets()

if (is.null(atlas_data_global$sample_data) || is.null(atlas_data_global$expression_data)) {
    stop("Failed to load atlas data. Please check data files.")
}

cat("Data pre-loaded successfully! Sample rows:", nrow(atlas_data_global$sample_data), "Expression rows:", nrow(atlas_data_global$expression_data), "\n")
cat("=== DATA PRE-LOADING COMPLETE ===\n")

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
    # Use pre-loaded data
    gene_sets <- gene_sets_global

    # Reactive values - initialize with pre-loaded data
    values <- reactiveValues(
        sample_data = atlas_data_global$sample_data,
        expression_data = atlas_data_global$expression_data,
        contrast_data = NULL,
        data_loaded = TRUE,
        gene_page = 1,
        app_mode = "target",
        cell_type_toggles = list(),
        validated_genes = c(),
        available_genes = NULL,
        # Simple gene sharing between modes - only manual text preserved
        target_mode_textarea = "",
        explorer_mode_textarea = "",
        target_mode_input_method = "manual",
        explorer_mode_input_method = "manual"
    )

    cat("Server initialized with pre-loaded data\n")
    cat("Gene sets available:", length(gene_sets), "\n")
    if (length(gene_sets) > 0) {
        cat("Gene set names:", names(gene_sets), "\n")
    }
    observeEvent(input$gene_file_upload, {
        req(input$gene_file_upload)
        
        tryCatch({
            file_path <- input$gene_file_upload$datapath
            file_ext <- tolower(tools::file_ext(input$gene_file_upload$name))
            
            genes <- NULL
            
            # Handle different file types
            if (file_ext == "csv") {
                # Read CSV and detect gene column
                data <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
                
                # Look for gene column by common names
                gene_col <- NULL
                col_names <- tolower(names(data))
                for (col_name in c("symbol", "gene", "gene_symbol", "gene_name", "genes")) {
                    if (col_name %in% col_names) {
                        gene_col <- which(col_names == col_name)[1]
                        break
                    }
                }
                
                # If no gene column found, use first column
                if (is.null(gene_col)) gene_col <- 1
                
                genes <- as.character(data[[gene_col]])
                
            } else if (file_ext %in% c("tsv", "txt")) {
                # Handle TSV/TXT files
                if (file_ext == "tsv") {
                    data <- read.delim(file_path, stringsAsFactors = FALSE, check.names = FALSE)
                    
                    # Same column detection logic
                    gene_col <- NULL
                    col_names <- tolower(names(data))
                    for (col_name in c("symbol", "gene", "gene_symbol", "gene_name", "genes")) {
                        if (col_name %in% col_names) {
                            gene_col <- which(col_names == col_name)[1]
                            break
                        }
                    }
                    
                    if (is.null(gene_col)) gene_col <- 1
                    genes <- as.character(data[[gene_col]])
                } else {
                    # Plain text file - one gene per line
                    genes <- readLines(file_path, warn = FALSE)
                }
            } else {
                # Try to read as plain text for other extensions
                genes <- readLines(file_path, warn = FALSE)
            }
            
            # Clean up genes
            genes <- trimws(genes)
            genes <- genes[genes != "" & !is.na(genes)]
            
            if (length(genes) > 0) {
                # Convert uploaded genes directly to manual text - no separate storage needed
                gene_string <- paste(genes, collapse = ", ")
                updateTextAreaInput(session, "gene_textarea", value = gene_string)
                
                # Always switch to manual mode - this makes the genes part of the manual text system
                updateRadioButtons(session, "gene_input_method", selected = "manual")
                
                showNotification(
                    paste("Loaded", length(genes), "genes from", toupper(file_ext), "file and converted to manual entry"),
                    type = "success",
                    duration = 3
                )
            } else {
                showNotification("No valid genes found in file", type = "error")
            }
            
        }, error = function(e) {
            showNotification(paste("Error reading file:", e$message), type = "error")
        })
    })

    # Server-side sidebar rendering based on mode
    output$sidebar_controls <- renderUI({
        cat("Rendering sidebar controls for mode:", values$app_mode, "\n")
        
        if (values$app_mode == "target") {
            # Target Mode Controls - Compact layout
            tagList(
                # Gene Selection (shared)
                create_simple_gene_selection_ui(app_theme),
                
                # Cell Type Selection (Target mode specific)
                div(
                    class = "cell-type-section",
                    style = "margin-bottom: 8px; width: 100%;", # Reduced margin
                    create_product_grouping_ui(app_theme) 
                ),
                
                # Analysis Options (shared)
                div(
                    class = "analysis-options-section", 
                    style = "margin-bottom: 8px; width: 100%;", # Reduced margin
                    create_analysis_options_ui(app_theme)
                )
            )
        } else {
            # Explorer Mode Controls - Compact layout
            tagList(
                # Gene Selection (shared)
                create_simple_gene_selection_ui(app_theme),
                
                # Group Comparison (Explorer mode specific)
                # div(
                #     class = "contrast-selection-section",
                #     style = "margin-bottom: 8px; width: 100%;", # Reduced margin
                #     create_contrast_selection_ui(app_theme)
                # ),
                
                # Analysis Options (shared)
                div(
                    class = "analysis-options-section",
                    style = "margin-bottom: 8px; width: 100%;", # Reduced margin
                    create_analysis_options_ui(app_theme)
                )
            )
        }
    })

    # Native reactive gene set dropdown - replaces updateSelectInput approach
    output$gene_set_dropdown <- renderUI({
        if (length(gene_sets) == 0) {
            return(selectInput("gene_set_selection",
                NULL,
                choices = list("Loading gene sets..." = ""),
                selected = "",
                width = "100%"
            ))
        }
        
        # Create choices with proper names
        gene_set_names <- names(gene_sets)
        choices <- list("Select a gene set..." = "")
        for (name in gene_set_names) {
            choices[[name]] <- name
        }
        
        selectInput("gene_set_selection",
            NULL,
            choices = choices,
            selected = "",
            width = "100%"
        )
    })
    
    # Gene set info display
    output$gene_set_info_display <- renderUI({
        if (is.null(input$gene_set_selection) || input$gene_set_selection == "" || input$gene_set_selection == "Select a gene set...") {
            return(div(
                style = "color: #6c757d; font-style: italic; font-size: 12px;",
                "Select a gene set to see details..."
            ))
        }
        
        if (!is.null(gene_sets) && input$gene_set_selection %in% names(gene_sets)) {
            genes <- gene_sets[[input$gene_set_selection]]
            return(div(
                style = "color: #28a745; font-size: 12px;",
                tags$strong(paste("📊", length(genes), "genes in this set")),
                br(),
                tags$small(paste("First few:", paste(head(genes, 5), collapse = ", ")))
            ))
        }
        
        return(div(
            style = "color: #dc3545; font-size: 12px;",
            "Gene set not found"
        ))
    })
    
    # Handle gene set selection changes - update textarea when a gene set is selected
    observeEvent(input$gene_set_selection, {
        if (!is.null(input$gene_set_selection) && 
            input$gene_set_selection != "" && 
            input$gene_set_selection != "Select a gene set..." &&
            !is.null(gene_sets) && input$gene_set_selection %in% names(gene_sets)) {
            
            selected_genes <- gene_sets[[input$gene_set_selection]]
            if (!is.null(selected_genes) && length(selected_genes) > 0) {
                # Update textarea with selected gene set
                gene_string <- paste(selected_genes, collapse = ", ")
                updateTextAreaInput(session, "gene_textarea", value = gene_string)
                
                showNotification(
                    paste("Loaded", length(selected_genes), "genes from", input$gene_set_selection),
                    type = "success",
                    duration = 3
                )
            }
        }
    })
    
    # Native reactive group comparison dropdowns  
    output$group_comparison_dropdowns <- renderUI({
        if (is.null(values$sample_data)) {
            return(tagList(
                div(style = "margin-bottom: 12px;",
                    selectInput("group1", "Group 1 (Baseline):", 
                               choices = c("Loading..." = ""), multiple = FALSE)
                ),
                div(style = "margin-bottom: 12px;",
                    selectInput("group2", "Group 2 (Comparison):", 
                               choices = c("Loading..." = ""), multiple = FALSE)
                )
            ))
        }
        
        # Get cell types
        celltype_col <- get_celltype_column(values$sample_data)
        if (is.null(celltype_col)) {
            return(tagList(
                div(style = "margin-bottom: 12px;",
                    selectInput("group1", "Group 1 (Baseline):", 
                               choices = c("Error loading data" = ""), multiple = FALSE)
                ),
                div(style = "margin-bottom: 12px;",
                    selectInput("group2", "Group 2 (Comparison):", 
                               choices = c("Error loading data" = ""), multiple = FALSE)
                )
            ))
        }
        
        cell_types <- unique(values$sample_data[[celltype_col]])
        cell_types <- sort(cell_types)
        
        # Create choices
        choices <- setNames(cell_types, cell_types)
        
        tagList(
            div(style = "margin-bottom: 12px;",
                selectInput("group1", 
                    "Group 1 (Baseline):",
                    choices = choices,
                    selected = if(length(cell_types) > 0) cell_types[1] else NULL,
                    multiple = FALSE
                )
            ),
            div(style = "margin-bottom: 12px;",
                selectInput("group2", 
                    "Group 2 (Comparison):",
                    choices = choices,
                    selected = if (length(cell_types) > 1) cell_types[2] else if(length(cell_types) > 0) cell_types[1] else NULL,
                    multiple = FALSE
                )
            )
        )
    })

    # Initialize validated genes on startup to ensure plots work immediately
    observe({
        if (!is.null(values$available_genes) && is.null(values$validated_genes)) {
            # Parse the default gene from textarea
            default_genes <- parse_gene_input("")
            if (!is.null(default_genes) && length(default_genes) > 0) {
                valid_genes <- intersect(default_genes, values$available_genes)
                if (length(valid_genes) > 0) {
                    values$validated_genes <- valid_genes
                    cat("Initialized validated genes with:", paste(valid_genes, collapse = ", "), "\n")
                }
            }
        }
    })

    # Mode switching observers with gene sharing
    observeEvent(input$mode_target, {
        # Save current textarea content before switching
        if (values$app_mode == "explorer") {
            values$explorer_mode_textarea <- input$gene_textarea %||% ""
            values$explorer_mode_input_method <- input$gene_input_method %||% "manual"
        }
        
        values$app_mode <- "target"
        updateRadioButtons(session, "app_mode", selected = "target")
        
        # Restore target mode content, but if it's empty and we have current content, preserve it
        restore_content <- values$target_mode_textarea
        if (restore_content == "" && !is.null(input$gene_textarea) && input$gene_textarea != "") {
            restore_content <- input$gene_textarea
        }
        updateTextAreaInput(session, "gene_textarea", value = restore_content)
        updateRadioButtons(session, "gene_input_method", selected = values$target_mode_input_method)
    })
    
    observeEvent(input$mode_explorer, {
        # Save current textarea content before switching
        if (values$app_mode == "target") {
            values$target_mode_textarea <- input$gene_textarea %||% ""
            values$target_mode_input_method <- input$gene_input_method %||% "manual"
        }
        
        values$app_mode <- "explorer"
        updateRadioButtons(session, "app_mode", selected = "explorer")
        
        # Restore explorer mode content, but if it's empty and we have current content, preserve it
        restore_content <- values$explorer_mode_textarea
        if (restore_content == "" && !is.null(input$gene_textarea) && input$gene_textarea != "") {
            restore_content <- input$gene_textarea
        }
        updateTextAreaInput(session, "gene_textarea", value = restore_content)
        updateRadioButtons(session, "gene_input_method", selected = values$explorer_mode_input_method)
    })
    
    # Handle radio button changes (for compatibility)
    observeEvent(input$app_mode, {
        old_mode <- values$app_mode
        
        # Save current textarea content before switching
        if (old_mode == "target" && input$app_mode == "explorer") {
            values$target_mode_textarea <- input$gene_textarea %||% ""
            values$target_mode_input_method <- input$gene_input_method %||% "manual"
            # Restore explorer mode content, but preserve current content if explorer is empty
            restore_content <- values$explorer_mode_textarea
            if (restore_content == "" && !is.null(input$gene_textarea) && input$gene_textarea != "") {
                restore_content <- input$gene_textarea
            }
            updateTextAreaInput(session, "gene_textarea", value = restore_content)
            updateRadioButtons(session, "gene_input_method", selected = values$explorer_mode_input_method)
        } else if (old_mode == "explorer" && input$app_mode == "target") {
            values$explorer_mode_textarea <- input$gene_textarea %||% ""
            values$explorer_mode_input_method <- input$gene_input_method %||% "manual"
            # Restore target mode content, but preserve current content if target is empty
            restore_content <- values$target_mode_textarea
            if (restore_content == "" && !is.null(input$gene_textarea) && input$gene_textarea != "") {
                restore_content <- input$gene_textarea
            }
            updateTextAreaInput(session, "gene_textarea", value = restore_content)
            updateRadioButtons(session, "gene_input_method", selected = values$target_mode_input_method)
        }
        
        values$app_mode <- input$app_mode
    })
    
    # Initialize data when loaded - simplified observer
    observe({
        req(values$sample_data)  # Ensure sample data is available
        
        # Initialize target mode toggle states if needed
        if (values$app_mode == "target" && length(values$cell_type_toggles) == 0) {
            celltype_col <- get_celltype_column(values$sample_data)
            if (!is.null(celltype_col)) {
                cell_types <- unique(values$sample_data[[celltype_col]])
                # Set default toggle states: Real* = TRUE, others = FALSE
                toggle_states <- list()
                for (cell_type in cell_types) {
                    toggle_states[[cell_type]] <- grepl("^Real", cell_type, ignore.case = TRUE)
                }
                values$cell_type_toggles <- toggle_states
                cat("Initialized cell type toggles for Target mode\n")
            }
        }

        # Initialize gene data
        if (!is.null(values$expression_data)) {
            genes <- sort(unique(values$expression_data$gene))  # Sort genes alphabetically
            cat("Found", length(genes), "genes in dataset\n")
            
            # Store genes list for validation
            values$available_genes <- genes
            cat("Gene choices updated and validation initialized\n")
        } else {
            cat("Expression data is NULL - gene choices not updated\n")
        }
    })

    # Unified observer to handle gene selection changes and update validated genes
    observe({
        # Only proceed if we have available genes
        if (is.null(values$available_genes) || length(values$available_genes) == 0) {
            return()
        }
        
        # Determine which genes to use based on input method
        input_method <- input$gene_input_method %||% "manual"
        selected_genes <- NULL
        
        tryCatch({
            # Simplified: always use manual textarea (file uploads are converted to manual text)
            textarea_value <- input$gene_textarea %||% ""
            selected_genes <- parse_gene_input(textarea_value)
        }, error = function(e) {
            selected_genes <- NULL
        })
        
        # Filter for genes that exist in the dataset
        if (is.null(selected_genes) || length(selected_genes) == 0) {
            values$validated_genes <- c("")
            return()
        }
        
        available_genes <- intersect(selected_genes, values$available_genes)
        
        if (length(available_genes) == 0) {
            values$validated_genes <- c("")
            return()
        }
        
        # Update validated genes
        values$validated_genes <- available_genes
        cat("Updated validated genes:", length(available_genes), "genes from method:", input_method, "\n")
    })

    # Reactive expression for pre-filtered contrast data
    contrast_data <- reactive({
        req(values$expression_data, values$sample_data, input$data_type)
        
        # Handle different modes
        if (values$app_mode == "target") {
            # Target mode: use toggle states to determine included cell types
            if (length(values$cell_type_toggles) == 0) return(NULL)
            
            # Get enabled cell types
            enabled_cell_types <- names(values$cell_type_toggles)[sapply(values$cell_type_toggles, function(x) x == TRUE)]
            
            if (length(enabled_cell_types) == 0) return(NULL)
            
            # For target mode, create a data frame similar to explorer mode
            celltype_col <- get_celltype_column(values$sample_data)
            sample_col <- get_sample_column(values$sample_data)
            
            if (is.null(celltype_col) || is.null(sample_col)) return(NULL)
            
            # Get samples for enabled cell types
            target_samples <- values$sample_data %>%
                filter(!!sym(celltype_col) %in% enabled_cell_types)
            
            if (nrow(target_samples) == 0) return(NULL)
            
            # Filter expression data for target samples and data type
            target_expression <- values$expression_data %>%
                filter(
                    sample %in% target_samples[[sample_col]],
                    data_type == input$data_type
                ) %>%
                left_join(
                    target_samples %>%
                        select(all_of(c(sample_col, celltype_col))) %>%
                        rename(sample = !!sample_col, celltype = !!celltype_col),
                    by = "sample"
                ) %>%
                mutate(
                    # Create violin_group for consistency with explorer mode
                    violin_group = celltype
                )
            
            return(target_expression)
        } else {
            # Explorer mode: use traditional group1/group2 logic
            req(input$group1, input$group2)
            
            create_contrast_data(
                values$expression_data,
                values$sample_data,
                input$group1,
                input$group2,
                input$data_type
            )
        }
    })

    # Reactive expression for currently active cell types (target mode)
    active_cell_types <- reactive({
        if (values$app_mode == "target" && length(values$cell_type_toggles) > 0) {
            enabled_types <- names(values$cell_type_toggles)[sapply(values$cell_type_toggles, function(x) x == TRUE)]
            return(enabled_types)
        }
        return(NULL)
    })

    # Parse and validate gene input from text area
    parse_gene_input <- function(text_input) {
        if (is.null(text_input) || text_input == "") return(NULL)
        
        # Split by common separators: comma, semicolon, newline, tab, multiple spaces
        # Use a more robust regex that handles mixed separators
        genes <- unique(trimws(unlist(strsplit(text_input, "[,;\\n\\t\\s]+"))))
        genes <- genes[genes != ""]  # Remove empty strings
        
        # Convert to uppercase for case-insensitive matching
        genes <- toupper(genes)
        
        return(genes)
    }

    # Suggest similar genes for invalid genes only (improved fuzzy matching)
    suggest_genes <- function(invalid_genes, available_genes, max_suggestions = 10) {
        if (length(invalid_genes) == 0 || length(available_genes) == 0) return(NULL)
        
        all_suggestions <- c()
        for (gene in invalid_genes) {
            # Only provide suggestions for invalid genes that user is actually typing
            gene_suggestions <- c()
            
            # Method 1: Exact case-insensitive prefix matching (highest priority)
            prefix_matches <- available_genes[grepl(paste0("^", toupper(gene)), toupper(available_genes))]
            if (length(prefix_matches) > 0) {
                gene_suggestions <- c(gene_suggestions, head(prefix_matches, 2))
            }
            
            # Method 2: Contains the entered text (case-insensitive)
            if (length(gene_suggestions) < 3 && nchar(gene) >= 2) {
                contains_matches <- available_genes[grepl(toupper(gene), toupper(available_genes))]
                contains_matches <- setdiff(contains_matches, gene_suggestions)  # Remove duplicates
                if (length(contains_matches) > 0) {
                    gene_suggestions <- c(gene_suggestions, head(contains_matches, 3 - length(gene_suggestions)))
                }
            }
            
            # Method 3: Similar starting letters (fallback, minimum 3 characters)
            if (length(gene_suggestions) < 3 && nchar(gene) >= 3) {
                similar_start <- available_genes[grepl(paste0("^", substr(gene, 1, min(3, nchar(gene)))), 
                                                     available_genes, ignore.case = TRUE)]
                similar_start <- setdiff(similar_start, gene_suggestions)  # Remove duplicates
                if (length(similar_start) > 0) {
                    gene_suggestions <- c(gene_suggestions, head(similar_start, 3 - length(gene_suggestions)))
                }
            }
            
            # Format suggestions for this gene
            if (length(gene_suggestions) > 0) {
                # Limit to top 3 suggestions per invalid gene
                top_suggestions <- head(gene_suggestions, 3)
                all_suggestions <- c(all_suggestions, paste0(gene, " → ", paste(top_suggestions, collapse = ", ")))
            }
        }
        # Return up to max_suggestions total
        head(all_suggestions, max_suggestions)
    }

    # Simplified helper function to generate validation UI
    generate_validation_ui <- function() {
        input_method <- input$gene_input_method %||% "manual"
        genes_entered <- NULL
        
        # Get genes based on input method
        tryCatch({
            # Simplified: always use manual textarea content
            textarea_value <- input$gene_textarea %||% ""
            genes_entered <- parse_gene_input(textarea_value)
        }, error = function(e) {
            genes_entered <- NULL
        })
        
        # Show loading message if available genes not ready
        if (is.null(values$available_genes) || length(values$available_genes) == 0) {
            return(div(
                style = "color: #007bff; padding: 10px; font-size: 13px; background-color: rgba(0,123,255,0.1); border-radius: 4px; border: 1px solid rgba(0,123,255,0.2);",
                tags$i(class = "fa fa-spinner fa-spin", style = "margin-right: 5px;"),
                "Loading gene list..."
            ))
        }
        
        # Show placeholder if no genes entered
        if (is.null(genes_entered) || length(genes_entered) == 0) {
            return(div(
                style = "color: #6c757d; font-style: italic; padding: 10px; font-size: 13px; background-color: rgba(108,117,125,0.1); border-radius: 4px; border: 1px solid rgba(108,117,125,0.2);",
                "Enter or select gene symbols to see validation..."
            ))
        }
        
        cat("Performing validation with", length(genes_entered), "genes\n")
        
        # Simple validation for testing
        valid_genes <- intersect(genes_entered, values$available_genes)
        invalid_genes <- setdiff(genes_entered, values$available_genes)
        
        cat("Valid genes:", length(valid_genes), "Invalid genes:", length(invalid_genes), "\n")
        
        # Create a simplified validation display with better suggestions
        validation_div <- div(
            style = "background-color: #f8f9fa; border: 2px solid #28a745; border-radius: 4px; padding: 15px; font-size: 14px; margin: 0; color: #495057; width: 100%; box-sizing: border-box;",
            # Summary with counts
            div(
                style = "margin-bottom: 12px; font-size: 16px;",
                span(style = "color: #28a745; font-weight: bold;", paste("✓", length(valid_genes), "valid")),
                if (length(invalid_genes) > 0) {
                    tagList(
                        span(style = "margin: 0 8px; color: #6c757d;", "|"),
                        span(style = "color: #dc3545; font-weight: bold;", paste("✗", length(invalid_genes), "invalid"))
                    )
                }
            ),
            
            # Show valid genes
            if (length(valid_genes) > 0) {
                div(
                    style = "background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 3px; padding: 8px; margin-bottom: 8px;",
                    div(
                        style = "color: #155724; font-weight: bold; margin-bottom: 4px;",
                        "Valid genes:"
                    ),
                    div(
                        style = "color: #155724;",
                        paste(head(valid_genes, 5), collapse = ", "),
                        if (length(valid_genes) > 5) {
                            paste("... and", length(valid_genes) - 5, "more")
                        }
                    )
                )
            },
            
            # Show invalid genes with better suggestions
            if (length(invalid_genes) > 0) {
                div(
                    style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 3px; padding: 8px;",
                    div(
                        style = "color: #721c24; font-weight: bold; margin-bottom: 4px;",
                        "Invalid genes:"
                    ),
                    div(
                        style = "color: #721c24; margin-bottom: 8px;",
                        paste(head(invalid_genes, 5), collapse = ", "),
                        if (length(invalid_genes) > 5) " ..."
                    ),
                    
                    # Add suggestions for each invalid gene
                    {
                        suggestions_ui <- lapply(head(invalid_genes, 3), function(invalid_gene) {
                            # Get suggestions for this specific gene
                            suggestions <- suggest_genes(c(invalid_gene), values$available_genes, 3)
                            if (length(suggestions) > 0) {
                                div(
                                    style = "margin-top: 4px; padding: 4px; background-color: #fff3cd; border-radius: 2px;",
                                    div(
                                        style = "color: #856404; font-size: 12px;",
                                        paste("💡", suggestions[1])  # Just show the first suggestion line
                                    )
                                )
                            }
                        })
                        # Remove NULL suggestions
                        suggestions_ui[!sapply(suggestions_ui, is.null)]
                    }
                )
            }
        )
        
        cat("Returning validation div for mode:", values$app_mode, "\n")
        return(validation_div)
    }

    # Unified validation output for both modes
    output$gene_validation <- renderUI({
        # Debug current inputs
        cat("Gene validation render called for mode:", values$app_mode, "\n")
        cat("gene_set_selection:", input$gene_set_selection %||% "NULL", "\n")
        cat("gene_textarea:", nchar(input$gene_textarea %||% ""), "characters\n")
        cat("available_genes:", length(values$available_genes), "genes\n")
        validation_result <- generate_validation_ui()
        return(validation_result)
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
        # Always use validated genes - the observer handles validation for all gene selection types
        return(values$validated_genes)
    })
    
    # Reactive expression for genes on current page (for expression plots)
    current_page_genes <- reactive({
        all_genes <- current_genes()
        if (is.null(all_genes) || length(all_genes) == 0) {
            return(NULL)
        }
        
        # Calculate pagination with fixed 5 genes per page for target mode
        genes_per_page <- 5
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
            genes_per_page <- 5
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

    # Handle cell type toggle clicks
    observeEvent(input$cell_type_clicked, {
        cell_type <- input$cell_type_clicked
        if (!is.null(cell_type) && cell_type %in% names(values$cell_type_toggles)) {
            # Toggle the state
            values$cell_type_toggles[[cell_type]] <- !values$cell_type_toggles[[cell_type]]
        }
    })

    # Handle group action buttons
    observeEvent(input$select_all_real, {
        # Select only Real* products
        real_types <- get_grouping_cell_types("real_products")
        celltype_col <- get_celltype_column(values$sample_data)
        if (!is.null(celltype_col)) {
            available_types <- unique(values$sample_data[[celltype_col]])
            for (cell_type in available_types) {
                values$cell_type_toggles[[cell_type]] <- cell_type %in% real_types
            }
        }
    })

    observeEvent(input$select_all_types, {
        # Select all cell types
        celltype_col <- get_celltype_column(values$sample_data)
        if (!is.null(celltype_col)) {
            available_types <- unique(values$sample_data[[celltype_col]])
            for (cell_type in available_types) {
                values$cell_type_toggles[[cell_type]] <- TRUE
            }
        }
    })

    observeEvent(input$select_none, {
        # Deselect all cell types
        celltype_col <- get_celltype_column(values$sample_data)
        if (!is.null(celltype_col)) {
            available_types <- unique(values$sample_data[[celltype_col]])
            for (cell_type in available_types) {
                values$cell_type_toggles[[cell_type]] <- FALSE
            }
        }
    })
    
    # Simple pagination info UI
    output$gene_pagination_info <- renderUI({
        all_genes <- current_genes()
        if (is.null(all_genes) || length(all_genes) <= 5) {
            return(span("", style = "color: #666;"))
        }
        
        genes_per_page <- 5
        total_genes <- length(all_genes)
        max_pages <- ceiling(total_genes / genes_per_page)
        start_idx <- (values$gene_page - 1) * genes_per_page + 1
        end_idx <- min(values$gene_page * genes_per_page, total_genes)
        
        span(
            paste0(start_idx, "-", end_idx, " of ", total_genes, 
                   " (Page ", values$gene_page, " of ", max_pages, ")"),
            style = "color: #666; font-size: 10px;"
        )
    })
    
    # Output to control conditional panel for gene navigation
    output$show_gene_pagination <- reactive({
        all_genes <- current_genes()
        !is.null(all_genes) && length(all_genes) > 5
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
        
        # Determine gene source - simplified since everything is now manual text
        gene_source <- "Manual entry"
        
        div(
            style = paste0("color: ", app_theme$text_white, "; font-size: ", app_theme$font_size_small, "; margin-bottom: ", app_theme$spacing_sm, ";"),
            tags$strong(paste(length(genes), "genes selected")),
            br(),
            tags$em(paste("Source:", gene_source))
        )
    })
    
    # Output for checking if genes are available (used in conditional panels)
    output$current_genes_available <- reactive({
        genes <- current_genes()
        return(!is.null(genes) && length(genes) > 0)
    })
    outputOptions(output, "current_genes_available", suspendWhenHidden = FALSE)

    # Render grouped cell type toggles for target mode
    output$grouped_cell_type_toggles <- renderUI({
        req(values$sample_data)
        
        # Only render for target mode
        if (values$app_mode != "target") return(NULL)
        
        celltype_col <- get_celltype_column(values$sample_data)
        if (is.null(celltype_col)) return(NULL)
        
        cell_types <- unique(values$sample_data[[celltype_col]])
        
        # Initialize toggle states if not already set using the configuration
        if (length(values$cell_type_toggles) == 0) {
            values$cell_type_toggles <- get_default_toggle_states()
        }
        
        # Get cell types organized by group
        grouped_types <- get_cell_types_by_group()
        
        # Create UI for each group
        group_sections <- lapply(names(grouped_types), function(group_key) {
            group_info <- grouped_types[[group_key]]
            group_cell_types <- intersect(group_info$cell_types, cell_types)
            
            if (length(group_cell_types) == 0) return(NULL)
            
            # Create toggle buttons for this group
            toggle_buttons <- lapply(group_cell_types, function(cell_type) {
                is_active <- values$cell_type_toggles[[cell_type]] %||% FALSE
                
                button_class <- if (is_active) "cell-type-toggle-btn active" else "cell-type-toggle-btn inactive"
                
                div(
                    class = "cell-type-toggle",
                    actionButton(
                        inputId = paste0("toggle_", gsub("[^A-Za-z0-9]", "_", cell_type)),
                        label = cell_type,
                        class = button_class,
                        onclick = paste0("Shiny.setInputValue('cell_type_clicked', '", cell_type, "', {priority: 'event'});")
                    )
                )
            })
            
            # Return group section
            div(
                class = "cell-type-toggle-section",
                div(
                    class = "cell-type-section-header",
                    style = paste0("color: ", app_theme$text_white, "; margin-bottom: ", app_theme$spacing_sm, ";"),
                    group_info$name
                ),
                div(
                    class = "cell-type-toggle-container",
                    toggle_buttons
                )
            )
        })
        
        # Remove NULL sections
        group_sections[!sapply(group_sections, is.null)]
    })

    # Render cell type toggles for target mode
    output$cell_type_toggles <- renderUI({
        req(values$sample_data)
        
        # Only render for target mode
        if (values$app_mode != "target") return(NULL)
        
        celltype_col <- get_celltype_column(values$sample_data)
        if (is.null(celltype_col)) return(NULL)
        
        cell_types <- unique(values$sample_data[[celltype_col]])
        
        # Initialize toggle states if not already set
        if (length(values$cell_type_toggles) == 0) {
            # Set defaults: Real* products enabled, primary/hiPSC disabled
            toggle_states <- sapply(cell_types, function(ct) {
                # Enable Real* products by default
                if (grepl("^Real", ct, ignore.case = TRUE)) {
                    return(TRUE)
                }
                # Disable primary human and hiPSC by default
                if (grepl("primary|hipsc|ips", ct, ignore.case = TRUE)) {
                    return(FALSE)
                }
                # Enable other cell types by default
                return(TRUE)
            })
            values$cell_type_toggles <- as.list(toggle_states)
        }
        
        # Group cell types for better organization
        real_products <- cell_types[grepl("^Real", cell_types, ignore.case = TRUE)]
        primary_cells <- cell_types[grepl("primary|hipsc|ips", cell_types, ignore.case = TRUE)]
        other_cells <- cell_types[!cell_types %in% c(real_products, primary_cells)]
        
        # Create toggle buttons
        create_toggle_section <- function(section_title, cell_list, section_id) {
            if (length(cell_list) == 0) return(NULL)
            
            div(
                class = "cell-type-toggle-section",
                div(class = "cell-type-section-header", section_title),
                div(
                    class = "cell-type-toggle-container",
                    lapply(cell_list, function(ct) {
                        is_active <- values$cell_type_toggles[[ct]] %||% FALSE
                        button_class <- if (is_active) "cell-type-toggle-btn active" else "cell-type-toggle-btn inactive"
                        
                        div(
                            class = "cell-type-toggle",
                            tags$button(
                                class = button_class,
                                id = paste0("toggle_", gsub("[^A-Za-z0-9]", "_", ct)),
                                onclick = paste0("Shiny.setInputValue('cell_type_toggle', {cell_type: '", ct, "', state: !", is_active, "}, {priority: 'event'});"),
                                ct
                            )
                        )
                    })
                )
            )
        }
        
        tagList(
            create_toggle_section("Anatomic Products (Real*)", real_products, "real"),
            create_toggle_section("Primary/hiPSC References", primary_cells, "primary"),
            if (length(other_cells) > 0) create_toggle_section("Other Cell Types", other_cells, "other")
        )
    })

    # Handle cell type toggle clicks
    observeEvent(input$cell_type_toggle, {
        if (!is.null(input$cell_type_toggle$cell_type)) {
            cell_type <- input$cell_type_toggle$cell_type
            new_state <- input$cell_type_toggle$state
            
            # Update the toggle state
            values$cell_type_toggles[[cell_type]] <- new_state
            
            # Trigger re-render of the toggles UI
            # The renderUI will automatically reflect the new states
        }
    })

    # Optimized expression boxplot with reduced reactive dependencies
    output$expression_histogram <- renderPlotly({
        # Direct reactive triggers instead of going through contrast_data
        req(values$expression_data, values$sample_data, input$data_type)
        
        # Get genes to plot (paginated)
        genes_to_plot <- current_page_genes()
        
        # Early return if no genes selected
        if (is.null(genes_to_plot) || length(genes_to_plot) == 0) {
            return(plotly_empty(type = "scatter", mode = "markers") %>%
                add_annotations(
                    text = "Select genes to display expression plots",
                    showarrow = FALSE
                ))
        }

        # Create plot data based on mode - optimized to only process needed genes
        if (values$app_mode == "target") {
            # Target mode: use toggle states
            if (length(values$cell_type_toggles) == 0) {
                return(plotly_empty(type = "scatter", mode = "markers") %>%
                    add_annotations(
                        text = "Select cell types to display expression plots",
                        showarrow = FALSE
                    ))
            }
            
            # Get enabled cell types
            enabled_cell_types <- names(values$cell_type_toggles)[sapply(values$cell_type_toggles, function(x) x == TRUE)]
            if (length(enabled_cell_types) == 0) {
                return(plotly_empty(type = "scatter", mode = "markers") %>%
                    add_annotations(
                        text = "Enable cell types to display expression plots",
                        showarrow = FALSE
                    ))
            }
            
            # Optimized data filtering - only get data for selected genes and enabled cell types
            celltype_col <- get_celltype_column(values$sample_data)
            sample_col <- get_sample_column(values$sample_data)
            
            # Filter samples first (smaller dataset)
            target_samples <- values$sample_data %>%
                filter(!!sym(celltype_col) %in% enabled_cell_types) %>%
                select(all_of(c(sample_col, celltype_col))) %>%
                rename(sample = !!sample_col, celltype = !!celltype_col)
            
            # Then filter expression data for only needed genes and samples
            plot_data <- values$expression_data %>%
                filter(
                    gene %in% genes_to_plot,  # Filter genes first
                    sample %in% target_samples$sample,  # Then samples
                    data_type == input$data_type  # Then data type
                ) %>%
                left_join(target_samples, by = "sample") %>%
                mutate(violin_group = celltype)
                
        } else {
            # Explorer mode: use group1/group2 logic - optimized
            req(input$group1, input$group2)
            
            # Create optimized contrast data for only the needed genes
            plot_data <- create_contrast_data(
                values$expression_data %>% filter(gene %in% genes_to_plot),  # Pre-filter genes
                values$sample_data,
                input$group1,
                input$group2,
                input$data_type
            )
        }

        # Create Y-axis label based on data type
        y_label <- if (input$data_type == "log2_cpm") {
            "Expression Level (log2(CPM + 1))"
        } else {
            "Expression Level (VST)"
        }

        # Create optimized subtitle
        data_type_label <- if (input$data_type == "log2_cpm") "log2(CPM + 1)" else "VST"
        all_selected_genes <- current_genes()
        genes_per_page <- 5
        
        subtitle_parts <- paste0("Data: ", data_type_label)
        
        # Add pagination info if needed
        if (!is.null(all_selected_genes) && length(all_selected_genes) > genes_per_page) {
            total_genes <- length(all_selected_genes)
            max_pages <- ceiling(total_genes / genes_per_page)
            subtitle_parts <- paste(subtitle_parts, 
                paste0("| Showing ", length(genes_to_plot), " of ", total_genes, " genes (Page ", values$gene_page, "/", max_pages, ")"))
        }
        
        # Assign subtitle text for use in plot
        subtitle_text <- subtitle_parts

        # Check if we have data after filtering
        if (is.null(plot_data) || nrow(plot_data) == 0) {
            return(plotly_empty(type = "scatter", mode = "markers") %>%
                add_annotations(
                    text = "No expression data available for selected genes",
                    showarrow = FALSE
                ))
        }

        # Optimized plot creation
        plot_theme <- get_plot_theme(app_theme)

        # Color mapping based on mode
        if (values$app_mode == "target") {
            unique_groups <- unique(plot_data$violin_group)
            n_groups <- length(unique_groups)
            
            # Efficient color palette generation
            if (n_groups <= 3) {
                colors_used <- plot_theme$colors[1:n_groups]
            } else {
                colors_used <- c(
                    plot_theme$colors,
                    RColorBrewer::brewer.pal(min(8, max(3, n_groups - length(plot_theme$colors))), "Set2")
                )[1:n_groups]
            }
            
            group_colors <- setNames(colors_used, unique_groups)
            plot_title <- "Expression Distribution in Target Mode"
        } else {
            group_colors <- setNames(plot_theme$colors, c(input$group1, input$group2, "Other"))
            plot_title <- "Expression Distribution of Selected Genes"
        }

        # Create plot with minimal processing
        p <- ggplot(plot_data, aes(x = violin_group, y = expression, fill = violin_group, text = paste("Sample:", sample, "<br>Gene:", gene))) +
            geom_boxplot(alpha = 0.7, outlier.shape = NA) +
            scale_fill_manual(
                name = "Sample Group",
                values = group_colors
            ) +
            facet_wrap(~gene, scales = "free_y", nrow = 1) +  # Changed to nrow = 1
            labs(
                title = plot_title,
                subtitle = subtitle_text,
                x = NULL,
                y = y_label
            ) +
            theme_minimal() +
            theme(
                legend.position = "bottom",
                plot.title = element_text(size = plot_theme$title_size, face = "bold", color = plot_theme$text_color),
                plot.subtitle = element_text(size = plot_theme$subtitle_size, color = app_theme$text_light),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8, color = plot_theme$text_color),
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
                    layout(
                        showlegend = TRUE,
                        legend = list(
                            orientation = "h",   # horizontal orientation
                            x = 0.5,            # center horizontally
                            xanchor = 'center', # anchor at center
                            y = -0.2,           # position below the plot
                            yanchor = 'top'     # anchor at top of legend
                        )
                    ) %>%
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

    # Single gene set heatmap - now uses target heatmap functionality for explorer mode
    output$gene_set_heatmap <- renderPlotly({
        req(values$expression_data, values$sample_data, input$data_type)
        
        # Use unified gene selection
        selected_genes <- current_genes()
        
        if (is.null(selected_genes) || length(selected_genes) == 0) {
            return(create_empty_plot("Select genes to display heatmap", 
                                   get_plot_theme(app_theme)))
        }

        # For explorer mode, use ALL cell types (same as target mode) - no filtering
        # This makes it an exact copy of the target mode heatmap
        enabled_types <- NULL

        # Create heatmap using the same function as target mode
        plot <- create_target_heatmap(
            expression_data = values$expression_data,
            sample_data = values$sample_data,
            selected_genes = selected_genes,
            data_type = input$data_type,
            plot_theme = list(
                background = app_theme$background_white,
                text_color = app_theme$text_primary
            ),
            enabled_cell_types = enabled_types,
            mode = "explorer"
        )
        
        return(plot)
    })

    # Render dynamic comparison title
    output$contrast_title <- renderText({
        if (values$app_mode == "target") {
            # Target mode: show active cell types
            active_types <- active_cell_types()
            if (is.null(active_types) || length(active_types) == 0) {
                return("Select cell types using toggles")
            } else if (length(active_types) == 1) {
                return(paste("Analysis for:", active_types[1]))
            } else {
                return(paste("Analysis for", length(active_types), "cell types"))
            }
        } else {
            # Explorer mode: traditional group comparison
            if (is.null(input$group1) || is.null(input$group2)) {
                return("Select groups for comparison")
            }
            paste("Comparing:", input$group1, "vs", input$group2)
        }
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
        # Safely extract expression values, handling both data frame and list structures
        expr_values <- if (is.data.frame(contrast_expression)) {
            contrast_expression$expression
        } else if (is.list(contrast_expression) && "expression" %in% names(contrast_expression)) {
            contrast_expression$expression
        } else {
            return(div(
                class = "stat-card",
                div("Unable to extract expression data", class = "stat-card-content")
            ))
        }
        
        # Ensure expr_values is numeric
        if (!is.numeric(expr_values) || length(expr_values) == 0) {
            return(div(
                class = "stat-card",
                div("No numeric expression data available", class = "stat-card-content")
            ))
        }
        
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

        # Get enabled cell types for target mode filtering
        enabled_types <- if (values$app_mode == "target") {
            active_cell_types()
        } else {
            NULL
        }
        
        # Filter sample data by enabled cell types first if in target mode
        filtered_sample_data <- if (!is.null(enabled_types) && length(enabled_types) > 0) {
            values$sample_data %>% filter(!!sym(celltype_col) %in% enabled_types)
        } else {
            values$sample_data
        }

        # Join and filter for selected genes and data type using filtered samples
        plot_data <- values$expression_data %>%
            filter(
                gene %in% selected_genes,
                data_type == input$data_type
            ) %>%
            left_join(
                filtered_sample_data %>%
                    select(all_of(c(sample_col, celltype_col))) %>%
                    rename(sample = !!sample_col, celltype = !!celltype_col),
                by = "sample"
            ) %>%
            filter(!is.na(celltype))

        if (nrow(plot_data) == 0) {
            return(plotly_empty(type = "scatter", mode = "markers") %>%
                add_annotations(
                    text = "No expression data available for selected cell types",
                    showarrow = FALSE
                ))
        }

        # Define product categories for color coding
        real_products <- c("RealDRGx", "RealDRG", "RealMOTO", "RealMELO", "RealDHN", "RealSCP")
        hipsc_products <- c("hiPSCMN", "hiPSCMelo_1", "hiPSCMelo_2")
        primary_products <- c("hDRG", "hMelo_1", "hSCP")

        # Calculate mean expression by group (celltype) with category information
        group_means <- plot_data %>%
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

        # Create the base bar plot with category-based colors
        p <- plot_ly() %>%
            add_bars(
                data = group_means,
                x = ~mean_expression,
                y = ~reorder(celltype, mean_expression),
                orientation = 'h',
                name = "Group Average",
                marker = list(
                    color = ~bar_color,
                    opacity = 0.8,
                    line = list(color = "white", width = 1)
                ),
                hovertemplate = paste0(
                    "<b>Group:</b> %{y}<br>",
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

        # Create DT with comprehensive error handling and warning suppression
        suppressWarnings({
            suppressMessages({
                tryCatch({
                    # Ensure data is properly formatted for DT
                    safe_data <- expression_subset
                    
                    # Validate column structure
                    if (ncol(safe_data) < 2) {
                        stop("Insufficient columns in data")
                    }
                    
                    dt <- DT::datatable(
                        safe_data,
                        options = list(
                            pageLength = 25,
                            scrollX = TRUE,
                            scrollY = "400px",  # Reduced height to fit better in container
                            scrollCollapse = TRUE,
                            dom = "Bfrtip",
                            buttons = c("copy", "csv", "excel"),
                            columnDefs = list(
                                list(className = "dt-center", targets = "_all"),
                                list(width = "100px", targets = c(0, 1)),  # Reduced width for first two columns
                                list(width = "80px", targets = 2:(ncol(safe_data)-1))  # Fixed width for gene columns
                            ),
                            language = list(emptyTable = "No gene data available for selected criteria"),
                            autoWidth = FALSE,
                            responsive = TRUE,
                            fixedColumns = list(leftColumns = 2)  # Fix first two columns for better navigation
                        ),
                        rownames = FALSE,
                        class = "compact stripe hover cell-border",
                        extensions = c("Buttons", "Responsive", "FixedColumns")  # Add FixedColumns extension
                    )
                    
                    # Apply conditional formatting only in explorer mode with valid inputs
                    if (values$app_mode == "explorer" && "Group" %in% colnames(safe_data) && 
                        !is.null(input$group1) && !is.null(input$group2) &&
                        input$group1 != "" && input$group2 != "") {
                        
                        group_values <- unique(safe_data$Group)
                        valid_groups <- c(input$group1, input$group2)
                        existing_groups <- valid_groups[valid_groups %in% group_values]
                        
                        if (length(existing_groups) >= 2) {
                            tryCatch({
                                colors <- c(
                                    paste0("rgba(", paste(col2rgb(app_theme$plot_group1), collapse = ","), ",0.1)"),
                                    paste0("rgba(", paste(col2rgb(app_theme$plot_group2), collapse = ","), ",0.1)")
                                )
                                
                                dt <- dt %>% DT::formatStyle(
                                    "Group",
                                    backgroundColor = DT::styleEqual(existing_groups, colors[1:length(existing_groups)])
                                )
                            }, error = function(format_error) {
                                # Silently continue without formatting if it fails
                                NULL
                            })
                        }
                    }
                    dt
                }, error = function(e) {
                    # Return minimal table on any error
                    basic_data <- if (nrow(expression_subset) > 0) expression_subset else data.frame(Message = "No data available")
                    return(DT::datatable(
                        basic_data,
                        options = list(
                            pageLength = 25, 
                            scrollX = TRUE, 
                            scrollY = "500px",
                            scrollCollapse = TRUE,
                            dom = "tip",
                            autoWidth = FALSE,
                            responsive = TRUE
                        ),
                        rownames = FALSE,
                        class = "compact stripe hover cell-border"
                    ))
                })
            })
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
    # Target Mode Server Logic - Placeholder Outputs
    # =============================================================================
    
    # Portfolio ranking plot for target mode
    output$portfolio_ranking_plot <- renderPlotly({
        req(values$expression_data, values$sample_data, input$data_type)
        
        # Use unified gene selection
        selected_genes <- current_genes()
        
        if (is.null(selected_genes) || length(selected_genes) == 0) {
            return(create_empty_plot("Select genes to display portfolio ranking", 
                                   get_plot_theme(app_theme)))
        }
        
        # Get active cell types for filtering in target mode
        enabled_types <- if (values$app_mode == "target") {
            active_cell_types()
        } else {
            NULL
        }
        
        # Create portfolio ranking plot using the new function
        plot <- create_portfolio_ranking_plot(
            expression_data = values$expression_data,
            sample_data = values$sample_data,
            selected_genes = selected_genes,
            data_type = input$data_type,
            plot_theme = list(
                primary_color = app_theme$primary_color,
                text_secondary = app_theme$text_secondary,
                background = app_theme$background_white,
                text_color = app_theme$text_primary
            ),
            enabled_cell_types = enabled_types
        )
        
        return(plot)
    })
    
    # Portfolio summary table for target mode
    output$portfolio_summary_table <- DT::renderDataTable({
        req(values$expression_data, values$sample_data, input$data_type)
        
        # Use unified gene selection
        selected_genes <- current_genes()
        
        if (is.null(selected_genes) || length(selected_genes) == 0) {
            return(data.frame(Message = "Select genes to display portfolio summary"))
        }
        
        # Get active cell types for filtering in target mode
        enabled_types <- if (values$app_mode == "target") {
            active_cell_types()
        } else {
            NULL
        }
        
        # Create portfolio summary table using the new function
        table_data <- create_portfolio_summary_table(
            expression_data = values$expression_data,
            sample_data = values$sample_data,
            selected_genes = selected_genes,
            data_type = input$data_type,
            enabled_cell_types = enabled_types
        )
        
        # Create and return formatted DT table
        dt <- DT::datatable(
            table_data,
            options = list(
                dom = "t",
                pageLength = 15,
                scrollX = TRUE,
                scrollY = "200px",  # Further reduced height to prevent spilling
                scrollCollapse = TRUE,
                ordering = TRUE,
                order = list(list(2, "asc")),  # Order by ranking column
                autoWidth = FALSE,
                responsive = TRUE,
                columnDefs = list(
                    list(targets = c(0, 1), className = "dt-left", width = "100px"),      # Reduced width for Product and Category
                    list(targets = c(2, 3), className = "dt-center", width = "70px"),    # Reduced width for Ranking and Expression Level
                    list(targets = c(4, 5, 6, 7), className = "dt-right", width = "80px") # Reduced width for numeric columns
                )
            ),
            rownames = FALSE,
            class = "compact stripe hover cell-border nowrap"  # Add nowrap to prevent text overflow
        )
        
        # Apply formatting if we have the proper columns
        if ("Product" %in% colnames(table_data)) {
            dt <- dt %>%
                DT::formatStyle(
                    "Product",
                    fontWeight = "bold",
                    color = "#2C3E50"
                )
        }
        
        if ("Category" %in% colnames(table_data)) {
            dt <- dt %>%
                DT::formatStyle(
                    "Category",
                    backgroundColor = DT::styleEqual(
                        c("Real* Products", "Human iPSC", "Primary Cells", "Other"),
                        c("rgba(220, 20, 60, 0.1)", "rgba(255, 140, 0, 0.1)", "rgba(34, 139, 34, 0.1)", "rgba(128, 128, 128, 0.1)")
                    ),
                    fontWeight = "bold"
                )
        }
        
        if ("Level" %in% colnames(table_data)) {
            dt <- dt %>%
                DT::formatStyle(
                    "Level",
                    backgroundColor = DT::styleEqual(
                        c("Very High", "High", "Medium", "Low", "Very Low"),
                        c("#d4edda", "#d1ecf1", "#fff3cd", "#f8d7da", "#f5f5f5")
                    ),
                    fontWeight = "500"
                )
        }
        
        # Apply numeric formatting if we have numeric columns
        if (ncol(table_data) >= 8) {
            dt <- dt %>%
                DT::formatRound(columns = c(6, 7, 8), digits = 3)
        }
        
        return(dt)
    })
    
    # Target gene heatmap for target mode
    output$target_gene_heatmap <- renderPlotly({
        req(values$expression_data, values$sample_data, input$data_type)
        
        # Use unified gene selection
        selected_genes <- current_genes()
        
        if (is.null(selected_genes) || length(selected_genes) == 0) {
            return(create_empty_plot("Select genes to display target heatmap", 
                                   get_plot_theme(app_theme)))
        }
        
        # Get active cell types for filtering in target mode
        enabled_types <- if (values$app_mode == "target") {
            active_cell_types()
        } else {
            NULL
        }
        
        # Create target heatmap using the new function
        plot <- create_target_heatmap(
            expression_data = values$expression_data,
            sample_data = values$sample_data,
            selected_genes = selected_genes,
            data_type = input$data_type,
            plot_theme = list(
                background = app_theme$background_white,
                text_color = app_theme$text_primary
            ),
            enabled_cell_types = enabled_types,
            mode = "target"
        )
        
        return(plot)
    })
    
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
    
    # Summary table of found genes - REMOVED: Redundant Found Genes Summary
    # output$coexpressed_genes_summary <- DT::renderDataTable({
    #     results <- coexpression_results()
    #     
    #     if (is.null(results) || nrow(results$similar_genes) == 0) {
    #         return(data.frame(Message = "No results"))
    #     }
    #     
    #     summary_table <- results$similar_genes %>%
    #         select(Gene = similar_gene, `Avg Corr` = avg_correlation) %>%
    #         mutate(`Avg Corr` = round(`Avg Corr`, 3))
    #     
    #     suppressWarnings({
    #         DT::datatable(
    #             summary_table,
    #             options = list(
    #                 pageLength = 10,
    #                 dom = "t",
    #                 ordering = FALSE,
    #                 scrollY = "200px",
    #                 scrollCollapse = TRUE
    #             ),
    #             rownames = FALSE,
    #             selection = "none"
    #         ) %>%
    #             DT::formatStyle("Gene", fontWeight = "bold")
    #     })
    # })
    
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
        
        suppressWarnings({
            DT::datatable(
                detailed_table,
                options = list(
                    pageLength = 15,
                    scrollX = TRUE,
                    scrollY = "400px",
                    scrollCollapse = TRUE,
                    dom = "Bfrtip",
                    buttons = c("copy", "csv", "excel"),
                    autoWidth = FALSE,
                    responsive = TRUE,
                    columnDefs = list(
                        list(className = "dt-center", targets = "_all"),
                        list(width = "100px", targets = 0),  # Gene column
                        list(width = "90px", targets = 1:ncol(detailed_table)-1)  # Other columns
                    )
                ),
                rownames = FALSE,
                extensions = c("Buttons", "Responsive"),
                selection = "none",
                class = "compact stripe hover cell-border"
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
    })
}

# Run the application
shinyApp(ui = ui, server = server)
