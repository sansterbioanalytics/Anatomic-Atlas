# =============================================================================
# Anatomic RNA Atlas - Simple Interactive Visualization Application
# Single-file R Shiny Application for Contrast Analysis
# =============================================================================
# TODO: Review package installation method of rstudio connect apps
# Load required libraries
suppressPackageStartupMessages({
    library(shiny)
    library(shinydashboard)
    library(DT)
    library(plotly)
    library(ggplot2)
    library(dplyr)
    library(readr)
    library(tidyr)
    library(viridis)
    library(RColorBrewer)
    library(shinycssloaders)
})

# =============================================================================
# Data Loading Functions
# =============================================================================

# Function to load pre-computed data with progress
load_atlas_data <- function(progress = NULL) {
    # Need to update this to Rstudio connect data-safe uri
    base_path <- "./"

    if (!is.null(progress)) {
        progress$set(message = "Loading sample metadata...", value = 0.2)
    }

    # Try to load from different possible formats
    sample_data <- NULL
    expression_data <- NULL

    # Load sample metadata
    if (file.exists(file.path(base_path, "sample_metadata.rds"))) {
        sample_data <- readRDS(file.path(base_path, "sample_metadata.rds"))
    }

    if (!is.null(progress)) {
        progress$set(message = "Loading expression data...", value = 0.5)
    }

    # Load expression data (pre-computed VST or normalized counts)
    if (file.exists(file.path(base_path, "expression_data.rds"))) {
        expression_data <- readRDS(file.path(base_path, "expression_data.rds"))
    }

    if (!is.null(progress)) {
        progress$set(message = "Finalizing data setup...", value = 1.0)
    }

    return(list(
        sample_data = sample_data,
        expression_data = expression_data
    ))
}

# Function to load gene sets
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

# =============================================================================
# Data Helper Functions
# =============================================================================

# Helper function to determine celltype column name
get_celltype_column <- function(sample_data) {
    if (is.null(sample_data)) {
        return(NULL)
    }

    if ("PubCelltype" %in% colnames(sample_data)) {
        return("PubCelltype")
    } else if ("Celltype" %in% colnames(sample_data)) {
        return("Celltype")
    } else if ("celltype" %in% colnames(sample_data)) {
        return("celltype")
    }
    return(NULL)
}

# Helper function to determine sample column name
get_sample_column <- function(sample_data) {
    if (is.null(sample_data)) {
        return(NULL)
    }

    if ("PubName" %in% colnames(sample_data)) {
        return("PubName")
    } else if ("Name" %in% colnames(sample_data)) {
        return("Name")
    }
    return(NULL)
}

# Helper function to create pre-filtered contrast data
create_contrast_data <- function(expression_data, sample_data, group1, group2, data_type) {
    if (is.null(expression_data) || is.null(sample_data) ||
        is.null(group1) || is.null(group2) || is.null(data_type)) {
        return(NULL)
    }

    celltype_col <- get_celltype_column(sample_data)
    sample_col <- get_sample_column(sample_data)

    if (is.null(celltype_col) || is.null(sample_col)) {
        return(NULL)
    }

    # Get samples for the two contrast groups
    contrast_samples <- sample_data %>%
        filter(.data[[celltype_col]] %in% c(group1, group2)) %>%
        select(all_of(c(sample_col, celltype_col))) %>%
        rename(sample = !!sample_col, celltype = !!celltype_col)

    if (nrow(contrast_samples) == 0) {
        return(NULL)
    }

    # Filter expression data for contrast samples and data type
    contrast_expression <- expression_data %>%
        filter(
            sample %in% contrast_samples$sample,
            data_type == !!data_type
        ) %>%
        left_join(contrast_samples, by = "sample") %>%
        mutate(
            violin_group = case_when(
                celltype == group1 ~ group1,
                celltype == group2 ~ group2,
                TRUE ~ "Other"
            )
        )

    return(contrast_expression)
}

# =============================================================================
# THEME AND AESTHETIC CONFIGURATION
# =============================================================================

# Define the application theme and styling
app_theme <- list(
    # === CORE BRAND COLORS ===
    primary_color = "#DC143C", # Crimson red - main accent color
    secondary_color = "#8B0000", # Dark red - secondary accent
    tertiary_color = "#FFB6C1", # Light pink - subtle accent

    # === NEUTRAL COLORS ===
    background_white = "#FFFFFF", # Pure white backgrounds
    background_light = "#F8F9FA", # Light gray backgrounds
    background_dark = "#2C3E50", # Dark backgrounds for contrast
    text_primary = "#000000", # Black primary text
    text_secondary = "#4A4A4A", # Dark gray secondary text
    text_light = "#777777", # Light gray tertiary text
    text_white = "#FFFFFF", # White text for dark backgrounds

    # === BORDER AND SEPARATOR COLORS ===
    border_light = "#E9ECEF", # Light borders
    border_medium = "#DEE2E6", # Medium borders
    border_dark = "#6C757D", # Dark borders

    # === COMPONENT-SPECIFIC COLORS ===
    success_color = "#28A745", # Success/positive actions
    warning_color = "#FFC107", # Warning states
    error_color = "#DC3545", # Error states
    info_color = "#17A2B8", # Informational elements

    # === PLOT COLORS ===
    plot_group1 = "#DC143C", # Primary red for group 1
    plot_group2 = "#1f62a5", # Blue-gray for group 2
    plot_other = "#95A5A6", # Light gray for "other" category
    plot_background = "#FFFFFF", # White plot backgrounds

    # === UI SKIN AND LAYOUT ===
    dashboard_skin = "black", # Dashboard skin (red, blue, black, etc.)

    # === TYPOGRAPHY ===
    font_family_primary = "'Segoe UI', 'Helvetica Neue', Arial, sans-serif",
    font_family_mono = "'Consolas', 'Monaco', 'Courier New', monospace",
    font_size_h1 = "24px",
    font_size_h2 = "20px",
    font_size_h3 = "18px",
    font_size_h4 = "16px",
    font_size_h5 = "14px",
    font_size_body = "13px",
    font_size_small = "11px",

    # === SPACING ===
    spacing_xs = "4px",
    spacing_sm = "8px",
    spacing_md = "12px",
    spacing_lg = "16px",
    spacing_xl = "24px",
    spacing_xxl = "32px",

    # === LOGO CONFIGURATION ===
    logo_primary_url = "www/anatomic_logo.png",
    logo_link_url = "https://www.anatomic.com/",

    # === BOX SHADOWS AND EFFECTS ===
    shadow_light = "0 2px 4px rgba(0,0,0,0.1)",
    shadow_medium = "0 4px 8px rgba(0,0,0,0.15)",
    shadow_heavy = "0 8px 16px rgba(0,0,0,0.2)",

    # === BORDER RADIUS ===
    radius_sm = "4px",
    radius_md = "6px",
    radius_lg = "8px",
    radius_xl = "12px"
)

# CSS styling function
generate_app_css <- function(theme) {
    HTML(paste0("
        /* === GLOBAL STYLES === */
        body, .content-wrapper, .right-side {
            background-color: ", theme$background_white, " !important;
            font-family: ", theme$font_family_primary, ";
            color: ", theme$text_primary, ";
        }

        /* === HEADER STYLING === */
        .main-header {
            background-color: ", theme$background_white, " !important;
            border-bottom: 2px solid ", theme$primary_color, " !important;
        }

        .main-header .navbar {
            background-color: ", theme$background_white, " !important;
        }

        .main-header .logo {
            background-color: ", theme$background_white, " !important;
            color: ", theme$text_primary, " !important;
            font-family: ", theme$font_family_primary, ";
            font-weight: bold;
            font-size: ", theme$font_size_h3, ";
            border-right: 1px solid ", theme$border_light, ";
        }

        .main-header .logo:hover {
            background-color: ", theme$background_light, " !important;
        }

        /* === SIDEBAR STYLING === */
        .main-sidebar {
            background-color: ", theme$background_dark, " !important;
        }

        .sidebar {
            background-color: ", theme$background_dark, " !important;
            color: ", theme$text_white, ";
        }

        .sidebar-menu > li > a {
            color: ", theme$text_white, " !important;
            border-left: 3px solid transparent;
        }

        .sidebar-menu > li > a:hover {
            background-color: ", theme$primary_color, " !important;
            border-left-color: ", theme$secondary_color, ";
        }

        /* === CONTENT AREA === */
        .content {
            background-color: ", theme$background_white, " !important;
            padding: ", theme$spacing_lg, ";
        }

        /* === BOX STYLING === */
        .box {
            border-radius: ", theme$radius_md, ";
            box-shadow: ", theme$shadow_light, ";
            border-top: 3px solid ", theme$primary_color, " !important;
            background-color: ", theme$background_white, ";
        }

        .box-header {
            background-color: ", theme$background_light, " !important;
            border-bottom: 1px solid ", theme$border_light, ";
        }

        .box-header .box-title {
            color: ", theme$text_primary, " !important;
            font-weight: bold;
        }

        .box.box-primary .box-header {
            background-color: ", theme$primary_color, " !important;
            color: ", theme$text_white, " !important;
        }

        .box.box-primary .box-header .box-title {
            color: ", theme$text_white, " !important;
        }

        .box.box-info .box-header {
            background-color: ", theme$info_color, " !important;
            color: ", theme$text_white, " !important;
        }

        .box.box-info .box-header .box-title {
            color: ", theme$text_white, " !important;
        }

        /* === FORM CONTROLS === */
        .form-control {
            border: 1px solid ", theme$border_medium, ";
            border-radius: ", theme$radius_sm, ";
            color: ", theme$text_primary, ";
        }

        .form-control:focus {
            border-color: ", theme$primary_color, ";
            box-shadow: 0 0 0 0.2rem rgba(220, 20, 60, 0.25);
        }

        .btn-primary {
            background-color: ", theme$primary_color, " !important;
            border-color: ", theme$primary_color, " !important;
            color: ", theme$text_white, " !important;
        }

        .btn-primary:hover {
            background-color: ", theme$secondary_color, " !important;
            border-color: ", theme$secondary_color, " !important;
        }

        /* === LOADING OVERLAY === */
        .loading-overlay {
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            background-color: rgba(44, 62, 80, 0.95);
            z-index: 9999;
            display: flex;
            justify-content: center;
            align-items: center;
            flex-direction: column;
        }

        .loading-content {
            text-align: center;
            padding: ", theme$spacing_xxl, ";
            background: ", theme$background_white, ";
            border-radius: ", theme$radius_xl, ";
            box-shadow: ", theme$shadow_heavy, ";
            border: 2px solid ", theme$primary_color, ";
        }

        .loading-title {
            color: ", theme$primary_color, ";
            font-size: ", theme$font_size_h2, ";
            font-weight: bold;
            margin-bottom: ", theme$spacing_lg, ";
            font-family: ", theme$font_family_primary, ";
        }

        .progress-container {
            width: 300px;
            height: 20px;
            background-color: ", theme$background_light, ";
            border-radius: ", theme$radius_lg, ";
            overflow: hidden;
            margin: ", theme$spacing_lg, " 0;
            border: 1px solid ", theme$border_medium, ";
        }

        .progress-bar {
            width: 0%;
            height: 100%;
            background: linear-gradient(90deg, ", theme$primary_color, ", ", theme$secondary_color, ");
            transition: width 0.3s ease;
        }

        .loading-message {
            color: ", theme$text_secondary, ";
            margin: ", theme$spacing_md, " 0;
            font-size: ", theme$font_size_body, ";
        }

        .loading-description {
            color: ", theme$text_light, ";
            font-size: ", theme$font_size_small, ";
        }

        /* === LOGO STYLING === */
        .logo-container {
            display: flex;
            align-items: center;
            gap: ", theme$spacing_md, ";
            margin: ", theme$spacing_md, " 0;
        }

        .logo-primary {
            max-height: 50px;
            max-width: 150px;
            cursor: pointer;
            transition: opacity 0.2s ease;
        }

        .logo-primary:hover {
            opacity: 0.8;
        }

        .logo-link {
            text-decoration: none;
            display: flex;
            align-items: center;
            gap: ", theme$spacing_md, ";
        }

        .logo-link:hover {
            text-decoration: none;
        }

        /* === DATA TABLE STYLING === */
        .dataTables_wrapper {
            font-size: ", theme$font_size_small, ";
        }

        .table-striped > tbody > tr:nth-of-type(odd) {
            background-color: ", theme$background_light, ";
        }

        .dt-group1 {
            background-color: rgba(220, 20, 60, 0.1) !important;
        }

        .dt-group2 {
            background-color: rgba(44, 62, 80, 0.1) !important;
        }

        /* === HELP TEXT AND INSTRUCTIONS === */
        .help-text {
            color: ", theme$text_light, ";
            font-size: ", theme$font_size_small, ";
            font-style: italic;
        }

        .instructions-header {
            color: ", theme$text_primary, ";
            font-weight: bold;
            font-size: ", theme$font_size_h5, ";
        }

        .statistics-header {
            color: ", theme$text_primary, ";
            font-weight: bold;
            font-size: ", theme$font_size_h5, ";
            margin-bottom: ", theme$spacing_sm, ";
        }

        .analysis-stats-container {
            max-height: 320px;
            overflow-y: auto;
            padding-right: ", theme$spacing_sm, ";
        }

        .stat-card {
            background-color: ", theme$background_light, ";
            border: 1px solid ", theme$border_light, ";
            border-radius: ", theme$radius_sm, ";
            padding: ", theme$spacing_sm, ";
            margin-bottom: ", theme$spacing_sm, ";
        }

        .stat-card-header {
            font-weight: bold;
            color: ", theme$primary_color, ";
            font-size: ", theme$font_size_small, ";
            margin-bottom: ", theme$spacing_xs, ";
        }

        .stat-card-content {
            font-size: ", theme$font_size_small, ";
            color: ", theme$text_secondary, ";
            line-height: 1.4;
        }

        .stat-item {
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 2px 0;
            border-bottom: 1px solid ", theme$border_light, ";
        }

        .stat-item:last-child {
            border-bottom: none;
        }

        .stat-label {
            font-weight: 500;
            color: ", theme$text_primary, ";
        }

        .stat-value {
            color: ", theme$text_secondary, ";
            font-family: ", theme$font_family_mono, ";
        }

        .summary-box {
            font-size: ", theme$font_size_small, ";
            line-height: 1.4;
            max-height: 400px;
            overflow-y: auto;
            background-color: ", theme$background_light, ";
            padding: ", theme$spacing_md, ";
            border-radius: ", theme$radius_sm, ";
            border: 1px solid ", theme$border_light, ";
            font-family: ", theme$font_family_mono, ";
        }

        /* === RESPONSIVE DESIGN === */
        @media (max-width: 768px) {
            .logo-container {
                flex-direction: column;
                gap: ", theme$spacing_sm, ";
            }

            .loading-content {
                margin: ", theme$spacing_md, ";
                padding: ", theme$spacing_lg, ";
            }

            .progress-container {
                width: 250px;
            }
        }
    "))
}

# Plot theme function
get_plot_theme <- function(theme) {
    list(
        colors = c(theme$plot_group1, theme$plot_group2, theme$plot_other),
        background = theme$plot_background,
        text_color = theme$text_primary,
        grid_color = theme$border_light,
        title_size = 14,
        subtitle_size = 11,
        axis_text_size = 12,
        legend_text_size = 10
    )
}

# =============================================================================
# UI Definition
# =============================================================================

ui <- dashboardPage(
    skin = app_theme$dashboard_skin,
    dashboardHeader(
        title = div(
            class = "logo-container",
            tags$a(
                href = app_theme$logo_link_url,
                target = "_blank",
                class = "logo-link",
                img(src = "anatomic_logo.png", class = "logo-primary", alt = "Anatomic Logo")
            )
        )
    ),
    dashboardSidebar(
        width = 300,
        h4("Analysis Controls", style = paste0("margin: ", app_theme$spacing_lg, "; color: ", app_theme$text_white, ";")),

        # Contrast selection
        div(
            style = paste0("margin: ", app_theme$spacing_lg, ";"),
            selectInput("group1",
                "Group 1 (Baseline):",
                choices = NULL,
                multiple = FALSE
            ),
            selectInput("group2",
                "Group 2 (Comparison):",
                choices = NULL,
                multiple = FALSE
            ),
            br(),

            # Gene search
            selectizeInput("selected_genes",
                "Select Genes to Highlight:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                    placeholder = "Search for genes...",
                    maxItems = 20,
                    create = FALSE
                )
            ),
            br(),

            # Data type selection
            radioButtons("data_type",
                "Expression Data Type:",
                choices = list(
                    "log2(CPM + 1)" = "log2_cpm",
                    "VST (Variance Stabilizing Transform)" = "vst"
                ),
                selected = "log2_cpm"
            ),
            br(),

            # Plot options
            checkboxInput("show_points", "Show Individual Points", value = TRUE),
            br(),

            # Download button
            downloadButton("download_plot", "Download Plot", class = "btn-primary btn-block")
        )
    ),
    dashboardBody(
        # Apply custom theme CSS
        tags$head(
            tags$style(generate_app_css(app_theme))
        ),

        # Loading overlay (initially visible)
        div(
            id = "loading-overlay", class = "loading-overlay",
            div(
                class = "loading-content",
                div(
                    h3("Loading Anatomic RNA Atlas", class = "loading-title"),
                    div(
                        id = "loading-progress",
                        div(
                            class = "progress-container",
                            div(id = "progress-bar", class = "progress-bar")
                        ),
                        p(id = "loading-message", "Initializing data loading...", class = "loading-message")
                    ),
                    p("Please wait while we load the expression data and sample metadata.",
                        class = "loading-description"
                    )
                )
            )
        ),

        # Add JavaScript for loading progress
        tags$script(HTML("
            Shiny.addCustomMessageHandler('updateProgress', function(data) {
                document.getElementById('progress-bar').style.width = data.percent + '%';
                document.getElementById('loading-message').textContent = data.message;
            });

            Shiny.addCustomMessageHandler('hideLoading', function(data) {
                var overlay = document.getElementById('loading-overlay');
                if (overlay) {
                    overlay.style.display = 'none';
                }
            });

            Shiny.addCustomMessageHandler('addProgressHandlers', function(data) {
                // Any additional JavaScript setup can go here
            });
        ")),
        fluidRow(
            # Main expression plot
            box(
                title = "Expression Comparison",
                status = "primary",
                solidHeader = TRUE,
                width = 7,
                height = "600px",
                tabsetPanel(
                    tabPanel(
                        "Expression Distribution",
                        plotlyOutput("expression_histogram", height = "500px") %>% withSpinner()
                    ),
                    tabPanel(
                        "Gene Set Heatmap",
                        fluidRow(
                            column(
                                width = 3,
                                wellPanel(
                                    h5("Heatmap Controls", style = "margin-top: 0;"),
                                    selectInput("gene_set_selection",
                                        "Choose Gene Set:",
                                        choices = NULL,
                                        selected = "Custom Genes"
                                    ),
                                    hr(),
                                    h6("Upload Custom Gene Set:", style = "font-weight: bold;"),
                                    fileInput("gene_file_upload",
                                        "Upload CSV/TSV file:",
                                        accept = c(".csv", ".tsv", ".txt"),
                                        placeholder = "No file selected"
                                    ),
                                    helpText("File should contain gene symbols, one per line or in first column.",
                                        class = "help-text"
                                    )
                                )
                            ),
                            column(
                                width = 9,
                                plotlyOutput("gene_set_heatmap", height = "500px") %>% withSpinner()
                            )
                        )
                    )
                )
            ),

            # Summary statistics
            box(
                title = "Analysis Overview",
                status = "primary",
                solidHeader = TRUE,
                width = 5,
                height = "600px",
                plotOutput("group_expression_barplot", height = "220px"),
                h4(textOutput("contrast_title")),
                br(),
                div(
                    class = "analysis-stats-container",
                    uiOutput("sample_counts_ui"),
                    conditionalPanel(
                        condition = "input.selected_genes && input.selected_genes.length > 0",
                        h5("Gene Statistics:", class = "statistics-header", style = "margin-top: 12px;"),
                        DT::dataTableOutput("gene_stats_table", height = "200px")
                    ),
                    conditionalPanel(
                        condition = "!input.selected_genes || input.selected_genes.length == 0",
                        uiOutput("overall_stats_ui")
                    )
                )
            )
        ),
        fluidRow(
            # Expression data table
            box(
                title = textOutput("expression_table_title", inline = TRUE),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                DT::dataTableOutput("expression_table") %>% withSpinner()
            )
        )
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
        data_loaded = FALSE
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
            return(plotly_empty() %>%
                add_annotations(
                    text = "Unable to load data for selected groups",
                    showarrow = FALSE
                ))
        }

        # Only plot if genes are selected
        if (length(input$selected_genes) == 0) {
            return(plotly_empty() %>%
                add_annotations(
                    text = "Select genes to display expression plots",
                    showarrow = FALSE
                ))
        }

        # Limit to maximum 10 genes for performance
        genes_to_plot <- head(input$selected_genes, 10)
        if (length(input$selected_genes) > 10) {
            showNotification(
                paste(
                    "Limited to first 10 genes for performance. Selected:",
                    paste(genes_to_plot, collapse = ", ")
                ),
                type = "warning", duration = 3000
            )
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
            return(plotly_empty() %>%
                add_annotations(
                    text = "No expression data available for selected genes",
                    showarrow = FALSE
                ))
        }

        # Create subtitle with data type information
        data_type_label <- if (input$data_type == "log2_cpm") "log2(CPM + 1)" else "VST"
        subtitle_text <- paste0(
            "Expression distribution by group for: ", paste(input$selected_genes, collapse = ", "),
            " (Data: ", data_type_label, ")"
        )

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
            facet_wrap(~gene, scales = "free_y", nrow = 1) +
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
                axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = plot_theme$axis_text_size, color = plot_theme$text_color),
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
        req(input$gene_set_selection)

        # Determine which genes to use
        if (input$gene_set_selection == "Custom Genes") {
            if (length(input$selected_genes) == 0) {
                return(plotly_empty() %>%
                    add_annotations(
                        text = "Select genes to display heatmap or choose a gene set",
                        showarrow = FALSE
                    ))
            }
            selected_genes <- input$selected_genes
            data_type_label <- if (input$data_type == "log2_cpm") "log2(CPM + 1)" else "VST"
            heatmap_title <- paste0("Custom Gene Selection Heatmap (", data_type_label, ")")
        } else if (input$gene_set_selection == "Uploaded Gene Set") {
            if (is.null(values$uploaded_genes) || length(values$uploaded_genes) == 0) {
                return(plotly_empty() %>%
                    add_annotations(
                        text = "Upload a gene set file to display heatmap",
                        showarrow = FALSE
                    ))
            }
            selected_genes <- values$uploaded_genes
            data_type_label <- if (input$data_type == "log2_cpm") "log2(CPM + 1)" else "VST"
            heatmap_title <- paste0("Uploaded Gene Set Heatmap (", data_type_label, ")")
        } else {
            selected_genes <- gene_sets[[input$gene_set_selection]]
            data_type_label <- if (input$data_type == "log2_cpm") "log2(CPM + 1)" else "VST"
            heatmap_title <- paste0(input$gene_set_selection, " Expression Heatmap (", data_type_label, ")")
        }

        # Filter for genes that exist in the dataset
        available_genes <- intersect(selected_genes, unique(values$expression_data$gene))

        if (length(available_genes) == 0) {
            return(plotly_empty() %>%
                add_annotations(
                    text = paste("No genes from selection found in dataset"),
                    showarrow = FALSE
                ))
        }

        # Get expression data for selected groups and genes, filtered by data type
        plot_data_base <- contrast_data()

        if (is.null(plot_data_base)) {
            return(plotly_empty() %>%
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
            return(plotly_empty() %>%
                add_annotations(
                    text = "No expression data available for selected groups",
                    showarrow = FALSE
                ))
        }

        # Prepare matrix for heatmap
        gene_names <- heatmap_data$gene
        expr_matrix <- as.matrix(heatmap_data[, -1])
        rownames(expr_matrix) <- gene_names

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

        # Use helper function to determine celltype column
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
        if (length(input$selected_genes) == 0) {
            return(data.frame(Message = "No genes selected"))
        }

        # Get pre-filtered contrast data
        contrast_expression <- contrast_data()

        if (is.null(contrast_expression)) {
            return(data.frame(Message = "Unable to load data for selected groups"))
        }

        # Filter for selected genes
        contrast_expression <- contrast_expression %>%
            filter(gene %in% input$selected_genes)

        if (nrow(contrast_expression) == 0) {
            return(data.frame(Message = "No expression data available for selected genes"))
        }

        data_type_label <- if (input$data_type == "log2_cpm") "log2(CPM+1)" else "VST"

        # Calculate statistics for each gene
        gene_stats_list <- list()

        for (gene in input$selected_genes) {
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
    output$group_expression_barplot <- renderPlot({
        req(values$expression_data, values$sample_data, input$data_type)
        if (length(input$selected_genes) == 0) {
            return(NULL)
        }

        # Use helper functions to determine column names
        celltype_col <- get_celltype_column(values$sample_data)
        sample_col <- get_sample_column(values$sample_data)

        if (is.null(celltype_col) || is.null(sample_col)) {
            return(NULL)
        }

        # Join and filter for selected genes and data type
        plot_data <- values$expression_data %>%
            filter(
                gene %in% input$selected_genes,
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

        # Create dynamic title based on selected genes
        title_text <- if (length(input$selected_genes) == 1) {
            paste("Mean Expression by Group:", input$selected_genes[1])
        } else {
            paste("Mean Expression by Group (", length(input$selected_genes), "genes averaged)")
        }

        # Bar plot
        ggplot(group_means, aes(x = reorder(celltype, mean_expression), y = mean_expression, fill = celltype)) +
            geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +
            coord_flip() +
            labs(
                title = title_text,
                x = "Group",
                y = if (input$data_type == "log2_cpm") "Mean log2(CPM + 1)" else "Mean VST"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 13, face = "bold"),
                axis.text.y = element_text(size = 10),
                axis.text.x = element_text(size = 10),
                axis.title = element_text(size = 11)
            )
    })

    # Dynamic title for expression table
    output$expression_table_title <- renderText({
        if (length(input$selected_genes) > 0) {
            if (length(input$selected_genes) == 1) {
                paste("Expression Data for", input$selected_genes[1])
            } else {
                paste("Expression Data for", length(input$selected_genes), "Selected Genes")
            }
        } else {
            "Expression Data (First 50 Genes Alphabetically)"
        }
    })

    # Expression data table
    output$expression_table <- DT::renderDataTable({
        # If no genes selected, show all genes (limit to first 1 for performance in default case)
        genes_to_show <- if (length(input$selected_genes) > 0) {
            input$selected_genes
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

        # Rename columns to include data type information
        colnames(expression_subset) <- c(
            "Sample", "Group",
            paste0(colnames(expression_subset)[-(1:2)], data_type_label)
        )

        # Create DT with formatting
        DT::datatable(
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
        ) %>%
            DT::formatStyle(
                "Group",
                backgroundColor = DT::styleEqual(
                    c(input$group1, input$group2),
                    c(
                        paste0("rgba(", paste(col2rgb(app_theme$plot_group1), collapse = ","), ",0.1)"),
                        paste0("rgba(", paste(col2rgb(app_theme$plot_group2), collapse = ","), ",0.1)")
                    )
                )
            )
    })

    # Download handler
    output$download_plot <- downloadHandler(
        filename = function() {
            data_type_label <- if (input$data_type == "log2_cpm") "CPM" else "VST"
            paste0("anatomic_atlas_expression_", input$group1, "_vs_", input$group2, "_", data_type_label, "_", Sys.Date(), ".png")
        },
        content = function(file) {
            req(input$data_type, input$group1, input$group2)

            if (length(input$selected_genes) == 0) {
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
                    gene %in% input$selected_genes,
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
                    subtitle = paste0("Genes: ", paste(input$selected_genes, collapse = ", "), " (Data: ", data_type_label, ")"),
                    x = NULL, # Remove x-axis label
                    y = y_label
                ) +
                theme_minimal() +
                theme(
                    legend.position = "bottom",
                    plot.title = element_text(size = plot_theme$title_size, face = "bold", color = plot_theme$text_color),
                    plot.subtitle = element_text(size = plot_theme$subtitle_size, color = app_theme$text_light),
                    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = plot_theme$axis_text_size, color = plot_theme$text_color),
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
}

# Run the application
shinyApp(ui = ui, server = server)
