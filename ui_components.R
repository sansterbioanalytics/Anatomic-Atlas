# =============================================================================
# UI Components Module for Anatomic RNA Atlas
# Contains reusable UI components and layout functions
# =============================================================================

# Required libraries for this module
suppressPackageStartupMessages({
    library(shiny)
    library(shinydashboard)
    library(DT)
    library(shinycssloaders)
})

# =============================================================================
# Header Components
# =============================================================================

# Create application header with logo
create_app_header <- function(theme) {
    dashboardHeader(
        title = "Anatomic RNA Atlas",
        tags$li(
            class = "dropdown logo-container",
            tags$a(
                href = theme$logo_link_url,
                target = "_blank",
                class = "logo-link",
                img(src = "anatomic_logo.png", class = "logo-primary", alt = "Anatomic Logo")
            )
        )
    )
}

# =============================================================================
# Sidebar Components
# =============================================================================

# Create main sidebar with analysis controls
create_app_sidebar <- function(theme, width = 300) {
    dashboardSidebar(
        width = width,
        h4("Analysis Controls", style = paste0("margin: ", theme$spacing_lg, "; color: ", theme$text_white, ";")),
        
        # Contrast selection section
        create_contrast_selection_ui(theme),
        
        # Gene selection section
        create_gene_selection_ui(theme),
        
        # Analysis options section
        create_analysis_options_ui(theme)
    )
}

# Contrast selection UI component
create_contrast_selection_ui <- function(theme) {
    div(
        style = paste0("margin: ", theme$spacing_lg, ";"),
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
        br()
    )
}

# Gene selection UI component
create_gene_selection_ui <- function(theme) {
    div(
        style = paste0("margin: ", theme$spacing_lg, ";"),
        
        # Section header
        h5("Gene Selection", style = paste0("color: ", theme$text_white, "; margin-bottom: ", theme$spacing_sm, ";")),
        
        # Gene set selection dropdown
        selectInput("gene_set_selection",
            "Choose Gene Set:",
            choices = NULL,
            selected = "Custom Genes"
        ),
        
        # File upload for custom gene sets
        fileInput("gene_file_upload",
            "Upload Custom Gene Set:",
            accept = c(".csv", ".tsv", ".txt"),
            placeholder = "No file selected"
        ),
        helpText("Upload CSV/TSV with gene symbols in first column. Selected genes will be used across all plots.",
            class = "help-text"
        ),
        
        # Interactive gene search (conditional)
        conditionalPanel(
            condition = "input.gene_set_selection == 'Custom Genes'",
            selectizeInput("selected_genes",
                "Search & Select Genes:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                    placeholder = "Search for genes...",
                    maxItems = 20,
                    create = FALSE
                )
            )
        ),
        
        # Display selected genes information
        uiOutput("selected_genes_display"),
        
        # Gene pagination controls
        create_gene_pagination_ui(theme),
        
        br()
    )
}

# Gene pagination UI component
create_gene_pagination_ui <- function(theme) {
    conditionalPanel(
        condition = "output.show_gene_pagination",
        div(
            style = paste0("margin-top: ", theme$spacing_sm, ";"),
            h6("Expression Plot Controls", style = paste0("color: ", theme$text_white, "; margin-bottom: ", theme$spacing_xs, ";")),
            fluidRow(
                column(6, 
                    actionButton("prev_genes", "← Prev", 
                               class = "btn-outline-primary btn-sm btn-block",
                               style = paste0("color: ", theme$text_white, "; border-color: ", theme$text_white, ";"))
                ),
                column(6,
                    actionButton("next_genes", "Next →", 
                               class = "btn-outline-primary btn-sm btn-block",
                               style = paste0("color: ", theme$text_white, "; border-color: ", theme$text_white, ";"))
                )
            ),
            div(
                style = paste0("text-align: center; margin-top: ", theme$spacing_xs, "; color: ", theme$text_light, "; font-size: ", theme$font_size_small, ";"),
                textOutput("gene_pagination_info", inline = TRUE)
            )
        )
    )
}

# Analysis options UI component
create_analysis_options_ui <- function(theme) {
    div(
        style = paste0("margin: ", theme$spacing_lg, ";"),
        
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
}

# =============================================================================
# Main Content Components
# =============================================================================

# Create loading overlay
create_loading_overlay <- function(theme) {
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
    )
}

# Create main expression plot box
create_expression_plot_box <- function() {
    box(
        title = "Expression Comparison",
        status = "primary",
        solidHeader = TRUE,
        width = 7,
        height = "900px",
        tabsetPanel(
            create_expression_distribution_tab(),
            create_gene_heatmap_tab(),
            create_coexpression_tab()
        )
    )
}

# Expression distribution tab
create_expression_distribution_tab <- function() {
    tabPanel(
        "Expression Distribution",
        # Navigation controls for gene pagination
        conditionalPanel(
            condition = "output.show_gene_pagination",
            fluidRow(
                column(12,
                    div(
                        style = "margin-bottom: 15px; padding: 8px 12px; background-color: #f8f9fa; border-radius: 5px; text-align: center;",
                        fluidRow(
                            column(4,
                                actionButton("prev_genes", "← Previous 10", 
                                           class = "btn-sm btn-default", 
                                           style = "width: 100%; margin-right: 5px;")
                            ),
                            column(4,
                                div(
                                    style = "padding-top: 5px; font-size: 12px; color: #666;",
                                    uiOutput("gene_pagination_info")
                                )
                            ),
                            column(4,
                                actionButton("next_genes", "Next 10 →", 
                                           class = "btn-sm btn-default",
                                           style = "width: 100%; margin-left: 5px;")
                            )
                        )
                    )
                )
            )
        ),
        plotlyOutput("expression_histogram", height = "650px") %>% withSpinner()
    )
}

# Gene heatmap tab
create_gene_heatmap_tab <- function() {
    tabPanel(
        "Gene Set Heatmap",
        plotlyOutput("gene_set_heatmap", height = "800px") %>% withSpinner()
    )
}

# Co-expression analysis tab
create_coexpression_tab <- function() {
    tabPanel(
        "Co-Expression Analysis",
        fluidRow(
            column(
                width = 3,
                create_coexpression_controls()
            ),
            column(
                width = 9,
                create_coexpression_results()
            )
        )
    )
}

# Co-expression controls panel
create_coexpression_controls <- function() {
    wellPanel(
        h5("Co-Expression Controls", style = "margin-top: 0;"),
        helpText("Uses currently selected genes to find correlated co-expressed genes across the Atlas (May take >5 minutes).",
               class = "help-text"),
        selectInput("coexpression_data_type",
            "Data Type for Analysis:",
            choices = list(
                "log2(CPM + 1)" = "log2_cpm",
                "VST (Variance Stabilized)" = "vst"
            ),
            selected = "log2_cpm"
        ),
        helpText("Data type selection is independent of the main plot data type. Choose the normalization method for correlation analysis.",
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
            "Batch Size:",
            value = 250,
            min = 100,
            max = 2000,
            step = 50
        ),
        checkboxInput("use_parallel",
            "Use parallel processing",
            value = FALSE
        ),
        helpText("Analysis uses streaming mode with automatic memory management. Smaller batch sizes use less memory.",
               class = "help-text"),
        helpText("Parallel processing can help with very large datasets but may be unstable.",
               class = "help-text"),
        br(),
        actionButton("run_coexpression", 
                   "Find Co-expressed Genes", 
                   class = "btn-primary btn-block")
    )
}

# Co-expression results panel
create_coexpression_results <- function() {
    tabsetPanel(
        tabPanel(
            "Analysis & Results",
            fluidRow(
                column(
                    width = 4,
                    h6("Analysis Progress", style = "font-weight: bold;"),
                    create_coexpression_log_panel(),
                    br(),
                    h6("Found Genes Summary", style = "font-weight: bold;"),
                    DT::dataTableOutput("coexpressed_genes_summary", height = "250px")
                ),
                column(
                    width = 8,
                    h6("Co-expressed Genes Details", style = "margin-top: 0px;"),
                    DT::dataTableOutput("coexpression_detailed_table", height = "600px") %>%
                        shinycssloaders::withSpinner()
                )
            )
        ),
        tabPanel(
            "Expression Heatmap",
            plotlyOutput("coexpression_heatmap", height = "700px") %>% 
                shinycssloaders::withSpinner()
        ),
        tabPanel(
            "Correlation Network",
            networkD3::forceNetworkOutput("coexpression_network", height = "700px") %>%
                shinycssloaders::withSpinner()
        )
    )
}

# Co-expression log panel
create_coexpression_log_panel <- function() {
    div(
        id = "coexpression_log_container",
        style = "height: 300px; overflow-y: auto; border: 1px solid #E9ECEF; border-radius: 4px; padding: 8px; background-color: #F8F9FA;",
        verbatimTextOutput("coexpression_live_log")
    )
}

# Create analysis overview box
create_analysis_overview_box <- function() {
    box(
        title = "Analysis Overview",
        status = "primary",
        solidHeader = TRUE,
        width = 5,
        height = "900px",
        plotlyOutput("group_expression_barplot", height = "320px"),
        h4(textOutput("contrast_title")),
        br(),
        div(
            class = "analysis-stats-container",
            style = "max-height: 480px; overflow-y: auto; padding-right: 8px;",
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
}

# Create expression data table box
create_expression_table_box <- function() {
    box(
        title = textOutput("expression_table_title", inline = TRUE),
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        DT::dataTableOutput("expression_table") %>% withSpinner()
    )
}

# =============================================================================
# Helper UI Functions
# =============================================================================

# Create stat card component
create_stat_card <- function(title, content, theme) {
    div(
        class = "stat-card",
        div(class = "stat-card-header", title),
        div(class = "stat-card-content", content)
    )
}

# Create stat item component
create_stat_item <- function(label, value) {
    div(
        class = "stat-item",
        span(class = "stat-label", label),
        span(class = "stat-value", value)
    )
}

# Create help text component
create_help_text <- function(text, class = "help-text") {
    helpText(text, class = class)
}

# Create section header
create_section_header <- function(title, style = NULL) {
    if (is.null(style)) {
        style <- "font-weight: bold; margin-bottom: 10px;"
    }
    h5(title, style = style)
}

# =============================================================================
# Layout Functions
# =============================================================================

# Create main application layout
create_main_layout <- function(theme) {
    fluidRow(
        create_expression_plot_box(),
        create_analysis_overview_box()
    )
}

# Create bottom section layout
create_bottom_layout <- function() {
    fluidRow(
        create_expression_table_box()
    )
}

# =============================================================================
# Validation Functions
# =============================================================================

# Validate UI component inputs
validate_ui_inputs <- function(input_list, required_inputs) {
    
    missing_inputs <- required_inputs[!required_inputs %in% names(input_list)]
    
    if (length(missing_inputs) > 0) {
        return(list(
            valid = FALSE,
            message = paste("Missing required inputs:", paste(missing_inputs, collapse = ", "))
        ))
    }
    
    return(list(valid = TRUE, message = "All required inputs present"))
}
