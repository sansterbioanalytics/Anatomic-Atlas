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

# Create application header with logo, title, and mode selector
create_app_header <- function(theme) {
    dashboardHeader(
        title = NULL,  # Remove default title to create custom header Review?
        tags$li(
            class = "dropdown custom-header-container",
            style = "width: 100%; display: flex; align-items: center; justify-content: space-between; padding: 8px 20px; margin: 0;",
            
            # Left side: Logo
            div(
                class = "header-logo-section",
                style = "display: flex; align-items: center; flex-shrink: 0;",
                tags$a(
                    href = theme$logo_link_url,
                    target = "_blank",
                    class = "logo-link",
                    img(src = "anatomic_logo.png", class = "logo-primary", alt = "Anatomic Logo", 
                        style = "height: 45px; margin-right: 15px;")
                )
            ),
            
            # Center: Title and Mode Selector
            div(
                class = "header-center-section",
                style = "display: flex; flex-direction: column; align-items: center; flex-grow: 1; margin: 0 20px;",
                
                # Application Title
                h2("Anatomic RNA Atlas", 
                   class = "header-title",
                   style = paste0("margin: 0 0 8px 0; color: ", theme$text_primary, "; font-weight: 700; font-size: 24px; letter-spacing: -0.5px;")),
                
                # Mode Selector Buttons
                create_enhanced_mode_selector_ui(theme)
            ),
            
            # Right side: Spacer (for balance)
            div(
                class = "header-spacer-section",
                style = "width: 60px; flex-shrink: 0;"
            )
        )
    )
}

# =============================================================================
# Mode Selector Component  
# =============================================================================

# Create enhanced mode selector UI component with large aesthetic buttons
create_enhanced_mode_selector_ui <- function(theme) {
    div(
        class = "enhanced-mode-selector",
        style = "display: flex; gap: 12px; align-items: center;",
        
        # Target Mode Button
        actionButton("mode_target",
            HTML("<div style='display: flex; flex-direction: column; align-items: center;'>
                    <i class='fa fa-bullseye' style='font-size: 18px; margin-bottom: 4px;'></i>
                    <span style='font-weight: 600;'>Target Mode</span>
                  </div>"),
            class = "mode-button mode-target-btn",
            style = paste0("
                background: linear-gradient(135deg, ", theme$primary_color, ", ", theme$secondary_color, ");
                border: none;
                border-radius: 12px;
                color: white;
                padding: 12px 24px;
                font-size: 13px;
                font-weight: 500;
                box-shadow: 0 4px 12px rgba(220, 20, 60, 0.3);
                transition: all 0.3s ease;
                cursor: pointer;
                min-width: 120px;
                height: 65px;
            ")
        ),
        
        # Explorer Mode Button  
        actionButton("mode_explorer",
            HTML("<div style='display: flex; flex-direction: column; align-items: center;'>
                    <i class='fa fa-flask' style='font-size: 18px; margin-bottom: 4px;'></i>
                    <span style='font-weight: 600;'>Explorer Mode</span>
                    <small style='font-size: 10px; opacity: 0.8; margin-top: 2px;'>Beta</small>
                  </div>"),
            class = "mode-button mode-explorer-btn",
            style = paste0("
                background: ", theme$background_light, ";
                border: 2px solid ", theme$border_medium, ";
                border-radius: 12px;
                color: ", theme$text_secondary, ";
                padding: 12px 24px;
                font-size: 13px;
                font-weight: 500;
                transition: all 0.3s ease;
                cursor: pointer;
                min-width: 120px;
                height: 65px;
            ")
        ),
        
        # Hidden radio buttons for JavaScript compatibility
        div(style = "display: none;",
            radioButtons("app_mode",
                label = NULL,
                choices = list("Target Mode" = "target", "Explorer Mode" = "explorer"),
                selected = "target"
            )
        )
    )
}

# Create simple mode selector UI component (backup)
create_mode_selector_ui <- function(theme) {
    div(
        style = "display: inline-block;",
        radioButtons("app_mode",
            label = NULL,
            choices = list(
                "Target Mode" = "target",
                "Explorer Mode (Beta)" = "explorer"
            ),
            selected = "target",
            inline = TRUE
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
        
        # Mode-specific controls
        conditionalPanel(
            condition = "input.app_mode == 'target'",
            # Target Mode Controls - Simplified interface
            create_target_mode_controls(theme)
        ),
        
        conditionalPanel(
            condition = "input.app_mode == 'explorer'",
            # Explorer Mode Controls - Full interface (current functionality)
            create_explorer_mode_controls(theme)
        )
    )
}

# Target Mode Controls - Simplified interface focused on gene input
create_target_mode_controls <- function(theme) {
    div(
        class = "target-mode-controls",
        tagList(
            # Simplified contrast selection (limit Group 1 to Real* products)
            create_target_contrast_selection_ui(theme),
            
            # Simplified gene selection - focus on single/multiple gene input
            div(
                class = "gene-selection-container",
                create_target_gene_selection_ui(theme)
            ),
            
            # Primary comparison checkbox (disabled by default)
            div(
                class = "primary-comparison-section",
                create_primary_comparison_checkbox(theme)
            ),
            
            # Basic analysis options
            create_basic_analysis_options_ui(theme)
        )
    )
}

# Explorer Mode Controls - Full interface (existing functionality)
create_explorer_mode_controls <- function(theme) {
    div(
        class = "explorer-mode-controls",
        tagList(
            # Full contrast selection section
            create_contrast_selection_ui(theme),
            
            # Full gene selection section
            create_gene_selection_ui(theme),
            
            # Full analysis options section
            create_analysis_options_ui(theme)
        )
    )
}

# Target mode contrast selection - toggle-based cell type selector
create_target_contrast_selection_ui <- function(theme) {
    div(
        class = "target-mode-contrast-container",
        style = paste0("margin: ", theme$spacing_lg, ";"),
        
        h5("Cell Type Selection", 
           style = paste0("color: ", theme$text_white, "; margin-bottom: ", theme$spacing_md, ";")),
        
        helpText("Toggle cell types to include/exclude from analysis. Real* products are enabled by default.",
                class = "help-text",
                style = paste0("color: ", theme$text_light, "; margin-bottom: ", theme$spacing_md, ";")),
        
        # Dynamic cell type toggle buttons will be populated by server
        uiOutput("cell_type_toggles"),
        
        br()
    )
}

# Target mode gene selection - simplified for single/multiple gene input
create_target_gene_selection_ui <- function(theme) {
    div(
        style = paste0("margin: ", theme$spacing_lg, ";"),
        
        # Section header
        h5("Target Gene Selection", style = paste0("color: ", theme$text_white, "; margin-bottom: ", theme$spacing_sm, ";")),
        
        # Single gene input with search
        selectizeInput("target_gene_input",
            "Enter Gene Symbol(s):",
            choices = NULL,
            multiple = TRUE,
            options = list(
                placeholder = "Type gene symbols...",
                maxItems = 20,
                create = FALSE
            )
        ),
        
        # Display selected genes
        uiOutput("target_genes_display"),
        
        br()
    )
}

# Primary comparison checkbox component
create_primary_comparison_checkbox <- function(theme) {
    div(
        style = paste0("margin: ", theme$spacing_lg, ";"),
        h6("Additional Comparisons", 
           style = paste0("color: ", theme$text_white, "; margin-bottom: ", theme$spacing_sm, "; font-weight: bold;")),
        checkboxInput("include_primary_comparison",
            "Include Primary/hiPSC Cell Comparisons",
            value = FALSE
        ),
        helpText("Adds primary cell data for reference (visually separated from Anatomic products)",
                class = "help-text",
                style = paste0("color: ", theme$text_light, "; margin-top: ", theme$spacing_xs, ";")),
        helpText("Disabled by default to focus on product portfolio",
                class = "help-text", 
                style = paste0("color: ", theme$text_light, "; font-size: 10px;"))
    )
}

# Basic analysis options for target mode
create_basic_analysis_options_ui <- function(theme) {
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
        )
    )
}

# Contrast selection UI component (existing - for explorer mode)
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

# Create main expression plot box (Explorer mode only)
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
            create_coexpression_tab()  # Co-expression only in Explorer mode
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

# Create main application layout with conditional mode rendering
create_main_layout <- function(theme) {
    tagList(
        conditionalPanel(
            condition = "input.app_mode == 'target'",
            create_target_mode_layout()
        ),
        conditionalPanel(
            condition = "input.app_mode == 'explorer'",
            create_explorer_mode_layout()
        )
    )
}

# Target Mode Layout: [Analysis Overview] [Portfolio Ranking Plot]
create_target_mode_layout <- function() {
    fluidRow(
        create_analysis_overview_box_left(),  # Move to left column
        create_portfolio_ranking_box()        # New portfolio ranking on right
    )
}

# Explorer Mode Layout: [Expression Plot] [Analysis Overview] (current)
create_explorer_mode_layout <- function() {
    fluidRow(
        create_expression_plot_box(),
        create_analysis_overview_box()
    )
}

# Analysis overview box for left column (target mode)
create_analysis_overview_box_left <- function() {
    box(
        title = "Analysis Overview",
        status = "primary",
        solidHeader = TRUE,
        width = 6,  # Changed from width = 5 to 6 for better balance
        height = "900px",
        plotlyOutput("group_expression_barplot", height = "320px"),
        h4(textOutput("contrast_title")),
        br(),
        div(
            class = "analysis-stats-container",
            style = "max-height: 480px; overflow-y: auto; padding-right: 8px;",
            uiOutput("sample_counts_ui"),
            conditionalPanel(
                condition = "input.target_gene_input && input.target_gene_input.length > 0 || input.selected_genes && input.selected_genes.length > 0",
                h5("Gene Statistics:", class = "statistics-header", style = "margin-top: 12px;"),
                DT::dataTableOutput("gene_stats_table", height = "200px")
            ),
            conditionalPanel(
                condition = "(!input.target_gene_input || input.target_gene_input.length == 0) && (!input.selected_genes || input.selected_genes.length == 0)",
                uiOutput("overall_stats_ui")
            )
        )
    )
}

# Portfolio ranking box for target mode (no co-expression analysis)
create_portfolio_ranking_box <- function() {
    box(
        title = "Portfolio Expression Ranking",
        status = "primary",
        solidHeader = TRUE,
        width = 6,  # Right column
        height = "900px",
        tabsetPanel(
            tabPanel(
                "Portfolio Overview",
                helpText("Expression ranking across Anatomic product portfolio", 
                        class = "help-text"),
                plotlyOutput("portfolio_ranking_plot", height = "400px") %>% withSpinner(),
                hr(),
                h5("Expression Summary by Product"),
                DT::dataTableOutput("portfolio_summary_table", height = "350px") %>% withSpinner()
            ),
            tabPanel(
                "Target Heatmap",
                helpText("Genes on X-axis, Products on Y-axis", 
                        class = "help-text"),
                plotlyOutput("target_gene_heatmap", height = "800px") %>% withSpinner()
            )
        )
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
