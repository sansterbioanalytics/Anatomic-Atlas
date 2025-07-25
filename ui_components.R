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
        title = div(
            # Custom logo that will replace the default title area
            tags$a(
                href = theme$logo_link_url,
                target = "_blank",
                class = "logo-link",
                style = "display: flex; align-items: center; height: 60px; padding-left: 15px;",
                img(src = "anatomic_logo.png", class = "logo-primary", alt = "Anatomic Logo", 
                    style = "height: 42px; transition: transform 0.2s ease;"),
                onmouseover = "this.querySelector('img').style.transform = 'scale(1.05)'",
                onmouseout = "this.querySelector('img').style.transform = 'scale(1)'"
            )
        ),
        titleWidth = 200,  # Reserve space for logo
        
        # Add title and mode selector in the navbar area
        tags$li(
            class = "dropdown",
            style = "margin: 0; padding: 0; height: 60px; display: flex; align-items: center;",
            
            # Center content container
            div(
                class = "header-center-content",
                style = "display: flex; align-items: center; gap: 30px; height: 60px; margin-left: 20px;",
                
                # Application Title with gradient effect
                h1("Anatomic RNA Atlas", 
                   class = "header-title",
                   style = paste0("margin: 0; font-weight: 700; font-size: 26px; letter-spacing: -0.8px;")),
                
                # Mode Selector Buttons - Modern design
                create_compact_mode_selector_ui(theme)
            )
        )
    )
}

# =============================================================================
# Mode Selector Component  
# =============================================================================

# Create compact mode selector UI component for header - Modern design
create_compact_mode_selector_ui <- function(theme) {
    div(
        class = "compact-mode-selector",
        style = "display: flex; gap: 6px; align-items: center;",
        
        # Use Shiny's radioButtons with custom CSS styling to look like modern toggle buttons
        radioButtons("app_mode",
            label = NULL,
            choices = list(
                "Target Mode" = "target",
                "Explorer Mode" = "explorer"
            ),
            selected = "target",
            inline = TRUE
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
create_app_sidebar <- function(theme, width = 380) {
    dashboardSidebar(
        width = width,
        div(
            style = "height: 100%; overflow-y: auto; padding-right: 10px;",
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
    )
}

# Target Mode Controls - Simplified interface focused on gene input
create_target_mode_controls <- function(theme) {
    div(
        class = "target-mode-controls",
        tagList(
            # 1. Gene selection FIRST - primary control
            div(
                class = "gene-selection-container",
                create_gene_selection_ui(theme)  # Use unified gene selection
            ),
            
            # 2. Cell type selection with visual grouping
            div(
                class = "product-grouping-section",
                create_product_grouping_ui(theme)
            ),
            
            # 3. Analysis options (unified with explorer mode)
            create_analysis_options_ui(theme)
        )
    )
}

# Explorer Mode Controls - Full interface (existing functionality)
create_explorer_mode_controls <- function(theme) {
    div(
        class = "explorer-mode-controls",
        tagList(
            # Gene selection section FIRST - matching target mode layout
            div(
                class = "gene-selection-container",
                create_gene_selection_ui(theme)
            ),
            
            # Group comparison section for explorer mode
            div(
                class = "contrast-selection-container",
                create_contrast_selection_ui(theme)
            ),
            
            # Analysis options section
            create_analysis_options_ui(theme)
        )
    )
}

# Product grouping selection - predefined cell type groups
create_product_grouping_ui <- function(theme) {
    div(
        class = "product-grouping-container",
        style = paste0("margin: ", theme$spacing_sm, ";"),
        
        h5("Cell Type Selection", 
           style = paste0("color: ", theme$text_white, "; margin-bottom: ", theme$spacing_md, ";")),
        
        helpText("Click cell types to enable/disable them for analysis. Real* products are selected by default:",
                class = "help-text",
                style = paste0("color: ", theme$text_light, "; margin-bottom: ", theme$spacing_md, ";")),
        
        # Dynamic grouped cell type toggles will be populated by server
        uiOutput("grouped_cell_type_toggles"),
        
        # Action buttons for group selection
        div(
            class = "group-action-buttons",
            style = paste0("margin-top: ", theme$spacing_md, "; text-align: center;"),
            
            fluidRow(
                column(4,
                    actionButton("select_all_real", "All Real*", 
                               class = "btn btn-sm btn-outline-primary btn-block",
                               style = "font-size: 10px; padding: 4px 8px;")
                ),
                column(4,
                    actionButton("select_all_types", "Select All", 
                               class = "btn btn-sm btn-outline-success btn-block",
                               style = "font-size: 10px; padding: 4px 8px;")
                ),
                column(4,
                    actionButton("select_none", "Clear All", 
                               class = "btn btn-sm btn-outline-secondary btn-block",
                               style = "font-size: 10px; padding: 4px 8px;")
                )
            )
        )
    )
}

# Target mode contrast selection - toggle-based cell type selector (moved to manual override)
create_target_contrast_selection_ui <- function(theme) {
    div(
        class = "target-mode-contrast-container",
        style = paste0("margin: ", theme$spacing_lg, ";"),
        
        h5("Manual Cell Type Override", 
           style = paste0("color: ", theme$text_white, "; margin-bottom: ", theme$spacing_md, ";")),
        
        helpText("Override the product grouping above by manually toggling individual cell types:",
                class = "help-text",
                style = paste0("color: ", theme$text_light, "; margin-bottom: ", theme$spacing_md, ";")),
        
        # Dynamic cell type toggle buttons will be populated by server
        uiOutput("cell_type_toggles"),
        
        br()
    )
}

# Contrast selection UI component (existing - for explorer mode)
create_contrast_selection_ui <- function(theme) {
    div(
        style = paste0("margin: ", theme$spacing_sm, ";"),
        
        # Section header
        h5("Group Comparison", 
           style = paste0("color: ", theme$text_white, "; margin-bottom: ", theme$spacing_md, ";")),
        
        helpText("Select two groups to compare expression levels:",
                class = "help-text",
                style = paste0("color: ", theme$text_light, "; margin-bottom: ", theme$spacing_md, ";")),
        
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

# Gene selection UI component - reorganized with proper order
create_gene_selection_ui <- function(theme) {
    div(
        style = paste0("margin: ", theme$spacing_sm, ";"),
        
        # Section header
        h5("Gene Selection", style = paste0("color: ", theme$text_white, "; margin-bottom: ", theme$spacing_md, ";")),
        
        helpText("Enter gene symbols or select from predefined gene sets:",
                class = "help-text",
                style = paste0("color: ", theme$text_light, "; margin-bottom: ", theme$spacing_md, ";")),
        
        # 1. FIRST: Gene symbol input text area
        textAreaInput(
            "gene_textarea",
            "Gene symbols (comma, space, or line separated):",
            placeholder = "e.g., SCN11A, CACNA1A, KCNQ1",
            value = "SCN11A",  # Default gene
            rows = 6  # Increased from 4 to 6 for better usability
        ),
        
        # 2. SECOND: Real-time validation output - always visible
        uiOutput("gene_validation"),
        
        # 3. THIRD: Gene set selection dropdown
        div(
            style = "margin-top: 15px;",
            selectInput("gene_set_selection",
                "Or choose from predefined gene sets:",
                choices = c("Custom Genes" = "Custom Genes"),
                selected = "Custom Genes"
            ),
            helpText(
                "Select a predefined gene set to populate the text area above.",
                class = "help-text",
                style = paste0("color: ", theme$text_light, "; font-size: ", theme$font_size_small, ";")
            )
        ),
        
        # 4. FOURTH: File upload for large lists
        div(
            style = "margin-top: 15px; padding: 12px; background-color: rgba(255,255,255,0.05); border-radius: 6px; border: 1px solid rgba(255,255,255,0.1);",
            fileInput(
                "gene_file_upload",
                "Or upload gene list (.txt, .csv):",
                accept = c(".csv", ".tsv", ".txt"),
                placeholder = "No file selected"
            ),
            helpText(
                "Upload a file with one gene per line or column.",
                class = "help-text",
                style = paste0("color: ", theme$text_light, "; font-size: ", theme$font_size_small, "; margin-top: 5px;")
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

# Analysis options UI component - shared between modes (consolidated)
create_analysis_options_ui <- function(theme) {
    div(
        style = paste0("margin: ", theme$spacing_sm, ";"),
        
        # Section header
        h5("Analysis Options", 
           style = paste0("color: ", theme$text_white, "; margin-bottom: ", theme$spacing_md, ";")),
        
        helpText("Configure analysis parameters and visualization options:",
                class = "help-text",
                style = paste0("color: ", theme$text_light, "; margin-bottom: ", theme$spacing_md, ";")),
        
        # Data type selection with proper choices
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
        downloadButton("download_plot", "Download Plot", class = "btn-primary btn-sm")
    )
}

# Create expression plot box for explorer mode
create_expression_plot_box <- function() {
    box(
        title = "Expression Distribution",
        status = "primary",
        solidHeader = TRUE,
        width = 7,
        height = "900px",
        tabsetPanel(
            tabPanel(
                "Expression Distribution",
                helpText("Boxplots showing expression distribution for selected genes", 
                        class = "help-text"),
                plotlyOutput("expression_histogram", height = "800px") %>% withSpinner()
            ),
            tabPanel(
                "Gene Set Heatmap", 
                helpText("Heatmap of mean expression across groups", 
                        class = "help-text"),
                plotlyOutput("gene_set_heatmap", height = "800px") %>% withSpinner()
            ),
            tabPanel(
                "Co-Expression Analysis",
                create_coexpression_controls(),
                br(),
                create_coexpression_results()
            )
        )
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
            "Similar Genes Per Query Gene:",
            value = 25,
            min = 1,
            max = 250,
            step = 1
        ),
        helpText("Finds the top N most correlated genes for each query gene, then combines and deduplicates results.",
               class = "help-text"),
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
                condition = "!output.current_genes_available",
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
        # Expression distribution plot at the top
        conditionalPanel(
            condition = "output.current_genes_available",
            h5("Expression Distribution", style = "margin-bottom: 8px;"),
            # Navigation controls for gene pagination
            conditionalPanel(
                condition = "output.show_gene_pagination",
                div(
                    style = "margin-bottom: 8px; padding: 4px 8px; background-color: #f8f9fa; border-radius: 4px; text-align: center;",
                    fluidRow(
                        column(4,
                            actionButton("prev_genes", "← Previous 10", 
                                       class = "btn-xs btn-default", 
                                       style = "width: 100%; font-size: 11px; padding: 2px 4px;")
                        ),
                        column(4,
                            div(
                                style = "padding-top: 3px; font-size: 11px; color: #666;",
                                uiOutput("gene_pagination_info")
                            )
                        ),
                        column(4,
                            actionButton("next_genes", "Next 10 →", 
                                       class = "btn-xs btn-default",
                                       style = "width: 100%; font-size: 11px; padding: 2px 4px;")
                        )
                    )
                )
            ),
            plotlyOutput("expression_histogram", height = "480px") %>% withSpinner(),
            br()
        ),
        h4(textOutput("contrast_title"), style = "margin-bottom: 8px;"),
        div(
            class = "analysis-stats-container",
            style = "max-height: 300px; overflow-y: auto; padding-right: 8px;",
            uiOutput("sample_counts_ui"),
            conditionalPanel(
                condition = "!output.current_genes_available",
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
