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

# Source modular UI components
source("ui_coexpression_module.R")  # Co-expression analysis UI components

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
                "Explorer Mode (BETA)" = "explorer"
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

# Create main sidebar with analysis controls - improved spacing
create_app_sidebar <- function(theme, width = 380) {
    dashboardSidebar(
        width = width,
        div(
            style = "height: 100%; overflow-y: auto; padding: 10px;", # Better padding for readability
            h4("Analysis Controls", 
               style = paste0("margin: 0 0 15px 0; color: ", theme$text_white, "; font-size: 18px; font-weight: bold;")), # Better margins and larger font
            
            # Mode-specific controls - unified components
            conditionalPanel(
                condition = "input.app_mode == 'target'",
                create_target_mode_controls(theme)
            ),
            
            conditionalPanel(
                condition = "input.app_mode == 'explorer'",
                create_explorer_mode_controls(theme)
            )
        )
    )
}

# Target Mode Controls - Using Explorer Mode gene selection structure - improved spacing
create_target_mode_controls <- function(theme) {
    div(
        class = "target-mode-controls",
        style = "padding: 0;", # No additional padding needed with improved component spacing
        tagList(
            # Gene selection section - reuse unified component (same as Explorer)
            create_gene_selection_ui(theme),
            
            # Cell type selection with visual grouping - unique to target mode
            div(
                class = "product-grouping-section",
                style = "margin-top: 12px;", # Better margin between sections
                create_product_grouping_ui(theme)
            ),
            
            # Analysis options - reuse unified component (same as Explorer)
            div(
                class = "analysis-options-section",
                style = "margin-top: 12px;", # Better margin between sections
                create_analysis_options_ui(theme)
            )
        )
    )
}

# Explorer Mode Controls - Compact interface with group comparison - improved spacing
create_explorer_mode_controls <- function(theme) {
    div(
        class = "explorer-mode-controls",
        style = "padding: 0;", # No additional padding needed with improved component spacing
        tagList(
            # Gene selection section - reuse unified component
            create_gene_selection_ui(theme),
            
            # Group comparison section - unique to explorer mode
            div(
                class = "contrast-selection-section",
                style = "margin-top: 12px;", # Better margin between sections
                create_contrast_selection_ui(theme)
            ),
            
            # Analysis options - reuse unified component
            div(
                class = "analysis-options-section",
                style = "margin-top: 12px;", # Better margin between sections
                create_analysis_options_ui(theme)
            )
        )
    )
}



# Product grouping selection - improved spacing and readability (Target mode)
create_product_grouping_ui <- function(theme) {
    div(
        class = "product-grouping-container",
        style = "margin: 8px 0; padding: 12px; background-color: rgba(255,255,255,0.05); border-radius: 6px; border: 1px solid rgba(255,255,255,0.1);", # Better margins and padding
        
        h5("Cell Type Selection", 
           style = paste0("color: ", theme$text_white, "; margin: 0 0 10px 0; font-size: 16px; font-weight: bold;")), # Larger header
        
        helpText("Click cell types to enable/disable them for analysis. Real* products are selected by default:",
                class = "help-text",
                style = paste0("color: ", theme$text_light, "; margin-bottom: 12px; font-size: 12px;")), # Better spacing
        
        # Dynamic grouped cell type toggles will be populated by server
        div(
            style = "margin-bottom: 15px;", # More margin
            uiOutput("grouped_cell_type_toggles")
        ),
        
        # Action buttons for group selection - improved sizing and spacing
        div(
            class = "group-action-buttons",
            style = "margin-top: 10px; text-align: center;", # Better margin
            
            fluidRow(
                column(4,
                    actionButton("select_all_real", "All Real*", 
                               class = "btn btn-sm btn-outline-primary btn-block", # Changed to btn-sm from btn-xs
                               style = "font-size: 11px; padding: 6px 8px;") # Better font and padding
                ),
                column(4,
                    actionButton("select_all_types", "Select All", 
                               class = "btn btn-sm btn-outline-success btn-block", # Changed to btn-sm from btn-xs
                               style = "font-size: 11px; padding: 6px 8px;") # Better font and padding
                ),
                column(4,
                    actionButton("select_none", "Clear All", 
                               class = "btn btn-sm btn-outline-secondary btn-block", # Changed to btn-sm from btn-xs
                               style = "font-size: 11px; padding: 6px 8px;") # Better font and padding
                )
            )
        )
    )
}

# Contrast selection UI component (Explorer mode) - improved spacing
create_contrast_selection_ui <- function(theme) {
    div(
        class = "contrast-selection-container",
        style = "margin: 8px 0; padding: 12px; background-color: rgba(255,255,255,0.05); border-radius: 6px; border: 1px solid rgba(255,255,255,0.1);", # Better margins and padding
        
        # Section header
        h5("Group Comparison", 
           style = paste0("color: ", theme$text_white, "; margin: 0 0 10px 0; font-size: 16px; font-weight: bold;")), # Better margin and larger font
        
        helpText("Select two groups to compare expression levels:",
                class = "help-text",
                style = paste0("color: ", theme$text_light, "; margin-bottom: 12px; font-size: 12px;")), # Better margin and font size
        
        div(
            style = "margin-bottom: 12px;", # Better margin
            selectInput("group1",
                "Group 1 (Baseline):",
                choices = NULL,
                multiple = FALSE
            )
        ),
        
        div(
            style = "margin-bottom: 12px;", # Better margin
            selectInput("group2",
                "Group 2 (Comparison):",
                choices = NULL,
                multiple = FALSE
            )
        )
    )
}

# Gene selection UI component - improved spacing and readability
create_gene_selection_ui <- function(theme) {
    div(
        class = "gene-selection-container",
        style = "margin: 8px 0; padding: 12px; background-color: rgba(255,255,255,0.05); border-radius: 6px; border: 1px solid rgba(255,255,255,0.1);", # Better margins and padding
        
        # Section header
        h5("Gene Selection", style = paste0("color: ", theme$text_white, "; margin: 0 0 10px 0; font-size: 16px; font-weight: bold;")), # Larger header
        
        helpText("Enter gene symbols or select from predefined gene sets:",
                class = "help-text",
                style = paste0("color: ", theme$text_light, "; margin-bottom: 10px; font-size: 12px;")), # Better spacing
        
        # 1. FIRST: Gene symbol input text area - larger and more spacious
        div(
            style = "margin-bottom: 12px;", # More margin
            textAreaInput(
                "gene_textarea",
                "Gene symbols (comma, space, or line separated):",
                placeholder = "e.g., SCN11A, CACNA1A, KCNQ1",
                value = "SCN11A",  # Default gene
                rows = 6,  # Increased from 4 to 6 for better usability
                width = "100%"
            )
        ),
        
        # 2. SECOND: Real-time validation output - always visible with better spacing
        div(
            style = "margin-bottom: 12px;", # More margin
            uiOutput("gene_validation")
        ),
        
        # 3. THIRD: Gene set selection dropdown - better spacing
        div(
            style = "margin-bottom: 12px;", # More margin
            selectInput("gene_set_selection",
                "Or choose from predefined gene sets:",
                choices = c("Custom Genes" = "Custom Genes"),
                selected = "Custom Genes",
                width = "100%"
            ),
            helpText(
                "Select a predefined gene set to populate the text area above.",
                class = "help-text",
                style = paste0("color: ", theme$text_light, "; font-size: 11px;") # Slightly larger font
            )
        ),
        
        # 4. FOURTH: File upload for large lists - improved styling
        div(
            style = "margin-bottom: 12px; padding: 8px; background-color: rgba(255,255,255,0.08); border-radius: 6px; border: 1px solid rgba(255,255,255,0.15);", # Better margins and padding
            fileInput(
                "gene_file_upload",
                "Or upload gene list (.txt, .csv):",
                accept = c(".csv", ".tsv", ".txt"),
                placeholder = "No file selected",
                width = "100%"
            ),
            helpText(
                "Upload a file with one gene per line or column.",
                class = "help-text",
                style = paste0("color: ", theme$text_light, "; font-size: 11px; margin-top: 4px;") # Better spacing
            )
        ),
        
        # Display selected genes information - better spacing
        div(
            style = "margin-bottom: 8px;", # Better margin
            uiOutput("selected_genes_display")
        ),
        
        # Gene pagination controls
        create_gene_pagination_ui(theme)
    )
}

# Gene pagination UI component - improved button sizing
create_gene_pagination_ui <- function(theme) {
    conditionalPanel(
        condition = "output.show_gene_pagination",
        div(
            style = "margin-top: 8px;", # Better margin
            h6("Expression Plot Controls", style = paste0("color: ", theme$text_white, "; margin-bottom: 6px; font-size: 14px;")), # Better spacing and larger font
            fluidRow(
                column(6, 
                    actionButton("prev_genes", "← Prev", 
                               class = "btn-outline-primary btn-sm btn-block", # Changed to btn-sm for better size
                               style = paste0("color: ", theme$text_white, "; border-color: ", theme$text_white, "; font-size: 12px; padding: 6px 8px;")) # Better font and padding
                ),
                column(6,
                    actionButton("next_genes", "Next →", 
                               class = "btn-outline-primary btn-sm btn-block", # Changed to btn-sm for consistency
                               style = paste0("color: ", theme$text_white, "; border-color: ", theme$text_white, "; font-size: 12px; padding: 6px 8px;")) # Better font and padding
                )
            ),
            div(
                style = paste0("text-align: center; margin-top: 6px; color: ", theme$text_light, "; font-size: 11px;"), # Better margin and font size
                textOutput("gene_pagination_info", inline = TRUE)
            )
        )
    )
}

# =============================================================================
# Main Content Components
# =============================================================================


# Analysis options UI component - improved spacing and readability (shared between modes)
create_analysis_options_ui <- function(theme) {
    div(
        class = "analysis-options-container",
        style = "margin: 8px 0; padding: 12px; background-color: rgba(255,255,255,0.05); border-radius: 6px; border: 1px solid rgba(255,255,255,0.1);", # Better margins and padding
        
        # Section header
        h5("Analysis Options", 
           style = paste0("color: ", theme$text_white, "; margin: 0 0 10px 0; font-size: 16px; font-weight: bold;")), # Larger header
        
        helpText("Configure analysis parameters and visualization options:",
                class = "help-text",
                style = paste0("color: ", theme$text_light, "; margin-bottom: 12px; font-size: 12px;")), # Better spacing
        
        # Data type selection with proper choices - better spacing
        div(
            style = "margin-bottom: 15px;", # More margin
            radioButtons("data_type",
                "Expression Data Type:",
                choices = list(
                    "log2(CPM + 1)" = "log2_cpm",
                    "VST (Variance Stabilizing Transform)" = "vst"
                ),
                selected = "log2_cpm"
            )
        ),
        
        # Plot options - better spacing
        div(
            style = "margin-bottom: 15px;", # More margin
            checkboxInput("show_points", "Show Individual Points", value = TRUE)
        ),
        
        # Download button - improved styling
        div(
            style = "text-align: center;",
            downloadButton("download_plot", "Download Plot", class = "btn-primary",
                         style = "font-size: 12px; padding: 8px 16px;") # Better button size
        )
    )
}

# =============================================================================
# Unified Components (Shared between modes)
# =============================================================================

# Unified expression plot box - used by Explorer mode
create_unified_expression_plot_box <- function() {
    box(
        title = "Expression Analysis (BETA)",
        status = "primary",
        solidHeader = TRUE,
        width = 12,  # Full width to match expression data table
        height = "900px",
        tabsetPanel(
            tabPanel(
                "Gene Set Heatmap",
                div(
                    style = "padding: 8px;", # Better padding for readability
                    helpText("Heatmap of mean expression across selected groups", 
                            class = "help-text",
                            style = "font-size: 12px; margin-bottom: 10px;"), # Better font size and margin
                    plotlyOutput("gene_set_heatmap", height = "820px") %>% withSpinner()
                )
            ),
            tabPanel(
                "Co-Expression Analysis",
                div(
                    style = "padding: 8px;", # Better padding for readability
                    fluidRow(
                        column(
                            width = 4,
                            wellPanel(
                                style = "padding: 12px; height: 800px; overflow-y: auto;", # Better padding
                                create_coexpression_controls()
                            )
                        ),
                        column(
                            width = 8,
                            div(
                                style = "height: 800px;",
                                create_coexpression_results()
                            )
                        )
                    )
                )
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

# Create main application layout - Support both Target and Explorer modes
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

# Target Mode Layout: [GeneSet Analysis Overview] [Product Portfolio Overview]
create_target_mode_layout <- function() {
    fluidRow(
        create_geneset_analysis_box(),  # Left: GeneSet expression analysis 
        create_product_portfolio_overview_box()  # Right: Product portfolio overview
    )
}

# Explorer Mode Layout: [Expression Plot] [Co-Expression Analysis] - BETA
create_explorer_mode_layout <- function() {
    fluidRow(
        create_unified_expression_plot_box(),  # Left: Unified expression plot
    )
}

# GeneSet Analysis box - optimized expression distribution boxplot
create_geneset_analysis_box <- function() {
    box(
        title = "GeneSet Expression Analysis",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        height = "900px",
        
        # Expression distribution boxplot - focused on responsive performance
        div(
            id = "geneset-expression-container",
            style = "padding: 4px;", # Minimal padding
            h5("Expression Distribution", style = "margin: 0 0 6px 0; font-weight: bold;"),
            
            # Compact navigation controls for gene pagination
            conditionalPanel(
                condition = "output.show_gene_pagination",
                div(
                    style = "margin-bottom: 6px; padding: 4px 6px; background-color: #f8f9fa; border-radius: 4px; text-align: center;",
                    fluidRow(
                        column(4,
                            actionButton("prev_genes", "← Previous 10", 
                                       class = "btn-xs btn-default", 
                                       style = "width: 100%; font-size: 10px; padding: 2px 4px;")
                        ),
                        column(4,
                            div(
                                style = "padding-top: 2px; font-size: 10px; color: #666;",
                                uiOutput("gene_pagination_info")
                            )
                        ),
                        column(4,
                            actionButton("next_genes", "Next 10 →", 
                                       class = "btn-xs btn-default",
                                       style = "width: 100%; font-size: 10px; padding: 2px 4px;")
                        )
                    )
                )
            ),
            
            # Main expression boxplot - optimized for fast rendering
            plotlyOutput("expression_histogram", height = "520px") %>% withSpinner()
        ),
        
        # Analysis stats section - improved spacing
        div(
            id = "analysis-stats-section",
            style = "padding: 8px;", # Better padding for readability
            h4(textOutput("contrast_title"), style = "margin: 10px 0 8px 0; font-weight: bold; font-size: 16px;"), # Better spacing and larger font
            div(
                class = "analysis-stats-container",
                style = "max-height: 280px; overflow-y: auto; padding-right: 8px;", # Better padding
                uiOutput("sample_counts_ui"),
                conditionalPanel(
                    condition = "!output.current_genes_available",
                    uiOutput("overall_stats_ui")
                )
            )
        )
    )
}

# Product Portfolio Overview box - Target Mode specific
create_product_portfolio_overview_box <- function() {
    box(
        title = "Product Portfolio Overview",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        height = "900px",
        
        tabsetPanel(
            tabPanel(
                "Portfolio Ranking",
                div(
                    style = "padding: 8px;", # Better padding
                    h5("Expression Ranking by Product", style = "margin: 0 0 10px 0; font-weight: bold; font-size: 14px;"), # Better spacing
                    helpText("Products ranked by mean gene expression (bars = product average, points = individual genes)", 
                            class = "help-text",
                            style = "font-size: 12px; margin-bottom: 10px;"), # Better font size and margin
                    plotlyOutput("portfolio_ranking_plot", height = "380px") %>% withSpinner()
                )
            ),
            tabPanel(
                "Target Heatmap",
                div(
                    style = "padding: 8px;", # Better padding
                    h5("Gene × Product Expression Matrix", style = "margin: 0 0 10px 0; font-weight: bold; font-size: 14px;"), # Better spacing
                    helpText("Heatmap showing expression of selected genes across Anatomic products", 
                            class = "help-text", 
                            style = "font-size: 12px; margin-bottom: 10px;"), # Better font size and margin
                    plotlyOutput("target_gene_heatmap", height = "380px") %>% withSpinner()
                )
            ),
            tabPanel(
                "Summary Table",
                div(
                    style = "padding: 8px;", # Better padding
                    h5("Product Expression Summary", style = "margin: 0 0 10px 0; font-weight: bold; font-size: 14px;"), # Better spacing
                    helpText("Detailed statistics for each product showing expression levels and rankings", 
                            class = "help-text",
                            style = "font-size: 12px; margin-bottom: 10px;"), # Better font size and margin
                    DT::dataTableOutput("portfolio_summary_table", height = "380px") %>% withSpinner()
                )
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
