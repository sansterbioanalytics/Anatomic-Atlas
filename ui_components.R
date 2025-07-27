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
        title = "Anatomic RNA Atlas",  # Simple title that won't be escaped
        titleWidth = 300,  # Increase width to accommodate the title
        
        # Add logo and mode selector in the navbar area
        tags$li(
            class = "dropdown",
            style = "margin: 0; padding: 0; height: 60px; display: flex; align-items: center;",
            
            # Logo link
            tags$a(
                href = theme$logo_link_url,
                target = "_blank",
                class = "logo-link",
                style = "display: flex; align-items: center; height: 60px; padding: 0 15px; margin-right: 20px;",
                img(src = "anatomic_logo.png", class = "logo-primary", alt = "Anatomic Logo", 
                    style = "height: 42px; transition: transform 0.2s ease;"),
                onmouseover = "this.querySelector('img').style.transform = 'scale(1.05)'",
                onmouseout = "this.querySelector('img').style.transform = 'scale(1)'"
            ),
            
            # Mode Selector Buttons - Modern design
            create_compact_mode_selector_ui(theme)
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

# Create main sidebar with analysis controls - fully server-side rendering
create_app_sidebar <- function(theme, width = 380) {
    dashboardSidebar(
        width = width,
        div(
            style = "height: 100%; overflow-y: auto; padding: 10px;",
            h4("Analysis Controls", 
               style = paste0("margin: 0 0 15px 0; color: ", theme$text_white, "; font-size: 18px; font-weight: bold;")),
            
            # Server-side rendered controls based on mode
            uiOutput("sidebar_controls")
        )
    )
}

# Simple Gene Selection UI - Completely redesigned for better functionality
create_simple_gene_selection_ui <- function(theme) {
    div(
        class = "gene-selection-container",
        style = paste0("margin: 10px 0; padding: 15px; background: linear-gradient(135deg, ", 
                      theme$primary_dark, " 0%, ", theme$secondary_dark, " 100%); ",
                      "border-radius: 8px; border: 2px solid rgba(255,255,255,0.1); box-shadow: 0 4px 6px rgba(0,0,0,0.1);"),
        
        # Header
        div(
            style = "margin-bottom: 15px; text-align: center;",
            h5("🧬 Gene Selection", 
               style = paste0("color: ", theme$text_white, "; margin: 0; font-weight: bold; font-size: 16px; text-shadow: 1px 1px 2px rgba(0,0,0,0.3);"))
        ),
        
        # Gene Set Selection Method - Improved Radio Buttons
        div(
            style = "margin-bottom: 20px; padding: 12px; background-color: rgba(255,255,255,0.08); border-radius: 6px; border: 1px solid rgba(255,255,255,0.15);",
            h6("Select gene input method:", 
               style = paste0("color: ", theme$text_white, "; margin: 0 0 10px 0; font-size: 14px; font-weight: bold;")),
            
            radioButtons("gene_input_method",
                NULL,
                choices = list(
                    "Manual Entry" = "manual",
                    "Predefined Gene Sets" = "predefined",
                    "Upload File" = "upload"
                ),
                selected = "manual",
                inline = TRUE
            ),
            
            tags$style(HTML(paste0("
                #gene_input_method .radio-inline {
                    margin-right: 20px;
                    color: ", theme$text_white, ";
                    font-size: 13px;
                }
                #gene_input_method input[type='radio'] {
                    margin-right: 8px;
                    transform: scale(1.2);
                }
            ")))
        ),
        
        # Manual Entry Panel
        conditionalPanel(
            condition = "input.gene_input_method == 'manual'",
            div(
                style = "padding: 12px; background-color: rgba(255,255,255,0.05); border-radius: 6px; border: 1px solid rgba(255,255,255,0.1);",
                h6("Enter gene symbols:", 
                   style = paste0("color: ", theme$text_white, "; margin: 0 0 8px 0; font-size: 13px;")),
                p("Separate multiple genes with commas, spaces, or new lines",
                  style = paste0("color: ", theme$text_light, "; margin: 0 0 8px 0; font-size: 12px; font-style: italic;")),
                textAreaInput("gene_textarea",
                    NULL,
                    value = "SCN11A",
                    placeholder = "Enter gene symbols (e.g., SCN11A, CACNA1A, TRPV1)",
                    rows = 4,
                    width = "100%"
                )
            )
        ),
        
        # Predefined Gene Sets Panel  
        conditionalPanel(
            condition = "input.gene_input_method == 'predefined'",
            div(
                style = "padding: 12px; background-color: rgba(255,255,255,0.05); border-radius: 6px; border: 1px solid rgba(255,255,255,0.1);",
                h6("Choose from predefined gene sets:", 
                   style = paste0("color: ", theme$text_white, "; margin: 0 0 8px 0; font-size: 13px;")),
                
                # Native reactive gene set dropdown
                uiOutput("gene_set_dropdown"),
                
                # Gene set info display
                div(id = "gene_set_info",
                    style = "margin-top: 8px; padding: 8px; background-color: rgba(255,255,255,0.1); border-radius: 4px; min-height: 30px;",
                    uiOutput("gene_set_info_display")
                )
            )
        ),
        
        # File Upload Panel
        conditionalPanel(
            condition = "input.gene_input_method == 'upload'",
            div(
                style = "padding: 12px; background-color: rgba(255,255,255,0.05); border-radius: 6px; border: 1px solid rgba(255,255,255,0.1);",
                h6("Upload gene list file:", 
                   style = paste0("color: ", theme$text_white, "; margin: 0 0 8px 0; font-size: 13px;")),
                p("Supported formats: .txt, .csv, .tsv",
                  style = paste0("color: ", theme$text_light, "; margin: 0 0 8px 0; font-size: 12px; font-style: italic;")),
                
                div(
                    style = "border: 2px dashed rgba(255,255,255,0.3); border-radius: 6px; padding: 15px; text-align: center; background-color: rgba(255,255,255,0.02);",
                    fileInput("gene_file_upload",
                             NULL,
                             accept = c(".txt", ".csv", ".tsv"),
                             width = "100%",
                             buttonLabel = "📁 Browse Files",
                             placeholder = "No file selected"
                    )
                )
            )
        ),
        
        # VALIDATION PANEL - Enhanced
        div(
            id = "gene-validation-panel",
            style = "margin-top: 15px; padding: 12px; background-color: rgba(255,255,255,0.08); border: 1px solid rgba(255,255,255,0.2); border-radius: 6px;",
            h6("🔍 Gene Validation Results", 
               style = paste0("color: ", theme$text_white, "; margin: 0 0 8px 0; font-size: 14px; font-weight: bold;")),
            uiOutput("gene_validation")
        ),
        
        # Selected genes summary
        div(
            style = "margin-top: 10px; padding: 8px; background-color: rgba(255,255,255,0.05); border-radius: 4px; border-left: 4px solid #28a745;",
            uiOutput("selected_genes_display")
        )
    )
}

# Unified Gene Selection Inputs (used by both modes)
create_gene_selection_inputs <- function(theme) {
    tagList(
        # Gene symbol input text area
        div(
            style = "margin-bottom: 15px; width: 100%;",
            textAreaInput(
                "gene_textarea",
                "Gene symbols (comma, space, or line separated):",
                placeholder = "e.g., SCN11A, CACNA1A, KCNQ1",
                value = "SCN11A",
                rows = 5,
                width = "100%"
            )
        ),
        
        # VALIDATION PANEL
        div(
            class = "validation-panel-container",
            style = "margin-bottom: 15px; width: 100%; min-height: 100px;",
            div(
                style = "background-color: rgba(255,255,255,0.1); border: 2px dashed rgba(255,255,255,0.3); border-radius: 6px; padding: 10px;",
                h6("🔍 Gene Validation", style = paste0("color: ", theme$text_white, "; margin: 0 0 8px 0; font-weight: bold;")),
                uiOutput("gene_validation")
            )
        ),
        
        # Gene set selection dropdown
        div(
            style = "margin-bottom: 15px; width: 100%;",
            selectInput("gene_set_selection",
                "Or choose from predefined gene sets:",
                choices = c("Custom Genes" = "Custom Genes", "Loading..." = "loading"),
                selected = "Custom Genes",
                width = "100%"
            ),
            
            helpText("Select a predefined gene set to populate the text area above.",
                class = "help-text",
                style = paste0("color: ", theme$text_light, "; font-size: 11px; margin-top: 4px;"))
        ),
        
        # File upload option
        div(
            style = "margin-bottom: 15px; width: 100%;",
            h6("Or upload gene list (.txt, .csv):", 
               style = paste0("color: ", theme$text_white, "; margin-bottom: 8px; font-size: 14px;")),
            
            div(
                style = "background-color: rgba(255,255,255,0.05); border: 1px dashed rgba(255,255,255,0.2); border-radius: 4px; padding: 10px;",
                fileInput("gene_file_upload",
                         NULL,
                         accept = c(".txt", ".csv"),
                         width = "100%",
                         buttonLabel = "Browse...",
                         placeholder = "No file selected"
                ),
                
                helpText("Upload a file with one gene per line or column.",
                        class = "help-text",
                        style = paste0("color: ", theme$text_light, "; font-size: 11px; margin-top: 4px;"))
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
        
        # Native reactive group dropdowns
        uiOutput("group_comparison_dropdowns")
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

# Unified expression plot box - used by Explorer mode with full height utilization
create_unified_expression_plot_box <- function() {
    box(
        title = "Expression Analysis (BETA)",
        status = "primary",
        solidHeader = TRUE,
        width = 12,  # Full width to match expression data table
        height = "900px",
        
        # Use flexbox to fill available height
        div(
            style = "height: calc(100% - 50px);", # Account for box header
            tabsetPanel(
                id = "expression_tabs",
                tabPanel(
                    "Gene Set Heatmap",
                    div(
                        style = "padding: 4px; height: 100%; display: flex; flex-direction: column; overflow: hidden;",
                        div(
                            style = "flex-shrink: 0; margin-bottom: 5px;",
                            helpText("Heatmap of mean expression across selected groups", 
                                    class = "help-text",
                                    style = "font-size: 12px; margin: 0;")
                        ),
                        div(
                            style = "flex: 1; min-height: 600px;",
                            plotlyOutput("gene_set_heatmap", height = "100%") %>% withSpinner()
                        )
                    )
                ),
                tabPanel(
                    "Co-Expression Analysis",
                    div(
                        style = "padding: 8px; height: 100%; overflow: hidden;",
                        fluidRow(
                            style = "height: 100%;",
                            column(
                                width = 4,
                                wellPanel(
                                    style = "padding: 12px; height: 100%; overflow-y: auto; margin: 0;",
                                    create_coexpression_controls()
                                )
                            ),
                            column(
                                width = 8,
                                div(
                                    style = "height: 100%; padding-left: 15px; overflow: hidden;",
                                    create_coexpression_results()
                                )
                            )
                        )
                    )
                )
            )
        )
    )
}

# Create expression data table box with proper height constraints and overflow handling
create_expression_table_box <- function() {
    box(
        title = textOutput("expression_table_title", inline = TRUE),
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        height = "500px",  # Set explicit height for the table section
        
        # Use flexbox with proper overflow control to prevent spilling
        div(
            style = "height: calc(100% - 50px); display: flex; flex-direction: column; overflow: hidden;", # Account for box header and prevent overflow
            div(
                style = "flex: 1; min-height: 300px; max-height: 450px; overflow: hidden;",  # Add max-height and overflow control
                DT::dataTableOutput("expression_table", height = "100%") %>% withSpinner()
            )
        )
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

# GeneSet Analysis box - optimized expression distribution boxplot with full height utilization
create_geneset_analysis_box <- function() {
    box(
        title = "GeneSet Expression Analysis",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        height = "900px",
        
        # Use flexbox to utilize full height
        div(
            style = "display: flex; flex-direction: column; height: calc(100% - 50px);", # Account for box header
            
            # Expression distribution section - flexible height
            div(
                id = "geneset-expression-container",
                style = "flex: 1; display: flex; flex-direction: column; padding: 4px; min-height: 0;", # min-height: 0 allows flex shrinking
                h5("Expression Distribution", style = "margin: 0 0 6px 0; font-weight: bold; flex-shrink: 0;"),
                
                # Compact navigation controls for gene pagination
                conditionalPanel(
                    condition = "output.show_gene_pagination",
                    div(
                        style = "margin-bottom: 6px; padding: 4px 6px; background-color: #f8f9fa; border-radius: 4px; text-align: center; flex-shrink: 0;",
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
                
                # Main expression boxplot - takes remaining height
                div(
                    style = "flex: 1; min-height: 300px;",
                    plotlyOutput("expression_histogram", height = "100%") %>% withSpinner()
                )
            ),
            
            # Summary Table section - fixed height but optimized with overflow control
            div(
                id = "summary-table-section",
                style = "flex-shrink: 0; height: 320px; padding: 8px; border-top: 1px solid #dee2e6; overflow: hidden;", # Fixed height for table section with overflow control
                h5("Product Expression Summary", style = "margin: 0 0 10px 0; font-weight: bold; font-size: 14px;"),
                helpText("Detailed statistics for each product showing expression levels and rankings", 
                        class = "help-text",
                        style = "font-size: 12px; margin-bottom: 10px;"),
                div(
                    style = "height: 320px; overflow: hidden;",  # Reduced wrapper container height to match table scrollY
                    DT::dataTableOutput("portfolio_summary_table", height = "100%") %>% withSpinner()
                )
            )
        )
    )
}

# Product Portfolio Overview box - Target Mode specific with full height utilization
create_product_portfolio_overview_box <- function() {
    box(
        title = "Product Portfolio Overview",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        height = "900px",
        
        # Use flexbox container to fill available height
        div(
            style = "height: calc(100% - 50px);", # Account for box header
            tabsetPanel(
                id = "portfolio_tabs",
                tabPanel(
                    "Portfolio Ranking",
                    div(
                        style = "padding: 4px; height: 100%; display: flex; flex-direction: column; overflow: hidden;",
                        div(
                            style = "flex-shrink: 0; margin-bottom: 5px;",
                            h5("Expression Ranking by Product", style = "margin: 0 0 5px 0; font-weight: bold; font-size: 14px;"),
                            helpText("Products ranked by mean gene expression (bars = product average, points = individual genes)", 
                                    class = "help-text",
                                    style = "font-size: 12px; margin: 0;")
                        ),
                        div(
                            style = "flex: 1; min-height: 600px;",
                            plotlyOutput("portfolio_ranking_plot", height = "100%") %>% withSpinner()
                        )
                    )
                ),
                tabPanel(
                    "Target Heatmap",
                    div(
                        style = "padding: 4px; height: 100%; display: flex; flex-direction: column; overflow: hidden;",
                        div(
                            style = "flex-shrink: 0; margin-bottom: 5px;",
                            h5("Gene × Product Expression Matrix", style = "margin: 0 0 5px 0; font-weight: bold; font-size: 14px;"),
                            helpText("Heatmap showing expression of selected genes across Anatomic products", 
                                    class = "help-text", 
                                    style = "font-size: 12px; margin: 0;")
                        ),
                        div(
                            style = "flex: 1; min-height: 600px;",
                            plotlyOutput("target_gene_heatmap", height = "100%") %>% withSpinner()
                        )
                    )
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
