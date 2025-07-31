# =============================================================================
# Co-Expression Analysis UI Module
# Modular UI components for co-expression analysis functionality
# =============================================================================

# Co-Expression Analysis box - focused on gene correlation analysis  
create_coexpression_analysis_box <- function() {
    box(
        title = "Co-Expression Analysis",
        status = "primary",
        solidHeader = TRUE,
        width = 6,  # Right column
        height = "900px",
        tabsetPanel(
            tabPanel(
                "Co-Expression Network",
                div(
                    style = "padding: 4px;", # Minimal padding
                    helpText("Find genes co-expressed with your selected targets across the Atlas", 
                            class = "help-text",
                            style = "font-size: 11px; margin-bottom: 6px;"),
                    
                    # Compact co-expression controls
                    div(
                        style = "margin-bottom: 8px; padding: 4px; background-color: #f8f9fa; border-radius: 4px;",
                        fluidRow(
                            column(6,
                                selectInput("coexpression_data_type",
                                    "Data Type:",
                                    choices = list(
                                        "log2(CPM + 1)" = "log2_cpm",
                                        "VST" = "vst"
                                    ),
                                    selected = "log2_cpm"
                                )
                            ),
                            column(6,
                                div(
                                    style = "margin-top: 25px;",
                                    actionButton("run_coexpression", "Run Analysis", 
                                               class = "btn-primary btn-sm btn-block",
                                               style = "font-size: 11px; padding: 4px 8px;")
                                )
                            )
                        )
                    ),
                    
                    # Co-expression results
                    plotlyOutput("coexpression_network", height = "600px") %>% withSpinner(),
                    
                    # Progress and log output - compact
                    div(
                        style = "margin-top: 4px; font-size: 10px;",
                        uiOutput("coexpression_progress"),
                        verbatimTextOutput("coexpression_log")
                    )
                )
            ),
            tabPanel(
                "Co-Expression Table",
                div(
                    style = "padding: 4px; height: 800px; overflow: hidden;", # Minimal padding with height control
                    helpText("Detailed correlation results for selected genes", 
                            class = "help-text",
                            style = "font-size: 11px; margin-bottom: 6px;"),
                    div(
                        style = "height: calc(100% - 30px); overflow: hidden;",  # Container to properly contain the table
                        DT::dataTableOutput("coexpression_table", height = "100%") %>% withSpinner()
                    )
                )
            )
        )
    )
}

# Co-expression controls panel - Compact version (shared by both modes)
create_coexpression_controls <- function() {
    div(
        style = "display: flex; flex-direction: column; height: 100%;",
        div(
            style = "flex-shrink: 0;", # Controls section that doesn't shrink
            h4("Co-Expression Controls", style = "margin: 0 0 6px 0; font-size: 14px;"),
            helpText("Find correlated genes across the Atlas (May take >5 minutes).",
                   class = "help-text",
                   style = "font-size: 10px; margin-bottom: 6px;"),
            
            selectInput("coexpression_data_type",
                "Data Type:",
                choices = list(
                    "log2(CPM + 1)" = "log2_cpm",
                    "VST" = "vst"
                ),
                selected = "log2_cpm"
            ),
            
            fluidRow(
                column(6,
                    numericInput("min_correlation_coexpr",
                        "Min Correlation:",
                        value = 0.6,
                        min = 0.1,
                        max = 1.0,
                        step = 0.1
                    )
                ),
                column(6,
                    numericInput("n_similar_genes",
                        "Genes Per Query:",
                        value = 25,
                        min = 1,
                        max = 250,
                        step = 1
                    )
                )
            ),
            
            fluidRow(
                column(6,
                    numericInput("max_genes_batch",
                        "Batch Size:",
                        value = 1000,
                        min = 100,
                        max = 2000,
                        step = 100
                    )
                ),
                column(6,
                    div(
                        style = "margin-top: 25px;",
                        checkboxInput("use_parallel",
                            "Parallel processing",
                            value = FALSE
                        )
                    )
                )
            ),
            
            helpText("Top N genes per query, then combined and deduplicated.",
                   class = "help-text",
                   style = "font-size: 9px; margin: 4px 0;"),
            
            actionButton("run_coexpression", 
                       "Find Co-expressed Genes", 
                       class = "btn-primary btn-block btn-sm",
                       style = "font-size: 10px; padding: 3px 8px;"),
            
            # Section header for progress
            br(),
            h6("Analysis Progress", style = "font-weight: bold; font-size: 12px; margin: 10px 0 6px 0;")
        ),
        
        # Expanding log panel
        create_coexpression_log_panel()
    )
}

# Co-expression results panel (shared by both modes) - Redesigned without Found Genes Summary with full height
create_coexpression_results <- function() {
    div(
        style = "height: 100%;",
        tabsetPanel(
            id = "coexpression_results_tabs",
            tabPanel(
                "Co-expressed Genes",
                div(
                    style = "padding: 8px; height: 100%; display: flex; flex-direction: column; overflow: hidden;",
                    div(
                        style = "flex-shrink: 0;",
                        h6("Co-expressed Genes Details", style = "margin-top: 0px; font-size: 14px; font-weight: bold;"),
                        helpText("Detailed correlation results for selected genes", 
                                class = "help-text",
                                style = "font-size: 12px; margin-bottom: 10px;")
                    ),
                    div(
                        style = "flex: 1; min-height: 0; overflow: hidden;",
                        DT::dataTableOutput("coexpression_detailed_table", height = "100%") %>%
                            shinycssloaders::withSpinner()
                    )
                )
            ),
            tabPanel(
                "Expression Heatmap",
                div(
                    style = "padding: 8px; height: 100%; display: flex; flex-direction: column; overflow: hidden;",
                    div(
                        style = "flex-shrink: 0;",
                        h6("Co-expression Heatmap", style = "margin-top: 0px; font-size: 14px; font-weight: bold;"),
                        helpText("Heatmap visualization of co-expressed genes", 
                                class = "help-text",
                                style = "font-size: 12px; margin-bottom: 10px;")
                    ),
                    div(
                        style = "flex: 1; min-height: 0; overflow: hidden;",
                        plotlyOutput("coexpression_heatmap", height = "100%") %>% 
                            shinycssloaders::withSpinner()
                    )
                )
            ),
            tabPanel(
                "Correlation Network",
                div(
                    style = "padding: 8px; height: 100%; display: flex; flex-direction: column; overflow: hidden;",
                    div(
                        style = "flex-shrink: 0;",
                        h6("Correlation Network", style = "margin-top: 0px; font-size: 14px; font-weight: bold;"),
                        helpText("Network visualization of gene correlations", 
                                class = "help-text",
                                style = "font-size: 12px; margin-bottom: 10px;")
                    ),
                    div(
                        style = "flex: 1; min-height: 0; overflow: hidden;",
                        networkD3::forceNetworkOutput("coexpression_network", height = "100%") %>%
                            shinycssloaders::withSpinner())
                    )
                )
            )
        )
}

# Co-expression log panel (shared by both modes) - Expanded to fill remaining height
create_coexpression_log_panel <- function() {
    div(
        id = "coexpression_log_container",
        style = "flex: 1; min-height: 300px; max-height: 500px; overflow-y: auto; border: 1px solid #E9ECEF; border-radius: 4px; padding: 4px; background-color: #F8F9FA; font-size: 10px;",
        verbatimTextOutput("coexpression_live_log")
    )
}
