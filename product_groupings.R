# =============================================================================
# Product Portfolio Groupings Configuration
# Define pre-defined cell type groupings for Target Mode
# =============================================================================

# Visual groupings for cell type organization
# Users can click individual cell types to toggle them on/off within each group

# Define visual product groupings
product_groupings <- list(
    
    # Anatomic's Real* Products
    "real_products" = list(
        name = "Real* Products (Anatomic)",
        description = "Anatomic's proprietary cell line products",
        cell_types = c("RealDRGx", "RealDRG", "RealMoto", "RealMelo", "RealDHN", "RealSCP"),
        default_enabled = TRUE  # These should be enabled by default
    ),
    
    # hiPSC-derived Products
    "human_iPSC" = list(
        name = "hiPSC Products", 
        description = "Human induced pluripotent stem cell-derived products",
        cell_types = c("hiPSCMN", "hiPSCMelo_1", "hiPSCMelo_2"),
        default_enabled = FALSE  # These should be disabled by default
    ),
    
    # Human Primary Cell Types
    "human_primary" = list(
        name = "Human Primary",
        description = "Primary human cell types for comparison",
        cell_types = c("hDRG", "hMelo_1", "hSCP"),
        default_enabled = FALSE  # These should be disabled by default
    )
)

# Helper function to get cell types for a grouping
get_grouping_cell_types <- function(grouping_key) {
    if (grouping_key %in% names(product_groupings)) {
        return(product_groupings[[grouping_key]]$cell_types)
    }
    return(c())
}

# Helper function to get default enabled status for a grouping
get_grouping_default_enabled <- function(grouping_key) {
    if (grouping_key %in% names(product_groupings)) {
        return(product_groupings[[grouping_key]]$default_enabled)
    }
    return(FALSE)
}

# Helper function to get grouping description
get_grouping_description <- function(grouping_key) {
    if (grouping_key %in% names(product_groupings)) {
        return(product_groupings[[grouping_key]]$description)
    }
    return("Unknown grouping")
}

# Helper function to get all cell types organized by group
get_all_cell_types_by_group <- function() {
    result <- list()
    for (key in names(product_groupings)) {
        result[[key]] <- product_groupings[[key]]
    }
    return(result)
}

# Helper function to initialize default toggle states
get_default_toggle_states <- function() {
    toggle_states <- list()
    for (group_key in names(product_groupings)) {
        group <- product_groupings[[group_key]]
        default_state <- group$default_enabled
        for (cell_type in group$cell_types) {
            toggle_states[[cell_type]] <- default_state
        }
    }
    return(toggle_states)
}

# Helper function to get all cell types with their default enabled status
get_all_cell_types_with_defaults <- function() {
    all_types <- list()
    
    for (group_key in names(product_groupings)) {
        group <- product_groupings[[group_key]]
        for (cell_type in group$cell_types) {
            all_types[[cell_type]] <- group$default_enabled
        }
    }
    
    return(all_types)
}

# Helper function to get cell types organized by group
get_cell_types_by_group <- function() {
    return(product_groupings)
}

# Print summary for verification
cat("=== PRODUCT GROUPINGS LOADED ===\n")
for (key in names(product_groupings)) {
    group <- product_groupings[[key]]
    default_status <- if(group$default_enabled) "(enabled by default)" else "(disabled by default)"
    cat(paste0("• ", group$name, " ", default_status, ":\n"))
    cat(paste0("  Cell types: ", paste(group$cell_types, collapse = ", "), "\n"))
}
cat("=== END GROUPINGS ===\n")
