# =============================================================================
# Data Utilities Module for Anatomic RNA Atlas
# Contains data loading, processing, and helper functions
# =============================================================================

# Required libraries for this module
suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(rlang)
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
# Data Validation Functions
# =============================================================================

# Validate expression data structure
validate_expression_data <- function(expression_data) {
    required_cols <- c("sample", "gene", "expression", "data_type")
    
    if (is.null(expression_data)) {
        return(list(valid = FALSE, message = "Expression data is NULL"))
    }
    
    if (!all(required_cols %in% colnames(expression_data))) {
        missing_cols <- required_cols[!required_cols %in% colnames(expression_data)]
        return(list(
            valid = FALSE, 
            message = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
        ))
    }
    
    return(list(valid = TRUE, message = "Expression data structure is valid"))
}

# Validate sample metadata structure
validate_sample_data <- function(sample_data) {
    if (is.null(sample_data)) {
        return(list(valid = FALSE, message = "Sample data is NULL"))
    }
    
    celltype_col <- get_celltype_column(sample_data)
    sample_col <- get_sample_column(sample_data)
    
    if (is.null(celltype_col)) {
        return(list(
            valid = FALSE, 
            message = "No recognized cell type column found (PubCelltype, Celltype, celltype)"
        ))
    }
    
    if (is.null(sample_col)) {
        return(list(
            valid = FALSE, 
            message = "No recognized sample column found (PubName, Name)"
        ))
    }
    
    return(list(valid = TRUE, message = "Sample data structure is valid"))
}

# =============================================================================
# Gene Set Processing Functions
# =============================================================================

# Process uploaded gene set file
process_uploaded_gene_file <- function(file_path, file_ext) {
    if (file_ext == "csv") {
        uploaded_data <- readr::read_csv(file_path, show_col_types = FALSE)
    } else if (file_ext %in% c("tsv", "txt")) {
        uploaded_data <- readr::read_tsv(file_path, show_col_types = FALSE)
    } else {
        stop("Unsupported file format. Please use CSV, TSV, or TXT files.")
    }
    
    # Extract first column as gene symbols
    gene_symbols <- uploaded_data[[1]]
    
    # Remove NA values and convert to character
    gene_symbols <- as.character(gene_symbols[!is.na(gene_symbols)])
    
    # Remove empty strings
    gene_symbols <- gene_symbols[gene_symbols != ""]
    
    return(gene_symbols)
}

# Filter genes that exist in the dataset
filter_available_genes <- function(selected_genes, expression_data) {
    if (is.null(selected_genes) || length(selected_genes) == 0 || is.null(expression_data)) {
        return(character(0))
    }
    
    available_genes <- unique(expression_data$gene)
    return(intersect(selected_genes, available_genes))
}
