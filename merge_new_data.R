# R Script to Merge New Gene Expression and Metadata with Existing Data
# This script combines new data files with existing RDS objects
# UNTESTED 

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# Configuration
cat("=== ANATOMIC ATLAS DATA MERGER ===\n")
cat("This script merges new expression and metadata files with existing data\n\n")

# Function to validate data structure
validate_sample_metadata <- function(df, file_name) {
  cat("Validating", file_name, "structure...\n")
  
  required_cols <- c()
  optional_cols <- c("PubName", "Name", "PubCelltype", "Celltype", "celltype")
  
  # Check for sample identifier column
  sample_col <- NULL
  if ("PubName" %in% colnames(df)) {
    sample_col <- "PubName"
  } else if ("Name" %in% colnames(df)) {
    sample_col <- "Name"
  } else {
    stop("Sample metadata must have either 'PubName' or 'Name' column")
  }
  
  # Check for cell type column
  celltype_col <- NULL
  if ("PubCelltype" %in% colnames(df)) {
    celltype_col <- "PubCelltype"
  } else if ("Celltype" %in% colnames(df)) {
    celltype_col <- "Celltype"
  } else if ("celltype" %in% colnames(df)) {
    celltype_col <- "celltype"
  } else {
    stop("Sample metadata must have a cell type column (PubCelltype, Celltype, or celltype)")
  }
  
  cat("✓ Found sample column:", sample_col, "\n")
  cat("✓ Found cell type column:", celltype_col, "\n")
  cat("✓ Metadata structure is valid\n")
  
  return(list(sample_col = sample_col, celltype_col = celltype_col))
}

validate_expression_data <- function(df, file_name) {
  cat("Validating", file_name, "structure...\n")
  
  required_cols <- c("gene", "sample", "expression")
  missing_cols <- setdiff(required_cols, colnames(df))
  
  if (length(missing_cols) > 0) {
    stop("Expression data missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check for data_type column (optional but recommended)
  if (!"data_type" %in% colnames(df)) {
    warning("Expression data missing 'data_type' column. Will add default 'vst' values.")
  }
  
  cat("✓ Expression data structure is valid\n")
  cat("✓ Found", nrow(df), "expression measurements for", length(unique(df$gene)), "genes across", length(unique(df$sample)), "samples\n")
  
  return(TRUE)
}

# Function to merge sample metadata
merge_sample_metadata <- function(existing_data, new_data, merge_strategy = "append") {
  cat("Merging sample metadata...\n")
  
  # Get column information for both datasets
  existing_info <- validate_sample_metadata(existing_data, "existing metadata")
  new_info <- validate_sample_metadata(new_data, "new metadata")
  
  # Standardize column names if needed
  existing_sample_col <- existing_info$sample_col
  new_sample_col <- new_info$sample_col
  
  # Check for overlapping samples
  existing_samples <- existing_data[[existing_sample_col]]
  new_samples <- new_data[[new_sample_col]]
  overlapping_samples <- intersect(existing_samples, new_samples)
  
  if (length(overlapping_samples) > 0) {
    cat("⚠️  Found", length(overlapping_samples), "overlapping samples\n")
    cat("Overlapping samples:", paste(head(overlapping_samples, 10), collapse = ", "))
    if (length(overlapping_samples) > 10) cat(" ...")
    cat("\n")
    
    if (merge_strategy == "append") {
      stop("Cannot append data with overlapping samples. Use merge_strategy = 'update' or 'skip_duplicates'")
    } else if (merge_strategy == "update") {
      cat("Strategy: Updating existing samples with new data\n")
      # Remove overlapping samples from existing data
      existing_data <- existing_data[!existing_data[[existing_sample_col]] %in% overlapping_samples, ]
    } else if (merge_strategy == "skip_duplicates") {
      cat("Strategy: Skipping duplicate samples, keeping existing data\n")
      # Remove overlapping samples from new data
      new_data <- new_data[!new_data[[new_sample_col]] %in% overlapping_samples, ]
    }
  }
  
  # Align column structures
  all_columns <- union(colnames(existing_data), colnames(new_data))
  
  # Add missing columns with NA values
  for (col in all_columns) {
    if (!col %in% colnames(existing_data)) {
      existing_data[[col]] <- NA
    }
    if (!col %in% colnames(new_data)) {
      new_data[[col]] <- NA
    }
  }
  
  # Reorder columns to match
  existing_data <- existing_data[, all_columns]
  new_data <- new_data[, all_columns]
  
  # Combine the data
  merged_data <- rbind(existing_data, new_data)
  
  cat("✓ Merged metadata successfully\n")
  cat("  - Original samples:", length(existing_samples), "\n")
  cat("  - New samples:", nrow(new_data), "\n")
  cat("  - Final samples:", nrow(merged_data), "\n")
  
  return(merged_data)
}

# Function to merge expression data
merge_expression_data <- function(existing_data, new_data, merge_strategy = "append") {
  cat("Merging expression data...\n")
  
  validate_expression_data(existing_data, "existing expression")
  validate_expression_data(new_data, "new expression")
  
  # Add data_type column if missing
  if (!"data_type" %in% colnames(existing_data)) {
    existing_data$data_type <- "vst"
    cat("Added default data_type 'vst' to existing data\n")
  }
  if (!"data_type" %in% colnames(new_data)) {
    new_data$data_type <- "vst"
    cat("Added default data_type 'vst' to new data\n")
  }
  
  # Check for overlapping gene-sample combinations
  existing_keys <- paste(existing_data$gene, existing_data$sample, existing_data$data_type, sep = "_")
  new_keys <- paste(new_data$gene, new_data$sample, new_data$data_type, sep = "_")
  overlapping_keys <- intersect(existing_keys, new_keys)
  
  if (length(overlapping_keys) > 0) {
    cat("⚠️  Found", length(overlapping_keys), "overlapping gene-sample-datatype combinations\n")
    
    if (merge_strategy == "append") {
      stop("Cannot append data with overlapping gene-sample combinations. Use merge_strategy = 'update' or 'skip_duplicates'")
    } else if (merge_strategy == "update") {
      cat("Strategy: Updating existing measurements with new data\n")
      # Remove overlapping measurements from existing data
      existing_data <- existing_data[!existing_keys %in% overlapping_keys, ]
    } else if (merge_strategy == "skip_duplicates") {
      cat("Strategy: Skipping duplicate measurements, keeping existing data\n")
      # Remove overlapping measurements from new data
      new_data <- new_data[!new_keys %in% overlapping_keys, ]
    }
  }
  
  # Align column structures
  all_columns <- union(colnames(existing_data), colnames(new_data))
  
  # Add missing columns with appropriate defaults
  for (col in all_columns) {
    if (!col %in% colnames(existing_data)) {
      if (col == "data_type") {
        existing_data[[col]] <- "vst"
      } else {
        existing_data[[col]] <- NA
      }
    }
    if (!col %in% colnames(new_data)) {
      if (col == "data_type") {
        new_data[[col]] <- "vst"
      } else {
        new_data[[col]] <- NA
      }
    }
  }
  
  # Reorder columns to match
  existing_data <- existing_data[, all_columns]
  new_data <- new_data[, all_columns]
  
  # Combine the data
  merged_data <- rbind(existing_data, new_data)
  
  cat("✓ Merged expression data successfully\n")
  cat("  - Original measurements:", nrow(existing_data), "\n")
  cat("  - New measurements:", nrow(new_data), "\n")
  cat("  - Final measurements:", nrow(merged_data), "\n")
  cat("  - Total genes:", length(unique(merged_data$gene)), "\n")
  cat("  - Total samples:", length(unique(merged_data$sample)), "\n")
  
  return(merged_data)
}

# Function to load data from various formats
load_data_file <- function(file_path, file_type = "auto") {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  # Auto-detect file type if not specified
  if (file_type == "auto") {
    ext <- tolower(tools::file_ext(file_path))
    if (ext == "rds") {
      file_type <- "rds"
    } else if (ext %in% c("csv", "tsv", "txt")) {
      file_type <- ext
    } else {
      stop("Unsupported file type: ", ext)
    }
  }
  
  # Load data based on type
  if (file_type == "rds") {
    data <- readRDS(file_path)
  } else if (file_type == "csv") {
    data <- readr::read_csv(file_path, show_col_types = FALSE)
  } else if (file_type == "tsv") {
    data <- readr::read_tsv(file_path, show_col_types = FALSE)
  } else if (file_type == "txt") {
    # Try to auto-detect delimiter
    sample_lines <- readLines(file_path, n = 5)
    if (any(grepl("\t", sample_lines))) {
      data <- readr::read_tsv(file_path, show_col_types = FALSE)
    } else {
      data <- readr::read_csv(file_path, show_col_types = FALSE)
    }
  }
  
  cat("Loaded", file_type, "file:", file_path, "\n")
  cat("  Dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
  
  return(as.data.frame(data))
}

# Main merge function
merge_atlas_data <- function(new_metadata_file = NULL, 
                            new_expression_file = NULL,
                            existing_metadata_file = "sample_metadata.rds",
                            existing_expression_file = "expression_data.rds",
                            output_metadata_file = "sample_metadata_merged.rds",
                            output_expression_file = "expression_data_merged.rds",
                            merge_strategy = "append",
                            backup_existing = TRUE) {
  
  cat("Starting data merge process...\n")
  cat("Merge strategy:", merge_strategy, "\n")
  cat("Backup existing files:", backup_existing, "\n\n")
  
  # Validate merge strategy
  if (!merge_strategy %in% c("append", "update", "skip_duplicates")) {
    stop("merge_strategy must be one of: 'append', 'update', 'skip_duplicates'")
  }
  
  # Create backups if requested
  if (backup_existing) {
    if (file.exists(existing_metadata_file)) {
      backup_metadata <- paste0(tools::file_path_sans_ext(existing_metadata_file), "_backup_", 
                                format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      file.copy(existing_metadata_file, backup_metadata)
      cat("Created backup:", backup_metadata, "\n")
    }
    
    if (file.exists(existing_expression_file)) {
      backup_expression <- paste0(tools::file_path_sans_ext(existing_expression_file), "_backup_", 
                                  format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      file.copy(existing_expression_file, backup_expression)
      cat("Created backup:", backup_expression, "\n")
    }
  }
  
  # Load existing data
  cat("\nLoading existing data...\n")
  existing_metadata <- load_data_file(existing_metadata_file)
  existing_expression <- load_data_file(existing_expression_file)
  
  # Initialize merged data with existing data
  merged_metadata <- existing_metadata
  merged_expression <- existing_expression
  
  # Merge new metadata if provided
  if (!is.null(new_metadata_file)) {
    cat("\nProcessing new metadata...\n")
    new_metadata <- load_data_file(new_metadata_file)
    merged_metadata <- merge_sample_metadata(existing_metadata, new_metadata, merge_strategy)
  }
  
  # Merge new expression data if provided
  if (!is.null(new_expression_file)) {
    cat("\nProcessing new expression data...\n")
    new_expression <- load_data_file(new_expression_file)
    merged_expression <- merge_expression_data(existing_expression, new_expression, merge_strategy)
  }
  
  # Validate that all samples in expression data have metadata
  cat("\nValidating merged data consistency...\n")
  metadata_samples <- if ("PubName" %in% colnames(merged_metadata)) {
    merged_metadata$PubName
  } else {
    merged_metadata$Name
  }
  
  expression_samples <- unique(merged_expression$sample)
  missing_metadata <- setdiff(expression_samples, metadata_samples)
  missing_expression <- setdiff(metadata_samples, expression_samples)
  
  if (length(missing_metadata) > 0) {
    warning("Found ", length(missing_metadata), " samples in expression data without metadata: ", 
            paste(head(missing_metadata, 5), collapse = ", "))
  }
  
  if (length(missing_expression) > 0) {
    cat("ℹ️  Found", length(missing_expression), "samples in metadata without expression data (this is OK)\n")
  }
  
  # Save merged data
  cat("\nSaving merged data...\n")
  saveRDS(merged_metadata, output_metadata_file)
  saveRDS(merged_expression, output_expression_file)
  
  cat("✓ Saved merged metadata to:", output_metadata_file, "\n")
  cat("✓ Saved merged expression data to:", output_expression_file, "\n")
  
  # Summary
  cat("\n=== MERGE SUMMARY ===\n")
  cat("Final dataset contains:\n")
  cat("- Samples:", nrow(merged_metadata), "\n")
  cat("- Expression measurements:", nrow(merged_expression), "\n")
  cat("- Unique genes:", length(unique(merged_expression$gene)), "\n")
  cat("- Cell types:", length(unique(merged_metadata[[if ("PubCelltype" %in% colnames(merged_metadata)) "PubCelltype" else "Celltype"]])), "\n")
  
  return(list(
    metadata = merged_metadata,
    expression = merged_expression
  ))
}

# Example usage function
show_usage_examples <- function() {
  cat("\n=== USAGE EXAMPLES ===\n")
  cat("# Basic merge (append new data):\n")
  cat("result <- merge_atlas_data(\n")
  cat("  new_metadata_file = 'new_sample_metadata.csv',\n")
  cat("  new_expression_file = 'new_expression_data.csv'\n")
  cat(")\n\n")
  
  cat("# Update existing samples with new data:\n")
  cat("result <- merge_atlas_data(\n")
  cat("  new_metadata_file = 'updated_metadata.csv',\n")
  cat("  new_expression_file = 'updated_expression.csv',\n")
  cat("  merge_strategy = 'update'\n")
  cat(")\n\n")
  
  cat("# Skip duplicate samples:\n")
  cat("result <- merge_atlas_data(\n")
  cat("  new_expression_file = 'additional_expression.csv',\n")
  cat("  merge_strategy = 'skip_duplicates'\n")
  cat(")\n\n")
  
  cat("# Merge only metadata (no new expression data):\n")
  cat("result <- merge_atlas_data(\n")
  cat("  new_metadata_file = 'additional_samples.csv'\n")
  cat(")\n\n")
  
  cat("# Custom output files:\n")
  cat("result <- merge_atlas_data(\n")
  cat("  new_metadata_file = 'new_data.csv',\n")
  cat("  output_metadata_file = 'combined_metadata.rds',\n")
  cat("  output_expression_file = 'combined_expression.rds'\n")
  cat(")\n\n")
}

# Interactive mode function
run_interactive_merge <- function() {
  cat("=== INTERACTIVE DATA MERGE ===\n")
  
  # Get input files
  cat("Enter the path to new metadata file (or press Enter to skip): ")
  new_metadata_file <- readline()
  if (new_metadata_file == "") new_metadata_file <- NULL
  
  cat("Enter the path to new expression file (or press Enter to skip): ")
  new_expression_file <- readline()
  if (new_expression_file == "") new_expression_file <- NULL
  
  if (is.null(new_metadata_file) && is.null(new_expression_file)) {
    cat("No new data files specified. Exiting.\n")
    return()
  }
  
  # Get merge strategy
  cat("Choose merge strategy:\n")
  cat("1. append (default) - Add new data, fail if duplicates exist\n")
  cat("2. update - Replace existing data with new data for duplicates\n")
  cat("3. skip_duplicates - Keep existing data, skip duplicate entries\n")
  cat("Enter choice (1-3): ")
  choice <- readline()
  
  merge_strategy <- switch(choice,
                          "1" = "append",
                          "2" = "update", 
                          "3" = "skip_duplicates",
                          "append")  # default
  
  # Run the merge
  result <- merge_atlas_data(
    new_metadata_file = new_metadata_file,
    new_expression_file = new_expression_file,
    merge_strategy = merge_strategy
  )
  
  cat("Merge completed successfully!\n")
  return(result)
}

# Display usage information
cat("Data merge script loaded successfully!\n")
cat("Available functions:\n")
cat("- merge_atlas_data(): Main merge function\n")
cat("- run_interactive_merge(): Interactive merge wizard\n")
cat("- show_usage_examples(): Show example usage\n")
cat("\nRun show_usage_examples() to see how to use this script.\n")
