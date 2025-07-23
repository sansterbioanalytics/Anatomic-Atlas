# =============================================================================
# Test Data Preparation for Simple Anatomic Atlas App
# Creates test data in the expected format for the Shiny application
# =============================================================================

renv::activate()
library(dplyr)
library(readr)
library(arrow)
library(DESeq2)
library(SummarizedExperiment)
library(tibble)
library(tidyr)

# Check if we have the DESeq2 object from the notebook analysis
dds_path <- "/workspaces/Anatomic-RNA/data/export/DESEQ2/2025-07-14-Anatomic-Atlas_salmon_deseq2_dds.rds"

if (file.exists(dds_path)) {
    # Load the DESeq2 object
    dds <- readRDS(dds_path)

    # Extract sample metadata and clean up columns
    sample_data <- as.data.frame(colData(dds))

    # Drop unwanted columns
    columns_to_drop <- c("R1", "R2", "SalmonQuant")
    sample_data <- sample_data[, !names(sample_data) %in% columns_to_drop]

    # Extract raw counts and convert to CPM
    raw_counts <- counts(dds, normalized = FALSE)

    # Calculate CPM (counts per million)
    # CPM = (counts / total counts per sample) * 1,000,000
    library_sizes <- colSums(raw_counts)
    cpm_data <- sweep(raw_counts, 2, library_sizes, FUN = "/") * 1e6

    # Add pseudocount and log2 transform for better visualization
    # log2(CPM + 1) is commonly used for visualization
    log2_cpm_data <- log2(cpm_data + 1)

    # Get VST transformed data from DESeq2 object
    vst_data <- assay(vst(dds, blind = FALSE))

    # Convert CPM data to long format for plotting
    cpm_expression_data <- as.data.frame(log2_cpm_data) %>%
        tibble::rownames_to_column("gene") %>%
        tidyr::pivot_longer(-gene, names_to = "sample", values_to = "expression") %>%
        mutate(data_type = "log2_cpm")

    # Convert VST data to long format for plotting
    vst_expression_data <- as.data.frame(vst_data) %>%
        tibble::rownames_to_column("gene") %>%
        tidyr::pivot_longer(-gene, names_to = "sample", values_to = "expression") %>%
        mutate(data_type = "vst")

    # Combine both data types
    expression_data <- bind_rows(cpm_expression_data, vst_expression_data)

    # Create export directory
    dir.create("/workspaces/Anatomic-RNA/data/export", recursive = TRUE, showWarnings = FALSE)

    # save as feather for faster loading (if arrow is available)
    arrow::write_feather(sample_data, "/workspaces/Anatomic-RNA/data/export/sample_metadata.feather")
    arrow::write_feather(expression_data, "/workspaces/Anatomic-RNA/data/export/expression_data.feather")


    # Print data summary
    cat("Sample Data Summary:\n")
    print(sample_data %>% dplyr::count(PubCelltype))
    cat("\nExpression Data Summary:\n")
    cat("Genes:", length(unique(expression_data$gene)), "\n")
    cat("Samples:", length(unique(expression_data$sample)), "\n")
    cat("Total observations:", nrow(expression_data), "\n")
    cat("Data types included: log2(CPM + 1) and VST\n")
    cat("\nData type distribution:\n")
    print(table(expression_data$data_type))
    cat("\nLibrary sizes (total counts per sample):\n")
    print(summary(library_sizes))

    # Print data summary
    cat("Sample Data Summary:\n")
    print(sample_data %>% dplyr::count(PubCelltype))
    cat("\nExpression Data Summary:\n")
    cat("Genes:", length(unique(expression_data$gene)), "\n")
    cat("Samples:", length(unique(expression_data$sample)), "\n")
    cat("Total observations:", nrow(expression_data), "\n")
    cat("Data types included: log2(CPM + 1) and VST\n")

    # Print sample of library sizes for verification
    cat("\nLibrary sizes (total counts per sample):\n")
    print(summary(library_sizes))

    # Print data type summary
    cat("\nData type distribution:\n")
    print(table(expression_data$data_type))
} else {
    cat("DESeq2 object not found at:", dds_path, "\n")
    cat("Please run the data gathering notebook first or check the file path.\n")
}
