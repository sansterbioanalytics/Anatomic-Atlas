# Anatomic RNA Atlas - Simple Shiny Application

A streamlined R Shiny application for interactive exploration of RNA-seq expression data across different cell types in the Anatomic Atlas.

## Features

- **Simple Interface**: Single-file application that's easy to deploy and maintain
- **Pre-computed Data**: Works with pre-processed expression data (VST counts) and sample metadata
- **Interactive Visualizations**:
  - Expression histograms comparing two groups
  - Volcano plots for differential expression
  - Box plots for selected genes
- **Real-time Statistics**: Summary statistics and top differentially expressed genes
- **Export Functionality**: Download plots and results

## Data Requirements

The application expects data in the following format:

### Sample Metadata (`expression_data.rds`)
- `Name` or `PubName`: Sample identifier
- `PubCelltype` or `Celltype`: Cell type classification
- Additional metadata columns (DIV, Source, etc.)

### Expression Data (`sample_metadata.rds`)
- `gene`: Gene identifier/symbol
- `sample`: Sample identifier (matching metadata)
- `expression`: Normalized expression value (VST recommended)

## Usage

1. **Prepare Data**: Run the data gathering notebook to generate the DESeq2 object, then:
   ```r
   aws s3 cp s3://anatomicrna/sba-rnaseq_v4.0.0/Anatomic_Atlas/export/expression_data.rds expression_data.rds
   aws s3 cp s3://anatomicrna/sba-rnaseq_v4.0.0/Anatomic_Atlas/export/sample_metadata.rds sample_metadata.rds
   ```

2. **Launch Application**:
   ```r
   # In RStudio
   shiny::runApp("simple_app.R")
   
   # Or from command line
   Rscript -e "shiny::runApp('simple_app.R', host='0.0.0.0', port=3838)"
   ```

3. **Use Interface**:
   - Select two cell types to compare
   - Choose genes to highlight in visualizations
   - Adjust significance thresholds
   - Download results and plots

## Deployment

ShinyApps deployment guide https://docs.posit.co/shinyapps.io/guide/getting_started/


## File Structure

```
app/
├── simple_app.R           # Main Shiny application
├── prepare_test_data.R    # Data preparation script
└── README.md             # This file

data/export/
├── sample_metadata.csv    # Sample information
├── expression_data.csv    # Expression matrix (long format)
└── DESEQ2/
    └── *_deseq2_dds.rds  # Source DESeq2 object
```
