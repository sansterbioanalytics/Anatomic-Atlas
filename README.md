# Anatomic RNA Atlas - Simple Shiny Application

A streamlined R Shiny application for interactive exploration of RNA-seq expression data across different cell types in the Anatomic Atlas.

https://anatomicincorporated.shinyapps.io/anatomic-atlas/

## Features

- **Dual-Mode Interface**: Target Mode for product portfolio analysis and Explorer Mode for comparative studies
- **Pre-computed Data**: Works with pre-processed expression data (VST counts) and sample metadata
- **Interactive Visualizations**:
  - Expression boxplots and violin plots comparing cell types
  - Gene expression heatmaps with hierarchical clustering
  - Co-expression network analysis and correlation matrices
  - Product portfolio ranking and target gene analysis
- **Real-time Statistics**: Summary statistics and gene-specific metrics
- **Gene Set Management**: Upload custom gene sets or use predefined collections
- **Export Functionality**: Download plots and analysis results

## Data Requirements

The application expects data in the following format:

### Sample Metadata (`sample_metadata.rds`)
- `Name` or `PubName`: Sample identifier
- `PubCelltype` or `Celltype`: Cell type classification
- Additional metadata columns (DIV, Source, etc.)

### Expression Data (`expression_data.rds`)
- `gene`: Gene identifier/symbol
- `sample`: Sample identifier (matching metadata)
- `expression`: Normalized expression value (VST recommended)
- `data_type`: Data transformation type (e.g., "vst", "log2_cpm")

These files are tracked in git-lfs, which you may need to install on your system or through conda. If the whole files didn't pull, run `git lfs pull` after installing this dependency.

## Usage

1. **Launch Application**:
   ```r
   # conda environment recommended for local development
   # conda create -n atlas -c conda-forge -c bioconda -c nodefaults r-base=4.3 r-devtools r-shiny r-dplyr r-matrix r-mass r-lattice r-readr xz zlib quarto libcurl glpk icu xorg-libx11 pandoc
   # $ R
   # >> renv::restore()
   
   Rscript -e "shiny::runApp('app.R', host='0.0.0.0', port=3838)"
   ```

## Application Modes

The Anatomic RNA Atlas supports two distinct analysis modes:

### Target Mode
**Purpose**: Product portfolio analysis and target gene evaluation
**Features**:
- Pre-defined product groupings (RealCells, Primary Human, hiPSC-derived)
- Portfolio ranking and comparison across cell types
- Target gene heatmaps with product focus
- Default cell type selections optimized for product analysis

### Explorer Mode
**Purpose**: Comparative analysis between any two cell type groups
**Features**:
- Flexible group selection and comparison
- Expression distribution analysis
- Co-expression network analysis (beta feature)
- Custom gene set analysis across selected groups

Switch between modes using the toggle in the application header. Each mode provides specialized tools and visualizations optimized for different analysis workflows.

## Deployment

ShinyApps deployment guide https://docs.posit.co/shinyapps.io/guide/getting_started/

## Utility Scripts

### Data Management Scripts


#### Data Merger (`merge_new_data.R`)
**Purpose**: Merges new gene expression and metadata files with existing RDS objects
**Usage**:
```r
# Source the script
source("merge_new_data.R")

# Basic merge - add new data files
result <- merge_atlas_data(
  new_metadata_file = "new_samples.csv",
  new_expression_file = "new_expression.csv"
)

# Interactive mode
result <- run_interactive_merge()
```
**Features**:
- Supports multiple file formats (RDS, CSV, TSV, TXT)
- Three merge strategies: append, update, skip_duplicates
- Automatic data validation and structure checking
- Creates timestamped backups of original files
- Validates sample consistency between metadata and expression data
- Detailed logging and error reporting

**Expected Data Formats**:
- **Metadata**: Must have sample identifier (`PubName` or `Name`) and cell type column (`PubCelltype`, `Celltype`, or `celltype`)
- **Expression**: Must have `gene`, `sample`, `expression` columns; `data_type` is optional (defaults to "vst")

## File Structure

### Core Application Files
- **`app.R`** - Main Shiny application file containing UI definition and server logic
- **`renv.lock`** - Package dependency lock file
- **`README.md`** - Project documentation
- **`merge_new_data.R`** - Utility script for merging new data with existing RDS objects

### Modular Components

#### 1. Data Utilities (`data_utils.R`)
**Purpose**: Data loading, processing, and helper functions
**Key Functions**:
- `load_atlas_data()` - Loads pre-computed expression and sample data
- `load_gene_sets()` - Loads predefined gene sets from CSV files
- `get_celltype_column()` - Determines the cell type column name
- `get_sample_column()` - Determines the sample column name
- `create_contrast_data()` - Creates pre-filtered contrast data for analysis
- `validate_expression_data()` - Validates expression data structure
- `validate_sample_data()` - Validates sample metadata structure
- `process_uploaded_gene_file()` - Processes uploaded gene set files
- `filter_available_genes()` - Filters genes that exist in the dataset

#### 2. Theme Configuration (`theme_config.R`)
**Purpose**: Theme configuration, CSS generation, and styling functions
**Key Components**:
- `app_theme` - Comprehensive theme configuration object with colors, fonts, spacing
- `generate_app_css()` - Generates CSS styles for the application
- `get_plot_theme()` - Returns plot-specific theme settings
- `get_color_palette()` - Generates color palettes for visualizations
- `generate_app_javascript()` - Generates JavaScript for interactive features
- `validate_theme()` - Validates theme configuration
- `get_theme_color()` - Safely retrieves theme colors with fallbacks

#### 3. Plot Utilities (`plot_utils.R`)
**Purpose**: Plotting functions and visualization helpers
**Key Functions**:
- `create_expression_plot()` - Creates expression boxplot/violin plots
- `create_gene_heatmap()` - Creates gene expression heatmaps
- `create_group_barplot()` - Creates group expression ranking plots
- `apply_plot_theme()` - Standardizes plot appearance
- `create_empty_plot()` - Creates empty plots with messages
- `generate_plot_subtitle()` - Generates contextual plot subtitles
- `validate_plot_data()` - Validates plot data structure
- `validate_gene_selection()` - Checks if genes exist in dataset

#### 4. UI Components (`ui_components.R`)
**Purpose**: Reusable UI components and layout functions
**Key Components**:
- `create_app_header()` - Application header with logo and mode selector
- `create_app_sidebar()` - Main sidebar with analysis controls
- `create_simple_gene_selection_ui()` - Gene selection interface with file upload
- `create_product_grouping_ui()` - Product portfolio groupings (Target mode)
- `create_contrast_selection_ui()` - Group comparison controls (Explorer mode)
- `create_analysis_options_ui()` - Analysis options panel
- `create_unified_expression_plot_box()` - Main expression plot container
- `create_geneset_analysis_box()` - Gene set analysis interface
- `create_product_portfolio_overview_box()` - Product portfolio visualization
- Layout helper functions for dual-mode interface

#### 5. Co-expression UI Module (`ui_coexpression_module.R`)
**Purpose**: Specialized UI components for co-expression analysis
**Key Components**:
- `create_coexpression_analysis_box()` - Co-expression analysis interface
- `create_coexpression_controls()` - Co-expression parameter controls
- `create_coexpression_results()` - Co-expression results display
- `create_coexpression_log_panel()` - Real-time analysis progress logging

#### 6. Product Groupings (`product_groupings.R`)
**Purpose**: Configuration for cell type groupings and product portfolios
**Key Components**:
- `product_groupings` - Defines RealCells, Primary Human, and hiPSC groupings
- `get_grouping_cell_types()` - Retrieves cell types for specific groupings
- `get_default_toggle_states()` - Sets default enabled/disabled states
- Helper functions for portfolio organization and Target mode functionality

#### 7. Server Utilities (`server_utils.R`)
**Purpose**: Server helper functions and reactive utilities
**Key Functions**:
- `process_gene_statistics()` - Processes gene statistics for display
- `calculate_overall_statistics()` - Calculates dataset-wide statistics
- `process_expression_table_data()` - Prepares expression data for tables
- `generate_gene_stat_cards()` - Creates gene-specific statistics cards
- `generate_sample_counts_card()` - Creates sample count summaries
- `calculate_pagination_info()` - Handles gene pagination logic
- `validate_uploaded_gene_file()` - Validates uploaded gene files
- `format_coexpression_summary()` - Formats co-expression results
- `format_coexpression_detailed()` - Formats detailed correlation results

#### 8. Co-expression Analysis (`coexpression_analysis.R`)
**Purpose**: Co-expression analysis functions with optimized algorithms
**Key Functions**:
- `find_coexpressed_genes()` - Main co-expression analysis function with parallelization
- `find_coexpressed_genes_streaming()` - Memory-efficient streaming analysis for large datasets
- `find_coexpressed_genes_streaming_parallel()` - Parallel streaming analysis
- `create_coexpression_heatmap()` - Co-expression heatmaps with clustering
- `create_coexpression_network()` - Interactive correlation network visualizations
- `create_correlation_table()` - Detailed correlation tables with statistics

### Data Files
- **Gene set CSV files**: Ion channel and neuroscience-related gene collections (TRP_channels.csv, Sodium_Channels.csv, Calcium_Channels.csv, Cell_Adhesion_Molecules.csv, Price_GPCRs.csv, Price_Neuropeptides.csv)
- **Expression data** (`expression_data.rds`): Pre-processed expression values with sample and gene identifiers
- **Sample metadata** (`sample_metadata.rds`): Cell type classifications and sample annotations
- **Static assets** (`www/anatomic_logo.png`): Application logo and branding

## Usage Guidelines

### Adding New Features
1. Determine which module the new feature belongs to
2. Add new functions to the appropriate module file
3. Update the main `app.R` file to use the new functions
4. Update this documentation

### Modifying Existing Features
1. Locate the relevant function in the appropriate module
2. Make changes while maintaining the function interface
3. Test the changes in the context of the full application
4. Update documentation if the interface changes

### Customizing Theme
1. Modify the `app_theme` object in `theme_config.R`
2. The changes will automatically propagate throughout the application
3. Use `validate_theme()` to ensure all required colors are present

### Adding New Visualizations
1. Add plotting functions to `plot_utils.R`
2. Add corresponding UI components to `ui_components.R`
3. Add server logic to `app.R`
4. Use existing theme and utility functions for consistency
