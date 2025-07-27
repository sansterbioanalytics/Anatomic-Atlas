# Anatomic RNA Atlas - Simple Shiny Application

A streamlined R Shiny application for interactive exploration of RNA-seq expression data across different cell types in the Anatomic Atlas.

https://anatomicincorporated.shinyapps.io/anatomic-atlas/

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

These files are tracked in git-lfs, which you may need to install on your system or through conda. If the whole files didn't pull, run `git lfs pull` after installing this dependency.

## Usage

1. **Launch Application**:
   ```r
   # conda environment recommended for local development
   # conda create -n atlas r-base=4.3 r-devtools r-shiny r-dplyr r-matrix r-mass r-lattice r-readr xz zlib quarto
   # In RStudio
   shiny::runApp("app.R")
   
   # Or from command line
   Rscript -e "shiny::runApp('app.R', host='0.0.0.0', port=3838)"
   ```

## Deployment

ShinyApps deployment guide https://docs.posit.co/shinyapps.io/guide/getting_started/

## File Structure

### Core Application Files
- **`app.R`** - Main Shiny application file containing UI definition and server logic
- **`renv.lock`** - Package dependency lock file
- **`README.md`** - Project documentation

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
- `create_app_header()` - Application header with logo
- `create_app_sidebar()` - Main sidebar with analysis controls
- `create_contrast_selection_ui()` - Group comparison controls
- `create_gene_selection_ui()` - Gene selection interface
- `create_gene_pagination_ui()` - Gene pagination controls
- `create_analysis_options_ui()` - Analysis options panel
- `create_loading_overlay()` - Loading screen
- `create_expression_plot_box()` - Main expression plot container
- `create_coexpression_tab()` - Co-expression analysis interface
- `create_analysis_overview_box()` - Analysis summary panel
- Layout helper functions for organizing UI components

#### 5. Server Utilities (`server_utils.R`)
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

#### 6. Co-expression Analysis (`coexpression_analysis.R`)
**Purpose**: Co-expression analysis functions (pre-existing module)
**Key Functions**:
- `find_coexpressed_genes()` - Main co-expression analysis function
- `find_coexpressed_genes_streaming()` - Memory-efficient streaming analysis
- `find_coexpressed_genes_streaming_parallel()` - Parallel streaming analysis
- `create_coexpression_heatmap()` - Co-expression heatmaps
- `create_coexpression_network()` - Correlation network visualizations
- `create_correlation_table()` - Detailed correlation tables

### Data Files
- Gene set CSV files (TRP_channels.csv, Sodium_Channels.csv, etc.)
- Expression data (expression_data.rds)
- Sample metadata (sample_metadata.rds)
- Static assets (www/anatomic_logo.png)

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
