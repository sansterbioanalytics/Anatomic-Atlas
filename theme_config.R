# =============================================================================
# Theme and Styling Module for Anatomic RNA Atlas
# Contains all theme configuration, CSS generation, and styling functions
# =============================================================================

# Required libraries for this module
suppressPackageStartupMessages({
    library(shiny)
})

# =============================================================================
# Theme Configuration
# =============================================================================

# Define the application theme and styling
app_theme <- list(
    # === CORE BRAND COLORS ===
    primary_color = "#DC143C", # Crimson red - main accent color
    secondary_color = "#8B0000", # Dark red - secondary accent
    accent_color = "#FFB6C1", # Light pink - subtle accent

    # === NEUTRAL COLORS ===
    background_white = "#FFFFFF", # Pure white backgrounds
    background_light = "#F8F9FA", # Light gray backgrounds
    background_dark = "#2C3E50", # Dark backgrounds for contrast
    text_primary = "#000000", # Black primary text
    text_secondary = "#4A4A4A", # Dark gray secondary text
    text_light = "#777777", # Light gray tertiary text
    text_white = "#FFFFFF", # White text for dark backgrounds

    # === BORDER AND SEPARATOR COLORS ===
    border_light = "#E9ECEF", # Light borders
    border_medium = "#DEE2E6", # Medium borders
    border_dark = "#6C757D", # Dark borders

    # === COMPONENT-SPECIFIC COLORS ===
    success_color = "#28A745", # Success/positive actions
    warning_color = "#FFC107", # Warning states
    error_color = "#DC3545", # Error states
    info_color = "#17A2B8", # Informational elements

    # === PLOT COLORS ===
    plot_group1 = "#DC143C", # Primary red for group 1
    plot_group2 = "#1f62a5", # Blue-gray for group 2
    plot_other = "#95A5A6", # Light gray for "other" category
    plot_background = "#FFFFFF", # White plot backgrounds

    # === UI SKIN AND LAYOUT ===
    dashboard_skin = "black", # Dashboard skin (red, blue, black, etc.)

    # === TYPOGRAPHY ===
    font_family_primary = "'Segoe UI', 'Helvetica Neue', Arial, sans-serif",
    font_family_mono = "'Consolas', 'Monaco', 'Courier New', monospace",
    font_size_h1 = "24px",
    font_size_h2 = "20px",
    font_size_h3 = "18px",
    font_size_h4 = "16px",
    font_size_h5 = "14px",
    font_size_body = "13px",
    font_size_small = "11px",

    # === SPACING ===
    spacing_xs = "4px",
    spacing_sm = "8px",
    spacing_md = "12px",
    spacing_lg = "16px",
    spacing_xl = "24px",
    spacing_xxl = "32px",

    # === LOGO CONFIGURATION ===
    logo_primary_url = "www/anatomic_logo.png",
    logo_link_url = "https://www.anatomic.com/",

    # === BOX SHADOWS AND EFFECTS ===
    shadow_light = "0 2px 4px rgba(0,0,0,0.1)",
    shadow_medium = "0 4px 8px rgba(0,0,0,0.15)",
    shadow_heavy = "0 8px 16px rgba(0,0,0,0.2)",

    # === BORDER RADIUS ===
    radius_sm = "4px",
    radius_md = "6px",
    radius_lg = "8px",
    radius_xl = "12px"
)

# =============================================================================
# CSS Generation Functions
# =============================================================================

# CSS styling function
generate_app_css <- function(theme) {
    HTML(paste0("
        /* === GLOBAL STYLES === */
        body, .content-wrapper, .right-side {
            background-color: ", theme$background_white, " !important;
            font-family: ", theme$font_family_primary, ";
            color: ", theme$text_primary, ";
        }

        /* === HEADER STYLING === */
        .main-header {
            background-color: ", theme$background_white, " !important;
            border-bottom: 2px solid ", theme$primary_color, " !important;
        }

        .main-header .navbar {
            background-color: ", theme$background_white, " !important;
        }

        .main-header .logo {
            background-color: ", theme$background_white, " !important;
            color: ", theme$text_primary, " !important;
            font-family: ", theme$font_family_primary, ";
            font-weight: bold;
            font-size: ", theme$font_size_h3, ";
            border-right: 1px solid ", theme$border_light, ";
        }

        .main-header .logo:hover {
            background-color: ", theme$background_light, " !important;
        }

        /* === ENHANCED HEADER STYLING === */
        .main-header {
            height: 50px !important;
            min-height: 50px !important;
            max-height: 50px !important;
        }
        
        .main-header .navbar {
            height: 50px !important;
            min-height: 50px !important;
        }
        
        /* Fix hamburger menu height and positioning */
        .main-header .sidebar-toggle {
            height: 50px !important;
            width: 50px !important;
            padding: 12px 15px !important;
            line-height: 26px !important;
            font-size: 18px !important;
        }
        
        /* Hide or minimize the sidebar toggle if not needed */
        .main-header .sidebar-toggle {
            display: none !important;
        }
        
        /* Fix navbar content positioning */
        .main-header .navbar-custom-menu {
            height: 50px !important;
            float: right !important;
        }
        
        .custom-header-container {
            background-color: ", theme$background_white, " !important;
            width: 100% !important;
            margin: 0 !important;
            padding: 0 !important;
            height: 50px !important;
            line-height: 50px !important;
            justify-content: flex-start !important;
        }
        
        .header-logo-section {
            height: 50px !important;
            display: flex !important;
            align-items: center !important;
            flex-shrink: 0 !important;
        }
        
        .header-center-section {
            height: 50px !important;
            display: flex !important;
            align-items: center !important;
            margin-left: 20px !important;
            gap: 15px !important;
            flex-grow: 0 !important;
        }
        
        .header-title {
            font-family: ", theme$font_family_primary, ";
            text-shadow: 0 1px 2px rgba(0,0,0,0.1);
            margin: 0 !important;
            line-height: 1.2 !important;
        }
        
        /* === ENHANCED MODE SELECTOR STYLING === */
        .enhanced-mode-selector {
            z-index: 1000;
        }
        
        .mode-button {
            transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1) !important;
            transform: translateY(0) !important;
            position: relative !important;
            overflow: hidden !important;
        }
        
        .mode-button:hover {
            transform: translateY(-2px) !important;
            box-shadow: 0 8px 25px rgba(0,0,0,0.15) !important;
        }
        
        .mode-button:active {
            transform: translateY(0) !important;
            transition: all 0.1s ease !important;
        }
        
        .mode-target-btn.active,
        .mode-target-btn:focus {
            box-shadow: 0 6px 20px rgba(220, 20, 60, 0.4) !important;
            outline: none !important;
        }
        
        .mode-explorer-btn.active {
            background: linear-gradient(135deg, ", theme$info_color, ", #1976D2) !important;
            color: white !important;
            border-color: ", theme$info_color, " !important;
            box-shadow: 0 4px 12px rgba(23, 162, 184, 0.3) !important;
        }
        
        .mode-explorer-btn:hover {
            background: ", theme$background_white, " !important;
            border-color: ", theme$primary_color, " !important;
            color: ", theme$primary_color, " !important;
        }
        
        .mode-explorer-btn.active:hover {
            background: linear-gradient(135deg, ", theme$info_color, ", #1565C0) !important;
            color: white !important;
        }

        /* === MODE SELECTOR STYLING (Legacy) === */
        .mode-selector-container {
            background-color: transparent !important;
        }
        
        .mode-selector-container .radio {
            margin: 0 !important;
            display: inline-block;
        }
        
        .mode-selector-container .radio + .radio {
            margin-left: 15px !important;
        }
        
        .mode-selector-container label {
            color: ", theme$text_primary, " !important;
            font-weight: 500;
            font-size: ", theme$font_size_small, ";
            margin-bottom: 0 !important;
            padding: 4px 8px;
            border-radius: 4px;
            transition: background-color 0.2s ease;
        }
        
        .mode-selector-container label:hover {
            background-color: ", theme$background_light, " !important;
        }
        
        .mode-selector-container input[type='radio']:checked + span {
            color: ", theme$primary_color, " !important;
            font-weight: bold;
        }

        /* === CELL TYPE TOGGLE STYLING === */
        .cell-type-toggle-container {
            display: grid;
            grid-template-columns: 1fr 1fr;
            gap: 6px;
            margin-bottom: 12px;
            max-width: 100%;
            overflow: hidden;
        }
        
        .cell-type-toggle {
            display: block;
            margin: 0;
            min-width: 0; /* Allow flex items to shrink */
        }
        
        .cell-type-toggle-btn {
            padding: 6px 8px;
            border: 2px solid #6c757d;
            border-radius: 6px;
            background-color: #6c757d;
            color: white;
            font-size: 11px;
            font-weight: 500;
            cursor: pointer;
            transition: all 0.2s ease;
            text-align: center;
            width: 100%;
            max-width: 100%;
            user-select: none;
            white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
            box-sizing: border-box;
        }
        
        .cell-type-toggle-btn:hover {
            transform: translateY(-1px);
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        .cell-type-toggle-btn.active {
            background-color: ", theme$primary_color, ";
            border-color: ", theme$primary_color, ";
            color: white;
            font-weight: bold;
        }
        
        .cell-type-toggle-btn.inactive {
            background-color: #6c757d;
            border-color: #6c757d;
            color: #ffffff;
            opacity: 0.7;
        }
        
        .cell-type-toggle-section {
            margin-bottom: 16px;
            padding: 16px;
            border-radius: 6px;
            background-color: rgba(255,255,255,0.05);
            border: 1px solid rgba(255,255,255,0.1);
        }
        
        .cell-type-section-header {
            color: ", theme$text_white, ";
            font-size: 13px;
            font-weight: bold;
            margin-bottom: 8px;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }

        /* === SIDEBAR STYLING === */
        .main-sidebar {
            background-color: ", theme$background_dark, " !important;
            max-width: 300px;
            overflow-x: hidden;
        }

        .sidebar {
            background-color: ", theme$background_dark, " !important;
            color: ", theme$text_white, ";
            padding-right: 15px;
            box-sizing: border-box;
        }

        .sidebar-menu > li > a {
            color: ", theme$text_white, " !important;
            border-left: 3px solid transparent;
        }

        .sidebar-menu > li > a:hover {
            background-color: ", theme$primary_color, " !important;
            border-left-color: ", theme$secondary_color, ";
        }

        /* === ENHANCED SIDEBAR FORM CONTROLS === */
        .sidebar .form-group {
            margin-bottom: ", theme$spacing_lg, ";
            max-width: 100%;
            overflow: hidden;
        }
        
        .sidebar .form-group label {
            color: ", theme$text_white, " !important;
            font-weight: 600;
            font-size: ", theme$font_size_body, ";
            margin-bottom: ", theme$spacing_sm, ";
            display: block;
            white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
        }
        
        .sidebar .form-control {
            background-color: ", theme$background_white, " !important;
            border: 1px solid ", theme$border_medium, " !important;
            border-radius: ", theme$radius_sm, " !important;
            color: ", theme$text_primary, " !important;
            font-size: ", theme$font_size_body, " !important;
            max-width: 100%;
            box-sizing: border-box;
        }
        
        .sidebar .form-control:focus {
            border-color: ", theme$primary_color, " !important;
            box-shadow: 0 0 0 0.2rem rgba(220, 20, 60, 0.25) !important;
        }
        
        .sidebar .selectize-input {
            background-color: ", theme$background_white, " !important;
            border: 1px solid ", theme$border_medium, " !important;
            border-radius: ", theme$radius_sm, " !important;
            color: ", theme$text_primary, " !important;
            max-width: 100%;
            box-sizing: border-box;
        }
        
        .sidebar .selectize-input.focus {
            border-color: ", theme$primary_color, " !important;
            box-shadow: 0 0 0 0.2rem rgba(220, 20, 60, 0.25) !important;
        }
        
        .sidebar .radio {
            margin-bottom: ", theme$spacing_sm, " !important;
        }
        
        .sidebar .radio label {
            color: ", theme$text_white, " !important;
            font-weight: normal !important;
            font-size: ", theme$font_size_body, " !important;
            margin-bottom: 0 !important;
            padding-left: 20px;
        }
        
        .sidebar .radio input[type=\"radio\"] {
            margin-top: 2px;
        }
        
        .sidebar h4, .sidebar h5 {
            color: ", theme$text_white, " !important;
            font-weight: bold !important;
            margin-bottom: ", theme$spacing_md, " !important;
            text-transform: uppercase;
            letter-spacing: 0.5px;
            font-size: ", theme$font_size_h5, " !important;
        }
        
        .sidebar .help-text {
            color: ", theme$text_light, " !important;
            font-size: ", theme$font_size_small, " !important;
            line-height: 1.4;
            margin-bottom: ", theme$spacing_sm, " !important;
        }
        
        /* === SECTION DIVIDERS === */
        .gene-selection-container,
        .product-grouping-section {
            border-bottom: 1px solid rgba(255,255,255,0.1);
            padding-bottom: ", theme$spacing_lg, ";
            margin-bottom: ", theme$spacing_lg, ";
            max-width: 100%;
            overflow: hidden;
            box-sizing: border-box;
        }
        
        .gene-selection-container:last-child,
        .product-grouping-section:last-child {
            border-bottom: none;
        }
        
        .product-grouping-container {
            max-width: 100%;
            overflow: hidden;
            box-sizing: border-box;
        }
        
        /* === SIDEBAR ACTION BUTTONS === */
        .sidebar .btn {
            border-radius: ", theme$radius_sm, " !important;
            font-weight: 500 !important;
            transition: all 0.2s ease !important;
        }
        
        .sidebar .btn-outline-primary {
            border-color: ", theme$primary_color, " !important;
            color: ", theme$primary_color, " !important;
            background-color: ", theme$background_white, " !important;
        }
        
        .sidebar .btn-outline-primary:hover {
            background-color: ", theme$primary_color, " !important;
            color: ", theme$text_white, " !important;
        }
        
        .sidebar .btn-outline-success {
            border-color: ", theme$success_color, " !important;
            color: ", theme$success_color, " !important;
            background-color: ", theme$background_white, " !important;
        }
        
        .sidebar .btn-outline-success:hover {
            background-color: ", theme$success_color, " !important;
            color: ", theme$text_white, " !important;
        }
        
        .sidebar .btn-outline-secondary {
            border-color: ", theme$border_dark, " !important;
            color: ", theme$text_light, " !important;
            background-color: ", theme$background_white, " !important;
        }
        
        .sidebar .btn-outline-secondary:hover {
            background-color: ", theme$border_dark, " !important;
            color: ", theme$text_white, " !important;
        }
        
        .group-action-buttons {
            border-top: 1px solid rgba(255,255,255,0.1);
            padding-top: ", theme$spacing_md, ";
        }

        /* === CONTENT AREA === */
        .content {
            background-color: ", theme$background_white, " !important;
            padding: ", theme$spacing_lg, ";
        }

        /* === BOX STYLING === */
        .box {
            border-radius: ", theme$radius_md, ";
            box-shadow: ", theme$shadow_light, ";
            border-top: 3px solid ", theme$primary_color, " !important;
            background-color: ", theme$background_white, ";
        }

        .box-header {
            background-color: ", theme$background_light, " !important;
            border-bottom: 1px solid ", theme$border_light, ";
        }

        .box-header .box-title {
            color: ", theme$text_primary, " !important;
            font-weight: bold;
        }

        .box.box-primary .box-header {
            background-color: ", theme$primary_color, " !important;
            color: ", theme$text_white, " !important;
        }

        .box.box-primary .box-header .box-title {
            color: ", theme$text_white, " !important;
        }

        .box.box-info .box-header {
            background-color: ", theme$info_color, " !important;
            color: ", theme$text_white, " !important;
        }

        .box.box-info .box-header .box-title {
            color: ", theme$text_white, " !important;
        }

        /* === FORM CONTROLS === */
        .form-control {
            border: 1px solid ", theme$border_medium, ";
            border-radius: ", theme$radius_sm, ";
            color: ", theme$text_primary, ";
        }

        .form-control:focus {
            border-color: ", theme$primary_color, ";
            box-shadow: 0 0 0 0.2rem rgba(220, 20, 60, 0.25);
        }

        .btn-primary {
            background-color: ", theme$primary_color, " !important;
            border-color: ", theme$primary_color, " !important;
            color: ", theme$text_white, " !important;
        }

        .btn-primary:hover {
            background-color: ", theme$secondary_color, " !important;
            border-color: ", theme$secondary_color, " !important;
        }

        /* === LOGO STYLING === */
        .logo-container {
            display: flex;
            align-items: center;
            gap: ", theme$spacing_md, ";
            margin: ", theme$spacing_md, " 0;
        }

        .logo-primary {
            max-height: 50px;
            max-width: 150px;
            cursor: pointer;
            transition: opacity 0.2s ease;
        }

        .logo-primary:hover {
            opacity: 0.8;
        }

        .logo-link {
            text-decoration: none;
            display: flex;
            align-items: center;
            gap: ", theme$spacing_md, ";
        }

        .logo-link:hover {
            text-decoration: none;
        }

        /* === DATA TABLE STYLING === */
        .dataTables_wrapper {
            font-size: ", theme$font_size_small, ";
        }

        .table-striped > tbody > tr:nth-of-type(odd) {
            background-color: ", theme$background_light, ";
        }

        .dt-group1 {
            background-color: rgba(220, 20, 60, 0.1) !important;
        }

        .dt-group2 {
            background-color: rgba(44, 62, 80, 0.1) !important;
        }

        /* === HELP TEXT AND INSTRUCTIONS === */
        .help-text {
            color: ", theme$text_light, " !important;
            font-size: ", theme$font_size_small, " !important;
            font-style: italic;
            margin-bottom: ", theme$spacing_sm, " !important;
            line-height: 1.4;
        }

        .instructions-header {
            color: ", theme$text_primary, " !important;
            font-weight: bold;
            font-size: ", theme$font_size_h5, " !important;
            margin-bottom: ", theme$spacing_sm, " !important;
        }

        .statistics-header {
            color: ", theme$text_primary, " !important;
            font-weight: bold;
            font-size: ", theme$font_size_h5, " !important;
            margin-bottom: ", theme$spacing_sm, " !important;
        }

        /* === SHARED COMPONENT STYLING === */
        .analysis-stats-container {
            max-height: 480px;
            overflow-y: auto;
            padding-right: ", theme$spacing_sm, ";
            border: 1px solid ", theme$border_light, ";
            border-radius: ", theme$radius_sm, ";
            background-color: ", theme$background_white, ";
        }

        .mode-specific-container {
            padding: ", theme$spacing_md, ";
            border-radius: ", theme$radius_md, ";
            background-color: ", theme$background_light, ";
            border: 1px solid ", theme$border_light, ";
            margin-bottom: ", theme$spacing_md, ";
        }

        .target-mode-controls {
            background-color: ", theme$background_dark, ";
            padding: ", theme$spacing_lg, ";
            border-radius: ", theme$radius_md, ";
        }

        .explorer-mode-controls {
            background-color: ", theme$background_dark, ";
            padding: ", theme$spacing_lg, ";
            border-radius: ", theme$radius_md, ";
        }

        .primary-comparison-section {
            border-top: 2px solid ", theme$border_medium, ";
            padding-top: ", theme$spacing_md, ";
            margin-top: ", theme$spacing_md, ";
        }

        .gene-selection-container {
            background-color: rgba(220, 20, 60, 0.05);
            border: 1px solid rgba(220, 20, 60, 0.2);
            border-radius: ", theme$radius_sm, ";
            padding: ", theme$spacing_md, ";
            margin-bottom: ", theme$spacing_md, ";
        }

        .stat-card {
            background-color: ", theme$background_light, ";
            border: 1px solid ", theme$border_light, ";
            border-radius: ", theme$radius_sm, ";
            padding: ", theme$spacing_sm, ";
            margin-bottom: ", theme$spacing_sm, ";
        }

        .stat-card-header {
            font-weight: bold;
            color: ", theme$primary_color, ";
            font-size: ", theme$font_size_small, ";
            margin-bottom: ", theme$spacing_xs, ";
        }

        .stat-card-content {
            font-size: ", theme$font_size_small, ";
            color: ", theme$text_secondary, ";
            line-height: 1.4;
        }

        .stat-item {
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 2px 0;
            border-bottom: 1px solid ", theme$border_light, ";
        }

        .stat-item:last-child {
            border-bottom: none;
        }

        .stat-label {
            font-weight: 500;
            color: ", theme$text_primary, ";
        }

        .stat-value {
            color: ", theme$text_secondary, ";
            font-family: ", theme$font_family_mono, ";
        }

        .summary-box {
            font-size: ", theme$font_size_small, ";
            line-height: 1.4;
            max-height: 400px;
            overflow-y: auto;
            background-color: ", theme$background_light, ";
            padding: ", theme$spacing_md, ";
            border-radius: ", theme$radius_sm, ";
            border: 1px solid ", theme$border_light, ";
            font-family: ", theme$font_family_mono, ";
        }

        /* === RESPONSIVE DESIGN === */
        @media (max-width: 768px) {
            .logo-container {
                flex-direction: column;
                gap: ", theme$spacing_sm, ";
            }

            .loading-content {
                margin: ", theme$spacing_md, ";
                padding: ", theme$spacing_lg, ";
            }

            .progress-container {
                width: 250px;
            }
        }
    "))
}

# =============================================================================
# Plot Theme Functions
# =============================================================================

# Plot theme function
get_plot_theme <- function(theme) {
    list(
        colors = c(theme$plot_group1, theme$plot_group2, theme$plot_other),
        background = theme$plot_background,
        text_color = theme$text_primary,
        grid_color = theme$border_light,
        title_size = 14,
        subtitle_size = 11,
        axis_text_size = 12,
        legend_text_size = 10
    )
}

# Generate color palette for plots
get_color_palette <- function(theme, n_colors = 3) {
    base_colors <- c(theme$plot_group1, theme$plot_group2, theme$plot_other)
    
    if (n_colors <= length(base_colors)) {
        return(base_colors[1:n_colors])
    } else {
        # Generate additional colors if needed
        additional_colors <- colorRampPalette(c(theme$primary_color, theme$secondary_color, theme$info_color))(n_colors - length(base_colors))
        return(c(base_colors, additional_colors))
    }
}

# =============================================================================
# JavaScript Generation Functions
# =============================================================================

# Generate JavaScript for UI interactions
generate_app_javascript <- function() {
    HTML("
        <!-- Font Awesome for icons -->
        <link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css'>
        
        <script>
        $(document).ready(function() {
            
            // Enhanced Mode Selector Logic
            var currentMode = 'target';
            
            function updateModeButtons(mode) {
                // Remove active class from all mode buttons
                $('.mode-target-btn, .mode-explorer-btn').removeClass('active');
                
                if (mode === 'target') {
                    $('.mode-target-btn').addClass('active');
                    // Update the hidden radio button
                    $('input[name=\"app_mode\"][value=\"target\"]').prop('checked', true);
                } else if (mode === 'explorer') {
                    $('.mode-explorer-btn').addClass('active');
                    // Update the hidden radio button
                    $('input[name=\"app_mode\"][value=\"explorer\"]').prop('checked', true);
                }
                currentMode = mode;
                console.log('Mode updated to:', mode); // Debug log
            }
            
            // Initialize mode selector
            updateModeButtons('target');
            
            // Handle mode button clicks
            $(document).on('click', '#mode_target', function(e) {
                e.preventDefault();
                console.log('Target mode clicked'); // Debug log
                updateModeButtons('target');
            });
            
            $(document).on('click', '#mode_explorer', function(e) {
                e.preventDefault();
                console.log('Explorer mode clicked'); // Debug log
                updateModeButtons('explorer');
            });
            
            // Listen for Shiny input changes and update visual state accordingly
            $(document).on('shiny:inputchanged', function(event) {
                if (event.name === 'app_mode') {
                    updateModeButtons(event.value);
                }
            });
        });
        
        // Auto-scroll coexpression log to bottom when updated
        $(document).on('shiny:value', function(event) {
            if (event.target.id === 'coexpression_live_log') {
                var container = document.getElementById('coexpression_log_container');
                if (container) {
                    setTimeout(function() {
                        container.scrollTop = container.scrollHeight;
                    }, 100);
                }
            }
        });
        </script>
    ")
}

# =============================================================================
# Theme Validation Functions
# =============================================================================

# Validate theme configuration
validate_theme <- function(theme) {
    required_colors <- c(
        "primary_color", "secondary_color", "background_white", 
        "text_primary", "plot_group1", "plot_group2"
    )
    
    missing_colors <- required_colors[!required_colors %in% names(theme)]
    
    if (length(missing_colors) > 0) {
        warning(paste("Missing theme colors:", paste(missing_colors, collapse = ", ")))
        return(FALSE)
    }
    
    return(TRUE)
}

# Get theme color by name with fallback
get_theme_color <- function(theme, color_name, fallback = "#000000") {
    if (color_name %in% names(theme)) {
        return(theme[[color_name]])
    } else {
        warning(paste("Color", color_name, "not found in theme, using fallback"))
        return(fallback)
    }
}
