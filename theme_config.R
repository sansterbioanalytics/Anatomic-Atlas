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
    tertiary_color = "#FFB6C1", # Light pink - subtle accent

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

        /* === SIDEBAR STYLING === */
        .main-sidebar {
            background-color: ", theme$background_dark, " !important;
        }

        .sidebar {
            background-color: ", theme$background_dark, " !important;
            color: ", theme$text_white, ";
        }

        .sidebar-menu > li > a {
            color: ", theme$text_white, " !important;
            border-left: 3px solid transparent;
        }

        .sidebar-menu > li > a:hover {
            background-color: ", theme$primary_color, " !important;
            border-left-color: ", theme$secondary_color, ";
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

        /* === LOADING OVERLAY === */
        .loading-overlay {
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            background-color: rgba(44, 62, 80, 0.95);
            z-index: 9999;
            display: flex;
            justify-content: center;
            align-items: center;
            flex-direction: column;
        }

        .loading-content {
            text-align: center;
            padding: ", theme$spacing_xxl, ";
            background: ", theme$background_white, ";
            border-radius: ", theme$radius_xl, ";
            box-shadow: ", theme$shadow_heavy, ";
            border: 2px solid ", theme$primary_color, ";
        }

        .loading-title {
            color: ", theme$primary_color, ";
            font-size: ", theme$font_size_h2, ";
            font-weight: bold;
            margin-bottom: ", theme$spacing_lg, ";
            font-family: ", theme$font_family_primary, ";
        }

        .progress-container {
            width: 300px;
            height: 20px;
            background-color: ", theme$background_light, ";
            border-radius: ", theme$radius_lg, ";
            overflow: hidden;
            margin: ", theme$spacing_lg, " 0;
            border: 1px solid ", theme$border_medium, ";
        }

        .progress-bar {
            width: 0%;
            height: 100%;
            background: linear-gradient(90deg, ", theme$primary_color, ", ", theme$secondary_color, ");
            transition: width 0.3s ease;
        }

        .loading-message {
            color: ", theme$text_secondary, ";
            margin: ", theme$spacing_md, " 0;
            font-size: ", theme$font_size_body, ";
        }

        .loading-description {
            color: ", theme$text_light, ";
            font-size: ", theme$font_size_small, ";
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
            color: ", theme$text_light, ";
            font-size: ", theme$font_size_small, ";
            font-style: italic;
        }

        .instructions-header {
            color: ", theme$text_primary, ";
            font-weight: bold;
            font-size: ", theme$font_size_h5, ";
        }

        .statistics-header {
            color: ", theme$text_primary, ";
            font-weight: bold;
            font-size: ", theme$font_size_h5, ";
            margin-bottom: ", theme$spacing_sm, ";
        }

        .analysis-stats-container {
            max-height: 320px;
            overflow-y: auto;
            padding-right: ", theme$spacing_sm, ";
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

# Generate JavaScript for progress handling and loading
generate_app_javascript <- function() {
    HTML("
        Shiny.addCustomMessageHandler('updateProgress', function(data) {
            document.getElementById('progress-bar').style.width = data.percent + '%';
            document.getElementById('loading-message').textContent = data.message;
        });

        Shiny.addCustomMessageHandler('hideLoading', function(data) {
            var overlay = document.getElementById('loading-overlay');
            if (overlay) {
                overlay.style.display = 'none';
            }
        });

        Shiny.addCustomMessageHandler('addProgressHandlers', function(data) {
            // Any additional JavaScript setup can go here
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
