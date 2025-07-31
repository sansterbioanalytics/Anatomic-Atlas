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
        
        /* Smooth transitions for better UX */
        * {
            transition: background-color 0.2s ease, border-color 0.2s ease, color 0.2s ease;
        }
        
        /* Remove default focus outline and add custom styling */
        *:focus {
            outline: none !important;
        }

        /* === MODERN HEADER STYLING === */
        .main-header {
            background: linear-gradient(135deg, ", theme$background_white, " 0%, #f8f9fa 100%) !important;
            border-bottom: 3px solid ", theme$primary_color, " !important;
            box-shadow: ", theme$shadow_medium, " !important;
        }

        .main-header .navbar {
            background: transparent !important;
        }

        .main-header .logo {
            background: transparent !important;
            color: ", theme$text_primary, " !important;
            font-family: ", theme$font_family_primary, ";
            font-weight: bold;
            font-size: ", theme$font_size_h3, ";
            border-right: none;
        }

        .main-header .logo:hover {
            background: rgba(220, 20, 60, 0.05) !important;
        }

        /* === ENHANCED HEADER LAYOUT === */
        .main-header {
            height: 60px !important;
            min-height: 60px !important;
            max-height: 60px !important;
            position: relative !important;
        }
        
        .main-header .navbar {
            height: 60px !important;
            min-height: 60px !important;
        }
        
        /* Completely remove sidebar toggle */
        .main-header .sidebar-toggle {
            display: none !important;
            width: 0 !important;
            padding: 0 !important;
            margin: 0 !important;
        }
        
        /* Style the logo area properly */
        .main-header .logo {
            background: transparent !important;
            border: none !important;
            padding: 0 15px !important;
            height: 60px !important;
            line-height: 60px !important;
            display: flex !important;
            align-items: center !important;
            font-family: ", theme$font_family_primary, " !important;
            font-weight: 700 !important;
            font-size: 22px !important;
            color: ", theme$primary_color, " !important;
            text-decoration: none !important;
            letter-spacing: -0.5px !important;
        }
        
        .main-header .logo:hover {
            background: rgba(220, 20, 60, 0.05) !important;
            color: ", theme$secondary_color, " !important;
        }
        
        /* Fix navbar content positioning */
        .main-header .navbar-custom-menu {
            height: 60px !important;
            margin: 0 !important;
            padding: 0 !important;
        }
        
        .main-header .navbar-nav {
            margin: 0 !important;
            padding: 0 !important;
            height: 60px !important;
        }
        
        .main-header .navbar-nav > li {
            height: 60px !important;
            display: flex !important;
            align-items: center !important;
        }
        
        /* === HEADER CENTER CONTENT STYLING === */
        .header-center-content {
            display: flex !important;
            align-items: center !important;
            gap: 30px !important;
            height: 60px !important;
            margin-left: 20px !important;
        }
        
        .header-title {
            font-family: ", theme$font_family_primary, ";
            background: linear-gradient(135deg, ", theme$primary_color, ", ", theme$secondary_color, ");
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            background-clip: text;
            text-shadow: none !important;
            margin: 0 !important;
            line-height: 1.2 !important;
            font-weight: 700 !important;
            font-size: 24px !important;
            letter-spacing: -0.8px !important;
        }
        
        .header-title {
            font-family: ", theme$font_family_primary, ";
            text-shadow: 0 1px 2px rgba(0,0,0,0.1);
            margin: 0 !important;
            line-height: 1.2 !important;
        }
        
        /* === MODERN MODE SELECTOR STYLING === */
        .compact-mode-selector {
            z-index: 1000;
            background: rgba(255, 255, 255, 0.8);
            backdrop-filter: blur(10px);
            border-radius: 12px;
            padding: 4px;
            border: 1px solid rgba(220, 20, 60, 0.1);
            box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
        }
        
        .compact-mode-selector .shiny-input-radiogroup {
            margin: 0;
        }
        
        .compact-mode-selector .radio {
            margin: 0 2px 0 0;
            display: inline-block;
        }
        
        .compact-mode-selector .radio:last-child {
            margin-right: 0;
        }
        
        .compact-mode-selector .radio input[type=\"radio\"] {
            display: none; /* Hide actual radio buttons */
        }
        
        .compact-mode-selector .radio label {
            display: inline-flex;
            align-items: center;
            justify-content: center;
            padding: 10px 18px;
            margin: 0;
            border-radius: 8px;
            font-size: 13px;
            font-weight: 600;
            cursor: pointer;
            transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
            border: none;
            background: transparent;
            color: ", theme$text_secondary, ";
            min-height: 40px;
            min-width: 120px;
            box-sizing: border-box;
            position: relative;
            overflow: hidden;
        }
        
        .compact-mode-selector .radio label::before {
            content: '';
            position: absolute;
            top: 0;
            left: -100%;
            width: 100%;
            height: 100%;
            background: linear-gradient(135deg, ", theme$primary_color, ", ", theme$secondary_color, ");
            transition: left 0.3s cubic-bezier(0.4, 0, 0.2, 1);
            z-index: -1;
        }
        
        .compact-mode-selector .radio label:hover {
            color: ", theme$primary_color, ";
            transform: translateY(-1px);
        }
        
        .compact-mode-selector .radio label:hover::before {
            left: 0;
            opacity: 0.1;
        }
        
        .compact-mode-selector .radio input[type=\"radio\"]:checked + span {
            background: linear-gradient(135deg, ", theme$primary_color, ", ", theme$secondary_color, ");
            color: white !important;
            box-shadow: 0 4px 12px rgba(220, 20, 60, 0.4) !important;
            transform: translateY(-2px) !important;
            font-weight: 700 !important;
            border: 2px solid rgba(255, 255, 255, 0.3) !important;
        }
        
        .compact-mode-selector .radio input[type=\"radio\"]:checked + span::before {
            left: 0;
            opacity: 1;
        }
        
        /* Add icon content */
        .compact-mode-selector .radio:first-child label::after {
            content: '🎯';
            margin-left: 8px;
            font-size: 14px;
        }
        
        .compact-mode-selector .radio:nth-child(2) label::after {
            content: '🔬';
            margin-left: 8px;
            font-size: 14px;
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
        
        .mode-selector-container input[type=\"radio\"]:checked + span {
            color: ", theme$primary_color, " !important;
            font-weight: bold;
        }

        /* CELL TYPE TOGGLES */
        .cell-type-toggle-container {
            display: grid;
            grid-template-columns: 1fr 1fr 1fr;
            gap: 4px;
            margin-bottom: 6px;
            max-width: 100%;
            overflow: hidden;
        }
        
        .cell-type-toggle {
            display: block;
            margin: 0;
            min-width: 0;
        }
        
        .cell-type-toggle-btn {
            padding: 4px 6px;
            border: 1px solid #6c757d;
            border-radius: 4px;
            background-color: #6c757d;
            color: white;
            font-size: 10px;
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
            min-height: 28px;
            display: flex;
            align-items: center;
            justify-content: center;
        }
        
        .cell-type-toggle-btn:hover {
            background-color: ", theme$background_white, ";
            border-color: ", theme$primary_color, ";
            color: ", theme$primary_color, ";
            transform: translateY(-1px);
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        
        .cell-type-toggle-btn.active {
            background-color: ", theme$primary_color, ";
            border-color: ", theme$primary_color, ";
            color: white;
            font-weight: bold;
            box-shadow: 0 1px 4px rgba(220, 20, 60, 0.3);
        }
        
        .cell-type-toggle-btn.inactive {
            background-color: #6c757d;
            border-color: #6c757d;
            color: #ffffff;
        }
        
        .cell-type-toggle-section {
            margin-bottom: 6px;
            padding: 4px 6px;
            border-radius: 4px;
            background-color: rgba(255,255,255,0.05);
            border: 1px solid rgba(255,255,255,0.1);
        }
        
        .cell-type-section-header {
            color: ", theme$text_white, ";
            font-size: 11px;
            font-weight: bold;
            margin-bottom: 4px;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }

        /* SIDEBAR */
        .main-sidebar {
            background-color: ", theme$background_dark, " !important;
            width: 350px !important;
            max-width: 350px !important;
            overflow-x: hidden;
            overflow-y: auto;
        }

        .sidebar {
            background-color: ", theme$background_dark, " !important;
            color: ", theme$text_white, ";
            padding-right: 15px;
            box-sizing: border-box;
            width: 100%;
        }

        .sidebar-menu > li > a {
            color: ", theme$text_white, " !important;
            border-left: 3px solid transparent;
        }

        .sidebar-menu > li > a:hover {
            background-color: ", theme$primary_color, " !important;
            border-left-color: ", theme$secondary_color, ";
        }

        .sidebar .form-group {
            margin-bottom: ", theme$spacing_sm, ";
            max-width: 100%;
            overflow: hidden;
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
            min-height: 38px !important;
        }
        
        .sidebar .selectize-input.focus {
            border-color: ", theme$primary_color, " !important;
            box-shadow: 0 0 0 0.2rem rgba(220, 20, 60, 0.25) !important;
        }
        
        /* SELECTIZE CONTROLS */
        .selectize-input {
            background: ", theme$background_white, " !important;
            border: 2px solid ", theme$border_medium, " !important;
            border-radius: ", theme$radius_md, " !important;
            color: ", theme$text_primary, " !important;
            font-size: ", theme$font_size_body, " !important;
            line-height: 1.5 !important;
            padding: 8px 12px !important;
            transition: all 0.2s ease !important;
        }
        
        .selectize-input input {
            color: ", theme$text_primary, " !important;
        }
        
        .selectize-input.dropdown-active {
            border-color: ", theme$primary_color, " !important;
        }
        
        .selectize-input:focus,
        .selectize-input.focus {
            border-color: ", theme$primary_color, " !important;
            box-shadow: 0 0 0 3px rgba(220, 20, 60, 0.1) !important;
            outline: none !important;
        }
        
        .selectize-input.has-items {
            padding: 4px 12px !important;
        }
        
        .selectize-control.multi .selectize-input > div {
            background: ", theme$primary_color, " !important;
            color: white !important;
            border: none !important;
            border-radius: ", theme$radius_sm, " !important;
            padding: 4px 8px !important;
            margin: 2px 4px 2px 0 !important;
            font-size: ", theme$font_size_small, " !important;
            font-weight: 500 !important;
        }
        
        .selectize-control.multi .selectize-input > div .remove {
            color: rgba(255, 255, 255, 0.8) !important;
            border-left: 1px solid rgba(255, 255, 255, 0.3) !important;
            margin-left: 6px !important;
            padding-left: 6px !important;
        }
        
        .selectize-control.multi .selectize-input > div .remove:hover {
            color: white !important;
            background: rgba(0, 0, 0, 0.1) !important;
        }
        
        .selectize-dropdown {
            background: ", theme$background_white, " !important;
            border: 1px solid ", theme$border_medium, " !important;
            border-radius: ", theme$radius_md, " !important;
            box-shadow: ", theme$shadow_medium, " !important;
            z-index: 9999 !important;
            position: absolute !important;
            visibility: visible !important;
            opacity: 1 !important;
        }
        
        .selectize-dropdown-content {
            max-height: 200px !important;
            overflow-y: auto !important;
        }
        
        .selectize-dropdown .option {
            padding: 8px 12px !important;
            font-size: ", theme$font_size_body, " !important;
            color: ", theme$text_primary, " !important;
            background: ", theme$background_white, " !important;
            border-bottom: 1px solid ", theme$border_light, " !important;
            transition: background-color 0.15s ease !important;
            cursor: pointer !important;
        }
        
        .selectize-dropdown .option:last-child {
            border-bottom: none !important;
        }
        
        .selectize-dropdown .option:hover,
        .selectize-dropdown .option.active {
            background: ", theme$background_light, " !important;
            color: ", theme$text_primary, " !important;
        }
        
        .selectize-dropdown .option.selected {
            background: rgba(220, 20, 60, 0.1) !important;
            color: ", theme$primary_color, " !important;
            font-weight: 500 !important;
        }
        
        .selectize-dropdown .no-results {
            padding: 12px !important;
            color: ", theme$text_light, " !important;
            font-style: italic !important;
            text-align: center !important;
        }
        
        .sidebar .radio {
            margin-bottom: ", theme$spacing_sm, " !important;
        }
        
        /* === ENHANCED FILE UPLOAD STYLING === */
        .sidebar .form-group input[type=\"file\"] {
            border: 2px dashed ", theme$border_medium, " !important;
            border-radius: ", theme$radius_md, " !important;
            background-color: ", theme$background_white, " !important;
            padding: 12px !important;
            width: 100% !important;
            box-sizing: border-box !important;
            font-size: ", theme$font_size_small, " !important;
            color: ", theme$text_secondary, " !important;
            transition: all 0.2s ease !important;
        }
        
        .sidebar .form-group input[type=\"file\"]:hover {
            border-color: ", theme$primary_color, " !important;
            background-color: rgba(220, 20, 60, 0.05) !important;
        }
        
        .sidebar .form-group input[type=\"file\"]:focus {
            border-color: ", theme$primary_color, " !important;
            box-shadow: 0 0 0 2px rgba(220, 20, 60, 0.2) !important;
            outline: none !important;
        }
        
        /* === IMPROVED TEXTAREA STYLING === */
        .sidebar textarea.form-control {
            resize: vertical !important;
            min-height: 120px !important;
            font-family: ", theme$font_family_mono, " !important;
            font-size: ", theme$font_size_small, " !important;
            line-height: 1.4 !important;
            background-color: ", theme$background_white, " !important;
            border: 2px solid ", theme$border_medium, " !important;
            border-radius: ", theme$radius_md, " !important;
            transition: all 0.2s ease !important;
        }
        
        .sidebar textarea.form-control:focus {
            border-color: ", theme$primary_color, " !important;
            box-shadow: 0 0 0 3px rgba(220, 20, 60, 0.1) !important;
            outline: none !important;
        }
        
        .sidebar textarea.form-control:hover {
            border-color: rgba(220, 20, 60, 0.6) !important;
        }
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
        
        /* SECTION DIVIDERS */
        .gene-selection-container,
        .product-grouping-section,
        .contrast-selection-container {
            border-bottom: 1px solid rgba(255,255,255,0.1);
            padding-bottom: ", theme$spacing_sm, ";
            margin-bottom: ", theme$spacing_sm, ";
            max-width: 100%;
            overflow: hidden;
            box-sizing: border-box;
            position: relative;
        }
        
        .gene-selection-container:last-child,
        .product-grouping-section:last-child,
        .contrast-selection-container:last-child {
            border-bottom: none;
        }
        
        .product-grouping-container {
            max-width: 100%;
            overflow: hidden;
            box-sizing: border-box;
        }
        
        /* Add subtle background for better section separation */
        .gene-selection-container {
            background-color: rgba(220, 20, 60, 0.05);
            border-radius: ", theme$radius_md, ";
            border: 1px solid rgba(220, 20, 60, 0.1);
            padding: ", theme$spacing_lg, ";
        }
        
        .contrast-selection-container {
            background-color: rgba(23, 162, 184, 0.05);
            border-radius: ", theme$radius_md, ";
            border: 1px solid rgba(23, 162, 184, 0.1);
            padding: ", theme$spacing_lg, ";
        }
        
        /* === IMPROVED FORM SPACING === */
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
            word-wrap: break-word;
            overflow-wrap: break-word;
        }
        
        /* === SIDEBAR ACTION BUTTONS === */
        .sidebar .btn {
            border-radius: ", theme$radius_sm, " !important;
            font-weight: 500 !important;
            transition: background-color 0.15s ease !important;
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
        
        /* === FILE UPLOAD STYLING === */
        .sidebar .form-group .shiny-input-container input[type='file'] {
            background-color: ", theme$background_white, " !important;
            border: 2px dashed ", theme$border_medium, " !important;
            border-radius: ", theme$radius_md, " !important;
            padding: 16px !important;
            width: 100% !important;
            box-sizing: border-box !important;
            transition: border-color 0.2s ease !important;
            font-size: ", theme$font_size_small, " !important;
        }
        
        .sidebar .form-group .shiny-input-container input[type='file']:hover {
            border-color: ", theme$primary_color, " !important;
            background-color: rgba(220, 20, 60, 0.02) !important;
        }
        
        .sidebar .form-group .shiny-input-container input[type='file']:focus {
            border-color: ", theme$primary_color, " !important;
            box-shadow: 0 0 0 0.2rem rgba(220, 20, 60, 0.25) !important;
            outline: none !important;
        }

        /* === CONTENT AREA === */
        .content-wrapper {
            margin-left: 350px !important;
            background-color: ", theme$background_white, " !important;
        }
        
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

        /* === ENHANCED LOGO STYLING === */
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
            transition: all 0.3s ease;
            filter: drop-shadow(0 2px 4px rgba(0,0,0,0.1));
        }

        .logo-primary:hover {
            transform: scale(1.05);
            filter: drop-shadow(0 4px 8px rgba(0,0,0,0.15));
        }

        .logo-link {
            text-decoration: none;
            display: flex;
            align-items: center;
            gap: ", theme$spacing_md, ";
            transition: all 0.2s ease;
        }

        .logo-link:hover {
            text-decoration: none;
        }

        /* === DATA TABLE STYLING === */
        .dataTables_wrapper {
            font-size: ", theme$font_size_small, ";
            overflow-x: auto;
            overflow-y: hidden;  /* Prevent vertical overflow from wrapper */
        }
        
        .dataTables_wrapper .dataTable {
            width: 100% !important;
            border-collapse: collapse;
            table-layout: fixed;  /* Ensure fixed table layout for better control */
        }
        
        .dataTables_wrapper .dataTable th,
        .dataTables_wrapper .dataTable td {
            padding: 8px 12px !important;
            word-wrap: break-word;
            max-width: 200px;
            overflow: hidden;
            text-overflow: ellipsis;
            font-size: ", theme$font_size_small, " !important;
            white-space: nowrap;  /* Prevent text wrapping to control height */
        }
        
        /* Table container specific fixes */
        .dataTables_wrapper .dataTables_scrollBody {
            overflow-x: auto !important;
            overflow-y: auto !important;
            max-height: 400px !important;  /* Ensure scroll body doesn't exceed container */
        }
        
        .dataTables_wrapper .dataTable.nowrap th,
        .dataTables_wrapper .dataTable.nowrap td {
            white-space: nowrap !important;
        }
        
        .dataTables_wrapper .dataTable th {
            background-color: ", theme$background_light, " !important;
            font-weight: bold !important;
            border-bottom: 2px solid ", theme$border_medium, " !important;
        }

        /* === TAB STYLING === */  
        .nav-tabs {
            border-bottom: 1px solid ", theme$border_medium, " !important;
        }
        
        .nav-tabs > li > a {
            padding: 10px 20px !important; /* Increased left/right padding */
            margin-right: 2px !important;
            border-radius: ", theme$radius_sm, " ", theme$radius_sm, " 0 0 !important;
            color: ", theme$text_secondary, " !important;
            background-color: ", theme$background_light, " !important;
            border: 1px solid ", theme$border_light, " !important;
            transition: all 0.2s ease !important;
        }
        
        .nav-tabs > li > a:hover {
            background-color: ", theme$background_white, " !important;
            color: ", theme$primary_color, " !important;
            border-color: ", theme$border_medium, " !important;
        }
        
        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:hover,
        .nav-tabs > li.active > a:focus {
            background-color: ", theme$primary_color, " !important;
            color: ", theme$text_white, " !important;
            border-color: ", theme$primary_color, " !important;
            font-weight: bold !important;
            padding: 10px 20px !important; /* Maintain increased padding for active tabs */
        }
        
        .tab-content {
            background-color: ", theme$background_white, " !important;
            border: 1px solid ", theme$border_light, " !important;
            border-top: none !important;
            padding: 15px !important;
        }

        .table-striped > tbody > tr:nth-of-type(odd) {
            background-color: ", theme$background_light, ";
        }
        
        .table-striped > tbody > tr:nth-of-type(even) {
            background-color: ", theme$background_white, ";
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
            
            .custom-header-container {
                padding: 0 10px !important;
                height: 50px !important;
            }
            
            .header-center-section {
                margin-left: 15px !important;
                gap: 15px !important;
            }
            
            .header-title {
                font-size: 20px !important;
            }
            
            .compact-mode-selector .radio label {
                padding: 8px 12px !important;
                font-size: 12px !important;
                min-width: 90px !important;
            }
        }
        
        @media (max-width: 480px) {
            .custom-header-container {
                flex-direction: column !important;
                height: auto !important;
                padding: 10px !important;
            }
            
            .header-center-section {
                margin-left: 0 !important;
                gap: 10px !important;
                flex-direction: column !important;
            }
        }
        
        /* === FILE INPUT IMPROVEMENTS === */
        .form-group .shiny-input-container input[type='file'] {
            background-color: rgba(255,255,255,0.9) !important;
            border: 1px solid ", theme$border_medium, " !important;
            border-radius: ", theme$radius_sm, " !important;
            padding: 8px 12px !important;
            font-size: ", theme$font_size_small, " !important;
            color: ", theme$text_primary, " !important;
            transition: all 0.2s ease !important;
        }
        
        .form-group .shiny-input-container input[type='file']:hover {
            border-color: ", theme$primary_color, " !important;
            background-color: rgba(255,255,255,1) !important;
        }
        
        .form-group .shiny-input-container input[type='file']:focus {
            border-color: ", theme$primary_color, " !important;
            box-shadow: 0 0 0 2px rgba(220, 20, 60, 0.1) !important;
        }
        
        /* File upload container styling */
        .form-group .shiny-input-container label {
            color: ", theme$text_white, " !important;
            font-weight: 500 !important;
            margin-bottom: 8px !important;
        }
        
        /* Progress bar for file uploads */
        .progress {
            background-color: rgba(255,255,255,0.1) !important;
            border-radius: ", theme$radius_sm, " !important;
        }
        
        .progress-bar {
            background-color: ", theme$primary_color, " !important;
        }
        
        /* === GENE VALIDATION STYLING === */
        .gene-validation-container {
            background-color: ", theme$background_light, " !important;
            border: 1px solid ", theme$border_medium, " !important;
            border-radius: ", theme$radius_sm, " !important;
            padding: 6px !important;
            margin: 4px 0 !important;
            color: ", theme$text_primary, " !important;
            font-size: ", theme$font_size_small, " !important;
        }
        
        .gene-validation-container .validation-summary {
            font-weight: bold !important;
            color: ", theme$text_primary, " !important;
            margin-bottom: 3px !important;
        }
        
        .gene-validation-container .validation-valid {
            color: ", theme$success_color, " !important;
            font-weight: bold !important;
        }
        
        .gene-validation-container .validation-invalid {
            color: ", theme$error_color, " !important;
            font-weight: bold !important;
        }
        
        .gene-validation-container .validation-suggestions {
            color: ", theme$info_color, " !important;
            font-weight: bold !important;
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
            console.log('Anatomic Atlas loaded successfully');
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
