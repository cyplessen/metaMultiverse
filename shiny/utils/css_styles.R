# ==============================================================================
# UTILS/CSS_STYLES.R - Cosmic Minimalism Design System
# ==============================================================================

#' Get App CSS Styles
get_app_css <- function() {
  tags$head(
    tags$style(HTML("
    /* ========================================================================
       DESIGN SYSTEM: COSMIC MINIMALISM
       Architecture-inspired, astronomy-themed, data-focused
       ======================================================================== */

    /* === CSS VARIABLES === */
    :root {
      /* Colors: Deep Space Minimalism */
      --space-dark: #0a0e27;
      --space-mid: #1a1f3a;
      --cosmic-accent: #6366f1;
      --cosmic-accent-light: #818cf8;
      --nebula-accent: #8b5cf6;
      --stardust: #e0e7ff;
      --moonlight: #f8fafc;
      --concrete: #64748b;
      --concrete-light: #94a3b8;

      /* Spacing */
      --space-xs: 8px;
      --space-sm: 16px;
      --space-md: 24px;
      --space-lg: 32px;
      --space-xl: 48px;
      --space-2xl: 64px;
      --space-3xl: 96px;

      /* Border radius */
      --radius-sm: 8px;
      --radius-md: 12px;
      --radius-lg: 16px;
      --radius-xl: 24px;
      --radius-full: 100px;

      /* Shadows */
      --shadow-sm: 0 1px 3px rgba(0,0,0,0.05);
      --shadow-md: 0 4px 8px rgba(0,0,0,0.06), 0 0 1px rgba(0,0,0,0.04);
      --shadow-lg: 0 20px 40px rgba(0,0,0,0.08), 0 0 1px rgba(0,0,0,0.04);
      --shadow-xl: 0 30px 60px rgba(0,0,0,0.12), 0 0 1px rgba(0,0,0,0.04);

      /* Typography */
      --font-body: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', system-ui, sans-serif;
      --font-mono: 'SF Mono', 'Monaco', 'Consolas', monospace;
    }

    /* === GLOBAL RESETS === */
    * {
      margin: 0;
      padding: 0;
      box-sizing: border-box;
    }

    html {
      scroll-behavior: smooth;
      font-size: 16px;
    }

    body {
      margin: 0 !important;
      padding: 0 !important;
      font-family: var(--font-body);
      background: var(--moonlight);
      color: var(--space-dark);
      line-height: 1.7;
      -webkit-font-smoothing: antialiased;
      -moz-osx-font-smoothing: grayscale;
    }

    /* Hide Shiny defaults */
    .container-fluid > h1:first-child {
      display: none !important;
    }

    .container-fluid {
      padding: 0 !important;
      margin: 0 !important;
    }

    /* === TYPOGRAPHY === */
    h1, h2, h3, h4, h5, h6 {
      font-weight: 600;
      letter-spacing: -0.03em;
      line-height: 1.2;
      margin-bottom: var(--space-md);
    }

    h1 {
      font-size: clamp(2.5rem, 5vw, 4rem);
      font-weight: 700;
      color: var(--space-dark);
    }

    h2 {
      font-size: clamp(2rem, 4vw, 3rem);
      color: var(--space-mid);
    }

    h3 {
      font-size: clamp(1.5rem, 3vw, 2rem);
      color: var(--space-mid);
    }

    h4 {
      font-size: 1.25rem;
      color: var(--concrete);
      font-weight: 600;
    }

    p {
      color: var(--concrete);
      margin-bottom: var(--space-sm);
    }

    .text-muted {
      color: var(--concrete-light);
      font-size: 0.875rem;
    }

    /* === LAYOUT STRUCTURE === */
    .app-container {
      display: grid;
      grid-template-columns: 280px 1fr;
      min-height: 100vh;
      background: var(--moonlight);
    }

    /* Sidebar Navigation */
    .app-sidebar {
      background: var(--space-dark);
      border-right: 1px solid rgba(255,255,255,0.05);
      padding: var(--space-xl) var(--space-md);
      position: fixed;
      width: 280px;
      height: 100vh;
      overflow-y: auto;
      z-index: 100;
    }

    .app-logo {
      color: white;
      font-size: 1.5rem;
      font-weight: 700;
      margin-bottom: var(--space-2xl);
      letter-spacing: -0.02em;
    }

    .app-logo span {
      background: linear-gradient(135deg, #e0e7ff, #c7d2fe);
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
    }

    /* Main Content Area */
    .app-main {
      margin-left: 280px;
      padding: var(--space-3xl) var(--space-2xl);
      max-width: 1400px;
      animation: fadeIn 0.4s cubic-bezier(0.4, 0, 0.2, 1);
    }

    @keyframes fadeIn {
      from { opacity: 0; transform: translateY(10px); }
      to { opacity: 1; transform: translateY(0); }
    }

    /* === PROGRESS INDICATOR === */
    .progress-timeline {
      margin-bottom: var(--space-3xl);
    }

    .timeline-step {
      display: flex;
      align-items: flex-start;
      margin-bottom: var(--space-lg);
      position: relative;
    }

    .timeline-step::before {
      content: '';
      position: absolute;
      left: 15px;
      top: 40px;
      bottom: -32px;
      width: 2px;
      background: rgba(255,255,255,0.1);
    }

    .timeline-step:last-child::before {
      display: none;
    }

    .step-indicator {
      width: 32px;
      height: 32px;
      border-radius: 50%;
      background: rgba(255,255,255,0.1);
      border: 2px solid rgba(255,255,255,0.2);
      display: flex;
      align-items: center;
      justify-content: center;
      margin-right: var(--space-md);
      flex-shrink: 0;
      transition: all 0.3s ease;
      font-size: 0.875rem;
      color: rgba(255,255,255,0.5);
      font-weight: 600;
    }

    .step-indicator.active {
      background: var(--cosmic-accent);
      border-color: var(--cosmic-accent);
      color: white;
      box-shadow: 0 0 20px rgba(99,102,241,0.4);
    }

    .step-indicator.completed {
      background: rgba(99,102,241,0.2);
      border-color: var(--cosmic-accent);
      color: var(--cosmic-accent);
    }

    .step-content {
      flex: 1;
      padding-top: 4px;
    }

    .step-title {
      color: rgba(255,255,255,0.6);
      font-size: 0.875rem;
      font-weight: 500;
      text-transform: uppercase;
      letter-spacing: 0.05em;
      transition: color 0.3s ease;
    }

    .timeline-step.active .step-title {
      color: white;
      font-weight: 600;
    }

    .step-description {
      color: rgba(255,255,255,0.4);
      font-size: 0.75rem;
      margin-top: 4px;
      line-height: 1.4;
    }

    /* === CARDS === */
    .card {
      background: white;
      border: 1px solid rgba(0,0,0,0.06);
      border-radius: var(--radius-lg);
      padding: var(--space-lg);
      margin-bottom: var(--space-md);
      box-shadow: var(--shadow-sm);
      transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
    }

    .card:hover {
      box-shadow: var(--shadow-md);
      transform: translateY(-2px);
    }

    .card-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      margin-bottom: var(--space-md);
      padding-bottom: var(--space-md);
      border-bottom: 1px solid rgba(0,0,0,0.06);
    }

    .card-title {
      font-size: 1.125rem;
      font-weight: 600;
      color: var(--space-dark);
      margin: 0;
    }

    .card-body {
      color: var(--concrete);
    }

    /* === BUTTONS === */
    .btn {
      display: inline-flex;
      align-items: center;
      justify-content: center;
      padding: 12px 24px;
      border-radius: var(--radius-full);
      font-weight: 500;
      font-size: 0.9375rem;
      letter-spacing: 0.01em;
      border: none;
      cursor: pointer;
      transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
      text-decoration: none;
      white-space: nowrap;
    }

    .btn-primary {
      background: var(--cosmic-accent);
      color: white;
      box-shadow: 0 2px 8px rgba(99,102,241,0.2);
    }

    .btn-primary:hover {
      background: var(--cosmic-accent-light);
      transform: translateY(-2px);
      box-shadow: 0 6px 16px rgba(99,102,241,0.3);
    }

    .btn-secondary {
      background: transparent;
      color: var(--cosmic-accent);
      border: 1.5px solid var(--cosmic-accent);
    }

    .btn-secondary:hover {
      background: rgba(99,102,241,0.05);
      border-color: var(--cosmic-accent-light);
    }

    .btn-ghost {
      background: transparent;
      color: var(--concrete);
      border: 1px solid rgba(0,0,0,0.1);
    }

    .btn-ghost:hover {
      background: rgba(0,0,0,0.02);
      border-color: rgba(0,0,0,0.2);
    }

    .btn:disabled {
      opacity: 0.5;
      cursor: not-allowed;
      transform: none !important;
    }

    /* === FORMS === */
    .form-group {
      margin-bottom: var(--space-md);
    }

    .form-label {
      display: block;
      font-size: 0.875rem;
      font-weight: 500;
      color: var(--space-dark);
      margin-bottom: var(--space-xs);
      text-transform: uppercase;
      letter-spacing: 0.05em;
    }

    .form-control {
      width: 100%;
      padding: 12px 16px;
      border: 1.5px solid rgba(0,0,0,0.1);
      border-radius: var(--radius-sm);
      font-family: var(--font-body);
      font-size: 0.9375rem;
      color: var(--space-dark);
      background: white;
      transition: all 0.2s ease;
    }

    .form-control:focus {
      outline: none;
      border-color: var(--cosmic-accent);
      box-shadow: 0 0 0 3px rgba(99,102,241,0.1);
    }

    .form-control::placeholder {
      color: var(--concrete-light);
    }

    /* Checkbox/Radio styling */
    input[type='checkbox'],
    input[type='radio'] {
      width: 18px;
      height: 18px;
      margin-right: var(--space-xs);
      accent-color: var(--cosmic-accent);
    }

    /* === LANDING PAGE === */
    .landing-wrapper {
      position: relative;
      width: 100vw;
      height: 100vh;
      margin: 0;
      padding: 0;
      overflow: hidden;
      background: radial-gradient(ellipse at top, #1e293b 0%, #0f172a 100%);
    }

    /* Starfield background */
    .landing-wrapper::before {
      content: '';
      position: absolute;
      width: 100%;
      height: 100%;
      background-image:
        radial-gradient(2px 2px at 20px 30px, rgba(255,255,255,0.3), transparent),
        radial-gradient(2px 2px at 60px 70px, rgba(255,255,255,0.2), transparent),
        radial-gradient(1px 1px at 50px 50px, rgba(255,255,255,0.4), transparent),
        radial-gradient(1px 1px at 130px 80px, rgba(255,255,255,0.2), transparent),
        radial-gradient(2px 2px at 90px 10px, rgba(255,255,255,0.3), transparent);
      background-size: 200px 200px;
      background-position: 0 0, 40px 60px, 130px 270px, 70px 100px, 150px 50px;
      animation: starfield 120s linear infinite;
      opacity: 0.6;
    }

    @keyframes starfield {
      from { transform: translateY(0); }
      to { transform: translateY(-200px); }
    }

    .landing-content {
      position: relative;
      z-index: 2;
      display: flex;
      align-items: center;
      justify-content: center;
      height: 100%;
      padding: var(--space-2xl);
    }

    .hero-content {
      text-align: center;
      max-width: 900px;
    }

    .hero-title {
      font-size: clamp(3rem, 8vw, 6rem);
      font-weight: 700;
      margin-bottom: var(--space-lg);
      letter-spacing: -0.04em;
      line-height: 1.1;
      background: linear-gradient(135deg,
        #e0e7ff 0%,
        #c7d2fe 50%,
        #a5b4fc 100%);
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
      background-clip: text;
    }

    .hero-subtitle {
      font-size: clamp(1.125rem, 2vw, 1.5rem);
      color: rgba(255,255,255,0.8);
      margin-bottom: var(--space-xl);
      line-height: 1.6;
      font-weight: 400;
    }

    .hero-cta {
      background: transparent;
      border: 1.5px solid rgba(255,255,255,0.2);
      color: white;
      padding: 16px 40px;
      border-radius: var(--radius-full);
      font-weight: 500;
      font-size: 1.0625rem;
      letter-spacing: 0.02em;
      cursor: pointer;
      transition: all 0.3s ease;
      display: inline-block;
    }

    .hero-cta:hover {
      background: rgba(255,255,255,0.1);
      border-color: rgba(255,255,255,0.4);
      backdrop-filter: blur(20px);
      transform: translateY(-2px);
      box-shadow: 0 10px 30px rgba(0,0,0,0.2);
    }

    /* === UPLOAD ZONE === */
    .upload-zone {
      border: 2px dashed rgba(99,102,241,0.3);
      border-radius: var(--radius-xl);
      padding: var(--space-3xl) var(--space-xl);
      background: linear-gradient(135deg,
        rgba(99,102,241,0.02) 0%,
        rgba(139,92,246,0.02) 100%);
      text-align: center;
      transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
      cursor: pointer;
    }

    .upload-zone:hover {
      border-color: rgba(99,102,241,0.6);
      background: rgba(99,102,241,0.04);
      transform: scale(1.01);
    }

    .upload-icon {
      font-size: 3rem;
      color: var(--cosmic-accent);
      margin-bottom: var(--space-md);
      opacity: 0.8;
    }

    .upload-text {
      color: var(--concrete);
      font-size: 1.125rem;
      margin-bottom: var(--space-xs);
    }

    .upload-subtext {
      color: var(--concrete-light);
      font-size: 0.875rem;
    }

    /* === STAT CARDS === */
    .stat-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
      gap: var(--space-md);
      margin-bottom: var(--space-xl);
    }

    .stat-card {
      background: linear-gradient(135deg,
        rgba(99,102,241,0.05) 0%,
        rgba(139,92,246,0.05) 100%);
      border: 1px solid rgba(99,102,241,0.1);
      border-radius: var(--radius-lg);
      padding: var(--space-lg) var(--space-md);
      text-align: center;
      transition: all 0.2s ease;
    }

    .stat-card:hover {
      border-color: rgba(99,102,241,0.3);
      transform: translateY(-4px);
    }

    .stat-value {
      font-size: 2.5rem;
      font-weight: 700;
      background: linear-gradient(135deg, #6366f1, #8b5cf6);
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
      background-clip: text;
      font-variant-numeric: tabular-nums;
      line-height: 1.2;
      margin-bottom: var(--space-xs);
    }

    .stat-label {
      font-size: 0.8125rem;
      text-transform: uppercase;
      letter-spacing: 0.1em;
      color: var(--concrete);
      font-weight: 500;
    }

    /* === TABLES === */
    .table-wrapper {
      background: white;
      border: 1px solid rgba(0,0,0,0.06);
      border-radius: var(--radius-lg);
      overflow: hidden;
      margin-bottom: var(--space-md);
    }

    table {
      width: 100%;
      border-collapse: collapse;
    }

    thead {
      background: var(--stardust);
    }

    th {
      padding: var(--space-md);
      text-align: left;
      font-size: 0.8125rem;
      font-weight: 600;
      text-transform: uppercase;
      letter-spacing: 0.05em;
      color: var(--space-dark);
      border-bottom: 1px solid rgba(0,0,0,0.06);
    }

    td {
      padding: var(--space-md);
      border-bottom: 1px solid rgba(0,0,0,0.04);
      color: var(--concrete);
      font-variant-numeric: tabular-nums;
    }

    tr:hover {
      background: rgba(99,102,241,0.02);
    }

    /* DataTables specific */
    .dataTables_wrapper {
      padding: 0 !important;
    }

    .dataTables_wrapper .dataTables_length,
    .dataTables_wrapper .dataTables_filter,
    .dataTables_wrapper .dataTables_info,
    .dataTables_wrapper .dataTables_paginate {
      color: var(--concrete) !important;
      padding: var(--space-sm) !important;
    }

    /* Warnings/verbatim output */
    pre {
      background: var(--moonlight);
      border: 1px solid rgba(0,0,0,0.06);
      border-radius: var(--radius-sm);
      padding: var(--space-md);
      color: var(--concrete);
      font-family: var(--font-mono);
      font-size: 0.875rem;
      line-height: 1.6;
      overflow-x: auto;
      white-space: pre-wrap;
      word-wrap: break-word;
    }

    /* Shiny tabset styling to match cosmic design */
    .nav-pills {
      border-bottom: 2px solid rgba(0,0,0,0.06);
      margin-bottom: var(--space-xl);
      padding-bottom: 0;
    }

    .nav-pills .nav-link {
      background: none !important;
      border: none !important;
      border-radius: 8px 8px 0 0 !important;
      color: var(--concrete) !important;
      padding: var(--space-sm) var(--space-lg) !important;
      margin-bottom: -2px;
      border-bottom: 3px solid transparent !important;
      transition: all 0.2s ease;
    }

    .nav-pills .nav-link:hover {
      color: var(--cosmic-accent) !important;
    }

    .nav-pills .nav-link.active {
      background: none !important;
      color: var(--cosmic-accent) !important;
      border-bottom: 3px solid var(--cosmic-accent) !important;
    }

    .tab-content {
      border: none !important;
    }

    /* === ALERTS === */
    .alert {
      padding: var(--space-md);
      border-radius: var(--radius-md);
      margin-bottom: var(--space-md);
      border-left: 4px solid;
    }

    .alert-info {
      background: rgba(99,102,241,0.05);
      border-color: var(--cosmic-accent);
      color: var(--space-dark);
    }

    .alert-warning {
      background: rgba(251,191,36,0.05);
      border-color: #fbbf24;
      color: #92400e;
    }

    .alert-success {
      background: rgba(34,197,94,0.05);
      border-color: #22c55e;
      color: #14532d;
    }

    /* === LOADING STATES === */
    .loading-overlay {
      display: flex;
      align-items: center;
      justify-content: center;
      padding: var(--space-3xl);
    }

    .loading-spinner {
      width: 40px;
      height: 40px;
      border: 3px solid rgba(99,102,241,0.1);
      border-top-color: var(--cosmic-accent);
      border-radius: 50%;
      animation: spin 0.8s linear infinite;
    }

    @keyframes spin {
      to { transform: rotate(360deg); }
    }

    /* === RESPONSIVE === */
    @media (max-width: 1024px) {
      .app-container {
        grid-template-columns: 1fr;
      }

      .app-sidebar {
        position: relative;
        width: 100%;
        height: auto;
      }

      .app-main {
        margin-left: 0;
        padding: var(--space-xl) var(--space-md);
      }
    }

    @media (max-width: 768px) {
      :root {
        font-size: 14px;
      }

      .stat-grid {
        grid-template-columns: repeat(2, 1fr);
      }

      .app-main {
        padding: var(--space-lg) var(--space-md);
      }
    }

    /* === UTILITIES === */
    .mb-0 { margin-bottom: 0 !important; }
    .mb-sm { margin-bottom: var(--space-sm) !important; }
    .mb-md { margin-bottom: var(--space-md) !important; }
    .mb-lg { margin-bottom: var(--space-lg) !important; }
    .mb-xl { margin-bottom: var(--space-xl) !important; }

    .mt-0 { margin-top: 0 !important; }
    .mt-sm { margin-top: var(--space-sm) !important; }
    .mt-md { margin-top: var(--space-md) !important; }
    .mt-lg { margin-top: var(--space-lg) !important; }
    .mt-xl { margin-top: var(--space-xl) !important; }

    .text-center { text-align: center !important; }
    .text-right { text-align: right !important; }

    .d-flex { display: flex !important; }
    .align-center { align-items: center !important; }
    .justify-between { justify-content: space-between !important; }
    .justify-center { justify-content: center !important; }
    .gap-sm { gap: var(--space-sm) !important; }
    .gap-md { gap: var(--space-md) !important; }
    "))
  )
}
