# ==============================================================================
# UTILS/CSS_STYLES.R
# ==============================================================================

#' Get App CSS Styles
get_app_css <- function() {
  tags$head(
    tags$style(HTML("
    /* Hide Shiny's default title and eliminate all white space */
    .container-fluid > h1:first-child {
      display: none !important;
    }
    .container-fluid {
      padding: 0 !important;
      margin: 0 !important;
    }

    body {
      margin: 0 !important;
      padding: 0 !important;
    }

    /* Landing wrapper and layered approach */
    .landing-wrapper {
      position: relative;
      width: 100vw;
      height: 100vh;
      margin-left: calc(-50vw + 50%);
      overflow: hidden;
    }

    .landing-block {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
    }

    .background-content {
      z-index: 1;
    }

    .background-content img {
      width: 100%;
      height: 100%;
      object-fit: cover;
      object-position: center;
    }

    .foreground-content {
      z-index: 2;
      display: flex;
      align-items: center;
      justify-content: center;
      background: rgba(0, 0, 0, 0.3);
      color: white;
      flex-direction: column;
    }

    /* Hero content styling */
    .hero-content {
      text-align: center;
      max-width: 900px;
      padding: 0 40px;
    }

    .hero-title {
      font-size: clamp(3rem, 8vw, 5rem);
      font-weight: 700;
      margin-bottom: 2rem;
      text-shadow: 2px 2px 20px rgba(0,0,0,0.5);
      color: white;
      letter-spacing: -0.02em;
      line-height: 1.1;
    }

    .hero-subtitle {
      font-size: clamp(1.2rem, 3vw, 1.8rem);
      margin-bottom: 3rem;
      text-shadow: 1px 1px 10px rgba(0,0,0,0.5);
      line-height: 1.4;
      opacity: 0.95;
      font-weight: 300;
      color: white;
    }

    .hero-cta {
      padding: 18px 40px;
      font-size: 1.2rem;
      font-weight: 600;
      background: rgba(255, 255, 255, 0.15);
      color: white;
      border: 2px solid rgba(255, 255, 255, 0.3);
      border-radius: 50px;
      backdrop-filter: blur(10px);
      transition: all 0.3s ease;
      cursor: pointer;
      text-transform: uppercase;
      letter-spacing: 1px;
    }

    .hero-cta:hover {
      background: rgba(255, 255, 255, 0.25);
      border-color: rgba(255, 255, 255, 0.6);
      transform: translateY(-3px);
      box-shadow: 0 15px 35px rgba(0,0,0,0.2);
    }

    /* Content section styling */
    .content-section {
      background: linear-gradient(to bottom, #f8f9fa, #ffffff);
      padding: 100px 0;
      position: relative;
    }

    .content-container {
      max-width: 1200px;
      margin: 0 auto;
      padding: 0 20px;
    }

    .section-title {
      text-align: center;
      font-size: clamp(2.5rem, 5vw, 3.5rem);
      font-weight: 700;
      margin-bottom: 4rem;
      color: #2c3e50;
      position: relative;
    }

    .section-title::after {
      content: '';
      position: absolute;
      bottom: -15px;
      left: 50%;
      transform: translateX(-50%);
      width: 80px;
      height: 4px;
      background: linear-gradient(135deg, #667eea, #764ba2);
      border-radius: 2px;
    }

    /* Feature cards */
    .feature-card {
      background: white;
      padding: 3rem 2rem;
      margin: 20px;
      border-radius: 20px;
      box-shadow: 0 10px 40px rgba(0,0,0,0.1);
      text-align: center;
      transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
      border: 1px solid rgba(255,255,255,0.2);
      position: relative;
      overflow: hidden;
    }

    .feature-card::before {
      content: '';
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
      height: 4px;
      background: linear-gradient(135deg, #667eea, #764ba2);
    }

    .feature-card:hover {
      transform: translateY(-10px);
      box-shadow: 0 20px 60px rgba(0,0,0,0.15);
    }

    .feature-icon {
      font-size: 4rem;
      margin-bottom: 1.5rem;
      background: linear-gradient(135deg, #667eea, #764ba2);
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
      background-clip: text;
    }

    .feature-title {
      font-size: 1.5rem;
      font-weight: 600;
      color: #2c3e50;
      margin-bottom: 1rem;
    }

    .feature-description {
      color: #6c757d;
      line-height: 1.6;
      font-size: 1.1rem;
    }

    /* Navigation menu styles */
    .menu-overlay {
      position: fixed;
      top: 0;
      right: -400px;
      width: 400px;
      height: 100%;
      background: rgba(0, 0, 0, 0.95);
      z-index: 9999;
      backdrop-filter: blur(10px);
      transition: right 0.3s ease;
      box-shadow: -5px 0 15px rgba(0, 0, 0, 0.3);
    }

    .menu-overlay.open {
      right: 0;
    }

    .menu-content {
      padding: 80px 40px 40px 40px;
      color: white;
    }

    .menu-close {
      position: absolute;
      top: 20px;
      right: 20px;
      font-size: 30px;
      color: white;
      cursor: pointer;
      background: none;
      border: none;
    }

    .menu-item {
      display: block;
      color: white;
      font-size: 18px;
      margin: 15px 0;
      padding: 12px 20px;
      border: none;
      background: none;
      cursor: pointer;
      border-radius: 8px;
      width: 100%;
      text-align: left;
    }

    .menu-item:hover {
      color: #667eea;
      background: rgba(255, 255, 255, 0.1);
    }

    .menu-toggle {
      position: fixed;
      top: 20px;
      right: 20px;
      z-index: 1000;
      background: rgba(0, 0, 0, 0.7);
      color: white;
      border: none;
      padding: 12px 16px;
      border-radius: 8px;
      cursor: pointer;
    }

    .menu-backdrop {
      position: fixed;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      background: rgba(0, 0, 0, 0.5);
      z-index: 9998;
      display: none;
    }

    @keyframes progress-animation {
      0% { width: 30%; }
      50% { width: 70%; }
      100% { width: 30%; }
    }

    /* Responsive adjustments */
    @media (max-width: 768px) {
      .hero-content {
        padding: 0 20px;
      }

      .feature-card {
        margin: 10px 5px;
        padding: 2rem 1.5rem;
      }

      .content-section {
        padding: 60px 0;
      }
    }

    /* Smooth scrolling */
    html {
      scroll-behavior: smooth;
    }
    "))
  )
}
