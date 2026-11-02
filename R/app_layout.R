# ============================================================================
# App layout: theme, page headers, Back/Next links and in-app navigation
# ============================================================================
# The app is a bslib page_navbar (Bootstrap 5) with five top-level pages:
#   Home | Bayesian Basics | MCMC Simulations | Prior Lab | Model Lab
# Any element with class "pl-goto" and data-page / data-sub / data-model /
# data-domain attributes navigates there when clicked (see APP_GOTO_JS and the
# observer in server/server_main.R), so Home cards, overview cards and
# Back/Next links all share one mechanism.
# ============================================================================

APP_REPO_URL <- "https://github.com/godwillA33peo/Bayesian-Shiny-App"
APP_LINKEDIN_URL <- "https://www.linkedin.com/in/godwill-zulu/"

APP_FONT <- function() {
  bslib::font_collection(
    bslib::font_link("Source Sans 3",
                     href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@400;600;700&display=swap"),
    "Source Sans Pro", "system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif")
}

app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = "#2a78d6",
    secondary = "#52514e",
    "navbar-bg" = "#1f2328",
    base_font = APP_FONT(),
    heading_font = APP_FONT(),
    "font-size-base" = "1rem",
    "border-radius" = "0.4rem"
  )
}

APP_CSS <- "
body { background: #ffffff; }
/* dark navbar, set directly so it looks the same across bslib versions */
html body .navbar, html body .navbar.navbar-default, html body .navbar.navbar-inverse, html body .navbar.bg-light { background: #1f2328 !important; background-color: #1f2328 !important; border-bottom: 0; }
.navbar .navbar-brand { color: #ffffff !important; font-weight: 700; letter-spacing: 0.01em; }
.navbar .nav-link { color: #d0d4d9 !important; font-size: 1rem; border-radius: 6px; }
.navbar .nav-link:hover, .navbar .nav-link:focus { color: #ffffff !important; }
.navbar .nav-link.active { color: #ffffff !important; background: rgba(255, 255, 255, 0.12); }
.navbar .navbar-toggler, .navbar .navbar-toggle { border-color: rgba(255, 255, 255, 0.45); }
.navbar .navbar-toggler-icon { filter: invert(1); }
.bslib-page-navbar > .navbar + div, .tab-content > .tab-pane { min-height: 60vh; }

/* page header band */
.app-wrap { max-width: 1320px; margin: 0 auto; padding-left: 1.5rem; padding-right: 1.5rem; }
.app-pagehead { background: #f4f7fb; border-bottom: 1px solid #e3e8ef; margin: -1rem calc(-0.5 * var(--bs-gutter-x, 1.5rem)) 1.25rem; padding: 1.6rem 0 1.4rem; }
.app-pagehead .kicker { font-size: 0.8rem; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase; color: #1d63b8; }
.app-pagehead h1 { font-size: 2.1rem; font-weight: 700; margin: 0.15rem 0 0.25rem; }
.app-pagehead p { font-size: 1.07rem; color: #3d434b; margin: 0; max-width: 900px; }
.app-pagenav { display: flex; justify-content: space-between; flex-wrap: wrap; gap: 12px; border-top: 1px solid #eef0f3; margin-top: 2rem; padding: 1rem 0 1.5rem; font-size: 1.02rem; }
.app-pagenav .next { font-weight: 600; }

/* sub-navigation rows */
.app-subnav > .nav { border-bottom: 1px solid #e3e8ef; margin-bottom: 1.2rem; gap: 4px; flex-wrap: wrap; }
.app-subnav > .nav .nav-link { font-weight: 600; }
.app-subnav .app-navgroup { font-size: 0.72rem; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase; color: #52514e; padding: 0.55rem 0.2rem 0 0.8rem; }

/* home */
.home-hero { background: #f4f7fb; border-bottom: 1px solid #e3e8ef; margin: -1rem calc(-0.5 * var(--bs-gutter-x, 1.5rem)) 0; padding: 3rem 0 3.4rem; }
.home-wrap { max-width: 1240px; margin: 0 auto; padding-left: 1.5rem; padding-right: 1.5rem; }
.home-kicker { font-size: 0.85rem; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase; color: #1d63b8; }
.home-hero h1 { font-size: 3rem; line-height: 1.08; font-weight: 700; margin: 0.6rem 0 1rem; }
.home-lead { font-size: 1.2rem; line-height: 1.5; color: #3d434b; }
.home-cta { display: flex; flex-wrap: wrap; gap: 12px; margin-top: 1.4rem; }
.home-cta .btn { padding: 0.75rem 1.3rem; font-size: 1.05rem; font-weight: 600; }
.home-section { padding: 3rem 0 0.5rem; }
.home-section h2 { font-size: 1.85rem; font-weight: 700; margin-bottom: 0.3rem; }
.home-section > .home-wrap > p.sub { font-size: 1.07rem; color: #3d434b; margin-bottom: 1.4rem; }
.home-band { background: #f7f8fa; border-top: 1px solid #e8ebef; border-bottom: 1px solid #e8ebef; margin: 2.5rem calc(-0.5 * var(--bs-gutter-x, 1.5rem)) 0; padding: 0.5rem 0 2.5rem; }
.home-demo { background: #ffffff; border: 1px solid #dfe5ed; border-radius: 10px; padding: 1.1rem 1.25rem; }
.home-demo h2 { font-size: 1.3rem; margin: 0; font-weight: 700; }
.home-step { height: 100%; }
.home-step .badge-num { width: 34px; height: 34px; border-radius: 17px; background: #eaf2fc; color: #1d63b8; font-weight: 700; display: inline-flex; align-items: center; justify-content: center; }
.home-step h3 { font-size: 1.3rem; font-weight: 700; margin: 0.7rem 0 0.4rem; }
.home-step p { color: #3d434b; margin-bottom: 0.4rem; }
.home-step .inside { font-size: 0.86rem; color: #52514e; }
.home-theorem svg { width: 100%; height: 70px; display: block; }
.home-theorem h3 { font-size: 1.2rem; font-weight: 700; margin: 0.6rem 0 0.2rem; }
.home-theorem p { color: #3d434b; margin: 0; }
.home-op { font-size: 2rem; color: #52514e; align-self: center; text-align: center; }
.home-fact .num { font-size: 2.1rem; font-weight: 700; color: #1d63b8; line-height: 1.1; }
.home-fact .lab { color: #3d434b; }
.home-example { background: #ffffff; border: 1px solid #dfe5ed; border-radius: 10px; padding: 1.1rem 1.2rem; height: 100%; display: flex; flex-direction: column; gap: 0.4rem; }
.home-example .q { font-size: 1.1rem; font-weight: 600; line-height: 1.35; }
.home-example .how { color: #3d434b; }
.home-example .where { margin-top: auto; font-weight: 600; }
.home-footer { background: #1f2328; color: #d6d6d6; margin: 3rem calc(-0.5 * var(--bs-gutter-x, 1.5rem)) -1rem; padding: 2rem 0; }
.home-footer a { color: #ffffff; }
.pl-chips .shiny-options-group { display: flex; flex-wrap: wrap; gap: 6px; }
.pl-chips .radio-inline, .pl-chips .form-check-inline { margin: 0 !important; padding: 0 !important; }
.pl-chips .shiny-options-group { margin: 0 !important; padding: 0 !important; }
.pl-chips input[type=radio] { position: absolute; opacity: 0; width: 1px; height: 1px; }
.pl-chips label span { display: inline-block; border: 1px solid #b8c0c9; border-radius: 18px; padding: 0.4rem 0.85rem; font-weight: 600; font-size: 0.92rem; cursor: pointer; background: #ffffff; }
.pl-chips input[type=radio]:checked + span { background: #1d63b8; border-color: #1d63b8; color: #ffffff; }
.pl-chips input[type=radio]:focus-visible + span { outline: 2px solid #1d63b8; outline-offset: 2px; }
.app-overview-card { height: 100%; }
.app-overview-card h3 { font-size: 1.2rem; font-weight: 700; margin-bottom: 0.3rem; }
.app-overview-card p { color: #3d434b; }
/* wide tables scroll inside themselves on small screens instead of widening the page */
@media (max-width: 767.98px) { table.shiny-table, table.pl-table { display: block; overflow-x: auto; max-width: 100%; } }
"

# JavaScript: elements with class pl-goto send their target to input$pl_goto
APP_GOTO_JS <- "
$(document).on('click', '.pl-goto', function(e) {
  e.preventDefault();
  var d = $(this).data();
  Shiny.setInputValue('pl_goto', {
    page: d.page || null, sub: d.sub || null, model: d.model || null,
    domain: d.domain || null, nonce: Date.now()
  }, {priority: 'event'});
  window.scrollTo({top: 0, behavior: 'smooth'});
});
"

#' A link (or button-styled link) that navigates within the app
#' (.noWS = "outside" keeps it tight against surrounding punctuation)
pl_goto <- function(label, page, sub = NULL, model = NULL, domain = NULL, class = NULL, .noWS = NULL) {
  tags$a(href = "#", class = paste(c("pl-goto", class), collapse = " "),
         `data-page` = page, `data-sub` = sub, `data-model` = model, `data-domain` = domain,
         label, .noWS = .noWS)
}

#' Header band at the top of each top-level page
pl_page_header <- function(kicker, title, subtitle) {
  div(class = "app-pagehead",
      div(class = "app-wrap",
          div(class = "kicker", kicker),
          h1(title),
          p(subtitle)))
}

#' Back / Next links at the bottom of a page: each is list(label, page, sub)
pl_page_nav <- function(back = NULL, nxt = NULL) {
  div(class = "app-pagenav",
      if (!is.null(back)) pl_goto(paste("←", back$label), back$page, back$sub) else span(),
      if (!is.null(nxt)) pl_goto(paste(nxt$label, "→"), nxt$page, nxt$sub, class = "next") else span())
}

#' The content of a tabPanel, to reuse a module's tab UI inside another page
pl_pane_content <- function(tab) {
  do.call(tagList, tab$children)
}
