# ============================================================================
# Prior Lab: shared UI pieces
# ============================================================================
#   - colours and ggplot theme used by every Prior Lab plot
#   - small HTML helpers (help boxes, stat tiles, diagnosis boxes)
#   - priorPicker module: family dropdown + parameter inputs, reusable in
#     every tab, with optional "load from your prior library"
#   - prior library helpers (a per-session list of saved priors)
# ============================================================================

# ---- colours & theme -----------------------------------------------------------
# Validated for colour-blind separation; slot 4 is always paired with its own
# line type so it never relies on colour alone.
PL_COLS <- c(Prior = "#2a78d6", Posterior = "#eb6834", Likelihood = "#0f9b7a",
             Reference = "#52514e")
PL_SLOT_COLS <- c(A = "#2a78d6", B = "#eb6834", C = "#0f9b7a", D = "#a03fb0")
PL_SLOT_LTY <- c(A = "solid", B = "longdash", C = "dashed", D = "dotdash")
PL_INK <- "#52514e"

pl_theme <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      legend.key.width = grid::unit(2.2, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#ebebea", linewidth = 0.4),
      axis.title = element_text(colour = PL_INK),
      axis.text = element_text(colour = PL_INK),
      plot.title = element_text(face = "bold", size = base_size + 1),
      plot.subtitle = element_text(colour = PL_INK, size = base_size - 2),
      plot.caption = element_text(colour = PL_INK, size = base_size - 4, hjust = 0),
      strip.text = element_text(face = "bold", size = base_size - 1)
    )
}

# ---- HTML helpers ------------------------------------------------------------------

PL_CSS <- "
.pl-help { background:#f4f8fd; border-left:4px solid #2a78d6; padding:12px 16px;
           margin-bottom:16px; border-radius:4px; }
.pl-help h4 { margin-top:0; }
.pl-help p, .pl-help li { font-size:14px; }
.pl-note { color:#52514e; font-size:13px; }
.pl-tiles { display:flex; flex-wrap:wrap; gap:12px; margin:4px 0 14px; }
.pl-tile { flex:1 1 150px; border:1px solid #e3e3e0; border-radius:6px; padding:10px 12px; background:#fff; }
.pl-tile .v { font-size:20px; font-weight:600; color:#0b0b0b; line-height:1.2; }
.pl-tile .l { font-size:12px; color:#52514e; margin-top:2px; }
.pl-diag { padding:10px 14px; border-radius:4px; margin:6px 0 14px; border:1px solid #e3e3e0;
           border-left-width:4px; background:#fff; }
.pl-diag.ok { border-left-color:#0f9b7a; }
.pl-diag.warn { border-left-color:#eda100; }
.pl-diag.alert { border-left-color:#e34948; }
.pl-diag .tag { font-weight:600; margin-right:6px; }
.pl-slot { padding-top:10px; }
.well .nav-pills > li > a { padding:4px 12px; }
pre.pl-code { white-space:pre-wrap; font-size:12.5px; background:#f7f7f5; }
.pl-qrow .form-group { margin-bottom:6px; }
.pl-section { font-weight:600; margin:14px 0 6px; color:#0b0b0b; }
.pl-lib-load { border-top:1px dashed #d6d6d2; margin-top:6px; padding-top:6px; }
table.pl-table td, table.pl-table th { padding:4px 10px !important; }
"

pl_help <- function(title, ...) {
  div(class = "pl-help", h4(title), ...)
}

pl_tile <- function(value, label) {
  div(class = "pl-tile", div(class = "v", value), div(class = "l", label))
}

pl_diag <- function(level = c("ok", "warn", "alert"), tag, text) {
  level <- match.arg(level)
  div(class = paste("pl-diag", level), span(class = "tag", tag), text)
}

pl_interval_str <- function(lo, hi) sprintf("[%s, %s]", pl_fmt(lo, 3), pl_fmt(hi, 3))

pl_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "NA", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}

# Run expr; turn validation errors into a message, swallow silent req() errors.
pl_try <- function(expr) {
  tryCatch(expr, error = function(e) e)
}

pl_err_msg <- function(e) {
  if (!inherits(e, "error")) return(NULL)
  m <- conditionMessage(e)
  if (nzchar(m)) m else NULL
}

# ---- prior library --------------------------------------------------------------

pl_library_add <- function(lib, name, prior, source = "") {
  name <- trimws(name)
  if (!nzchar(name)) name <- pl_prior_label(prior)
  base <- name; i <- 2
  while (!is.null(lib[[name]])) {
    name <- sprintf("%s (%d)", base, i)
    i <- i + 1
  }
  lib[[name]] <- list(name = name, prior = prior, source = source,
                      saved = format(Sys.time(), "%Y-%m-%d %H:%M"))
  lib
}

pl_library_df <- function(lib) {
  if (!length(lib)) {
    return(data.frame(Name = character(0), Prior = character(0), Stan = character(0),
                      From = character(0), check.names = FALSE))
  }
  data.frame(
    Name = vapply(lib, `[[`, "", "name"),
    Prior = vapply(lib, function(it) pl_prior_label(it$prior), ""),
    Stan = vapply(lib, function(it) pl_stan_string(it$prior), ""),
    From = vapply(lib, `[[`, "", "source"),
    check.names = FALSE, row.names = NULL
  )
}

pl_library_export <- function(lib) {
  data.frame(
    name = vapply(lib, `[[`, "", "name"),
    family = vapply(lib, function(it) it$prior$family, ""),
    params = vapply(lib, function(it) pl_params_to_string(it$prior$params), ""),
    source = vapply(lib, `[[`, "", "source"),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

pl_library_import <- function(lib, df) {
  need <- c("name", "family", "params")
  if (!all(need %in% names(df))) stop("The file needs columns: name, family, params.")
  n_ok <- 0
  for (i in seq_len(nrow(df))) {
    fam <- trimws(as.character(df$family[i]))
    if (is.null(PL_DISTS[[fam]])) next
    par <- tryCatch(pl_params_from_string(as.character(df$params[i])), error = function(e) NULL)
    if (is.null(par) || !all(pl_par_names(fam) %in% names(par))) next
    pr <- list(family = fam, params = par[pl_par_names(fam)])
    if (!is.null(pl_check_prior(pr))) next
    src <- if ("source" %in% names(df)) as.character(df$source[i]) else "imported"
    lib <- pl_library_add(lib, as.character(df$name[i]), pr, src)
    n_ok <- n_ok + 1
  }
  attr(lib, "n_imported") <- n_ok
  lib
}

# ---- priorPicker module ----------------------------------------------------------
#' UI: family dropdown, parameter inputs, optional library loader.
priorPickerUI <- function(id, label = "Prior family", supports = NULL, selected = "normal") {
  ns <- NS(id)
  tagList(
    selectInput(ns("family"), label, choices = pl_family_choices(supports), selected = selected),
    uiOutput(ns("par_ui")),
    uiOutput(ns("lib_ui"))
  )
}

#' Server.
#' @param init     initial prior, list(family, params)
#' @param supports allowed support(s) of the parameter (static)
#' @param library  reactiveVal holding the session's prior library (optional)
#' @param reset    optional reactive returning list(supports, prior); whenever it
#'                 changes the dropdown is refreshed and the prior replaced
#' @return list(prior = reactive, set = function(prior))
priorPickerServer <- function(id, init = NULL, supports = NULL, library = NULL, reset = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pending <- reactiveVal(init)
    cur_supports <- reactiveVal(supports)

    par_id <- function(fam, name) paste0("p_", fam, "_", name)

    set_prior <- function(pr) {
      pending(pr)
      if (identical(isolate(input$family), pr$family)) {
        for (nm in names(pr$params)) {
          updateNumericInput(session, par_id(pr$family, nm), value = signif(pr$params[[nm]], 6))
        }
      } else {
        updateSelectInput(session, "family", selected = pr$family)
      }
    }

    if (!is.null(reset)) {
      observeEvent(reset(), {
        r <- reset()
        cur_supports(r$supports)
        pending(r$prior)
        ch <- pl_family_choices(r$supports)
        updateSelectInput(session, "family", choices = ch, selected = r$prior$family)
        if (identical(isolate(input$family), r$prior$family)) {
          for (nm in names(r$prior$params)) {
            updateNumericInput(session, par_id(r$prior$family, nm), value = signif(r$prior$params[[nm]], 6))
          }
        }
      })
    }

    output$par_ui <- renderUI({
      fam <- input$family
      req(fam, !is.null(PL_DISTS[[fam]]))
      d <- pl_dist(fam)
      pend <- isolate(pending())
      vals <- if (!is.null(pend) && identical(pend$family, fam)) pend$params else pl_default_params(fam)
      cols <- lapply(d$pars, function(pp) {
        column(6, numericInput(ns(par_id(fam, pp$name)), pp$label,
                               value = signif(vals[[pp$name]], 6), step = pp$step))
      })
      fluidRow(cols)
    })
    outputOptions(output, "par_ui", suspendWhenHidden = FALSE)

    prior <- reactive({
      fam <- input$family
      req(fam, !is.null(PL_DISTS[[fam]]))
      nm <- pl_par_names(fam)
      raw <- lapply(nm, function(n) input[[par_id(fam, n)]])
      req(all(!vapply(raw, is.null, logical(1))))
      vals <- setNames(vapply(raw, function(v) {
        if (length(v) == 0 || is.na(v)) NA_real_ else as.numeric(v)
      }, numeric(1)), nm)
      pr <- list(family = fam, params = vals)
      msg <- pl_check_prior(pr, cur_supports())
      validate(need(is.null(msg), msg))
      pr
    })

    # Remember the latest valid prior, so a re-drawn picker shows the user's values
    observe({
      pr <- tryCatch(prior(), error = function(e) NULL)
      if (!is.null(pr)) pending(pr)
    })

    if (!is.null(library)) {
      output$lib_ui <- renderUI({
        lib <- library()
        s <- cur_supports()
        ok <- Filter(function(it) is.null(pl_check_prior(it$prior, s)), lib)
        if (!length(ok)) return(NULL)
        div(class = "pl-lib-load",
            fluidRow(
              column(8, selectInput(ns("lib_pick"), "Load a saved prior",
                                    choices = c("Choose\u{2026}" = "", setNames(names(ok), names(ok))))),
              column(4, style = "margin-top:25px;",
                     actionButton(ns("lib_load"), "Load", class = "btn-default btn-sm"))
            ))
      })
      outputOptions(output, "lib_ui", suspendWhenHidden = FALSE)
      observeEvent(input$lib_load, {
        nm <- input$lib_pick
        req(nzchar(nm))
        it <- library()[[nm]]
        req(it)
        set_prior(it$prior)
      })
    }

    list(prior = prior, set = set_prior)
  })
}
