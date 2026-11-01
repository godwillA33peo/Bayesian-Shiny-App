pl_builder_card <- function(title, ...) {
  column(6, div(style = "border:1px solid #e3e3e0; border-radius:6px; padding:12px 14px; margin-bottom:16px; background:#fff;",
                h4(style = "margin-top:0;", title), ...))
}

priorPresetsUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Presets: health, education, general", value = "prior_presets",
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Preset library",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            radioButtons(ns("domain"), "Show", inline = TRUE,
                         choices = c("All", "Health", "Education", "General")),
            checkboxGroupInput(ns("pick"), "Tick presets to plot or save",
                               choices = setNames(as.character(seq_along(PL_PRESETS)),
                                                  vapply(PL_PRESETS, function(p) paste0(p$name, "  \u{2013}  ", pl_prior_label(p$prior)), "")),
                               selected = c("1", "2")),
            fluidRow(
              column(6, actionButton(ns("add"), "Save ticked to my library", class = "btn-primary btn-sm")),
              column(6, actionButton(ns("tick_none"), "Untick all", class = "btn-default btn-sm"))
            ),
            br(),
            p(class = "pl-note", "Saved presets appear under \u{201c}Load a saved prior\u{201d} in every Prior Lab tab, including the Model Lab.")
          ),
          mainPanel(
            width = 8,
            pl_help(
              "Presets: documented starting points, not defaults to copy blindly",
              p("Each preset says what parameter it is for, why it is reasonable, and where it comes from. Tick a few to compare them, save them to your library, then check them against your own data in the Model Lab.")
            ),
            plotOutput(ns("preset_plot"), height = "300px"),
            tableOutput(ns("preset_table")),
            h4("References"),
            uiOutput(ns("refs"))
          )
        )
      ),
      tabPanel(
        "Quick builders",
        br(),
        pl_help("Turn a plain-language statement into a prior",
                p("Each builder takes one or two judgements you can defend in a paper or a protocol, and returns the matching prior.")),
        fluidRow(
          pl_builder_card(
            "Sceptical and enthusiastic trial priors",
            p(class = "pl-note", "Spiegelhalter, Freedman & Parmar (1994). Effects on the log-ratio scale (odds, hazard or risk ratio)."),
            fluidRow(column(6, numericInput(ns("tr_ratio"), "Clinically hoped-for ratio", 0.75, min = 0.01, step = 0.05)),
                     column(6, numericInput(ns("tr_tail"), "Tail probability", 0.05, min = 0.001, max = 0.49, step = 0.01))),
            plotOutput(ns("tr_plot"), height = "220px"),
            uiOutput(ns("tr_text")),
            actionButton(ns("tr_save"), "Save both to my library", class = "btn-primary btn-sm")
          ),
          pl_builder_card(
            "PC prior for a standard deviation",
            p(class = "pl-note", "Simpson et al. (2017): \u{201c}the SD is unlikely to exceed U\u{201d}, as P(\u{03c3} > U) = \u{03b1}."),
            fluidRow(column(6, numericInput(ns("pc_U"), "Upper value U", 1, min = 0.0001, step = 0.1)),
                     column(6, numericInput(ns("pc_alpha"), "Probability \u{03b1}", 0.01, min = 0.0001, max = 0.99, step = 0.01))),
            plotOutput(ns("pc_plot"), height = "220px"),
            uiOutput(ns("pc_text")),
            actionButton(ns("pc_save"), "Save to my library", class = "btn-primary btn-sm")
          )
        ),
        fluidRow(
          pl_builder_card(
            "Beta prior from a mean and a sample size",
            p(class = "pl-note", "\u{201c}Previous data suggest about m, and I trust that as much as n observations.\u{201d}"),
            fluidRow(column(6, numericInput(ns("bp_mean"), "Mean m", 0.3, min = 0.001, max = 0.999, step = 0.05)),
                     column(6, numericInput(ns("bp_n"), "Worth n observations", 20, min = 0.01, step = 1))),
            plotOutput(ns("bp_plot"), height = "220px"),
            uiOutput(ns("bp_text")),
            actionButton(ns("bp_save"), "Save to my library", class = "btn-primary btn-sm")
          ),
          pl_builder_card(
            "Effect-size prior from a plausible range",
            p(class = "pl-note", "\u{201c}I am p sure the standardised effect is within \u{00b1}d.\u{201d} E.g. Kraft (2020): most education effects are under 0.2 SD."),
            fluidRow(column(6, numericInput(ns("es_d"), "Bound d", 0.2, min = 0.001, step = 0.05)),
                     column(6, numericInput(ns("es_p"), "Probability p", 0.8, min = 0.01, max = 0.999, step = 0.05))),
            plotOutput(ns("es_plot"), height = "220px"),
            uiOutput(ns("es_text")),
            actionButton(ns("es_save"), "Save to my library", class = "btn-primary btn-sm")
          )
        )
      )
    )
  )
}
