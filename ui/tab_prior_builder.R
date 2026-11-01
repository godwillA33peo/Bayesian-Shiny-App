pl_builder_slot <- function(ns, slot, show, family) {
  tabPanel(
    slot, value = slot,
    div(class = "pl-slot",
        checkboxInput(ns(paste0("show_", slot)), "Show this prior", value = show),
        textInput(ns(paste0("label_", slot)), "Label (optional)", value = "",
                  placeholder = "auto, e.g. Normal(0, 1)"),
        priorPickerUI(ns(paste0("pick_", slot)), "Family", selected = family))
  )
}

priorBuilderUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Prior Builder", value = "prior_builder",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h4("Priors to compare"),
        tabsetPanel(
          id = ns("slot_tabs"), type = "pills",
          pl_builder_slot(ns, "A", TRUE, "normal"),
          pl_builder_slot(ns, "B", TRUE, "normal"),
          pl_builder_slot(ns, "C", TRUE, "student_t"),
          pl_builder_slot(ns, "D", TRUE, "normal")
        ),
        hr(),
        h4("Display"),
        selectInput(ns("transform"), "Implied scale (what the prior means after a link function)",
                    choices = pl_transform_choices(), selected = "inv_logit"),
        radioButtons(ns("curve"), "Curve", inline = TRUE,
                     choices = c("Density" = "density", "Cumulative (CDF)" = "cdf")),
        checkboxInput(ns("manual_range"), "Set the x-axis range myself", FALSE),
        conditionalPanel(
          "input.manual_range", ns = ns,
          fluidRow(column(6, numericInput(ns("xmin"), "From", -5)),
                   column(6, numericInput(ns("xmax"), "To", 5)))
        ),
        sliderInput(ns("level"), "Interval level", min = 0.5, max = 0.99, value = 0.95, step = 0.01),
        numericInput(ns("threshold"), "Threshold c for P(\u{03b8} > c)", value = 0)
      ),
      mainPanel(
        width = 8,
        pl_help(
          "Prior Builder: see what a prior really says",
          p("Choose up to four priors and compare them side by side. The",
            strong("implied scale"), "panel shows what each prior means after the model's link function, which is where many priors that look \u{201c}vague\u{201d} turn out to be strong."),
          p(strong("Try this:"), "the defaults are four common choices for a logistic-regression coefficient. Look at the implied probability panel: Normal(0, 10) puts most of its mass at probabilities near 0 or 1, while Normal(0, 1) spreads its mass over moderate probabilities."),
          p(class = "pl-note", "Save any prior to your library (last tab) to reuse it in the other Prior Lab tabs. Use the download button there to keep it after you close the app.")
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Compare",
            br(),
            uiOutput(ns("slot_messages")),
            plotOutput(ns("density_plot"), height = "380px"),
            conditionalPanel(
              "input.transform != 'none'", ns = ns,
              plotOutput(ns("implied_plot"), height = "340px"),
              tableOutput(ns("implied_table"))
            ),
            h4("Summary on the parameter scale"),
            tableOutput(ns("summary_table")),
            p(class = "pl-note", "Mean or SD shows NA where it does not exist (e.g. Cauchy, Student-t with df \u{2264} 2).")
          ),
          tabPanel(
            "Code & export",
            br(),
            selectInput(ns("brms_class"), "brms prior class", choices = PL_BRMS_CLASSES, selected = "b"),
            h4("brms"),
            verbatimTextOutput(ns("code_brms")),
            h4("Stan"),
            verbatimTextOutput(ns("code_stan")),
            h4("R (simulate prior draws)"),
            verbatimTextOutput(ns("code_r")),
            downloadButton(ns("download_draws"), "Download 10,000 draws per prior (CSV)")
          ),
          tabPanel(
            "Prior library",
            br(),
            p("Saved priors appear in the \u{201c}Load a saved prior\u{201d} box under every prior picker in the Prior Lab."),
            fluidRow(
              column(3, selectInput(ns("save_slot"), "Save prior", choices = c("A", "B", "C", "D"))),
              column(5, textInput(ns("save_name"), "Name", placeholder = "e.g. Effect of GenAI use (logit)")),
              column(4, style = "margin-top:25px;", actionButton(ns("save"), "Save to library", class = "btn-primary"))
            ),
            tableOutput(ns("library_table")),
            fluidRow(
              column(4, downloadButton(ns("download_library"), "Download library (CSV)")),
              column(5, fileInput(ns("upload_library"), NULL, accept = ".csv",
                                  buttonLabel = "Upload library\u{2026}", placeholder = "CSV from this app")),
              column(3, actionButton(ns("clear_library"), "Clear library", class = "btn-default"))
            )
          )
        )
      )
    )
  )
}
