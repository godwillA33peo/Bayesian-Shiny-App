PL_EIGHT_SCHOOLS_TEXT <- paste(
  c("group,estimate,se",
    sprintf("%s,%s,%s", PL_EIGHT_SCHOOLS$group, PL_EIGHT_SCHOOLS$estimate, PL_EIGHT_SCHOOLS$se)),
  collapse = "\n")

priorHierUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Hierarchical priors", value = "prior_hier",
    tabsetPanel(
      type = "tabs",

      # ---- (a) group spread ---------------------------------------------------------
      tabPanel(
        "Group-level SD (\u{03c4}): what it implies",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            selectInput(ns("gs_link"), "Outcome and link",
                        choices = c("Proportion, logit link (e.g. pass rate)" = "logit",
                                    "Rate, log link (e.g. logins per week)" = "log",
                                    "Continuous, identity link (e.g. score)" = "identity")),
            numericInput(ns("gs_baseline"), "Typical group's value (on the outcome scale)", 0.7, step = 0.05),
            div(class = "pl-section", "Prior on the between-group SD \u{03c4}"),
            priorPickerUI(ns("gs_tau"), "Family", supports = "positive", selected = "half_normal"),
            radioButtons(ns("gs_on_var"), "The prior is placed on", inline = TRUE,
                         choices = c("\u{03c4} (SD)" = "sd", "\u{03c4}\u{00b2} (variance)" = "var")),
            sliderInput(ns("gs_J"), "Groups per simulated draw (e.g. modules, schools)", min = 3, max = 60, value = 20)
          ),
          mainPanel(
            width = 8,
            pl_help(
              "How different can groups be under your \u{03c4} prior?",
              p("In a hierarchical model, group effects are drawn as u\u{2c7c} ~ Normal(0, \u{03c4}). The prior on \u{03c4} is hard to judge on the link scale, so this tab turns it into statements about groups: ",
                em("\u{201c}how much better is a top-10% module than a bottom-10% module?\u{201d}")),
              p(strong("Try:"), " compare Half-Normal(1) with Half-Cauchy(5) on the logit scale, and look at how often the prior allows absurd gaps between groups.")
            ),
            uiOutput(ns("gs_tiles")),
            fluidRow(
              column(6, plotOutput(ns("gs_tau_plot"), height = "300px")),
              column(6, plotOutput(ns("gs_ratio_plot"), height = "300px"))
            ),
            plotOutput(ns("gs_strip_plot"), height = "420px")
          )
        )
      ),

      # ---- (b) partial pooling -------------------------------------------------------
      tabPanel(
        "Partial pooling (8 schools)",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            textAreaInput(ns("pp_data"), "Group estimates (CSV: group, estimate, se)",
                          value = PL_EIGHT_SCHOOLS_TEXT, rows = 9),
            helpText("Default: the classic 8 schools coaching study (Rubin, 1981). Paste your own group means and standard errors."),
            div(class = "pl-section", "Prior on \u{03c4}"),
            priorPickerUI(ns("pp_tau"), "Family", supports = "positive", selected = "half_cauchy"),
            radioButtons(ns("pp_on_var"), "The prior is placed on", inline = TRUE,
                         choices = c("\u{03c4} (SD)" = "sd", "\u{03c4}\u{00b2} (variance)" = "var")),
            div(class = "pl-section", "Presets from Gelman (2006)"),
            div(style = "display:flex; flex-wrap:wrap; gap:6px;",
                actionButton(ns("pre_hc"), "Half-Cauchy(25)", class = "btn-default btn-sm"),
                actionButton(ns("pre_unif"), "Uniform(0, 100)", class = "btn-default btn-sm"),
                actionButton(ns("pre_ig1"), "Inv-Gamma(1, 1) on \u{03c4}\u{00b2}", class = "btn-default btn-sm"),
                actionButton(ns("pre_ig001"), "Inv-Gamma(0.001, 0.001) on \u{03c4}\u{00b2}", class = "btn-default btn-sm")),
            br(),
            radioButtons(ns("pp_mu_prior"), "Prior on the overall mean \u{03bc}", inline = TRUE,
                         choices = c("Flat" = "flat", "Normal" = "normal")),
            conditionalPanel(
              "input.pp_mu_prior == 'normal'", ns = ns,
              fluidRow(column(6, numericInput(ns("pp_m0"), "Mean", 0)),
                       column(6, numericInput(ns("pp_s0"), "SD", 20, min = 0.001)))
            ),
            sliderInput(ns("pp_level"), "Credible level", min = 0.5, max = 0.99, value = 0.95, step = 0.01)
          ),
          mainPanel(
            width = 8,
            pl_help(
              "How the \u{03c4} prior drives shrinkage",
              p("Model: estimate\u{2c7c} ~ Normal(\u{03b8}\u{2c7c}, se\u{2c7c}), \u{03b8}\u{2c7c} ~ Normal(\u{03bc}, \u{03c4}). The posterior is computed exactly (\u{03bc} and \u{03b8} integrated analytically, \u{03c4} on a fine grid), so you can change the prior and see the result instantly with no sampler."),
              p(strong("Try the presets:"), " Inv-Gamma(0.001, 0.001) on \u{03c4}\u{00b2}, once a popular \u{201c}non-informative\u{201d} default, puts a spike near \u{03c4} = 0 and pulls every school towards the pooled mean. Gelman (2006) used this example to argue for half-t / half-Cauchy priors on \u{03c4}.")
            ),
            uiOutput(ns("pp_tiles")),
            fluidRow(
              column(6, plotOutput(ns("pp_tau_plot"), height = "320px")),
              column(6, plotOutput(ns("pp_profile_plot"), height = "320px"))
            ),
            plotOutput(ns("pp_forest"), height = "380px"),
            tableOutput(ns("pp_table")),
            h4("brms code"),
            verbatimTextOutput(ns("pp_code"))
          )
        )
      ),

      # ---- (c) LKJ ----------------------------------------------------------------------
      tabPanel(
        "Correlation prior (LKJ)",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            sliderInput(ns("lkj_K"), "Size of the correlation matrix K (random effects per group)",
                        min = 2, max = 10, value = 2),
            checkboxGroupInput(ns("lkj_eta"), "LKJ shape \u{03b7} (pick up to 4)",
                               choices = c("0.5" = 0.5, "1" = 1, "2" = 2, "4" = 4, "10" = 10),
                               selected = c(1, 2, 4), inline = TRUE),
            sliderInput(ns("lkj_cut"), "Report P(|r| > c) for c =", min = 0.1, max = 0.9, value = 0.5, step = 0.05)
          ),
          mainPanel(
            width = 8,
            pl_help(
              "LKJ prior for correlations between random effects",
              p("When groups have random intercepts ", em("and"), " random slopes, brms and Stan put an LKJ(\u{03b7}) prior on their correlation matrix. \u{03b7} = 1 is uniform over correlation matrices; \u{03b7} > 1 pulls correlations towards 0; \u{03b7} < 1 favours strong correlations."),
              p("Each single correlation r has marginal prior (r + 1)/2 ~ Beta(\u{03b7} \u{2212} 1 + K/2, \u{03b7} \u{2212} 1 + K/2), so a larger matrix (bigger K) also pulls each correlation towards 0.")
            ),
            plotOutput(ns("lkj_plot"), height = "360px"),
            tableOutput(ns("lkj_table")),
            verbatimTextOutput(ns("lkj_code"))
          )
        )
      )
    )
  )
}
