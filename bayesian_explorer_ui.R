ui <- navbarPage(
  title = "Bayesian Inference for Proportions",
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  
  ######################### Beta Binomial TabPanel #########################
  tabPanel("Binomial Distribution",
           sidebarLayout(
             sidebarPanel( 
               width = 3,
               "Observed Data",
               sliderInput(inputId = "n_trails", label = "Number of trails", min = 0,
                           max = 100, value = 10),
               sliderInput("x_successes", "No of successes (x):", min = 0, max = 100, 
                           value = 6),
               "Prior Beliefs (Prior)",
               selectInput("prior_type", "Type of prior (cont/discrete)",
                           choices = c("Continuous (Beta)", "Discrete"), 
                           selected = "Continuous (Beta)"),
               "Beta Distribution Parameters",
               sliderInput("alpha", "Alpha", min = 1, max=100, value=4, step=1),
               sliderInput("beta", "Beta", min=1, max=100, value=4, step=1),
               "Discrte Prior Parameters",
               textInput("discrete_thetas", "Enter Theta Values (csv):", 
                         value = "0.05, 0.2, 0.35, 0.5, 0.65"),
               textInput("prior", "Enter prior Values (sum to 1):", 
                         value = "0.05, 0.05, 0.35, 0.5, 0.05")
             ),
             mainPanel(
               fluidRow(
                 column(
                   width = 6,
                   h4("Prior Distribution"),
                   plotOutput("binom_prior_plot")
                 ),
                 column(
                   width = 6,
                   h4("Posterior Prior Plot"),
                   plotOutput("binom_posterior_plot")
                 )
               ),
               fluidRow(
                 column(
                   width = 6,
                 ),
                 column(
                   width = 6,
                 )
               )
             )# end if Beta main panel
           )
  ),
  
  ########################## Poison Gamma Distribtuion TabPanel ############
  tabPanel("Poison Distribtuion",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               radioButtons(inputId = "poisson_prior_type", label = "Type of prior (continuous/discrete)",
                            choices = c("Continuous", "Discrete"), 
                            selected = "Continunous (Gamma)"),
               sliderInput(inputId = "poisson_observed_counts", label = "Observed Counts (x)", 
                           min = 0, max = 100, value = 10),
               "Gamma Distribution Parameters",
               sliderInput("poisson_alpha", "Alpha", min = 1, max=100, value=4, step=1),
               sliderInput("poisson_beta", "Beta", min=1, max=100, value=4, step=1),
               "Discrte Prior Parameters",
               textInput("poisson_discrete_thetas", "Enter Theta Values (csv):", 
                         value = "0.05, 0.2, 0.35, 0.5, 0.65"),
               textInput("poisson_prior", "Enter prior Values (sum to 1):", 
                         value = "0.05, 0.05, 0.35, 0.5, 0.05")
             ),
             mainPanel(
             )# end of Poi main panel
           )
  ),
  
  ########################## Normal Distribtuion TabPanel ##################
  tabPanel("Normal Distribtion",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               numericInput("normal_sample_mean", "Sample Mean (x̄):", value = 50),
               numericInput("normal_sample_size", "Sample Size (n):", value = 30),
               numericInput("normal_known_sd", "Known Standard Deviation (σ):", value = 10),
               "Normal Prior Parameters",
               numericInput("normal_prior_mean", "Prior Mean (μ₀):", value = 40),
               numericInput("normal_prior_sd", "Prior Standard Deviation (σ₀):", value = 15)
             ),
             mainPanel(
             )# end of Nor main panel
           )
  ),
  
  ########################## MCMC Simulations TabPanel #####################
  tabPanel("MCMC Simulations",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               fileInput(
                 "mcmc_data_file", "Upload Data File (CSV):",
                 accept = c(".csv")
               ),
               selectInput(
                 "mcmc_model_type", "Select Model Type:",
                 choices = c("Selected Model ......" = "", "Binomial", "Poisson", "Normal"),
                 selected = ""
               ),
               uiOutput("model_inputs"),
               "MCMC Settings",
               numericInput("mcmc_iterations", "Number of Iterations:", value = 1000, min = 100, step = 100),
               numericInput("mcmc_burnin", "Burn-in Period:", value = 200, min = 0, step = 50),
               numericInput("mcmc_thin", "Thinning Interval:", value = 5, min = 1, step = 1),
               numericInput("n_chains", "Number of Chains:", value = 3, min = 1, step = 1)
             ),
             
             mainPanel(
             ) # end of MCMC main panel
           )
  ),
  tabPanel("About",
           fluidPage()
           
  )
)