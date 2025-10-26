#loading required libraries 

library(shiny)
library(bslib)
library(TeachBayes)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)

# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = bs_theme(bootswatch = "minty"),
  "Bayesian Explorer",
  tabPanel(" Binomial Data",
           
           sidebarPanel(
             width = 4,
             "Observed Data",
             sliderInput(inputId = "n_trails", label = "Number of trails", min = 0,
                         max = 100, value = 10),
             sliderInput("x_successes", "No of successes (x):", min = 0, max = 100, 
                         value = 6),
             "Prior Beliefs (Prior)",
             radioButtons("prior_type", "Type of variable (cont/discrete)",
                          choices = c("Continuous (Beta)", "Discrete"), 
                        selected = "Continuous (Beta)"),
             "Beta Distribution Parameters",
             sliderInput("alpha", "Alpha", min = 1, max=100, value=4, step=1),
             sliderInput("beta", "Beta", min=1, max=100, value=4, step=1),
             "Discrte Prior Parameters",
             textInput("discrete_thetas", "Enter Theta Values (csv):", 
                       value = "0.05, 0.2, 0.35, 0.5, 0.65"),
             textInput("prior", "Enter prior Values (sum to 1):", 
                       value = "0.05, 0.05, 0.35, 0.5, 0.05"),
             sliderInput("c_level", "Confidence Interval", min=0.5, max = 0.99,
                         value=0.95, step=0.01)
           ),
           
           mainPanel( width = 8,
                      layout_columns(
                        column(
                          tableOutput("summary_table"),  plotOutput("prior_plot", height = "60%",
                                                                  width = "230%"), width= 5
                        ),
                        column(
                          plotOutput("post_plot"), width = 12
                        )
                        
                      )
           ),
           
  ),
  tabPanel("Poison data",
           
  ),
  tabPanel("Normal Data",
           
  )
)

# Define server logic required to draw a histogram
server <- function(input, output){

################################# Binominal-Beta################################
  data_table <- reactive({
    k_data <- input$x_successes
    n_data <- input$n_trails
    
    if (input$prior_type == "Continuous (Beta)"){
        
        a_prior <- input$alpha
        b_prior <- input$beta
        
        validate(
          need(k_data <= n_data, "Error: 'Successes' (k) cannot be greater than 'Trials' (n).")
        )
        
        a_post <- a_prior + k_data
        b_post <- b_prior + n_data - k_data
        
        beta_df <- data.frame(
          Distribution = c("Prior", "Data", "Posterior"),
          Alpha = c(a_prior, k_data, a_post),
          Beta =c(b_prior, n_data - k_data, b_post)
        )
        return(list(type="continuous", data = (beta_df)))
        
      }else{
      
      #Discrete logic
        theta <- as.numeric(strsplit(input$discrete_thetas, ",")[[1]])
        prior <- as.numeric(strsplit(input$prior, ",")[[1]])
        
        
        #validation for the vector lengths and other neccesary conditions
        validate(
          need(length(theta) == length(prior),
               "Error: Your theta values and prior probabilities should be of same length")
        )
        validate(
          need(abs(sum(prior) -1) <0.01,
               "Error: Your prior probabilities must sum upto one")
        )
        validate(
          need(k_data <= n_data, "Error: 'Successes' (k) cannot be greater than 'Trials' (n).")
        )
        
        likelihood <- dbinom(x = k_data, size = n_data, prob = theta)
        product <- prior * likelihood
        posterior <- product /sum(product)
        
        #final Bayes data frame (putting everything defined above into a data frame)
        bayes_df <- data.frame(
          Theta = theta,
          Prior = prior,
          Likelihood = likelihood,
          Product = product,
          Posterior = posterior
        )
        return(list(type="descrete", data = bayes_df))
      }
    })
    
  #table output Bayes Data Frame(final data frame based on the user input)
  output$summary_table <- renderTable({
    
    results <- data_table()
    return(results$data)
  }, digits = 3)
  
  #Plot output for the prior data, beta_draw for continuous data and prob_plot for discrete data
  output$prior_plot <- renderPlot({
    
    results <- data_table()
    
    #condition for continous data Beta Distribution
    if (results$type == "continuous") {
      
      cont_data <- results$data
      prior_par <- c(cont_data[1, 2], cont_data[1, 3])
      prior_plot_cont <- beta_draw(prior_par)
      
      return(prior_plot_cont) # return the beta curve from the prior data
      
    } else { #condition for discrete data Binomial Distribtion
      bayes_df <- results$data
      prior_data <- bayes_df %>%
        select(Theta, Prior)
      prior_plot_dis <- prob_plot(prior_data)
      
      return(prior_plot_dis) # return the histogram from the prior data
    }
    
  })
  
  #Plot for the posterior vs the prior using the prior_post_plot function 
  #output$post_plot <- renderPlot(
    
  #)
}
################# End of Binomial-Beta##################################################



shinyApp(ui = ui, server = server)
