#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(TeachBayes)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
ui <- navbarPage(
  #theme = bs_theme(bootswatch = "minty"),
  "Bayesian Explorer",
  tabPanel(" Binomial Data",
           
           sidebarPanel(
             width = 4,
             "Observed Data",
             sliderInput(inputId = "n_trails", label = "Number of trails", min = 0,
                         max = 100, value = 10),
             sliderInput("x_successes", "No of successes (x):", min = 0, max = 100, 
                         value = 6),
             #"Prior Beliefs (Prior)",
             #radioButtons("prior_type", "Type of variable (cont/discrete)",
             #             choices = c("Continous (Beta)", "Discrete"), 
             #            selected = "Continuous (Beta)"),
             "Beta Distribution Parameters",
             sliderInput("alpha", "Alpha", min = 1, max=100, value=4, step=1),
             sliderInput("Beta", "Beta", min=1, max=100, value=4, step=1),
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
                          tableOutput("bayes_table"),  plotOutput("prior_plot", height = "49.5%",
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
  
  
  bayes_df <- reactive({
    
    theta <- as.numeric(strsplit(input$discrete_thetas, ",")[[1]])
    prior <- as.numeric(strsplit(input$prior, ",")[[1]])
    k <- input$x_successes
    n <- input$n_trails
    
    
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
      need(k <= n, "Error: 'Successes' (k) cannot be greater than 'Trials' (n).")
    )
    
    likelihood <- dbinom(x = k, size = n, prob = theta)
    product <- prior * likelihood
    posterior <- product /sum(product)
    
    #final Bayes data frame (putting everything defined above into a data frame)
    data.frame(
      Theta = theta,
      Prior = prior,
      Likelihood = likelihood,
      Product = product,
      Posterior = posterior
    )
  })
  
  #table output Bayes Data Frame(final data frame based on the user input)
  output$bayes_table <- renderTable({
    bayes_df()
  }, digits = 3)
  
  #subsetting the bayes_data frame to get just theta and prior columns for the prob_plot
  df_prior <- reactive({
    bayes_df() %>%
      select(Theta, Prior)
  })
  
  #Plot for the prior data using the prob_plot function from the Teachbayes library
  output$prior_plot <- renderPlot({
    #ggplot(bayes_df(), aes(x=Theta)) + geom_histogram(aes(y = ..density..))
    prob_plot(df_prior())
  })
  
  #Plot for the posterior vs the prior using the prior_post_plot function 
  output$post_plot <- renderPlot(
    prior_post_plot(bayes_df())
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
