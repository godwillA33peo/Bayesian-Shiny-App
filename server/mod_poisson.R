poissonServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    data_table <- reactive({
      
      
      req(input$poisson_prior_type)
      req(input$mean, input$sd)
      
      
      # user input to calculate and update prior and posterior Gamma params
      mu0 <- input$mean
      sigma0  <- input$sd
      n_counts <- input$n_counts
      n_obs <- input$n_observations 
      
      if (input$poisson_prior_type == "Continuous (Gamma)"){
        
        # alpha (shape) and beta (rate) prior parameters
        shape_prior <- (mu0^2)/(sigma0^2)
        rate_prior <- mu0/(sigma0^2)
        
        # alpha and beta posterior params
        shape_post <- shape_prior + n_counts
        rate_post <- rate_prior + n_obs

        
        gamma_df <- data.frame(
          Dist = c("Prior", "Data", "Post"),
          Shape = c(shape_prior, n_counts, shape_post),
          Rate =c(rate_prior,  n_obs, rate_post)
        )
        return(list(type="Continuous (Gamma)", data = (gamma_df)))
        
      }else{
        
        req(input$poisson_discrete_thetas, input$poisson_prior)
        
        #Discrete logic
        theta <- as.numeric(strsplit(input$poisson_discrete_thetas, ",")[[1]])
        prior <- as.numeric(strsplit(input$poisson_prior, ",")[[1]])
        
        
        #validation for the vector lengths and other neccesary conditions
        validate(
          need(length(theta) == length(prior),
               "Error: Your theta values and prior probabilities should be of same length")
        )
        #validate(
         # need(abs(sum(prior) -1) <0.01,
        #       "Error: Your prior probabilities must sum upto one")
        #)
        
        likelihood <- dpois(x= n_counts, lambda = theta)
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
        return(list(type="Discrete", data = bayes_df))
      }
    })
    
    output$summary_table <- renderTable({
      
      results <- data_table()
      return(results$data)
      
    }, digits = 3)
    
    # prior plot for both cases 
    output$prior_plot <- renderPlot({
      
      results <- data_table()
      
      if( results$type == "Discrete"){
        
        bayes_df <- results$data
        prior_data <- bayes_df %>%
          select(Theta, Prior)
        
        prior_plot_discrete <- ggplot(prior_data, aes(x=Theta, y = Prior)) +
          geom_col(fill = "red") +
          theme_grey(base_size = 18)
        
        return(prior_plot_discrete)
        
      }else{
        
        #bayes_df <- plot_data$data
        
      }
        
    })
    
    # post plot for both cases 
    output$post_plot <- renderPlot({
      results <- data_table()
      
      if(results$type == "Discrete"){
        
        post_data <- results$data
        discrete_plot <- prior_post_plot(post_data)
        return(discrete_plot)
        
      }else{
        
        
        
      }
      
    })
    
    
  })
}