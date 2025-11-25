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
        Dist = c("Prior", "Data", "Post"),
        Alpha = c(a_prior, k_data, a_post),
        Beta =c(b_prior,  n_data - k_data, b_post)
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
  output$post_plot <- renderPlot({
    
    results <- data_table()
    
    #posterior plot for a continuous prior
    if(results$type == "continuous"){
      
      cont_data <- results$data
      prior_par <- c(cont_data[1, 2], cont_data[1, 3])
      post_par <- c(cont_data[3, 2], cont_data[3, 3])
      post_prior_plot <- beta_prior_post(prior_par, post_par)
      return(post_prior_plot)
      
    }else{ # condition for discrete prior
      post_data <- results$data
      dis_post <- prior_post_plot(post_data)
      return(dis_post)
    }
    
  })
  
  output$conf_int <- renderPlot({
    results <- data_table()
    
    if(results$type == "continuous"){
      
      ci <- input$c_level
      cont_data <- results$data
      prior_par <- c(cont_data[1, 2], cont_data[1, 3])
      post_par <- c(cont_data[3, 2], cont_data[3, 3])
      credible_interval <- beta_interval(ci, post_par)
      return(credible_interval)
      
    }
  })
  
  output$point_estimates <- renderText({
    
    results <- data_table()
    
    if(results$type == "continuous"){
      
      cont_data <- results$data
      alpha <- as.numeric(cont_data[3, 2])
      beta <- as.numeric(cont_data[3, 3])
      mean <- as.numeric(alpha / alpha + beta)
      sprintf("The point estimate is [%.4f, %.4f].", mean)
      
    }
  }) # end of function for point estimates 
  
  output$sim_ci_level <- renderText({
    
    results <- data_table()
    
    if(results$type == "continuous"){
      
      cont_data <- results$data
      n <- input$n_sims
      ci_level <- input$sim_ci_level
      
      shape_1 <- cont_data[3, 2]
      shape_2 <- cont_data[3, 3]
      mysims <- rbeta(n, shape1 = shape_1, shape2 = shape_2)
      alpha <- 1 - ci_level
      lower_prob <- alpha / 2
      upper_prob <- 1 - (alpha / 2)
      lower_bound <- quantile(mysims, probs = lower_prob) # should we limit only to the 95% confidence interval if not whats the logic ??
      upper_bound <- quantile(mysims, probs = upper_prob)
      
      sprintf("The %.0f%% credible interval for the point estimate is [%.3f, %.3f]",
              ci_level * 100,
              lower_bound,
              upper_bound)
    }
  })
  
  ################# End of Binomial-Beta################################################## 
  
  # ###################### poison Gamma ######################################
  output$gamma_priorPlot <- renderPlot({
    mu_0 <- input$mean
    sd_0 <- input$sd
    my_shape <- (mu_0^2)/(sd_0^2)
    my_rate <- mu_0/(sd_0^2)
    data <- seq(0, 5, length =50)
    prior_plot_data <- dgamma(data, shape = my_shape, rate = my_rate)
    prior_plot <- plot(data, prior_plot_data, type = "l", col = "blue")
    return(prior_plot)
  })
  
  output$gamma_postPlot <- renderPlot({
    mu_0 <- input$mean
    sd_0 <- input$sd
    n_obs <- input$n_obs
    cases_data <- as.numeric(strsplit(input$cases_per_data_point, ",")[[1]])
    
    validate(
      need(length(cases_data) == n_obs,
           "Error: Number of data points entered does not match 'Number of Data Points observed'")
    )
    
    my_shape <- (mu_0^2)/(sd_0^2)
    my_rate <- mu_0/(sd_0^2)
    
    shape_post <- my_shape + sum(cases_data)
    rate_post <- my_rate + n_obs
    
    data <- seq(0, 5, length =50)
    post_plot_data <- dgamma(data, shape = shape_post, rate = rate_post)
    post_plot <- plot(data, post_plot_data, type = "l", col = "red")
    return(post_plot)
  })
  ######################## End of poison Gamma ######################################
  
  ####################### normal #########################################
  
  
  ############ RJags Simulations ##########################
  
  output$model_inputs <- renderUI({
    if (input$mcmc_dist_type == "Binomial") {
      tagList(
        numericInput("bin_k", "Successes (k):", value = 456),
        numericInput("n_trails", "Number of trails (n)", value = 100),
        numericInput("prior_alpha", "Prior Alpha", value = 10),
        numericInput("prior_beta", "Prior Beta", value = 10)
      )
    } else if (input$mcmc_dist_type == "Normal") {
      tagList(
        numericInput("prior_mu", "Prior Mean:", value = 10),
        numericInput("prior_sd", "Prior Standard Deviation", value = 30)
      )
    } else if (input$mcmc_dist_type == "Poisson") {
      tagList(
        numericInput("prior_lambda", "Prior Lambda (Rate):", value = 5)
      )
    } else {
      # Show nothing if no model is selected
      NULL
    }
  })  
  
}