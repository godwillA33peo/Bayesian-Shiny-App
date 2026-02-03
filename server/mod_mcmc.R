mcmcServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Reactive value to store uploaded data
    uploaded_data <- reactive({
      req(input$mcmc_data_file)

      tryCatch({
        df <- read.csv(input$mcmc_data_file$datapath)
        return(df)
      }, error = function(e) {
        return(NULL)
      })
    })

    # Display uploaded data preview
    output$data_preview <- renderTable({
      data <- uploaded_data()
      if (!is.null(data)) {
        head(data, 10)
      } else {
        data.frame(Message = "No data uploaded yet. Please upload a CSV file.")
      }
    }, striped = TRUE, hover = TRUE)

    # Dynamic UI for model-specific parameters
    output$model_inputs <- renderUI({
      ns <- session$ns

      model <- input$mcmc_model_type
      data <- uploaded_data()

      # Get column names if data is uploaded
      col_choices <- if (!is.null(data)) names(data) else c("Upload data first" = "")

      if (model == "Binomial") {
        tagList(
          h4("Binomial Model Parameters"),
          selectInput(ns("mcmc_binom_success_col"), "Success Column:", choices = col_choices),
          selectInput(ns("mcmc_binom_trials_col"), "Trials Column:", choices = col_choices),
          helpText("Prior: Beta(α, β)"),
          numericInput(ns("mcmc_binom_alpha"), "Prior α (shape1):", value = 1, min = 0.1),
          numericInput(ns("mcmc_binom_beta"), "Prior β (shape2):", value = 1, min = 0.1)
        )
      } else if (model == "Poisson") {
        tagList(
          h4("Poisson Model Parameters"),
          selectInput(ns("mcmc_poisson_count_col"), "Count Column:", choices = col_choices),
          helpText("Prior: Gamma(α, β)"),
          numericInput(ns("mcmc_poisson_alpha"), "Prior α (shape):", value = 2, min = 0.1),
          numericInput(ns("mcmc_poisson_beta"), "Prior β (rate):", value = 1, min = 0.1)
        )
      } else if (model == "Normal") {
        tagList(
          h4("Normal Model Parameters"),
          selectInput(ns("mcmc_normal_data_col"), "Data Column:", choices = col_choices),
          helpText("Estimating mean μ with known variance"),
          numericInput(ns("mcmc_normal_known_var"), "Known Variance (σ²):", value = 1, min = 0.1),
          numericInput(ns("mcmc_normal_prior_mean"), "Prior Mean (μ₀):", value = 0),
          numericInput(ns("mcmc_normal_prior_var"), "Prior Variance (τ₀²):", value = 10, min = 0.1)
        )
      }
    })

    # Metropolis-Hastings MCMC sampler
    run_mcmc <- eventReactive(input$run_mcmc, {
      req(input$mcmc_model_type)
      req(uploaded_data())

      data <- uploaded_data()
      model_type <- input$mcmc_model_type
      n_iter <- input$mcmc_iterations
      burnin <- input$mcmc_burnin
      thin <- input$mcmc_thin
      n_chains <- input$n_chains

      # Run multiple chains
      all_chains <- lapply(1:n_chains, function(chain) {

        if (model_type == "Binomial") {
          # Binomial model
          req(input$mcmc_binom_success_col, input$mcmc_binom_trials_col)

          successes <- sum(data[[input$mcmc_binom_success_col]], na.rm = TRUE)
          trials <- sum(data[[input$mcmc_binom_trials_col]], na.rm = TRUE)
          alpha_prior <- input$mcmc_binom_alpha
          beta_prior <- input$mcmc_binom_beta

          # Posterior is Beta(α + successes, β + trials - successes)
          samples <- rbeta(n_iter, alpha_prior + successes, beta_prior + trials - successes)

          return(data.frame(
            iteration = 1:n_iter,
            parameter = samples,
            chain = chain
          ))

        } else if (model_type == "Poisson") {
          # Poisson model
          req(input$mcmc_poisson_count_col)

          counts <- data[[input$mcmc_poisson_count_col]]
          counts <- counts[!is.na(counts)]
          total_count <- sum(counts)
          n_obs <- length(counts)
          alpha_prior <- input$mcmc_poisson_alpha
          beta_prior <- input$mcmc_poisson_beta

          # Posterior is Gamma(α + total_count, β + n_obs)
          samples <- rgamma(n_iter, alpha_prior + total_count, beta_prior + n_obs)

          return(data.frame(
            iteration = 1:n_iter,
            parameter = samples,
            chain = chain
          ))

        } else if (model_type == "Normal") {
          # Normal model (estimating mean with known variance)
          req(input$mcmc_normal_data_col)

          obs_data <- data[[input$mcmc_normal_data_col]]
          obs_data <- obs_data[!is.na(obs_data)]
          n_obs <- length(obs_data)
          sample_mean <- mean(obs_data)

          known_var <- input$mcmc_normal_known_var
          prior_mean <- input$mcmc_normal_prior_mean
          prior_var <- input$mcmc_normal_prior_var

          # Posterior calculations
          post_var <- 1 / (1/prior_var + n_obs/known_var)
          post_mean <- post_var * (prior_mean/prior_var + n_obs*sample_mean/known_var)

          # Sample from posterior Normal
          samples <- rnorm(n_iter, post_mean, sqrt(post_var))

          return(data.frame(
            iteration = 1:n_iter,
            parameter = samples,
            chain = chain
          ))
        }
      })

      # Combine all chains
      all_samples <- do.call(rbind, all_chains)

      # Apply burn-in and thinning
      all_samples <- all_samples %>%
        filter(iteration > burnin) %>%
        filter((iteration - burnin) %% thin == 0)

      return(all_samples)
    })

    # Trace plot
    output$trace_plot <- renderPlot({
      validate(
        need(input$run_mcmc > 0, "Click 'Run MCMC' to generate trace plot")
      )

      samples <- run_mcmc()

      ggplot(samples, aes(x = iteration, y = parameter, color = factor(chain))) +
        geom_line(alpha = 0.7) +
        labs(title = "MCMC Trace Plot",
             x = "Iteration",
             y = "Parameter Value",
             color = "Chain") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "top")
    })

    # Density plot
    output$density_plot <- renderPlot({
      validate(
        need(input$run_mcmc > 0, "Click 'Run MCMC' to generate density plot")
      )

      samples <- run_mcmc()

      ggplot(samples, aes(x = parameter, fill = factor(chain))) +
        geom_density(alpha = 0.5) +
        labs(title = "Posterior Density Plot",
             x = "Parameter Value",
             y = "Density",
             fill = "Chain") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "top")
    })

    # Combined density plot (all chains together)
    output$combined_density_plot <- renderPlot({
      validate(
        need(input$run_mcmc > 0, "Click 'Run MCMC' to generate combined density plot")
      )

      samples <- run_mcmc()

      ggplot(samples, aes(x = parameter)) +
        geom_density(fill = "steelblue", alpha = 0.6, linewidth = 1) +
        geom_vline(xintercept = mean(samples$parameter),
                   linetype = "dashed",
                   color = "red",
                   linewidth = 1) +
        labs(title = "Combined Posterior Density",
             x = "Parameter Value",
             y = "Density",
             caption = "Red line = Posterior Mean") +
        theme_minimal(base_size = 14)
    })

    # Autocorrelation plot
    output$autocorr_plot <- renderPlot({
      validate(
        need(input$run_mcmc > 0, "Click 'Run MCMC' to generate autocorrelation plot")
      )

      samples <- run_mcmc()

      # Calculate autocorrelation for first chain
      chain1 <- samples %>% filter(chain == 1) %>% pull(parameter)

      if (length(chain1) > 10) {
        acf_result <- acf(chain1, plot = FALSE, lag.max = 50)
        acf_df <- data.frame(
          lag = acf_result$lag,
          acf = acf_result$acf
        )

        ggplot(acf_df, aes(x = lag, y = acf)) +
          geom_hline(yintercept = 0, linetype = "solid", color = "gray") +
          geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf)) +
          geom_hline(yintercept = c(-1.96/sqrt(length(chain1)), 1.96/sqrt(length(chain1))),
                     linetype = "dashed", color = "blue") +
          labs(title = "Autocorrelation Plot (Chain 1)",
               x = "Lag",
               y = "Autocorrelation") +
          theme_minimal(base_size = 14)
      }
    })

    # Summary statistics
    output$mcmc_summary <- renderPrint({
      validate(
        need(input$run_mcmc > 0, "Click 'Run MCMC' to generate summary statistics")
      )

      samples <- run_mcmc()

      cat("MCMC Summary Statistics\n")
      cat("=======================\n\n")

      cat("Total samples (after burn-in & thinning):", nrow(samples), "\n")
      cat("Number of chains:", max(samples$chain), "\n\n")

      cat("Posterior Mean:", round(mean(samples$parameter), 4), "\n")
      cat("Posterior Median:", round(median(samples$parameter), 4), "\n")
      cat("Posterior SD:", round(sd(samples$parameter), 4), "\n\n")

      # Credible intervals
      ci_95 <- quantile(samples$parameter, probs = c(0.025, 0.975))
      ci_90 <- quantile(samples$parameter, probs = c(0.05, 0.95))

      cat("95% Credible Interval: [", round(ci_95[1], 4), ",", round(ci_95[2], 4), "]\n")
      cat("90% Credible Interval: [", round(ci_90[1], 4), ",", round(ci_90[2], 4), "]\n\n")

      # Effective sample size (rough estimate)
      by_chain <- samples %>%
        group_by(chain) %>%
        summarise(
          mean = mean(parameter),
          sd = sd(parameter),
          .groups = "drop"
        )

      cat("\nPer-Chain Statistics:\n")
      print(by_chain)
    })

  })
}