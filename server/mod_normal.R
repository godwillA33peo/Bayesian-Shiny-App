normalServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Calculate posterior parameters using Normal-Normal conjugate prior
    normal_data <- reactive({

      # User inputs
      x_bar <- input$normal_sample_mean      # Sample mean
      n <- input$normal_sample_size          # Sample size
      sigma <- input$normal_known_sd         # Known population SD
      mu0 <- input$normal_prior_mean         # Prior mean
      tau0 <- input$normal_prior_sd          # Prior SD

      # Validate inputs
      validate(
        need(n > 0, "Sample size must be positive"),
        need(sigma > 0, "Standard deviation must be positive"),
        need(tau0 > 0, "Prior standard deviation must be positive")
      )

      # Posterior calculations (Normal-Normal conjugate)
      # Precision = 1 / variance
      prior_precision <- 1 / (tau0^2)
      data_precision <- n / (sigma^2)

      # Posterior precision and variance
      post_precision <- prior_precision + data_precision
      post_variance <- 1 / post_precision
      post_sd <- sqrt(post_variance)

      # Posterior mean (weighted average of prior mean and sample mean)
      post_mean <- post_variance * (mu0 * prior_precision + n * x_bar * (1/sigma^2))

      # Create summary data frame
      summary_df <- data.frame(
        Distribution = c("Prior", "Likelihood", "Posterior"),
        Mean = c(mu0, x_bar, post_mean),
        SD = c(tau0, sigma/sqrt(n), post_sd)
      )

      return(list(
        summary = summary_df,
        prior_mean = mu0,
        prior_sd = tau0,
        post_mean = post_mean,
        post_sd = post_sd,
        sample_mean = x_bar,
        sample_se = sigma/sqrt(n)
      ))
    })

    # Summary table output
    output$summary_table <- renderTable({
      data <- normal_data()
      data$summary
    }, digits = 4)

    # Prior distribution plot
    output$prior_plot <- renderPlot({
      data <- normal_data()

      # Generate x values for plotting
      x_range <- c(
        min(data$prior_mean - 4*data$prior_sd, data$sample_mean - 4*data$sample_se),
        max(data$prior_mean + 4*data$prior_sd, data$sample_mean + 4*data$sample_se)
      )
      x_vals <- seq(x_range[1], x_range[2], length.out = 200)

      # Prior density
      prior_density <- dnorm(x_vals, mean = data$prior_mean, sd = data$prior_sd)

      # Plot prior
      plot_df <- data.frame(x = x_vals, y = prior_density)

      ggplot(plot_df, aes(x = x, y = y)) +
        geom_line(color = "blue", linewidth = 1.5) +
        labs(title = "Prior Distribution",
             x = "μ (Mean Parameter)",
             y = "Density") +
        theme_minimal(base_size = 14)
    })

    # Prior vs Posterior plot
    output$post_plot <- renderPlot({
      data <- normal_data()

      # Generate x values for plotting
      x_range <- c(
        min(data$prior_mean - 4*data$prior_sd, data$post_mean - 4*data$post_sd),
        max(data$prior_mean + 4*data$prior_sd, data$post_mean + 4*data$post_sd)
      )
      x_vals <- seq(x_range[1], x_range[2], length.out = 200)

      # Densities
      prior_density <- dnorm(x_vals, mean = data$prior_mean, sd = data$prior_sd)
      post_density <- dnorm(x_vals, mean = data$post_mean, sd = data$post_sd)

      # Create plot data
      plot_df <- data.frame(
        x = rep(x_vals, 2),
        density = c(prior_density, post_density),
        Distribution = rep(c("Prior", "Posterior"), each = length(x_vals))
      )

      ggplot(plot_df, aes(x = x, y = density, color = Distribution)) +
        geom_line(linewidth = 1.5) +
        scale_color_manual(values = c("Prior" = "blue", "Posterior" = "red")) +
        labs(title = "Prior vs Posterior Distribution",
             x = "μ (Mean Parameter)",
             y = "Density") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "top")
    })

    # Credible interval plot
    output$credible_interval_plot <- renderPlot({
      data <- normal_data()
      ci_level <- input$normal_ci_level

      # Calculate credible interval
      alpha <- 1 - ci_level
      lower <- qnorm(alpha/2, mean = data$post_mean, sd = data$post_sd)
      upper <- qnorm(1 - alpha/2, mean = data$post_mean, sd = data$post_sd)

      # Generate x values for plotting
      x_range <- c(data$post_mean - 4*data$post_sd, data$post_mean + 4*data$post_sd)
      x_vals <- seq(x_range[1], x_range[2], length.out = 200)
      post_density <- dnorm(x_vals, mean = data$post_mean, sd = data$post_sd)

      plot_df <- data.frame(x = x_vals, y = post_density)

      ggplot(plot_df, aes(x = x, y = y)) +
        geom_line(color = "red", linewidth = 1.5) +
        geom_vline(xintercept = c(lower, upper),
                   linetype = "dashed",
                   linewidth = 1,
                   color = "darkred") +
        geom_area(data = subset(plot_df, x >= lower & x <= upper),
                  aes(x = x, y = y),
                  fill = "red",
                  alpha = 0.3) +
        labs(title = paste0(ci_level*100, "% Credible Interval"),
             x = "μ (Mean Parameter)",
             y = "Density") +
        theme_minimal(base_size = 14)
    })

    # Credible interval text output
    output$credible_interval_text <- renderText({
      data <- normal_data()
      ci_level <- input$normal_ci_level

      alpha <- 1 - ci_level
      lower <- qnorm(alpha/2, mean = data$post_mean, sd = data$post_sd)
      upper <- qnorm(1 - alpha/2, mean = data$post_mean, sd = data$post_sd)

      sprintf("The %.0f%% credible interval is [%.3f, %.3f]",
              ci_level * 100, lower, upper)
    })

    # Point estimate output
    output$point_estimate <- renderText({
      data <- normal_data()
      sprintf("Posterior Mean (Point Estimate): %.4f\nPosterior SD: %.4f",
              data$post_mean, data$post_sd)
    })

  })
}