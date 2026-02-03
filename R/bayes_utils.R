# ============================================================================
# Bayesian Utility Functions
# ============================================================================
# These functions were extracted from the TeachBayes package to avoid
# deployment issues with shinyapps.io (TeachBayes is not on CRAN).
# Original source: https://github.com/bayesball/TeachBayes
# ============================================================================

# Load required libraries
library(ggplot2)

#' Draw a Beta Distribution Curve
#'
#' @param shape_pars A vector of two parameters: c(alpha, beta)
#' @return A ggplot object showing the beta density curve
beta_draw <- function(shape_pars){
  TH <- theme(
    plot.title = element_text(
      colour = "blue",
      size = 18,
      hjust = 0.5,
      vjust = 0.8,
      angle = 0
    )
  )
  Title <- paste("Beta(", shape_pars[1], ",",
                 shape_pars[2], ") Curve")
  x <- NULL
  ggplot(data.frame(x=c(0, 1)), aes(x)) +
    stat_function(fun=dbeta, geom="line",
                  color="red", size=2.5,
                  args=list(shape1=shape_pars[1],
                            shape2=shape_pars[2])) +
      ggtitle(Title) + TH +
     xlab("P") + ylab("Density")
}

#' Plot Beta Credible Interval
#'
#' @param prob Probability level for the credible interval (e.g., 0.95)
#' @param shape_par A vector of two parameters: c(alpha, beta)
#' @param Color Fill color for the credible interval region
#' @return A ggplot object showing the beta density with shaded credible interval
beta_interval <- function(prob, shape_par,
                          Color = "orange"){
  x <- seq(0, 1, by=.001)
  dx <- dbeta(x, shape_par[1], shape_par[2])
  p <- ggplot(data.frame(x=x, dx=dx), aes(x, dx)) +
    geom_line()
  interval <- qbeta(c((1 - prob) / 2, 1 - (1 - prob) / 2),
                    shape_par[1], shape_par[2])
  lo <- interval[1]
  hi <- interval[2]
  LO <- max(0, lo)
  HI <- min(1, hi)
  x0 <- seq(LO, HI, by=.001)
  y0 <- dbeta(x0, shape_par[1], shape_par[2])
  xx <- c(LO, x0, HI, lo)
  yy <- c(0, y0, 0, 0)
  betaprob <- diff(pbeta(c(lo, hi),
                         shape_par[1], shape_par[2]))
  message <- paste("P(",
                   round(lo, 3), "< P < ", round(hi, 3), ") =",
                   round(betaprob, 3))
  other_text <- paste("Beta(",shape_par[1], ", ",
                      shape_par[2], ")", sep="")

  x_text <- ifelse(shape_par[1] > shape_par[2],
                   .15, .85)
  y_text <- .70 * max(dx)
  TH <- theme(
    plot.title = element_text(
      colour = "blue",
      size = 18,
      hjust = 0.5,
      vjust = 0.8,
      angle = 0
    )
  )
  p + geom_polygon(data=data.frame(xx, yy), aes(xx, yy),
                   fill=Color)  +
    ggtitle(message) +
    xlab("P") + ylab("Density") + TH +
    annotate("text", x = x_text, y = y_text,
             label = other_text,
             size=6, color="blue")
}

#' Plot Prior and Posterior Beta Distributions
#'
#' @param prior_shapes A vector of prior beta parameters: c(alpha, beta)
#' @param post_shapes A vector of posterior beta parameters: c(alpha, beta)
#' @param label_1 Label for the prior distribution
#' @param label_2 Label for the posterior distribution
#' @return A ggplot object showing both distributions
beta_prior_post <- function(prior_shapes, post_shapes,
                            label_1="Prior",
                            label_2="Posterior"){
  TH <- theme(
    plot.title = element_text(
      colour = "blue",
      size = 18,
      hjust = 0.5,
      vjust = 0.8,
      angle = 0
    )
  )
x <- NULL
ggplot(data.frame(x=c(0, 1)), aes(x)) +
  stat_function(fun=dbeta, geom="line",
                aes(linetype=label_1), size=1.5,
                args=list(shape1=prior_shapes[1],
                          shape2=prior_shapes[2])) +
  stat_function(fun=dbeta, geom="line",
                aes(linetype=label_2), size=1.5,
                args=list(shape1=post_shapes[1],
                          shape2=post_shapes[2])) +
  scale_colour_manual(values=c("red", "blue")) +
  labs(colour = "Type") +
  xlab("P") + ylab("Density")
}

#' Plot Prior and Posterior for Discrete Distributions
#'
#' @param d Data frame with columns: Theta, Prior, Posterior
#' @param Color Color for the segments
#' @return A ggplot object with faceted prior and posterior plots
prior_post_plot <- function(d, Color = "orange"){
   N <- dim(d)[1]
   Size <- 100 / N
   Size <- ifelse(Size > 15, 15, Size)
   Size <- ifelse(Size < 2, 2, Size)
   Model <- rep(d[, 1], 2)
   Probability <- c(d$Prior, d$Posterior)
   Type <- c(rep("Prior", N), rep("Posterior", N))
   D1 <- data.frame(Model, Probability, Type)
   ggplot(D1, aes(Model, Probability)) +
     geom_segment(aes(xend = Model, yend = 0), size = Size,
               lineend = "butt", color = Color) +
     facet_wrap(~ Type, ncol=1) +
      theme(legend.position = "none")
}
