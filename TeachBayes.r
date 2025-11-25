spinner_plot <- function(probs, ...){
  args <- list(...)
  if("values" %in% names(args))
    values <- args$values else
      values <- seq_along(probs)
    Region <- factor(values)
    y <- probs
    df <- data.frame(Region, y)
    TH <- theme(
      plot.title = element_text(
        colour = "blue",
        size = 18,
        hjust = 0.5,
        vjust = 0.8,
        angle = 0
      )
    )
    p <- ggplot(df, aes(1, y, fill=Region)) +
      geom_bar(stat="identity") +
      coord_polar(theta = "y", direction=1) +
      xlab("") + ylab("") +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
    if ("title" %in% names(args))
      p <- p + ggtitle(args$title) + TH
    p
}

spinner_data <- function(regions, nsim=1000){
  sample(length(regions), size=nsim,
         prob=regions, replace=TRUE)
}

beta_interval <- function (prob, shape_par) 
{
  x <- seq(0, 1, by = 0.001)
  dx <- dbeta(x, shape_par[1], shape_par[2])
  p <- ggplot(data.frame(x = x, dx = dx), aes(x, dx)) + geom_line()
  interval <- qbeta(c((1 - prob)/2, 1 - (1 - prob)/2), shape_par[1], 
                    shape_par[2])
  lo <- interval[1]
  hi <- interval[2]
  LO <- max(0, lo)
  HI <- min(1, hi)
  x0 <- seq(LO, HI, by = 0.001)
  y0 <- dbeta(x0, shape_par[1], shape_par[2])
  xx <- c(LO, x0, HI, lo)
  yy <- c(0, y0, 0, 0)
  betaprob <- diff(pbeta(c(lo, hi), shape_par[1], shape_par[2]))
  message <- paste("P(", round(lo, 3), "< P < ", 
                   round(hi, 3), ") =", round(betaprob, 3))
  other_text <- paste("Beta(", shape_par[1], ", ", 
                      shape_par[2], ")", sep = "")
  x_text <- ifelse(shape_par[1] > shape_par[2], 0.15, 0.85)
  y_text <- 0.7 * max(dx)
  TH <- theme(plot.title = element_text(colour = "blue", 
                                        size = 18, hjust = 0.5, vjust = 0.8, angle = 0))
  p + geom_polygon(data = data.frame(xx, yy), aes(xx, yy), 
                   fill = "orange") + ggtitle(message) + xlab("P") + 
    ylab("Density") + TH + annotate("text", x = x_text, 
                                    y = y_text, label = other_text, size = 6, color = "blue")
}

beta_area <- function (lo, hi, shape_par) 
{
  x <- seq(0, 1, by = 0.001)
  dx <- dbeta(x, shape_par[1], shape_par[2])
  p <- ggplot(data.frame(x = x, dx = dx), aes(x, dx)) + geom_line()
  LO <- max(0, lo)
  HI <- min(1, hi)
  x0 <- seq(LO, HI, by = 0.001)
  y0 <- dbeta(x0, shape_par[1], shape_par[2])
  xx <- c(LO, x0, HI, lo)
  yy <- c(0, y0, 0, 0)
  betaprob <- diff(pbeta(c(lo, hi), shape_par[1], shape_par[2]))
  message <- paste("P(", round(lo, 3), "< P < ", 
                   round(hi, 3), ") =", round(betaprob, 3))
  other_text <- paste("Beta(", shape_par[1], ", ", 
                      shape_par[2], ")", sep = "")
  x_text <- ifelse(shape_par[1] > shape_par[2], 0.15, 0.85)
  y_text <- 0.7 * max(dx)
  TH <- theme(plot.title = element_text(colour = "blue", 
                                        size = 18, hjust = 0.5, vjust = 0.8, angle = 0))
  p + geom_polygon(data = data.frame(xx, yy), aes(xx, yy), 
                   fill = "orange") + ggtitle(message) + xlab("P") + 
    ylab("Density") + TH + annotate("text", x = x_text, 
                                    y = y_text, label = other_text, size = 6, color = "blue")
}

beta_draw <- function (shape_pars) 
{
  TH <- theme(plot.title = element_text(colour = "blue", 
                                        size = 18, hjust = 0.5, vjust = 0.8, angle = 0))
  Title <- paste("Beta(", shape_pars[1], ",", shape_pars[2], 
                 ") Curve")
  x <- NULL
  ggplot(data.frame(x = c(0, 1)), aes(x)) + stat_function(fun = dbeta, 
                                                          geom = "line", color = "red", size = 2.5, 
                                                          args = list(shape1 = shape_pars[1], shape2 = shape_pars[2])) + 
    ggtitle(Title) + TH + xlab("P") + ylab("Density")
}

beta_prior_post <- function (prior_shapes, post_shapes, label_1 = "Prior", 
                             label_2 = "Posterior") 
{
  TH <- theme(plot.title = element_text(colour = "blue", 
                                        size = 18, hjust = 0.5, vjust = 0.8, angle = 0))
  x <- NULL
  ggplot(data.frame(x = c(0, 1)), aes(x)) + stat_function(fun = dbeta, 
                                                          geom = "line", aes(color = label_1), size = 1.5, 
                                                          args = list(shape1 = prior_shapes[1], shape2 = prior_shapes[2])) + 
    stat_function(fun = dbeta, geom = "line", aes(color = label_2), 
                  size = 1.5, args = list(shape1 = post_shapes[1], 
                                          shape2 = post_shapes[2])) + scale_colour_manual(values = c("red", 
                                                                                                     "blue")) + labs(colour = "Type") + xlab("P") + 
    ylab("Density")
}

beta.select <- function (quantile1, quantile2) 
{
  betaprior1 = function(K, x, p) {
    m.lo = 0
    m.hi = 1
    flag = 0
    while (flag == 0) {
      m0 = (m.lo + m.hi)/2
      p0 = pbeta(x, K * m0, K * (1 - m0))
      if (p0 < p) 
        m.hi = m0
      else m.lo = m0
      if (abs(p0 - p) < 1e-04) 
        flag = 1
    }
    return(m0)
  }
  p1 = quantile1$p
  x1 = quantile1$x
  p2 = quantile2$p
  x2 = quantile2$x
  logK = seq(-3, 8, length = 100)
  K = exp(logK)
  m = sapply(K, betaprior1, x1, p1)
  prob2 = pbeta(x2, K * m, K * (1 - m))
  ind = ((prob2 > 0) & (prob2 < 1))
  app = approx(prob2[ind], logK[ind], p2)
  K0 = exp(app$y)
  m0 = betaprior1(K0, x1, p1)
  return(round(K0 * c(m0, (1 - m0)), 2))
}

bar_plot <- function (y, ...) 
{
  TH <- theme(plot.title = element_text(colour = "blue", 
                                        size = 18, hjust = 0.5, vjust = 0.8, angle = 0))
  Y <- NULL
  p <- ggplot(data.frame(Y = y), aes(Y)) + geom_bar(width = 0.5, 
                                                    fill = "red") + ylab("Frequency")
  if (nargs() == 2) 
    p <- p + ggtitle(...) + TH
  p
}

prob_plot <- function (d, Size = 10) 
{
  N <- dim(d)[1]
  Size <- 100/N
  Size <- ifelse(Size > 15, 15, Size)
  Size <- ifelse(Size < 2, 2, Size)
  p <- ggplot(d, aes_string(colnames(d)[1], colnames(d)[2])) + 
    geom_segment(aes_string(xend = colnames(d)[1], yend = 0), 
                 size = Size, lineend = "butt", color = "red")
  p
}

prior_post_plot <- function (d, ...) 
{
  N <- dim(d)[1]
  Size <- 100/N
  Size <- ifelse(Size > 15, 15, Size)
  Size <- ifelse(Size < 2, 2, Size)
  Model <- rep(d[, 1], 2)
  Probability <- c(d$Prior, d$Posterior)
  Type <- c(rep("Prior", N), rep("Posterior", N))
  D1 <- data.frame(Model, Probability, Type)
  p <- ggplot(D1, aes(Model, Probability, color = Type)) + 
    geom_segment(aes(xend = Model, yend = 0), size = Size, 
                 lineend = "butt") + facet_wrap(~Type, ncol = 1)
  if (nargs() == 2) 
    p <- p + xlab(...)
  p
}


many_normal_plots <- function (list_normal_par) 
{
  N <- length(list_normal_par)
  for (j in 1:N) list_normal_par[[j]] <- unlist(list_normal_par[[j]])
  Means <- sapply(list_normal_par, function(y) y[1])
  SDs <- sapply(list_normal_par, function(y) y[2])
  Means <- round(Means, 1)
  SDs <- round(SDs, 1)
  gmin <- min(Means - 4 * SDs)
  gmax <- max(Means + 4 * SDs)
  labels <- paste("N(", Means, ", ", SDs, ")", 
                  sep = "")
  x <- seq(gmin, gmax, length.out = 200)
  df <- NULL
  for (j in 1:N) {
    y <- x
    f <- dnorm(x, Means[j], SDs[j])
    Model <- labels[j]
    df <- rbind(df, data.frame(Model, y, f))
  }
  p <- ggplot(df, aes(y, f, group = Model, color = Model)) + 
    geom_line(size = 1.5)
  if (N == 2) {
    p <- p + scale_colour_manual(values = c("blue", 
                                            "red"))
  }
  p
}


normal_draw <- function (normal_pars) 
{
  normal_pars <- unlist(normal_pars)
  TH <- theme(plot.title = element_text(colour = "blue", 
                                        size = 18, hjust = 0.5, vjust = 0.8, angle = 0))
  Title <- paste("Normal(", normal_pars[1], ",", 
                 normal_pars[2], ") Curve")
  x_lo <- normal_pars[1] - 4 * normal_pars[2]
  x_hi <- normal_pars[1] + 4 * normal_pars[2]
  x <- NULL
  ggplot(data.frame(x = c(x_lo, x_hi)), aes(x)) + stat_function(fun = dnorm, 
                                                                geom = "line", color = "red", size = 2.5, 
                                                                args = list(mean = normal_pars[1], sd = normal_pars[2])) + 
    ggtitle(Title) + TH + ylab("Density")
}


normal_interval <- function (prob, normal_pars) 
{
  normal_pars <- unlist(normal_pars)
  x_lo <- normal_pars[1] - 4 * normal_pars[2]
  x_hi <- normal_pars[1] + 4 * normal_pars[2]
  x <- seq(x_lo, x_hi, length.out = 100)
  dx <- dnorm(x, normal_pars[1], normal_pars[2])
  p <- ggplot(data.frame(x = x, dx = dx), aes(x, dx)) + geom_line()
  interval <- qnorm(c((1 - prob)/2, 1 - (1 - prob)/2), normal_pars[1], 
                    normal_pars[2])
  lo <- interval[1]
  hi <- interval[2]
  LO <- max(x_lo, lo)
  HI <- min(x_hi, hi)
  x0 <- seq(LO, HI, length.out = 100)
  y0 <- dnorm(x0, normal_pars[1], normal_pars[2])
  xx <- c(LO, x0, HI, lo)
  yy <- c(0, y0, 0, 0)
  normprob <- diff(pnorm(c(lo, hi), normal_pars[1], normal_pars[2]))
  message <- paste("P(", round(lo, 3), "< M < ", 
                   round(hi, 3), ") =", round(normprob, 3))
  other_text <- paste("Normal(", normal_pars[1], ", ", 
                      normal_pars[2], ")", sep = "")
  x_text <- normal_pars[1] - 2.5 * normal_pars[2]
  y_text <- 0.8 * max(dx)
  TH <- theme(plot.title = element_text(colour = "blue", 
                                        size = 18, hjust = 0.5, vjust = 0.8, angle = 0))
  p + geom_polygon(data = data.frame(xx, yy), aes(xx, yy), 
                   fill = "orange") + ggtitle(message) + xlab("M") + 
    ylab("Density") + TH + annotate("text", x = x_text, 
                                    y = y_text, label = other_text, size = 6, color = "blue")
}


normal_update <- function (prior, data, teach = FALSE) 
{
  prior <- unlist(prior)
  prior_mean <- prior[1]
  prior_sd <- prior[2]
  data_mean <- data[1]
  data_se <- data[2]
  prior_precision <- 1/prior_sd^2
  data_precision <- 1/data_se^2
  post_precision <- prior_precision + data_precision
  post_sd <- sqrt(1/post_precision)
  post_mean <- weighted.mean(c(prior_mean, data_mean), c(prior_precision, 
                                                         data_precision))
  if (teach == TRUE) 
    data.frame(Type = c("Prior", "Data", "Posterior"), 
               Mean = c(prior_mean, data_mean, post_mean), Precision = c(prior_precision, 
                                                                         data_precision, post_precision), Stand_Dev = c(prior_sd, 
                                                                                                                        data_se, post_sd))
  else c(post_mean, post_sd)
}

bayesian_crank <- function(d){
  d$Product <- d$Likelihood * d$Prior
  d$Posterior <- d$Product / sum(d$Product)
  d
}