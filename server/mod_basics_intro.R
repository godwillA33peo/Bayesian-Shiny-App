BAYES_PEOPLE <- list(
  list(who = "Neutral", a = 1, b = 1, label = "Neutral: Beta(1, 1), no idea"),
  list(who = "Sceptic", a = 6, b = 14, label = "Sceptic: Beta(6, 14), expects ~30%"),
  list(who = "Enthusiast", a = 14, b = 6, label = "Enthusiast: Beta(14, 6), expects ~70%")
)

basicsIntroServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ---- screening test (Bayes' rule with natural frequencies) ----------------------
    screen <- reactive({
      N <- 10000
      prev <- input$prev / 100; sens <- input$sens / 100; spec <- input$spec / 100
      sick <- prev * N; well <- N - sick
      tp <- sens * sick; fn <- sick - tp; fp <- (1 - spec) * well; tn <- well - fp
      list(N = N, sick = sick, well = well, tp = tp, fn = fn, fp = fp, tn = tn,
           ppv = tp / (tp + fp), npv = tn / (tn + fn))
    })

    output$test_tiles <- renderUI({
      s <- screen()
      div(class = "pl-tiles",
          pl_tile(pl_pct(s$ppv), "P(has the condition | tests positive)"),
          pl_tile(pl_pct(s$fp / (s$tp + s$fp)), "Share of positive results that are false alarms"),
          pl_tile(pl_pct(s$npv, 2), "P(free of it | tests negative)"))
    })

    output$test_plot <- renderPlot({
      s <- screen()
      df <- data.frame(
        group = factor(rep(c("Tested positive", "Tested negative"), each = 2), levels = c("Tested negative", "Tested positive")),
        status = factor(rep(c("Has the condition", "Does not"), 2), levels = c("Does not", "Has the condition")),
        n = c(s$tp, s$fp, s$fn, s$tn))
      df$share <- ave(df$n, df$group, FUN = function(v) v / sum(v))
      df$lab <- ifelse(df$share > 0.08, paste0(format(round(df$n), big.mark = ","), " people"), "")
      ggplot(df, aes(x = share, y = group, fill = status)) +
        geom_col(width = 0.62, colour = "white", linewidth = 0.6) +
        geom_text(aes(label = lab), position = position_stack(vjust = 0.5), colour = "white", size = 3.8, fontface = "bold") +
        scale_fill_manual(values = c(`Has the condition` = PL_COLS[["Posterior"]], `Does not` = PL_COLS[["Prior"]]),
                          breaks = c("Has the condition", "Does not")) +
        scale_x_continuous(labels = function(b) paste0(100 * b, "%"), expand = expansion(mult = c(0, 0.03))) +
        labs(x = NULL, y = NULL, title = sprintf("Out of %s people tested", format(s$N, big.mark = ","))) +
        pl_theme(12) + theme(panel.grid.major.y = element_blank())
    }, res = 96)

    output$test_text <- renderUI({
      s <- screen()
      f <- function(v) format(round(v), big.mark = ",")
      p(sprintf("Out of %s people, about %s have the condition and %s of them test positive. Of the %s without it, %s still test positive. So of the %s positive results, only %s come from people who have it: %s.",
                f(s$N), f(s$sick), f(s$tp), f(s$well), f(s$fp), f(s$tp + s$fp), f(s$tp), pl_pct(s$ppv)),
        br(),
        span(class = "pl-note", "In Bayes’ terms: how common the condition is is the prior, the test’s accuracy is the likelihood, and the chance of having it after a positive result is the posterior. When the condition is rare, even a good test gives many false alarms."))
    })

    # ---- sequential updating ------------------------------------------------------------
    set.seed(903)    # a typical sequence: early noise, then settles near the true rate
    u <- runif(100)  # fixed, so moving the true rate changes the results smoothly

    seq_state <- reactive({
      outcomes <- as.integer(u < input$truth)
      n <- input$n
      k <- if (n > 0) sum(outcomes[seq_len(n)]) else 0
      list(n = n, k = k, people = lapply(BAYES_PEOPLE, function(pp) c(pp, a_post = pp$a + k, b_post = pp$b + n - k)))
    })

    output$seq_plot <- renderPlot({
      st <- seq_state()
      x <- seq(0.0025, 0.9975, length.out = 400)
      labs_ <- vapply(BAYES_PEOPLE, `[[`, "", "label")
      df <- do.call(rbind, lapply(st$people, function(pp) data.frame(x = x, y = dbeta(x, pp$a_post, pp$b_post), who = pp$label)))
      df$who <- factor(df$who, levels = labs_)
      cap <- 1.08 * max(df$y)
      ggplot(df, aes(x, y, colour = who, linetype = who)) +
        geom_vline(xintercept = input$truth, colour = PL_INK, linetype = "dotted") +
        geom_line(linewidth = 1.2) +
        scale_colour_manual(values = setNames(unname(PL_SLOT_COLS[1:3]), labs_)) +
        scale_linetype_manual(values = setNames(unname(PL_SLOT_LTY[1:3]), labs_)) +
        scale_x_continuous(labels = function(b) paste0(100 * b, "%"), breaks = seq(0, 1, 0.25)) +
        coord_cartesian(ylim = c(0, cap)) +
        labs(x = "Pass rate", y = "Belief (density)", subtitle = "Dotted line: the true pass rate",
             title = if (st$n == 0) "Before any data: three different priors" else sprintf("After %d students (%d passed)", st$n, st$k)) +
        pl_theme(13) + guides(colour = guide_legend(nrow = 2))
    }, res = 96)

    output$seq_table <- renderTable({
      st <- seq_state()
      do.call(rbind, lapply(st$people, function(pp) {
        q <- qbeta(c(0.025, 0.975), pp$a_post, pp$b_post)
        data.frame(Person = pp$who, Prior = sprintf("Beta(%s, %s)", pp$a, pp$b),
                   "Posterior mean" = pl_pct(pp$a_post / (pp$a_post + pp$b_post), 0),
                   "95% credible interval" = sprintf("%s to %s", pl_pct(q[1], 0), pl_pct(q[2], 0)),
                   check.names = FALSE)
      }))
    }, striped = TRUE, align = "l", class = "pl-table")

    output$seq_text <- renderUI({
      st <- seq_state()
      means <- vapply(st$people, function(pp) pp$a_post / (pp$a_post + pp$b_post), 0)
      gap <- diff(range(means))
      div(class = "pl-diag ok",
          span(class = "tag", if (st$n == 0) "Start:" else sprintf("After %d students:", st$n)),
          sprintf("the three posterior means are %s points apart. ", round(100 * gap)),
          if (st$n == 0) "Press play (▶ under the slider) to add students one at a time." else
            "Each new result updates the previous posterior, which acts as the prior for the next student. More data, less disagreement.")
    })
  })
}
