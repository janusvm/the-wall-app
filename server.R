# App server code
# -------------------------------------------------------------------------


server <- function(input, output, session) {
  
  output$trans_dist_plot <- renderPlot({
    dat <- tibble(
      x = factor(xs <- sprintf("%+d", c(-5:-1, 1:5)), levels = xs),
      p = diff(pnorm(seq(-10, 10, by  = 2), sd = input$transition_sd)),
      lab = percent(p, accuracy = 0.1)
    )
    ggplot(dat) +
      aes(x, p) +
      geom_col() +
      geom_text(aes(y = p + 0.005, label = lab, vjust = 0), size = 5) +
      theme_minimal(base_size = 16) +
      theme(
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank()
      )
  })
  
  quiz_beta <- reactive(35^input$quiz_beta)
  quiz_prob <- function(x) pbeta(x, 1, quiz_beta())
  quiz_dat <- reactive(tibble(
    round = factor(r <- paste("Round", 1:3), levels = r),
    x = 1/2:4,
    p = quiz_prob(x),
    lab = percent(p, accuracy = 0.01)
  ))
  output$quiz_dist_plot <- renderPlot({
    ggplot(quiz_dat()) +
      aes(round, p) +
      geom_col() +
      geom_text(aes(y = p - 0.01, label = lab, vjust = 1), fontface = "bold", size = 8, colour = "white") +
      theme_minimal(base_size = 16) +
      theme(
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank()
      )
  })
}