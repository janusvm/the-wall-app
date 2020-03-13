# App UI code
# -------------------------------------------------------------------------


header <- dashboardHeader(title = "The Wall")

sidebar <- dashboardSidebar(
   sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "dashboard",
      column(
        width = 4,
        box(
          title = "Game Parameters", 
          width = NULL, collapsible = TRUE,
          sliderInput("transition_sd", "Std. deviation of transition distribution", min = 0.1, max = 5, value = 1, step = 0.1),
          sliderInput("quiz_beta", "Certainty of quiz answers", min = 0, max = 1, value = 0, step = 0.05),
          sliderInput("gamble_prob", "Tendency to double and triple", min = 0, max = 1, value = 0.5, step = 0.05),
          numericInput("n_sim", "Number of simulations", min = 1000L, value = 1000L, step = 1000L)
        ),
        box(
          title = "About the game",
          width = NULL, collapsible = TRUE, collapsed = TRUE,
          p("TODO")
        )
      ),
      column(
        width = 8,
        fluidRow(
          box(
            title = "Ball movement distribution",
            width = 6,
            plotOutput("trans_dist_plot", height = "300px")
          ),
          box(
            title = "Probability of correctly answering questions by round",
            width = 6,
            plotOutput("quiz_dist_plot", height = "300px")
          )
        )
      )
    )
  )
)
  
ui <- dashboardPage(header, sidebar, body)
