library(plotly)
# Define UI ----
ui <- fluidPage(
  titlePanel("Datras QC Tool"),
  sidebarLayout(
    sidebarPanel(
      titlePanel("Controls"),
      #helpText("Select the required parameters"),
      uiOutput("choose_haulSubset"),
      #helpText(paste("We are operating on cruise year")),
      uiOutput("list_hauls"),
      titlePanel("Legends"),
      helpText("Top Plot: Comparing catch weight per haul (sample condition index, K) with the estimated weight based on the lengths of the fish in the haul. In RED: the mean and the standard deviation of K for this specific cruise (based on the available historical data). Orange: Highlighted haul(s)"),
      helpText("Bottom Plot: ORANGE line: Length frequency distribution for selected haul from the top plot compared to the BLUE line: historical length frequency from the same station of the haul.")
    ),
    mainPanel(
      titlePanel("Main Panel"),
      # Main Plot
      plotlyOutput("plot_main"),
      plotOutput("plot_sub"),
      verbatimTextOutput("info")
    )
  )
)

