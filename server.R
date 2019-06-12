# Define server logic ----
server <- function(input, output) {
  getSpecies <- function(haulSubsetType="lenWt"){
         if(is.null(input[["haulSubset"]]))
	   return(list("None"))
         else
	   return(as.list(getSpeciesList(input[["haulSubset"]])))
  }
  # Radio button for the type of haul subset
  output$choose_haulSubset <- renderUI({
       radioButtons("haulSubset", "Highlight:",
               choiceNames = list(
                 "Last haul",
                 "All hauls (Stand in for 24 hours)"
               ),
               choiceValues = list(
                 "last", "all"
               ))
  })
  # Drop-down selection box for which data set
  output$choose_species <- renderUI({
      selectInput("species", "Species", choices=getSpecies("lenWt"), selected=1)
  })
  # Print out Hauls involved
  output$list_hauls <- renderUI({
      if(is.null(input[["haulSubset"]]))
        helpText("")
      else
        helpText(paste("Haul(s) involved are:", toString(getHaulList(input[["haulSubset"]]))))
  })
  
  # Define for the plots
  output$plot_main <- renderPlotly({
    ggplotly(K_plot(parsedData, input$species, maxYear, getHaulList(input[["haulSubset"]])))
  })

  # Define for the sub-plots on-hover
  output$plot_sub <- renderPlot({
    d <- event_data("plotly_hover")
    if (!is.null(d))
        Brush_densityplot(maxYear, input$species, d$x, parsedData)
  })

  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
       paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }

    d <- event_data("plotly_hover")
    if(!is.null(d))
     paste0(
        "Hover status: ", xy_str(d),
        "Haul selected:", d$x
     )
    else
	"Please hover on a point"
  })


}

