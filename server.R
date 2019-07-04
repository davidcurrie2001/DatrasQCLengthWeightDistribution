# Define server logic ----
server <- function(input, output,session) {
  
  
  # Use reactive poll so that our data will be updated when the data/filteredData.rds is updated
  DataAndFilters <- reactivePoll(1000, session,
                                 # This function returns the time that files were last modified
                                 checkFunc = function() {
                                   myValue <- ''
                                   if (file.exists(AllDataFile)) {
                                     myValue <- paste(myValue , file.info(AllDataFile)$mtime[1])
                                   }
                                   if (file.exists(myFilters)) {
                                     myValue <- paste(myValue , file.info(myFilters)$mtime[1])
                                   }
                                   myValue
                                 },
                                 # This function returns the content the files
                                 valueFunc = function() {
                                   #print('Loading data')
                                   allData <- ''
                                   filters <- ''
                                   if (file.exists(AllDataFile)) {
                                     allData <- readICES(AllDataFile ,strict=TRUE)
                                   } 
                                   if (file.exists(myFilters)){
                                     filters <- read.csv(myFilters, header = TRUE)
                                   }
                                   list(allData,filters)
                                 }
  )
  

  # Reactive data
  myData<- reactive({
    
    d <-DataAndFilters()[[1]]
    f <-DataAndFilters()[[2]]
    
    dataToUse <- FilterData(d,f)
    
  })
  
  myUnfilteredData<- reactive({
    
    d <-DataAndFilters()[[1]]

  })
  
  # Reactive HL data
  HL<- reactive({
    myData()[["HL"]]
  })
  
  # Reactive HH data
  HH<- reactive({
    myData()[["HH"]]
  })
  
  # Reactive CA data
  CA<- reactive({
    myData()[["CA"]]
  })
  
  # Reactive HL data
  unfilteredHL<- reactive({
    myUnfilteredData()[["HL"]]
  })
  
  # Reactive HH data
  unfilteredHH<- reactive({
    myUnfilteredData()[["HH"]]
  })
  
  # Reactive CA data
  unfilteredCA<- reactive({
    myUnfilteredData()[["CA"]]
  })
  

  
  # Reactive max year
  maxYear<- reactive({
    
    max(unique(as.character(HL()$Year)))
    
  })

  # Radio button for the type of haul subset
  output$choose_haulSubset <- renderUI({
       radioButtons("haulSubset", "Highlight:",
               choiceNames = list(
                 "Last haul",
                 "Last day",
                 "All hauls"
               ),
               choiceValues = list(
                 "last", 
                 "lastday",
                 "all"
               ))
  })

  # Print out Hauls involved
  output$list_hauls <- renderUI({
    
      if(is.null(input[["haulSubset"]]))
        helpText("")
      else
        helpText(paste("Haul(s) involved are:", toString(getHaulList(input[["haulSubset"]],HLData=HL,HHData=HH))))
  })
  
  # Define for the plots
  output$plot_main <- renderPlotly({
    
    
    
    ggplotly(K_plot(This_year=maxYear(), Button_choice=getHaulList(input[["haulSubset"]],HLData=HL,HHData=HH),fish_data=HL, all_data=unfilteredHL ))

    
  })

  # Define for the sub-plots on-hover
  output$plot_sub <- renderPlot({
    

    ed <- event_data("plotly_hover")
    if (!is.null(ed))
        Brush_densityplot(maxYear(),  ed$x, HL, unfilteredHL)
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

    ed <- event_data("plotly_hover")
    if(!is.null(ed))
     paste0(
        "Hover status: ", xy_str(ed),
        "Haul selected:", ed$x
     )
    else
	"Please hover on a point"
  })


}

