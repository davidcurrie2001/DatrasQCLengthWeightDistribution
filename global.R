library(shiny)
library(DATRAS)
library(plotly)

library(tidyverse)

DefaultText <- "Any"

# File names
AllDataFile <- "data/DATRAS_Exchange_Data.csv"
#filteredFile <- "data/filteredData.rds"
myFilters <- "data/myFilters.csv"

# Use the filters on the data supplied
FilterData<-function(allData,filtersToUse){
  
  # Filter the data using the selected values
  filteredData <- allData
  
  # Try and filter the data by all vlaues in filtersToUse 
  for (i in colnames(filtersToUse)){
    filteredData <- filterDataByParameter(filteredData,filtersToUse,i)
  }
  
  filteredData
  
}

# Use the filter values to subset the DATRAS data
filterDataByParameter <- function(dataToFilter,filtersToUse,paramName){
  
  selectedValue <- ''
  dataToReturn <- dataToFilter
  
  if (paramName %in% colnames(filtersToUse)){
    selectedValue <- as.character(filtersToUse[1,paramName])
    if (selectedValue != DefaultText){
      
      conditionToCheck <- paste("dataToReturn <- subset.DATRASraw(dataToFilter,",paramName,"=='",selectedValue, "')",sep = "")
      #print(conditionToCheck)
      # Need to use eval and parse so we can dynamically build the command, otherwise the values passed to ... in the subset.DATRASraw 
      # function will be taken literally
      eval(parse(text=conditionToCheck))
    }
  }
  
  dataToReturn
}


# Get Data
#dataInputFile <- "/home/user/Documents/IE_IGFS_LFs.csv"
#dataInputFile <- system.file("../extdata", "IE_IGFS_LFs.csv", package = "icesHackathon2018G3")
#dataInputFile <- "data/IE_IGFS_LFs.csv"
#print(dataInputFile)
#parsedData <- read_csv(dataInputFile)

# Assuming we operate on one haulSubsetType of quarters, cruise, ship, and countries
#maxYear <- max(unique(parsedData$Year))

# TODO fix this
#maxYear <- 2018

# Color
Current_color <- "#e69500"
Hist_color <- "#00c4e6"

# Get the last haul of the selected year
getLastHaul <- function(year, dataToUse) {
  max(getHauls(year, dataToUse))
}

# Get last list of of the selected year
getHauls <- function(year=1, dataToUse) {
  #print('getHauls')
  #print(paste("year",year))
  #print(paste("nrow(dataToUse)",nrow(dataToUse)))
  #print(head(dataToUse))
  
  if (year==-1){
    returnValue <- unique(dataToUse()[,]$HaulNo)
  } else {
    returnValue <- unique(dataToUse()[dataToUse()$Year==year,]$HaulNo)
  }
  
  
  #returnValue <- unique(dataToUse[dataToUse$Year==year,]$HaulNo)
  #print(returnValue)
  returnValue
}

# Helper function to choose the correct get haul function (above)
getHaulList <- function(haulSubsetType, dataToUse){
  if(haulSubsetType=="all")
    hauls <- getHauls(dataToUse=dataToUse) 
  if(haulSubsetType=="last"){
    #print(as.character(dataToUse$Year))
    maxYear <- max(unique(as.character(dataToUse()$Year)))
    #print(paste("maxYear",maxYear))
    hauls <- getLastHaul(year=maxYear,dataToUse=dataToUse)
  }
  return(hauls)
}



getCurrentStation <- function(year, haul, dataToUse) {
  if(length(haul)==0) return(NULL)
  stations <- unique(trimws(as.character(dataToUse()[dataToUse()$Year==year & dataToUse()$HaulNo==haul,]$StNo)))
  return(stations)
}

Brush_densityplot <- function(This_year, Last_haul, fish_data){

  if(length(Last_haul)==0) return(NULL)
 
  #print(This_year)

  #print(paste("ALast haul:", Last_haul))

  # Find the nearest hauls
  #xp <- unique(unlist(fish_data[fish_data$Year==This_year, "HaulNo"]))
  #if(Last_haul < min(xp)) Last_haul <- min(xp)
  #else if(Last_haul > max(xp)) Last_haul <- max(xp)
  #else Last_haul <- which.min(abs(xp - as.integer(Last_haul))) 
  
  #print(paste("BLast haul:", Last_haul))

  Current_Station <- getCurrentStation(This_year, Last_haul,fish_data)

  #print(Current_Station)

  #Last Haul refers to the brush point from the K plot. 
density_histor <- fish_data() %>% 
  filter(Year != This_year)%>%
  #filter(SpecCode == Fish_choice)%>% #Fish selection var,
  filter(StNo == Current_Station)#Current Station to match current haul location

density_histor <- as.data.frame(rep(density_histor$LngtClas, density_histor$HLNoAtLngt))
colnames(density_histor) <- "den_hist"

density_curr <- fish_data() %>% 
  filter(Year == This_year)%>%
  #filter(SpecCode == Fish_choice)%>%
  filter(HaulNo == Last_haul)

density_curr <- as.data.frame(rep(density_curr$LngtClas, density_curr$HLNoAtLngt))
colnames(density_curr) <- "den_curr"


plotOut <- ggplot()+
  #historical density distribution for selected or last station
  geom_density(aes(x = den_hist),color = Hist_color, size = 2,data = density_histor )+
  #current density distribution for selected or last station 
  geom_density(aes(x = den_curr ), color = Current_color,size = 2,
                 alpha = 0.6, data = density_curr)+
  xlab(label = "Length Class (mm)")+
  theme_bw()

return(plotOut)

}

K_plot <- function(fish_data, Fish_choice, This_year, Button_choice){
#In the future this is where the AB table would be linked
A <- -5.446 # intercept
B <- 3.1818 #slope of the fit

#Calculates the estimated wieght for fish
fish_stats <- fish_data() %>%
  #filter(SpecCode == Fish_choice)%>%
  mutate(Pred_Wt = HLNoAtLngt * (exp(A)*(LngtClas/10)^B))%>%
  group_by(Year,HaulNo,CatIdentifier)%>%
  summarise(K = sum(Pred_Wt) - mean(SubWgt),
            Num_fish = sum(HLNoAtLngt)) 

fish_stat <- fish_stats %>% filter(Year != This_year)  

fish_mean <- mean(fish_stat$K,na.rm = TRUE)

fish_CI <- sd(fish_stat$K, na.rm = TRUE)

#Plot
plotOut <- ggplot()+
  geom_hline(aes(yintercept = fish_mean), color = "red", size = 2, alpha = 0.6)+
  geom_hline(aes(yintercept = fish_mean - fish_CI),
             color = "red", size = 2, alpha = 0.6, linetype = "dotted")+
  geom_hline(aes(yintercept = fish_mean + fish_CI),
             color = "red", size = 2, alpha = 0.6, linetype = "dotted")+
  geom_point(aes(x = HaulNo, y = K, size = Num_fish),shape = 21,
             alpha = 0.5,color = Hist_color,fill = Hist_color,
             data = fish_stats %>% filter(Year == This_year)%>%
               filter(!HaulNo %in% Button_choice))+
  geom_point(aes(x = HaulNo, y = K, size = Num_fish),shape = 21,
             alpha = 0.9, color = Current_color,fill = Current_color,
             data = fish_stats %>% filter(Year == This_year) %>%
               filter(HaulNo %in% Button_choice))+
  scale_size_continuous(range = c(0.5,16), breaks = seq(0,max(fish_stats$Num_fish),
                                                      by = 100),
                        name = "Fish (n)")+
  xlab(label = "Haul No")+
  theme_bw()+
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"))
return(plotOut)

}

