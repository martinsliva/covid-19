library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
library(maps)
library(viridis)
library(gganimate)
library(gifski)

#### FUNCTONS



data_top_ten_cases <- function(data, selected_day, number=10){
      ### return  top ten states and their cases cfor given date
      
      data_last_day <- data %>% 
            
            filter(DateRep==selected_day) %>%
            
            select(Cases, Countries) %>% 
            
            arrange(desc(Cases))
      
      data_last_day_short <- data_last_day[1:number,]
      
      data_last_day_short$Countries <- factor(data_last_day_short$Countries,    # Factor levels in increasing order
                                              levels = data_last_day_short$Countries[order(data_last_day_short$Cases)])
      
      
      return(data_last_day_short)
      
}


####### LOAD OR UPDATE DATA

WorldData <- map_data('world') %>% filter(region != "Antarctica")


data_download = FALSE

if (data_download) { 
      
      source(paste0(getwd(),"/ReadCovidData.R")) 
      
      #### BASIC DATA CLEANING 
      
      names(data)[7]<-"Countries"
      
      
      ## Removing "_" from country names
      data$Countries <- gsub("_", " ", data$Countries)
      
      
      countries <- unique(data$Countries)
      
      ## country names which are not in WorldData region names (definition of maps)
      problematic_countries<-countries[(!countries %in% unique(WorldData$region))]
      
      country_new_name<-c("Antigua", "Brunei", "Canada", "Cases on an international conveyance Japan", 
                          "Republic of Congo","Ivory Coast", "Swaziland", "Gibraltar", "Vatican", 
                          "Netherlands Antilles", "Macedonia", "Grenadines", "Timor-Leste",  
                          "Trinidad", "UK", "Tanzania", "USA" ) 
      
      for (i in 1:length(problematic_countries)){
            data[data$Countries==problematic_countries[i], ]$Countries <- country_new_name[i]
      }
      
}

### Data for given country

selected_country<-"Czech Republic"


data_country<-data[data$Countries==selected_country & !data$Cases==0, ]


ggplot(data = data_country)+aes(x=DateRep, y=log10(Cases+1)) + 
         geom_col() + 
         transition_time(DateRep) +
         labs(title = "Date: {frame_time}")





stop("Tady je konec!")


data_for_animation <- data[data$Countries %in% unique(WorldData$region),]


#### Animation test


cases_max<-max(data$Cases)

pa <- ggplot() +
      geom_map(data = WorldData, map = WorldData,
               aes(x = long, y = lat, group = group, map_id=region),
               fill = "white", colour = "#7f7f7f", size=0.5) +
      geom_map(data = data_for_animation, map = WorldData,
               aes(fill=log10(Cases+1), map_id=Countries)) +
      scale_fill_viridis_c(option = "A", direction = -1, limits=c(0,cases_max)) + 
      
      
      pa1 <-  pa + transition_time(DateRep) +
      labs(title = "Date: {frame_time}")

