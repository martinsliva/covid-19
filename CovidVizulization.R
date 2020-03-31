library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
library(maps)
library(viridis)
library(gganimate)
library(ggthemes)
 

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


data_top_ten_deaths <- function(data, selected_day, number=10){
      ### return  top ten states and their death for given date
  
      data_last_day <- data %>% 
    
        filter(DateRep==selected_day) %>%
    
        select(Deaths, Countries) %>% 
    
        arrange(desc(Deaths))
  
      data_last_day_short <- data_last_day[1:number,]
  
      data_last_day_short$Countries <- factor(data_last_day_short$Countries,    # Factor levels in increasing order
                                              levels = data_last_day_short$Countries[order(data_last_day_short$Deaths)])
  
  
      return(data_last_day_short)
  
}




####### LOAD OR UPDATE DATA

WorldData <- map_data('world') %>% filter(region != "Antarctica")


setwd("C:/Users/Martin_Sliva/Documents/R/Covid-19/")

data_download = TRUE

if (data_download) { 
  
      source(paste0(getwd(),"/ReadCovidData.R")) 
  
      source(paste0(getwd(),"/ReadCountryData.R"))  

      #### BASIC DATA CLEANING 

      names(data)[7]<-"Countries"


      ## Removing "_" from country names
      data$Countries <- gsub("_", " ", data$Countries)


      countries <- unique(data$Countries)

      ## country names which are not in WorldData region names (definition of maps)
      problematic_countries<-countries[(!countries %in% unique(WorldData$region))]

      country_new_name<-c("Antigua", "British Virgin Islands","Brunei", "Cases on an international conveyance Japan", 
                    "Republic of Congo", "Ivory Coast", "Curacao", "Swaziland", "Gibraltar", "Guinea-Bissau","Vatican", 
                     "Macedonia", "Nevis", "Grenadines", "Timor-Leste",  
                    "Trinidad", "Turks and Caicos Islands", "UK", "Tanzania", "USA", "Virgin Islands" ) 

      for (i in 1:length(problematic_countries)){
              data[data$Countries==problematic_countries[i], ]$Countries <- country_new_name[i]
      }
      
      names(data)[1] <- "DateRep"
      names(data)[5] <- "Cases"
      names(data)[6] <- "Deaths"
      names(data)[10] <- "Popul"

}


source(paste0(getwd(),"/DataTotals.R"))


#### Settings




selected_day <- nth(unique(data$DateRep),1)
a_week_ago <- nth(unique(data$DateRep),7)

mainDir<-paste0(getwd(), "/pictures/")
subDir <- as.character(selected_day)

dir.create(file.path(mainDir, subDir))
setwd(file.path(mainDir, subDir))

scale_height <- 180

chart_width <- 1.5* scale_height
chart_heigth <- scale_height

map_width <- 2*scale_height
map_height <- scale_height



### Data for given country
 
selected_country<-"Czech Republic"


data_country<-data[data$Countries==selected_country & !data$Cases==0, ]


p <- ggplot(data = data_country)+aes(x=DateRep, y=Cases, fill= Cases) +
           geom_col()  +  
           labs( title = paste0("COVID-19 Cases by day, ",selected_country, "    ", selected_day), 
                 caption = "Data source: https://www.ecdc.europa.eu. Created in R, gglot2. Martin Slíva, cc") +
           scale_fill_continuous(type = "viridis") +
           theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  
 
print(p)


ggsave(paste0(selected_country, selected_day,".png"), width = chart_width, height = chart_heigth, units = "mm")



#### top 10 Cases for the day

data_for_chart <- data_top_ten_cases(data = data, selected_day = selected_day, 10)          




p <- ggplot() + 
     geom_col(data = data_for_chart, aes( x=Countries, y=Cases, fill=Countries)) + 
     coord_flip()+ theme(legend.position="none") + 
     labs(title = paste0("COVID-19 Top 10 Cases for ", selected_day),
          caption = "Data source: https://www.ecdc.europa.eu. Created in R, gglot2. Martin Slíva, cc")


print(p)


ggsave(paste0("Top_Cases_", selected_day,".png"), width = chart_width, height = chart_heigth, units = "mm")



#### top 10 Deaths for the day

data_for_chart <- data_top_ten_deaths(data = data, selected_day = selected_day, 10)          




p <- ggplot() + 
  geom_col(data = data_for_chart, aes( x=Countries, y=Deaths, fill=Countries)) + 
  coord_flip()+ theme(legend.position="none") + 
  labs(title = paste0("COVID-19 Top 10 Deaths for ", selected_day),
        caption = "Data source: https://www.ecdc.europa.eu. Created in R, gglot2. Martin Slíva, cc") + 
  theme(panel.background = element_rect(fill = "black", color  =  NA),
        panel.grid.major = element_line(color = "grey35"),  
        panel.grid.minor = element_line(color = "grey20"),  
        panel.spacing = unit(0.5, "lines")
        )


print(p)


ggsave(paste0("Top_Deaths_", selected_day,".png"), width = chart_width, height = chart_heigth, units = "mm")





###### Cases for today


data_for_map <- data %>% 
      
      filter(DateRep==selected_day) %>% filter(!(Cases == 0))




p <- ggplot() +
        geom_map(data = WorldData, map = WorldData,
               aes(x = long, y = lat, group = group, map_id=region),
               fill = "white", colour = "#7f7f7f", size=0.5) +
        geom_map(data = data_for_map, map = WorldData,
               aes(fill=log10(Cases), map_id=Countries)) +
       scale_fill_viridis_c(option = "B", direction = -1, name="") + 
       labs(title=paste0("COVID-19 Cases for day ", selected_day), 
               caption = "Data source: https://www.ecdc.europa.eu. Created in R, gglot2. Martin Slíva, cc",
               x="", y="") +
       theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())


print(p)
      

ggsave(paste0("Cases_", selected_day,".png"), width = map_width, height = map_height, units = "mm")


##########Average daily growth of cases


      
data_for_map_prep1 <- data %>%
                  
                  select(DateRep, Cases, Countries) %>%
  
                  filter(DateRep %in% a_week_ago:selected_day) %>%
                  
                  group_by(Countries) %>%
                
                  summarise(
                    
                    Days=n(),
                    
                    Weekly_Cases = sum(Cases)
                    
                  )

data_week_ago <- data %>% filter(DateRep==a_week_ago) %>% select(Countries, Cases)

data_week_ago[data_week_ago$Cases == 0,2] <- 1

data_for_map_prep2 <- merge(data_for_map_prep1, data_week_ago, Countries =Countries)

data_for_map_prep2 <- data_for_map_prep2 %>% filter(!Weekly_Cases == 0)

data_for_map_2 <- data_for_map_prep2 %>% mutate( Weekly_Growth = (Weekly_Cases/Cases)**(1/7)) %>% select(Countries, Weekly_Growth)






p <- ggplot() +
         geom_map(data = WorldData, map = WorldData,
            aes(x = long, y = lat, group = group, map_id=region),
            fill = "white", colour = "#7f7f7f", size=0.5) +
         geom_map(data = data_for_map_2, map = WorldData,
            aes(fill=Weekly_Growth*100, map_id=Countries)) +
         scale_fill_viridis_c(option = "B", direction = -1, name="") + 
         labs(title=paste("Average Daily Growth of Covid-19 Cases per Week (in %) to ",selected_day), 
              caption = "Data source: https://www.ecdc.europa.eu. Created in R, gglot2. Martin Slíva, cc",
              x="", y="") +
         theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())


print(p)


ggsave(paste0("Average_Growth_Cases", selected_day,".png"), width = map_width, height = map_height, units = "mm")




###  Death rate 

data_for_map_3 <- data %>% 
              group_by(Countries) %>%
              summarise(
                Total_Cases = sum(Cases),
                Total_Death = sum(Deaths),
                Population =  max(Popul, na.rm = TRUE),
                Death_Rate = Total_Death / Total_Cases,
                Cases_per_pop = Total_Cases / Population,
                Death_per_pop = Total_Death / Population
               ) %>%
              filter(Total_Cases > 25 )


###

p <- ggplot() +
        geom_map(data = WorldData, map = WorldData,
             aes(x = long, y = lat, group = group, map_id=region),
             fill = "white", colour = "#7f7f7f", size=0.5) +
        geom_map(data = data_for_map_3, map = WorldData,
             aes(fill=Death_Rate*100 , map_id=Countries)) +
       scale_fill_viridis_c(option = "B", direction = -1, name="") + 
       labs(title=paste("COVID-19 Death Rate (in %) to ",selected_day), 
             caption = "Data source: https://www.ecdc.europa.eu.Created in R, gglot2. Martin Slíva, cc", 
             subtitle = "Countries with more than 25 cases.",
             x="", y="" ) +
       theme(axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank())

print(p)

ggsave(paste0("Death_rate", selected_day,".png"), width = map_width, height = map_height, units = "mm")


### Cases per population

p <- ggplot() +
        geom_map(data = WorldData, map = WorldData,
             aes(x = long, y = lat, group = group, map_id=region),
             fill = "white", colour = "#7f7f7f", size=0.5) +
        geom_map(data = data_for_map_3, map = WorldData,
             aes(fill=log(Cases_per_pop,10) , map_id=Countries)) +
        scale_fill_viridis_c(option = "B", direction = -1, name="") + 
        labs(title=paste("COVID-19 Cases per Population (log10 scale) to ",selected_day), 
             caption = "Data source: https://www.ecdc.europa.eu.Created in R, gglot2. Martin Slíva, cc", 
             subtitle = "Countries with more than 25 cases.",
             x="", y="" ) +
        theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())

print(p)


ggsave(paste0("Cases_per_population", selected_day,".png"), width = map_width, height = map_height, units = "mm")



###  Death per population
 
p <- ggplot() +
        geom_map(data = WorldData, map = WorldData,
             aes(x = long, y = lat, group = group, map_id=region),
             fill = "white", colour = "#7f7f7f", size=0.5) +
       geom_map(data = data_for_map_3, map = WorldData,
             aes(fill=log(Death_per_pop, 10) , map_id=Countries)) +
       scale_fill_viridis_c(option = "B", direction = -1, name="") + 
       labs(title=paste("COVID-19 Death per Population (log10 scale) to ",selected_day), 
             caption = "Data source: https://www.ecdc.europa.eu.Created in R, gglot2. Martin Slíva, cc", 
             subtitle = "Countries with more than 25 cases.",
             x="", y="" ) +
       theme(axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank())

print(p)


ggsave(paste0("Death_per_population", selected_day,".png"), width = map_width, height = map_height, units = "mm")


#### Total Cases

data_for_map_4<- data_totals %>% filter(DateRep == selected_day) %>% filter(!(TotalCases == 0))

p <- ggplot() +
      geom_map(data = WorldData, map = WorldData,
               aes(x = long, y = lat, group = group, map_id=region),
               fill = "white", colour = "#7f7f7f", size=0.5) +
      geom_map(data = data_for_map_4, map = WorldData,
               aes(fill=log(TotalCases, 10) , map_id=Countries)) +
      scale_fill_viridis_c(option = "B", direction = -1, name="") + 
      labs(title=paste("COVID-19 Total Cases (log10 scale) to ",selected_day), 
            caption = "Data source: https://www.ecdc.europa.eu.Created in R, gglot2. Martin Slíva, cc", 
            x="", y="" ) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())

print(p)



