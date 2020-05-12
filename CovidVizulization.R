library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
library(maps)
library(viridis)
library(ggthemes)
 

#### FUNCTIONS

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


print_and_save_map <- function(data_to_display, for_day, chart_title, chart_subtitle ="", chart_save_name){
      ###  data_to_display should have two collumns Countries and Values
      ###  
  
  
      p <- ggplot() +
           geom_map(data = WorldData, map = WorldData,
                    aes(x = long, y = lat, group = group, map_id=region),
                    fill = "white", colour = "#7f7f7f", size=0.5) +
           geom_map(data = data_to_display, map = WorldData,
                    aes(fill=Values , map_id=Countries)) +
           scale_fill_viridis_c(option = "B", direction = -1, name="") + 
           labs(title=paste(chart_title ,for_day), 
                   caption = "Data source: https://www.ecdc.europa.eu.Created in R, gglot2. Martin Slíva, cc", 
                   subtitle = chart_subtitle,
                   x="", y="" ) +
           theme(axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())

           print(p)


           ggsave(paste0(chart_save_name, for_day,".png"), width = map_width, height = map_height, units = "mm")


}


####### LOAD OR UPDATE DATA

WorldData <- map_data('world') %>% filter(region != "Antarctica")

 
setwd("C:/Users/Martin_Sliva/Documents/R/Covid-19/") ##working dir on my laptop

data_download = TRUE

if (data_download) { 
  
      source(paste0(getwd(),"/ReadCovidData.R")) 
  
      source(paste0(getwd(),"/ReadCountryData.R"))  
       
      source("DataCleaning.R")
  
  
}

### Sums Cases and deaths and creates data_totals tibble - TO DO refactor to function 
source(paste0(getwd(),"/DataTotals.R"))


#### Settings

output_dir <- "C:/Users/Martin_Sliva/Documents/R/_outputs" # output dir, main


selected_day <- nth(unique(data$DateRep),1)
a_week_ago <- nth(unique(data$DateRep),7)

mainDir<-paste0(output_dir, "/pictures")  
subDir <- as.character(selected_day)  

dir.create(file.path(mainDir, subDir))    #creates dir for processed date
setwd(file.path(mainDir, subDir))

scale_height <- 180    #setting output files height

chart_width <- 1.5* scale_height
chart_heigth <- scale_height

map_width <- 2*scale_height
map_height <- scale_height


### Chart for Czech republic



selected_country<-"Czech Republic"


data_country<-data[data$Countries==selected_country & !data$Cases==0, ]


p <- ggplot(data = data_country)+aes(x=DateRep, y=Cases, fill= Cases) +
  geom_col()  +  
  labs( title = paste0("COVID-19 Cases by day, ",selected_country, "    ", selected_day), 
        caption = "Data source: https://www.ecdc.europa.eu. Created in R, gglot2. Martin Slíva, cc") +
  scale_fill_continuous(type = "viridis") +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) 



print(p)


ggsave(paste0(selected_country, selected_day,".png"), width = chart_width, height = chart_heigth, units = "mm")



### Chart for France



selected_country<-"France"


data_country<-data[data$Countries==selected_country & !data$Cases==0, ]


p <- ggplot(data = data_country)+aes(x=DateRep, y=Cases, fill= Cases) +
  geom_col()  +  
  labs( title = paste0("COVID-19 Cases by day, ",selected_country, "    ", selected_day), 
        caption = "Data source: https://www.ecdc.europa.eu. Created in R, gglot2. Martin Slíva, cc") +
  scale_fill_continuous(type = "viridis") +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) 



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






### Charts for countrie in top 10 cases

for (i in data_for_chart$Countries){


      selected_country<-i


      data_country<-data[data$Countries==selected_country & !data$Cases==0, ]


      p <- ggplot(data = data_country)+aes(x=DateRep, y=Cases, fill= Cases) +
              geom_col()  +  
              labs( title = paste0("COVID-19 Cases by day, ",selected_country, "    ", selected_day), 
                    caption = "Data source: https://www.ecdc.europa.eu. Created in R, gglot2. Martin Slíva, cc") +
              scale_fill_continuous(type = "viridis") +
              theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) 


      print(p)

      ggsave(paste0(selected_country, selected_day,".png"), width = chart_width, height = chart_heigth, units = "mm")


}



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


###      ###
### Maps ###
###      ###

###### Cases for today


data_to_show<- data_totals %>% filter(DateRep==selected_day) %>% filter(!(Cases == 0)) %>% 
                               mutate(Values = log10(Cases)) %>% 
                               select(Countries, Values)

print_and_save_map(data_to_show, selected_day, "COVID-19 Cases for day", "", chart_save_name = "Cases_")


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

data_for_map_2 <- data_for_map_prep2 %>% mutate( Values = 100*(Weekly_Cases/Cases)**(1/7)) %>% select(Countries, Values)


print_and_save_map(data_for_map_2, for_day = selected_day,
                   chart_title = "Average Daily Growth of Covid-19 Cases per Week (in %) to ",
                   chart_save_name = "Average_Growth_Cases" )


### Top ten growth ##

pom <- data_for_map_2 %>% arrange(desc(Values))

top_ten_growth <- pom[1:10,]

top_ten_growth$Countries <- factor(top_ten_growth$Countries,    # Factor levels in increasing order
                                        levels = top_ten_growth$Countries[order(top_ten_growth$Values)])

##



p <- ggplot() + 
          geom_col(data = top_ten_growth, aes( x=Countries, y=Values, fill=Countries)) + 
          coord_flip()+ theme(legend.position="none") + 
          labs(title = paste0("COVID-19 Top 10 Average Weekly Growth in % for ", selected_day),
                   caption = "Data source: https://www.ecdc.europa.eu. Created in R, gglot2. Martin Slíva, cc")


print(p)


ggsave(paste0("Top_Growth_", selected_day,".png"), width = chart_width, height = chart_heigth, units = "mm")


###



###  Dataset preparation

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


### Death Rate


data_for_chart <- data_for_map_3 %>% mutate(Values = 100*Death_Rate) %>% select(Countries, Values)

print_and_save_map(data_for_chart, selected_day, 
                   chart_title = "COVID-19 Death Rate (in %) to ", 
                   chart_subtitle = "Countries with more than 25 cases.",
                   chart_save_name = "Death_rate")


### Cases per population


data_for_chart <- data_for_map_3 %>% mutate(Values = log10(Cases_per_pop)) %>% select(Countries, Values)

print_and_save_map(data_for_chart, selected_day, 
                   chart_title = "COVID-19 Cases per Population (log10 scale) ", 
                   chart_subtitle = "Countries with more than 25 cases.",
                   chart_save_name = "Cases_per_population")



###  Death per population


data_for_chart <- data_for_map_3 %>% mutate(Values = log10(Death_per_pop)) %>% select(Countries, Values)

print_and_save_map(data_for_chart, selected_day, 
                   chart_title = "COVID-19 Death per Population (log10 scale) ", 
                   chart_subtitle = "Countries with more than 25 cases.",
                   chart_save_name = "Death_per_population")



#### Total Cases

data_for_chart <- data_totals %>% filter(DateRep == selected_day) %>% filter(!(TotalCases == 0)) %>%
                                  mutate(Values = log10(TotalCases)) %>%
                                  select(Countries, Values)

print_and_save_map(data_for_chart, selected_day, 
                   chart_title = "COVID-19 Total Cases (log10 scale) to ", 
                   chart_save_name = "Total_cases")


### Total Death

data_for_chart <- data_totals %>% filter(DateRep == selected_day) %>% filter(!(TotalDeath == 0)) %>%
                                  mutate(Values = log10(TotalDeath)) %>%
                                  select(Countries, Values)

print_and_save_map(data_for_chart, selected_day, 
                   chart_title = "COVID-19 Total Deaths (log10 scale) to ", 
                   chart_save_name = "Total_Deaths")



####

source("C:/Users/Martin_Sliva/Documents/R/Covid-19/WeeklyGrowth.R", encoding = "UTF-8")
