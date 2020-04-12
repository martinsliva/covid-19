### more than one day growth calculation dased on data_totals
library(dplyr)

growth_step <- 10

### preparation indexis

countries_to_calculate <- unique(data_totals$Countries)
dates_to_calculate <- sort(unique(data_totals$DateRep), decreasing = TRUE)
number_dates <- length(dates_to_calculate)


pom <- NULL
total_pom <- NULL

for (i in countries_to_calculate){
      
      for (j in 1:(number_dates-growth_step)) {
            if(!length(data_experiment[data_experiment$Countries==i & data_experiment$DateRep == dates_to_calculate[j+growth_step], ]$TotalCases) == 0){
                 
                  pom <- data_totals %>% select(Countries, DateRep, TotalCases, TotalDeath) %>% 
                        filter(DateRep == dates_to_calculate[j+ growth_step] & Countries == i) %>% 
                        mutate(CasesWeek = TotalCases, DeathsWeek = TotalDeath, DateShifted = dates_to_calculate[j]) %>%
                        select( Countries, DateShifted, CasesWeek, DeathsWeek)
                  
                  total_pom <- rbind(total_pom, pom)
                  
            }
      }
}

colnames(total_pom)[2] <- "DateRep"

data_growth<-left_join(data_totals, total_pom, by=c("Countries", "DateRep"))

data_growth <- data_growth %>% 
                        mutate(
                              CasesGrowth = (TotalCases/CasesWeek)**(1/growth_step),
                              DeathsGrowth = (TotalDeath/DeathsWeek)**(1/growth_step)
                        )


number <- 10

data_last_day <- data_growth %>% 
      
      filter(DateRep==max(data_growth$DateRep)) %>%
      
      select(Cases, Countries) %>% 
      
      arrange(desc(Cases))

data_last_day_short <- data_last_day[1:number,]


data_last_day_short$Countries <- factor(data_last_day_short$Countries,    # Factor levels in increasing order
                                        levels = data_last_day_short$Countries[order(data_last_day_short$Cases)])



data_for_charts <- data_growth %>% 
      
            select(Countries, TotalCases, TotalDeath, CasesGrowth, DeathsGrowth) %>%
            filter(Countries %in% data_last_day_short$Countries) %>%
            filter(TotalCases >100) 


p <- ggplot(data_for_charts, aes(x=log10(TotalCases), y=CasesGrowth, group=Countries)) + 
      geom_line(aes(color=Countries, linetype=Countries), size= 1.5) +
      geom_point(aes(shape=Countries), size=3) +
      labs(title = paste0("COVID-19 Average ", growth_step, "-days Growths Against Total Cases (log) to ", selected_day),
           subtitle = "Top 10 Countries by Total Cases",
           caption = "Data source: https://www.ecdc.europa.eu. Created in R, gglot2. Martin Slíva, cc")

print(p)


ggsave(paste0("Avg_Growth_Cases_Cases", selected_day,".png"), width = map_width, height = map_height, units = "mm")

####

number <- 10

data_last_day <- data_growth %>% 
      
      filter(DateRep==max(data_growth$DateRep)) %>%
      
      select(Deaths, Countries) %>% 
      
      arrange(desc(Deaths))

data_last_day_short <- data_last_day[1:number,]

data_last_day_short$Countries <- factor(data_last_day_short$Countries,    # Factor levels in increasing order
                                        levels = data_last_day_short$Countries[order(data_last_day_short$Deaths)])


data_for_charts <- data_growth %>% 
      
      select(Countries, TotalCases, TotalDeath, CasesGrowth, DeathsGrowth) %>%
      filter(Countries %in% data_last_day_short$Countries) %>%
      filter(TotalCases >100) 


p <- ggplot(data_for_charts, aes(x=log10(TotalCases), y=CasesGrowth, group=Countries)) + 
      geom_line(aes(color=Countries, linetype=Countries), size= 1.5) +
      geom_point(aes(shape=Countries), size=3) +
      labs(title = paste0("COVID-19 Average ", growth_step, "-days Growths Against Total Cases (log) to ", selected_day),
           subtitle = "Top 10 Countries by Total Deaths",
           caption = "Data source: https://www.ecdc.europa.eu. Created in R, gglot2. Martin Slíva, cc")


print(p)

ggsave(paste0("Avg_Growth_Cases_Deaths", selected_day,".png"), width = map_width, height = map_height, units = "mm")