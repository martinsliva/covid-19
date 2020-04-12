
#### BASIC DATA CLEANING 

names(data)[7]<-"Countries"


## Removing "_" from country names
data$Countries <- gsub("_", " ", data$Countries)


countries <- unique(data$Countries)

## country names which are not in WorldData region names (definition of maps)
problematic_countries<-countries[(!countries %in% unique(WorldData$region))]

enum_problematic_countries<-read.csv("data/enum_country_names.csv", colClasses = "character")



for (i in 1:dim(enum_problematic_countries)[1]) {
      if (!sum(data$Countries==enum_problematic_countries[i,2]) == 0){
            
            data[data$Countries==enum_problematic_countries[i,2], ]$Countries <- enum_problematic_countries[i,3]   
            
      }
      
}

names(data)[1] <- "DateRep"
names(data)[5] <- "Cases"
names(data)[6] <- "Deaths"
names(data)[10] <- "Popul"

data[which(data$Cases<0),]$Cases <- 0



popul <- population %>% group_by(country) %>% summarise( pop=max(population))
country_without_popul_data <- unique(subset(data_totals, is.na(data_totals$Popul))$Countries)

for (i in country_without_popul_data) {
      
      if (!sum(popul$country== i)==0){
            data_totals[data_totals$Countries == i,]$Popul <- popul[popul$country== i,]$pop
      }
}



### cleaning no longer needed variables

rm(list=c("countries", "problematic_countries", "i" , "enum_problematic_countries", "popul", "country_without_popul_data"))


