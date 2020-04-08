
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

### cleaning no longer needed variables

rm(list=c("countries", "problematic_countries", "i" , "enum_problematic_countries"))


