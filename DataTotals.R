library(dplyr)

country_totals <- NULL


countries_to_process <- unique(data$Countries)

for (country_to_process in countries_to_process) {

      pokus<-data %>% filter(Countries == country_to_process) %>%  select(DateRep, Cases, Deaths) %>% arrange(DateRep)

      pokus_country<-NULL
      pokus_country[1:length(pokus[,1])]<-country_to_process

      pokus_a <- data.frame()
      pokus2<-as.data.frame(cbind(pokus$DateRep, cumsum(pokus$Cases), cumsum(pokus$Deaths), pokus_country), stringsAsFactors = FALSE)

      pokus2[ ,2] <- as.numeric(pokus2[, 2])
      pokus2[ ,3] <- as.numeric(pokus2[, 3])
      pokus2[ ,1] <- as.POSIXct( as.numeric(pokus2[, 1]), origin = "1970-1-1", tz="UTC")


      colnames(pokus2) <- c("DateRep","TotalCases","TotalDeath", "Countries")

      country_totals <- rbind(country_totals, pokus2)

}

data_totals <- left_join(data, country_totals, by = c('Countries','DateRep'))
