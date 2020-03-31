#### Credits: https://data.europa.eu/euodp/en/data/dataset/covid-19-coronavirus-data/resource/e64c8e99-c06b-4b69-adfc-f23890dd66c6

#these libraries are necessary

library(readxl)

library(httr)

#create the URL where the dataset is stored with automatic updates every day


url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")


if (!GET(url = url)$status_code==404){ ##Check if today's file exist
            
      #download the dataset from the website to a local temporary file

      GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

      #read the Dataset sheet into “R”

      data <- read_excel(tf)
}

