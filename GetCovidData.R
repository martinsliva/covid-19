
#these libraries are necessary 
library(readxl) 
library(httr) 


url2<- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx"

#download the dataset from the website to a local temporary file 
GET(url2, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx"))) 
#read the Dataset sheet into “R” 
data3 <- read_excel(tf)
