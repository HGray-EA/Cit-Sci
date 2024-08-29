rm(list=ls())

# Script which extracts Epicollect5 data
# Written by: harry.gray@environment-agency.gov.uk

# Due to rate limiting this API must be extracted in portions. 
#The data from this API can call whichever Cit Sci projects are on epicollect5.
#This API calls in data from the Angling Trust and their Water Quality Monitoring Network set up on Epicollect5.

#These are resources from Epicollect on how to handle their API:
# See below for epi5 example how to integrate an API
#https://community.epicollect.net/t/filtering-api-calls-between-dates-in-r/373/9

#filtering info: https://developers.epicollect.net/entries/get-branch-entries
####

library(httr)
library(jsonlite)
library(tidyverse)
library(leaflet)
library(magrittr)

df <- data.frame()

month <- c("01","02","03","04","05","06","07","08","09","10","11","12")
dae  <- c("01","12","24", "28")

# For loop which repeatedly calls API to bypass rate limiting.         
for( x in 1:12){
  
  for( y in 1:3){
    
    base <- "https://five.epicollect.net"
    ex <- "/api/export/entries/water-quality-monitoring-network"
    ex2 <- "?filter_by=created_at&filter_from=2023-"
    ex4 <- "T00:00:00.000&filter_to=2024-"
    ex5 <- "T00:00:00.000"
    
    url <- paste0(base,ex,ex2,month[x],"-",dae[y],ex4,month[x],"-",dae[y+1],ex5)
    url
    api <- GET(url)  
    api$status_code  
    api_char <- rawToChar(api$content)
    api2 <- fromJSON(api_char, flatten=T)    
    
    API <- api2$data$entries #SHORTEN the handle
    ############################################################################## 
    
    #Clean & wrangle data
    
    colnames(API) <- gsub("^\\d+", "", colnames(API)) # Remove any numbers from start of colnames.
    
    colnames(API)[23] <- "Latitude"
    colnames(API)[24] <- "Longitude"
    colnames(API)[6] <- "Date"
    colnames(API)[28] <- "UTMZone"
    colnames(API)[7] <- "River_Name"
    colnames(API)[17] <- "Nitrate_ppm"
    colnames(API)[18] <- "Ammon_ppm"
    colnames(API)[16] <- "Phos_ppm"
    colnames(API)[15] <- "Temp_°C"
    colnames(API)[5] <- "Sampler_Name"
    colnames(API)[8] <- "Site_Name"
    
    df <- rbind(API,df) 
    
  }
}  

range(df$created_at) #QA: Has 

df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)
df$Nitrate_ppm <- as.numeric(df$Nitrate_ppm)
df$Date <- as.Date(df$Date, format="%d/%m/%Y")
df$created_at <- as.POSIXct(df$created_at, format="%Y-%m-%dT%H:%M:%S")
df$River_level <- df[,10]
df[10] <- NULL

df %<>% 
  group_by(Site_Name) %>% 
  mutate(
    Mean_Nitrate = mean(Nitrate_ppm) 
  )

# Clean out missing values

df$Ammon_ppm <- as.numeric(df$Ammon_ppm)


