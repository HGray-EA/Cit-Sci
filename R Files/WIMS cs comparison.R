# WIMS call 

source(Analytics.R)

#Find determinant codes here: https://environment.data.gov.uk/water-quality/data/sample/AN-01M02-20150119-1785064/measurements.html?__htmlView=table
#The script plots a leaflet map of a selected WQ determinant in the Hampshire Avon

Leaflet_Map_Detrs <- function(sample_determinant, year){
  
  library(jsonlite)
  library(httr)
  library(ggplot2)
  library(sf)
  library(wesanderson)
  library(tidyverse)
  library(leaflet)
  
  sample_determinant <- "0180"
  year <- "2019"
  
  
  base_url <- "http://environment.data.gov.uk/water-quality/"
  ending <- "data/measurement?_limit=1800"
  
  url <- paste0(base_url, ending, "&determinand=", sample_determinant, "&year=", year)
  
  #Load in the api url using the httr and json packages. 
  A_stations <- GET(url) 
  A_stations$status_code  #if 200 all working :)
  
  api_char <- rawToChar(A_stations$content)
  
  api2 <- fromJSON(api_char, flatten=T)                             #change it from json into R readable data
  api2 <- api2$items 
  
  print(head(api2))
  
  
  api2$Date <- lubridate::ymd_hms(api2$sample.sampleDateTime)
  
  api2$Date <- as.Date(api2$Date)
  
}



# I guess lets do a violin plot 


a <- ggplot(api2, aes(x = "", y = result)) +
  geom_violin(fill = "#aab5d5", color = "black", alpha = 0.5) + # Violin plot with some transparency
  geom_boxplot(width = 0.2, color = "black", fill = "#efc86e") + # Boxplot on top
  labs(title = "Phos_ppm Distribution WIMS", x = "", y = "Phos_ppm") +
  scale_y_continuous(limits = c(0, 2.5)) # Adjust the y-axis limits as needed


# All well and good but we don't have river level

# Plot infers that the Hanna meter where it has ranges, elevates phosphate readings more than sondes.

cowplot::plot_grid(a,b)

# Initialize an empty list to store results
results <- list()



# Loop through each row in data1 and data2 to compare
for (i in 1:nrow(api2)) {
  for (j in 1:nrow(df)) {
    # Check if the dates are the same
    if (api2$Date[i] == df$Date[j]) {
      # Calculate the distance between the two points
      dist <- geosphere::distHaversine(c(api2$long[i], api2$lat[i]), c(df$long[j], df$lat[j]))
      
      # Check if the distance is within 1 km
      if (dist <= 1000) {
        results[[length(results) + 1]] <- c(i, j, dist)
      }
    }
  }
}

# Convert results to a data frame
results_df <- do.call(rbind, results)
colnames(results_df) <- c("data1_index", "data2_index", "distance_meters")

# Show the results
results_df

