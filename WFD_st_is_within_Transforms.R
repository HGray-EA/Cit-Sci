# Script which merges with CDE data to ascribe wbs to monitoring points

library(sf)
library(tidyverse)
library(lubridate)
library(uuid)
library(purrr)
library(sf)

wfd_sf <- read_sf("/dbfs/mnt/lab/unrestricted/harry.gray@environment-agency.gov.uk/CDE/England_Shapefile.shp")

# Transform from 4326 to planar 27700 so easier for spatial computation
  wfd_sf %<>% st_transform(crs= 27700)
  df_sf <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs=4326)

# Transform it into planar geometry
  df_sf <- st_transform(df_sf, crs = 27700)
  
# Spatial join
     joined <-  st_join(df_sf, wfd_sf)

######
# Remove NA values 
  df %<>% filter(!is.na(Longitude) &!is.na(Latitude)) 
     
#Load 
  ARMI <- read.csv("/dbfs/mnt/lab/unrestricted/harry.gray@environment-agency.gov.uk/CEP/Riverfly_bulk_download_2000_180924.csv")
# Remove NAs  
  ARMI <- ARMI %>%
    filter(!is.na(Location..Easting) & !is.na(Location..Northing))
  
# Convert from df to sf
  ARMI <- ARMI %>%
    st_as_sf(coords = c("Location..Easting", "Location..Northing"), crs = 27700) %>%
    st_transform(crs = 4326)
  
  df_sf <- st_transform(df_sf, crs = 4326)
  
# Find ARMI sites within 50m of df_sf using st_within_distance()
  within_distances <- st_is_within_distance(ARMI, df_sf, dist = 50)
  
# Step 4: Convert the result of st_within_distance into a flat data frame using purrr::map_dfr()
  results_df <- map_dfr(seq_along(within_distances), ~{
    tibble(
      ARMI_index = .x,
      df_sf_index = within_distances[[.x]]
    )
  })
  
  # Step 5: Isolate matched ARMI and df_sf sites within 50m using the indices
  ARMI_Twin <- ARMI[results_df$ARMI_index, ]
  AT_Twin <- df_sf[results_df$df_sf_index, ]
  
  #Assign index from which to join.
  ARMI_Twin$index <- results_df$df_sf_index
  AT_Twin$index <- results_df$df_sf_index
  
 #Convert dates
  ARMI_Twin$Recorded..Date <- dmy(ARMI_Twin$Recorded..Date)
  
  AT_Twin <- unique(AT_Twin)
  ARMI_Twin <- unique(ARMI_Twin)
  # There's repeat ARMI surveys, so AT doesn't equal ARMI dates
  
  
  leaflet() %>% 
    addProviderTiles(providers$Esri) %>% 
      addCircleMarkers(data=AT_Twin,
                       popup = paste0(AT_Twin$Date)) %>% 
    addCircleMarkers(data=ARMI_Twin,
                     col="seagreen",
                     radius=3,
                     popup = paste0(ARMI_Twin$Recorded..Date))
  

# Haven't given a unique identifier have just kept with index  

  
#  ARMI_Twin_2 <- ARMI_Twin %>%
#    inner_join(AT_Twin, by = index) #%>%
#    mutate(AT_ARMI_UID = paste0(uuid::UUIDgenerate(), row_number()))
  
  # Step 7: Assign the generated UIDs to the matched sites in AT_Twin
#  AT_Twin_2 <- AT_Twin %>%
#    mutate(AT_ARMI_UID = ARMI_Twin_2$AT_ARMI_UID)
  




