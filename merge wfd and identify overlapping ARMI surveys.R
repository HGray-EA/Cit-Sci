# Script which merges with CDE data to ascribe wbs to monitoring points

wfd_sf <- read_sf("/dbfs/mnt/lab/unrestricted/harry.gray@environment-agency.gov.uk/CDE/England_Shapefile.shp")

# Transform from 4326 to planar 27700 so easier for spatial computation
  wfd_sf %<>% st_transform(crs= 27700)
  today_sf <- st_as_sf(today, coords = c("Longitude", "Latitude"), crs=4326)

# Transform it into planar geometry
  today_sf <- st_transform(today_sf, crs = 27700)
  
# Spatial join
     today_sf <-  st_join(today_sf, wfd_sf)


######
# Remove NA values 
  df %<>% filter(!is.na(Longitude) &!is.na(Latitude)) 
     
     
  df_sf <-  st_as_sf(df, coords = c("Longitude", "Latitude"), crs=4326) %>% 
       st_transform(df_sf, crs = 27700)


# Load ARMI data and then transform it & convert to spatial object
  library(sf)
  library(dplyr)
  library(lubridate)
  library(uuid)
  library(purrr)
  library(tidyr)
  library(sf)
  
  # Step 1: Read the CSV and filter out missing coordinates
  ARMI <- read.csv("/dbfs/mnt/lab/unrestricted/harry.gray@environment-agency.gov.uk/CEP/Riverfly_bulk_download_2000_180924.csv")
  
  ARMI <- ARMI %>%
    filter(!is.na(Location..Easting) & !is.na(Location..Northing))
  
  # Step 2: Convert ARMI to sf object (EPSG:27700) and then to EPSG:4326
  ARMI <- ARMI %>%
    st_as_sf(coords = c("Location..Easting", "Location..Northing"), crs = 27700) %>%
    st_transform(crs = 4326)
  
  # Assuming df_sf is already in sf format, ensure it is in EPSG:4326 as well
  df_sf <- st_transform(df_sf, crs = 4326)
  
  # Step 3: Find ARMI sites within 50m of df_sf using st_within_distance()
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
  
 #Convert dates
  ARMI_Twin$Recorded..Date <- dmy(ARMI_Twin$Recorded..Date)
  
  ARMI_Twin <- unique(ARMI_Twin)
  
  AT_Twin <- unique(AT_Twin)
  
  
  ARMI_Twin %>% filter(Recorded..Date == AT_Twin$Date)
  # Join AT & ARMI
  ARMI_Twin_2 <- ARMI_Twin %>%
    st_drop_geometry() %>% 
    inner_join(AT_Twin, by = c("Recorded..Date" = "Date"), relationship = "many-to-many") 
  
  # Step 7: Assign the generated UIDs to the matched sites in AT_Twin
  AT_Twin_2 <- AT_Twin %>%
    mutate(AT_ARMI_UID = ARMI_Twin_2$AT_ARMI_UID)
  
  # Step 8: Ensure the spatial objects are in EPSG:4326, if necessary (done earlier)
  ARMI_Twin_2 <- st_transform(ARMI_Twin_2, 4326)
  AT_Twin_2 <- st_transform(AT_Twin_2, 4326)
  
  # Final results
  print(ARMI_Twin_2)
  print(AT_Twin_2)
  





