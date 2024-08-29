source("Extraction_Transform.R")

pal1 <- colorBin(palette = "viridis", domain=df$Mean_Nitrate)


#Plot mean nitrate on a map, experimental so only covers an arbitrary timespan

leaflet(df) %>% 
  addProviderTiles(providers$Esri) %>% 
  addCircleMarkers(lng= ~df$Longitude, 
                   lat= ~df$Latitude, 
                   col=~pal1(df$Mean_Nitrate), radius=5, 
                   popup = paste0("Site: ",df$Site_Name,"<br> ",
                                  "Mean Nitrate (ppm): ",df$Mean_Nitrate)) %>% 
  addLegend(pal = pal1, values = ~Mean_Nitrate, opacity = 0.7,
            title="Mean Nitrate (ppm)") %>% 
  addControl(paste0("Mean Nitrate (ppm) from ", range(df$Date)[1], " to ", 
                    range(df$Date)[2]),
             position = "bottomleft")


# Plots of national variation in data for Cit Sci QAQC

par(mfrow = c(2,2))

hist(df$Nitrate_ppm, breaks = 100, main = "Nitrate_ppm", xlab = "Nitrate_ppm", col = "lightblue", border = "black")
hist(df$Phos_ppm, breaks = 100, main = "Phos_ppm", xlab = "Phos_ppm", col = "lightblue", border = "black")

df$Ammon_ppm <- as.numeric(df$Ammon_ppm)

hist(df$`_What_is_the_Elect`, breaks = 100, main = "Elec_Cond", xlab = "Elec_Cond", col = "lightblue", border = "black")
hist(df$Ammon_ppm, breaks = 100, main = "Ammon_ppm", xlab = "Ammon_ppm", col = "lightblue", border = "black")
hist(df$`_Where_was_the_samp.accuracy`, breaks = 100, main = "samp.accuracy", xlab = "samp.accuracy", col = "lightblue", border = "black")



#### Experimental QAQC

for (x in 1:dim(df)[2]){
  print(paste0(colnames(df)[x], " has ", sum(is.na(df[x])), " zeroes"))
}


# Don't want to completley remove rows with NAs as may be that other data is okay - it may also be a dodgey instrument?


df$confidence <- ifelse(df$`Temp_°C` < 10 & df$River_level == "High", "Low", "Normal")


high_ri <-  df %>% filter(River_level == "High") # Storm events
norm_ri <- df %>% filter(River_level != "High")

par(mfrow = c(2,2))


# Below here, I'm guessing by trying to gra

high_river_level <- summary(high_ri)
norm_river_level <- summary(norm_ri)

stats_comp <- data.frame(Stats= row.names(high_river_level), 
                         as.vector(high_river_level), 
                         as.vector(norm_river_level))


hist(df$Nitrate_ppm, breaks = 100, main = "Nitrate_ppm", xlab = "Nitrate_ppm", col = "lightblue", border = "black")
hist(df$Phos_ppm, breaks = 100, main = "Phos_ppm", xlab = "Phos_ppm", col = "lightblue", border = "black")



library(ggplot2)

# The 
ggplot(high_ri, aes(x = Phos_ppm)) +
  geom_histogram(bins = 100, fill = "lightblue", color = "black") +
  labs(title = "Phos_ppm high river level", x = "Phos_ppm") +
  scale_y_continuous(limits = c(0, 200))+coord_flip()
ggplot(norm_ri, aes(x = Phos_ppm)) +
  geom_histogram(bins = 100, fill = "lightblue", color = "black") +
  labs(title = "Phos_ppm normal river level", x = "Phos_ppm") +
  scale_y_continuous(limits = c(0, 200))+coord_flip()

# these could be nice violin plots


library(ggplot2)

# Create the plot with both violin and boxplot layers
ggplot(high_ri, aes(x = "", y = Phos_ppm)) +
  geom_violin(fill = "#6f9969", color = "black", alpha = 0.5) + # Violin plot with some transparency
  geom_boxplot(width = 0.2, color = "black", fill = "#efc86e") + # Boxplot on top
  labs(title = "Phos_ppm Distribution at High River Level", x = "", y = "Phos_ppm") +
  scale_y_continuous(limits = c(0, 2.5)) # Adjust the y-axis limits as needed

ggplot(norm_ri, aes(x = "", y = Phos_ppm)) +
  geom_violin(fill = "#aab5d5", color = "black", alpha = 0.5) + # Violin plot with some transparency
  geom_boxplot(width = 0.2, color = "black", fill = "#efc86e") + # Boxplot on top
  labs(title = "Phos_ppm Distribution at Normal or Low River Level", x = "", y = "Phos_ppm") +
  scale_y_continuous(limits = c(0, 2.5)) # Adjust the y-axis limits as needed



df$River_level <- as.factor(df$River_level)

str(df)
df$River_level <- as.factor(df$River_level)


# Violin plot of phos

df_counts <- df %>%
  group_by(River_level) %>%
  summarise(count = n())

# See distribution change at different river leves and therein we're inferring turbidity levels.

ggplot(df, aes(x = "", y = Phos_ppm)) +
  geom_violin(aes(fill = River_level), color = "black", alpha = 0.5) + # Violin plot with some transparency
  geom_boxplot(width = 0.2, color = "black", aes(fill = River_level), alpha = 0.7) + # Boxplot on top
  geom_text(data = df_counts, aes(x = "", y = -Inf, label = paste("Count: ", count)), vjust = -0.5, hjust = 0.5, size = 4) + # Add counts below the x-axis
  labs(title = "Ammon_ppm Distribution by River Level", x = "", y = "Ammon_ppm") +
  scale_y_continuous(limits = c(0, 2.5)) + # Adjust the y-axis limits as needed
  facet_wrap(~ River_level, scales = "free_y") + # Create facets for each level
  theme_minimal() + # Use a minimal theme for a cleaner look
  theme(legend.position = "none") # Remove legend to avoid clutter





library(ggplot2)
library(dplyr)

# See distribution change at different river leves and therein we're inferring turbidity levels.
ggplot(df, aes(x = "", y = Ammon_ppm)) +
  geom_violin(aes(fill = River_level), color = "black", alpha = 0.5) + # Violin plot with some transparency
  geom_boxplot(width = 0.2, color = "black", aes(fill = River_level), alpha = 0.7) + # Boxplot on top
  geom_text(data = df_counts, aes(x = "", y = -Inf, label = paste("Count: ", count)), vjust = -0.5, hjust = 0.5, size = 4) + # Add counts below the x-axis
  labs(title = "Ammon_ppm Distribution by River Level", x = "", y = "Ammon_ppm") +
  scale_y_continuous(limits = c(0, 2.5)) + # Adjust the y-axis limits as needed
  facet_wrap(~ River_level, scales = "free_y") + # Create facets for each level
  theme_minimal() + # Use a minimal theme for a cleaner look
  theme(legend.position = "none") # Remove legend to avoid clutter

dev.off()

range(df$`Temp_°C`)

library(DT)

datatable( data.frame(unclass(summary(high_ri)), check.names = FALSE))  # high river levels actually have lower phosphate readings. Maybe it causes meters to under-measure.
datatable( data.frame(unclass(summary(norm_ri)), check.names = FALSE))
#Playing with a plot, what nitrate data do we have on the Hampshire Avon?


Random_Site <- df[df$River_Name=="Hampshire Avon",]