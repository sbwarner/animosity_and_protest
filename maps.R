###########################
# Create animosity visualizations
#
#
###########################

setwd("C:/Users/sbw10001/OneDrive/Research/Partisan Animosity and Protest/Analysis/")
load('data/contextual_vars.RData')

library(ggplot2)
library(dplyr)
library(sf)
library(tidyr)
library(tigris)


###
# 1. Create county-by-county map

# a. Pull fips and ovr animosity
data2017 <- county_ests %>%
  filter(year == 2019) %>%
  select(county14, ovr_animosity, dem_animosity, rep_animosity, pct_rep)

# b. Get shape file
counties <- counties(cb = TRUE, class = "sf", year = 2019)

# Ensure county codes are of the same type (character or numeric)
data2017$county14 <- sprintf("%05d", as.numeric(data2017$county14))
counties$GEOID <- as.character(counties$GEOID)

# Merge data
map_data <- left_join(counties, data2017, by = c("GEOID" = "county14"))
map_data$ovr_animosity1 <- (map_data$dem_animosity + map_data$rep_animosity) / 2

# Create plot 
max_animosity <- max(map_data$ovr_animosity1, na.rm = TRUE)
top_50_threshold <- quantile(map_data$ovr_animosity1, na.rm = TRUE)[3]
extra_breaks <- seq(top_50_threshold, max_animosity, length.out = 10)

# Combine with usual scale breaks
common_breaks <- seq(min(map_data$ovr_animosity1, na.rm = TRUE), top_50_threshold, length.out = 5)
all_breaks <- c(common_breaks, extra_breaks)

# Visualizations

# Animosity
ggplot(data = map_data) +
  geom_sf(aes(fill = ovr_animosity1), color = NA) +
  scale_fill_gradient(low = "black", high = "white", name = "Overall Animosity") +
  labs(title = "Mean Partisan Animosity in US Counties, 2019") +
  coord_sf(xlim = c(-125, -66), ylim = c(25, 50), expand = FALSE) +
  theme_minimal() +
  theme(axis.text = element_blank(),  # Remove lat/lon text
        axis.ticks = element_blank(),  # Remove axis ticks
        panel.grid = element_blank())  # Remove gridlines

# Partisan identification
ggplot(data = map_data) +
  geom_sf(aes(fill = pct_rep), color = NA) +
  scale_fill_gradient(low = "black", high = "white", name = "Pct Republican",
                      breaks = seq(15, 50, by = 5)) +
  labs(title = "Percent Republican Identifiers in US Counties, 2019") +
  coord_sf(xlim = c(-125, -66), ylim = c(25, 50), expand = FALSE) +
  theme_minimal() +
  theme(axis.text = element_blank(),  # Remove lat/lon text
        axis.ticks = element_blank(),  # Remove axis ticks
        panel.grid = element_blank())  # Remove gridlines
