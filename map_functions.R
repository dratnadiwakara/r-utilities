library(tigris)
library(ggplot2)
library(dplyr)
library(sf)
library(data.table)

countymap <- function(df, color_range = c("white", "blue"),legend_title="") {
  
  names(df) <- c("fips","value")
  # Download county-level shapefiles for the US
  counties <- counties(cb = TRUE, resolution = "20m", class = "sf")
  
  # Ensure everything uses the same CRS (9311, replacement for 2163)
  target_crs <- st_crs(9311)
  
  # Function to scale and shift the geometries
  shift_and_scale <- function(geometry, scale_factor, shift_x, shift_y) {
    # Scale and shift the geometry
    geometry <- st_geometry(geometry) * scale_factor + c(shift_x, shift_y)
    return(geometry)
  }
  
  # Transform and reposition Alaska
  alaska <- counties %>%
    filter(STATEFP == "02") %>%
    st_transform(crs = target_crs) %>%
    mutate(geometry = shift_and_scale(geometry, scale_factor = 0.35, shift_x = -1000000, shift_y = -2800000)) %>%
    st_set_crs(target_crs) 
  
  # Transform and reposition Hawaii
  hawaii <- counties %>%
    filter(STATEFP == "15") %>%
    st_transform(crs = target_crs) %>%
    mutate(geometry = shift_and_scale(geometry, scale_factor = 1, shift_x = 4500000, shift_y = -1200000)) %>%
    st_set_crs(target_crs) 
  
  # Filter out Guam, other territories, and unwanted islands in Hawaii
  counties <- counties %>% 
    filter(!STATEFP %in% c("66", "60", "69", "78", "72", "02", "15")) %>%
    st_transform(crs = target_crs)
  
  # Download state-level shapefiles
  states <- states(cb = TRUE, resolution = "20m", class = "sf") %>%
    filter(!STATEFP %in% c("66", "60", "69", "78", "72", "02", "15")) %>%  # Exclude Guam, territories
    st_transform(crs = target_crs)
  
  # Combine the county maps of the continental US, Alaska, and Hawaii
  us_map <- rbind(counties, alaska, hawaii)
  
  # Join the user-provided data frame with the county map using the 'fips' column
  us_map <- us_map %>%
    left_join(df, by = c("GEOID" = "fips"))
  
  # Plot the final map
  p <- ggplot() +
    geom_sf(data = us_map, aes(fill = value), color = "gray", size = 0.1) +  # County fill based on user-provided values
    geom_sf(data = states, fill = NA, color = "black", size = 0.5) +         # State borders in black
    scale_fill_gradientn(colors = color_range, na.value = "white", name = legend_title) +         # Use the user-defined color range
    theme_void() +
    theme(panel.grid.major = element_line(colour = 'transparent'))
  
  return(p)
}
