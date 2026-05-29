library(tidyverse)
library(ggmap)
library(osmdata)
library(ggspatial)
library(tmap)
library(grid)
library(cowplot)
library(ggmapinset)


# API key
register_google(key = "AIzaSyDuTze63zSNtxg7BGSvpY0I-QRLK-q4OS4", write = TRUE)
gps <- read.csv("Data/Zorio_plot_locations.csv")
register_stadiamaps("5673664f-130e-4a27-91df-b6c2d07ffd53", write = TRUE)

# bring in gps data ----------
gps <- read.csv("Data/Zorio_plot_locations.csv")
register_stadiamaps("", write = TRUE)

#reorder groups
#replace site names with elevations

gps$community[gps$community == 'sagebrush'] <- 'Sagebrush (2528 - 3109 m)'
gps$community[gps$community == 'spruce-fir'] <- 'Spruce-fir (3001 - 3519 m)'
gps$community[gps$community == 'upland-herb'] <- 'Upland-herb (3123 - 3849 m)'
gps$community[gps$community == 'alpine'] <- 'Alpine (3548 - 4012 m)'

gps$community <- factor(gps$community,
                              levels  = c("Sagebrush (2528 - 3109 m)",
                                          "Spruce-fir (3001 - 3519 m)",
                                          "Upland-herb (3123 - 3849 m)",
                                          "Alpine (3548 - 4012 m)"))
# create grob for inset -----

main_map <- get_map(location = "CO", zoom = 10) %>%
  ggmap()

# Create the inset map of Colorado
colorado_map <- get_map(location = "Colorado", zoom = 6) %>%
  ggmap() +
  # Optional: add a rectangle showing the main map area
  geom_rect(aes(xmin = -105.2, xmax = -104.8, 
                ymin = 39.6, ymax = 39.9),
            color = "red", fill = NA, size = 1) +
  theme_void() +  
  theme(plot.background = element_rect(fill = "white", color = "black"))

# Convert the inset map to a grob
grob <- ggplotGrob(colorado_map)

# Add the inset to your main map
main_map +
  inset(grob = colorado_grob, 
        xmin = -105.1, xmax = -104.85,  # Position in main map coordinates
        ymin = 39.85, ymax = 40.0)

# make figure -----
map <- get_stadiamap(bbox = c(left = -107.1, bottom = 38.52, right = -106.65, top = 39.09), 
                     maptype = "stamen_terrain")
ggmap(map)


scale_km <- 1  
scale_deg <- scale_km / 111.32  

# Position in bottom right (adjust percentages as needed)
lon_range <- diff(range(gps$Long))
lat_range <- diff(range(gps$Lat))
x_start <- max(gps$Long) - 0.3 * lon_range
y_pos <- min(gps$Lat) + 0.08 * lat_range

base_map <- ggmap(map) +
  geom_point(data = gps, aes(x = Long, y = Lat, colour = community), 
             size = 2)+
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_color_manual(values = c("yellow","#8FD744FF","#287C8EFF","#440154FF"))+
  labs(color='Community Type', y = "Latitude", x = "Longitude")+
  annotation_north_arrow(
    location = "br",
    pad_x = unit(0.5, "in"),
    pad_y = unit(7.5, "in"),
    style = north_arrow_fancy_orienteering
  ) 

plot(base_map)

ggsave("Figures/map.jpeg", height = 18, width = 10, dpi = 600)

# add inset ----
usa <- map_data("state")
western_states <- c("washington", "oregon", "california", "nevada", "idaho", 
                    "montana", "wyoming", "utah", "colorado", "arizona", 
                    "new mexico")
western_usa <- subset(usa, region %in% western_states)
colorado <- subset(usa, region == "colorado")

gothic_coords <- data.frame(lon = -106.9878, lat = 38.9586)

p_inset <- ggplot() +
  geom_polygon(data = western_usa, 
               aes(x = long, y = lat, group = group),
               fill = "white", color = "gray50", size = 0.5) +
  geom_point(data = gothic_coords,
             aes(x = lon, y = lat),
             shape = 8,  # star shape
             size = 3,
             color = "red") +
  coord_fixed(1.4) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", color = "black"))

map_final <- base_map + annotation_scale(location = "br",  
                   width_hint = 0.2,
                   pad_x = unit(0.5, "cm"),
                   pad_y = unit(0.5, "cm")) +
  inset_element(p_inset, 
                left = 0, bottom = 0.0, 
                right = 0.5, top = 0.3)

print(map_final)

ggsave("Figures/map.jpeg", height = 18, width = 10, dpi = 600)



# claude test ------------------------

base_map <- ggmap(map) +
  geom_point(data = gps, aes(x = Long, y = Lat, colour = community), 
             size = 2)+
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_color_manual(values = c("yellow","#8FD744FF","#287C8EFF","#440154FF"))+
  labs(color='Community Type', y = "Latitude", x = "Longitude")+
  annotation_north_arrow(
    location = "br",
    pad_x = unit(0.5, "in"),
    pad_y = unit(7.5, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  # Scale bar in bottom right corner (approximately 5 km)
  annotate("segment", 
           x = -106.75, xend = -106.68,  # 5 km scale bar
           y = 38.56, yend = 38.56,
           color = "black", linewidth = 1) +
  annotate("segment", 
           x = -106.75, xend = -106.75,
           y = 38.56, yend = 38.58,
           color = "black", linewidth = 1) +
  annotate("segment", 
           x = -106.68, xend = -106.68,
           y = 38.56, yend = 38.58,
           color = "black", linewidth = 1) +
  annotate("text", 
           x = -106.715, y = 38.54,
           label = "5 km", size = 4, fontface = "bold")

map_final <- base_map +
  inset_element(p_inset, 
                left = 0, bottom = 0.0, 
                right = 0.5, top = 0.3)

print(map_final)
ggsave("Figures/Figure1.jpeg", height = 18, width = 10, dpi = 600)
