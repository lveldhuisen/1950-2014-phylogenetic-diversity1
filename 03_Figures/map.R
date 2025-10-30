library(tidyverse)
library(ggmap)
library(osmdata)
library(ggspatial)
library(tmap)
library(grid)
library(cowplot)

# API key
register_google(key = "AIzaSyDuTze63zSNtxg7BGSvpY0I-QRLK-q4OS4", write = TRUE)

# bring in gps data ----------
gps <- read.csv("Data/Zorio_plot_locations.csv")
register_stadiamaps("5673664f-130e-4a27-91df-b6c2d07ffd53", write = TRUE)

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

ggmap(map) +
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

ggsave("Figures/map.jpeg", height = 18, width = 10, dpi = 600)

# make map for inset ----

df = data.frame(
  lon = c(-106.9898),
  lat = c(38.9592))
  
co_map <- get_stadiamap(bbox = c(left = -109.05, bottom = 37.0, right = -102.05, top = 41.0), 
                     maptype = "stamen_toner_lite")

ggsave("Figures/insetmap.jpeg", height = 8, width = 12, dpi = 600)

co_map +
  geom_point(data = df, aes(x=-106.9898, y=38.9592), color="red", size=10, alpha=0.5)

map_obj <- ggmap(co_map)
plot(map_obj)
