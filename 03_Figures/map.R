library(tidyverse)
library(ggmap)
library(osmdata)

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
  labs(color='Community type', y = "Latitude", x = "Longitude")

ggsave("Figures/map.jpeg", height = 20, width = 10, dpi = 600)
