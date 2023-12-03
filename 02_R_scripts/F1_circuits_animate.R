# 1 Libraries----
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(osmdata)
library(ggmap)
library(sf)
library(showtext)
library(ggfx)

# 2 Set fonts----
font_add_google("Luckiest Guy","ramp")
font_add_google("Bebas Neue","beb")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Roboto", "roboto")
font_add_google("Anton", "anton")
font_add_google("Ultra", "ultra")
font_add_google("Abril Fatface", "abril")
font_add_google("Luckiest Guy", "lucky")
font_add_google("Rye", "rye")
font_add_google("Lato", "lato")
showtext_auto()
showtext_opts(dpi = 320)

# 3 Digitized F1 circuit maps----
# used web plot digitizer to generate XY plot data for Australian F1 circuit map obtained from the McLaren app

oz_circuit <- read.csv('./00_raw_data/oz_track_dots.csv') %>%
  filter(!type == 'PL') %>% 
  arrange(order) %>% 
  mutate(sector_number = case_when(sector == 'red' ~ 'sector 1',
                                   TRUE ~ NA))

oz_circuit_static <- ggplot(oz_circuit, aes(x, y, colour = sector)) + 
  geom_path(linewidth = 3, 
            lineend = 'round') +
  scale_y_reverse()

oz_circuit_animate <- oz_circuit_static +
  transition_reveal(order)

anim_save("./04_gifs/first_saved_animation_anim_oz_circuit_animate.gif", oz_circuit_animate, height = 600, width = 800)

# osmdata ----
# https://www.openstreetmap.org/relation/280443
# https://joshuamccrain.com/tutorials/maps/streets_tutorial.html
# https://wiki.openstreetmap.org/wiki/Map_features#Water_related
# https://stackoverflow.com/questions/69803028/problem-with-plotting-a-polygon-with-ggplot-osmdata

tags <- available_tags("highway")
print(tags, n=80)

getbb("Melbourne Australia")

# F1_circuit_melbourne <- getbb("Melbourne Australia")%>%
#   opq()%>%
#   add_osm_feature(key = "name", value = "Albert Park Circuit") %>%
#   osmdata_sf()

F1_circuit_melbourne <- opq_osm_id(type = "relation", id = 280443)%>%
  opq_string() %>%
  osmdata_sf()

st_centroid(F1_circuit_melbourne$osm_lines)

# albert_park_lake <- getbb("Melbourne Australia")%>%
#   opq()%>%
#   add_osm_feature(key = "natural", 
#                   value= "water") %>%
#   osmdata_sf()

F1_albert_park_lake <- opq_osm_id(type = "relation", id = 346429) %>%
  opq_string() %>%
  osmdata_sf() 

F1_albert_park_lake$osm_multipolygons

gunn_island <- opq_osm_id(type = "relation", id = 2076069) %>%
  opq_string() %>%
  osmdata_sf()

gunn_island$osm_multipolygons

mud_island <- opq_osm_id(type = "way", id = c(45303423, 45303421)) %>%
  opq_string() %>%
  osmdata_sf()

mud_island$osm_polygons

grass_melbourne <- getbb("Melbourne Australia")%>%
  opq()%>%
  add_osm_feature(key = "surface", 
                  value = c("grass", "dirt")) %>%
  osmdata_sf()

grass_melbourne$osm_multipolygons

trees_melbourne <- getbb("Melbourne Australia")%>%
  opq()%>%
  add_osm_feature(key = "natural", 
                  value = "tree") %>%
  osmdata_sf()

trees_melbourne$osm_points

big_streets_melbourne <- getbb("Melbourne Australia")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

big_streets_melbourne

med_streets_melbourne <- getbb("Melbourne Australia")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

small_streets_melbourne <- getbb("Melbourne Australia")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf()

# river_melbourne <- getbb("Melbourne Australia")%>%
#   opq()%>%
#   add_osm_feature(key = "waterway", value = "river") %>%
#   osmdata_sf()

# railway_melbourne <- getbb("Melbourne Australia")%>%
#   opq()%>%
#   add_osm_feature(key = "railway", value="rail") %>%
#   osmdata_sf()

# grandstand_melbourne <- getbb("Melbourne Australia")%>%
#   opq()%>%
#   add_osm_feature(key = "building", 
#                   value= c("grandstand", "pavillion", "stadium", "sports_hall", "sports_centre", "service")) %>%
#   osmdata_sf()
# 
# grandstand_melbourne <- getbb("Melbourne Australia")%>%
#   opq()%>%
#   add_osm_feature(key = "building", 
#                   value= c("grandstand", "pavilion", "stadium", "sports_hall", "sports_centre", "service")) %>%
#   osmdata_sf()
# 
# grandstand_melbourne <- getbb("Melbourne Australia")%>%
#   opq()%>%
#   add_osm_feature(key = "building", 
#                   value= c("grandstand", "sports_hall")) %>%
#   osmdata_sf()
# 
# leisure_melbourne <- getbb("Melbourne Australia")%>%
#   opq()%>%
#   add_osm_feature(key = "leisure", 
#                   value= "stadium") %>%
#   osmdata_sf()

cols = rainbow(152548, s=.6, v=.9)[sample(1:152548,152548)]

# https://www.color-hex.com
# olive tree https://www.color-hex.com/color-palette/9479
fun_color_range_olive <- colorRampPalette(c("#729246", "#fcffc0", "#acda4d", "#8c6d00", "#e2e873")) 
my_colors_olive <- fun_color_range_olive(50)
cols_olive <- sample(my_colors_olive, 152548, replace = TRUE)

# bark https://www.color-hex.com/color-palette/39949
fun_color_range_bark <- colorRampPalette(c("#dfd8c9", "#a5633c", "#5f6344", "#694b37", "#7a6935")) 
my_colors_bark <- fun_color_range_bark(50)
cols_bark <- sample(my_colors_bark, 152548, replace = TRUE)

map_background <- "#f6eee3" 
map_background2 <- "floralwhite"

tree_size_range <- runif(n=50, min=0.5, max=6)

tree_size <- sample(tree_size_range, 152548, replace = TRUE)

ggplot() +
  geom_sf(data = med_streets_melbourne$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = small_streets_melbourne$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = big_streets_melbourne$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  with_outer_glow(geom_sf(data = F1_circuit_melbourne$osm_lines,
          inherit.aes = FALSE,
          color = "#00843D",
          #color = "deeppink",
          linewidth = 3,
          alpha = 1), colour='gold', sigma = 5, expand = 5) +
  geom_sf(data = F1_circuit_melbourne$osm_lines,
          inherit.aes = FALSE,
          color = "grey70",
          linewidth = 1.5,
          alpha = 1) +
  geom_sf(data = F1_albert_park_lake$osm_multipolygons,
          inherit.aes = FALSE,
          fill = "cornflowerblue",
          color = "lightskyblue",
          size = 0.1,
          alpha = 0.5) +
  geom_sf(data = gunn_island$osm_multipolygons,
          inherit.aes = FALSE,
          fill = "forestgreen",
          color = "forestgreen",
          size = 0.1,
          alpha = 0.5) +
  geom_sf(data = mud_island$osm_polygons,
          inherit.aes = FALSE,
          fill = "forestgreen",
          color = "forestgreen",
          size = 0.1,
          alpha = 0.5) +
  geom_sf(data = trees_melbourne$osm_points,
          inherit.aes = FALSE,
          color = cols_olive,
          linewidth = 2,
          size = tree_size,
          alpha = 0.75) +
  coord_sf(xlim = c(144.9615, 144.979),
           ylim = c(-37.855, -37.8375),
           expand = FALSE) + 
  theme_void() +
  theme(plot.background = element_rect(fill = map_background2, color = "#694b37", size = 2),
        #plot.caption = element_text(family = 'lato', hjust = 0.5, size = 12, color = "#694b37", margin = margin(t = -25)),
        plot.title = element_text(size = 20, family = "lato", face="bold", hjust=0.5, colour = "#694b37"), plot.subtitle = element_text(family = "lato", size = 16, colour = "#694b37", hjust=0.5),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        panel.border = element_rect(colour = "#694b37", fill=NA, size=1)) +
  #geom_sf_label(data = coordinates_sf_turnberry, aes(label = place), nudge_x = 0.6, nudge_y = 0.35, family = 'ultra', colour = 'black', size = 6, label.size = 0.5) +
  #labs(caption = "Design: Allan James | @allanjames1506") +
  annotate("label", y = -37.8545, x = 144.97, label = "Design: Allan James | @allanjames1506", lineheight = 0.75, family = 'lato',  size = 4.5, color = "#694b37", vjust = 0.5) +
  labs(title = "ALBERT PARK GRAND PRIX CIRCUIT", subtitle = "35.595°N / 82.552°W")
