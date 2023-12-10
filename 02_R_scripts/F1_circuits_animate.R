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
library(ozmaps) 
library(grid)
library(gt)
library(ggspatial)
library(patchwork)

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

# 4 osmdata ----
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

#F1_albert_park_lake$osm_multipolygons

gunn_island <- opq_osm_id(type = "relation", id = 2076069) %>%
  opq_string() %>%
  osmdata_sf()

mud_island <- opq_osm_id(type = "way", id = c(45303423, 45303421)) %>%
  opq_string() %>%
  osmdata_sf()

grass_melbourne <- getbb("Melbourne Australia")%>%
  opq()%>%
  add_osm_feature(key = "surface", 
                  value = c("grass", "dirt")) %>%
  osmdata_sf()

trees_melbourne <- getbb("Melbourne Australia")%>%
  opq()%>%
  add_osm_feature(key = "natural", 
                  value = "tree") %>%
  osmdata_sf()

big_streets_melbourne <- getbb("Melbourne Australia")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

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

#cols = rainbow(152548, s=.6, v=.9)[sample(1:152548,152548)]

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

# *4.1 main map ----
main_map<- ggplot() +
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
          #color = "gold",
          linewidth = 3,
          alpha = 1), colour='#694b37', sigma = 5, expand = 5) +
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
  labs(title = "ALBERT PARK GRAND PRIX CIRCUIT", subtitle = "37.842°S / 144.950°E") 

main_map

# save plot
ggsave('./03_plots/albert_park.png', dpi = 320, height = 20, width = 15, units = 'cm')
ggsave('./03_plots/albert_park.jpg', dpi = 320, height = 20, width = 15, units = 'cm')
ggsave('./03_plots/albert_park.pdf', units = "in", width = 5.5, height = 7)

# *4.2 main map - simple----

main_map_simple <- ggplot() +
  # geom_sf(data = med_streets_melbourne$osm_lines,
  #         inherit.aes = FALSE,
  #         color = "black",
  #         size = .3,
  #         alpha = .5) +
  # geom_sf(data = small_streets_melbourne$osm_lines,
  #         inherit.aes = FALSE,
  #         color = "#666666",
  #         size = .2,
  #         alpha = .3) +
  # geom_sf(data = big_streets_melbourne$osm_lines,
  #         inherit.aes = FALSE,
  #         color = "black",
  #         size = .5,
  #         alpha = .6) +
  # geom_sf(data = F1_circuit_melbourne$osm_lines,
  #                         inherit.aes = FALSE,
  #                         color = "#00843D",
  #                         #color = "gold",
  #                         linewidth = 3,
  #                         alpha = 1) +
  geom_sf(data = F1_circuit_melbourne$osm_lines,
          color = "grey70",
          linewidth = 1.5,
          alpha = 1,
          inherit.aes = FALSE) +
  # geom_sf(data = F1_albert_park_lake$osm_multipolygons,
  #         inherit.aes = FALSE,
  #         fill = "cornflowerblue",
  #         color = "lightskyblue",
  #         size = 0.1,
  #         alpha = 0.5) +
  # geom_sf(data = gunn_island$osm_multipolygons,
  #         inherit.aes = FALSE,
  #         fill = "forestgreen",
  #         color = "forestgreen",
  #         size = 0.1,
  #         alpha = 0.5) +
  # geom_sf(data = mud_island$osm_polygons,
  #         inherit.aes = FALSE,
  #         fill = "forestgreen",
  #         color = "forestgreen",
  #         size = 0.1,
  #         alpha = 0.5) +
  # geom_sf(data = trees_melbourne$osm_points,
  #         inherit.aes = FALSE,
  #         color = cols_olive,
  #         linewidth = 2,
  #         size = tree_size,
  #         alpha = 0.75) +
  coord_sf(xlim = c(144.9615, 144.979),
           ylim = c(-37.855, -37.8375),
           expand = FALSE) + 
  #theme_void()
  theme(plot.background = element_rect(fill = 'white'),
        #plot.caption = element_text(family = 'lato', hjust = 0.5, size = 12, color = "#694b37", margin = margin(t = -25)),
        plot.title = element_text(size = 20, family = "lato", face="bold", hjust=0.5, colour = "#694b37"), plot.subtitle = element_text(family = "lato", size = 16, colour = "#694b37", hjust=0.5),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        panel.border = element_rect(colour = "#694b37", fill=NA, size=1))
  #geom_sf_label(data = coordinates_sf_turnberry, aes(label = place), nudge_x = 0.6, nudge_y = 0.35, family = 'ultra', colour = 'black', size = 6, label.size = 0.5) +
  #labs(caption = "Design: Allan James | @allanjames1506") +
  #annotate("label", y = -37.8545, x = 144.97, label = "Design: Allan James | @allanjames1506", lineheight = 0.75, family = 'lato',  size = 4.5, color = "#694b37", vjust = 0.5) +
  #labs(title = "ALBERT PARK GRAND PRIX CIRCUIT", subtitle = "37.842°S / 144.950°E") 

main_map_simple

ggsave('./03_plots/albert_park_simple.png', dpi = 320, height = 20, width = 15, units = 'cm')

# *4.3 main map simple animate----
oz_circuit_map <- read.csv('./00_raw_data/oz_track_dots_map_nn.csv') %>% 
  arrange(order) %>% 
  rename(lng = x, lat = y)

oz_circuit_map_sf <- st_as_sf(oz_circuit_map,
                              coords = c("lng", "lat"),
                              crs = 4326,
                              remove = FALSE)

oz_circuit_map_static <- ggplot(oz_circuit_map_sf, aes(lng, lat)) + 
  geom_point(size = 3)

oz_circuit_map_static

main_map_animate <- ggplot() +
  # geom_sf(data = med_streets_melbourne$osm_lines,
  #       inherit.aes = FALSE,
  #       color = "black",
  #       size = .3,
  #       alpha = .5) +
  # geom_sf(data = small_streets_melbourne$osm_lines,
  #       inherit.aes = FALSE,
  #       color = "#666666",
  #       size = .2,
  #       alpha = .3) +
  # geom_sf(data = big_streets_melbourne$osm_lines,
  #       inherit.aes = FALSE,
  #       color = "black",
  #       size = .5,
  #       alpha = .6) +
  geom_sf(data = F1_circuit_melbourne$osm_lines,
                        inherit.aes = FALSE,
                        color = "#00843D",
                        #color = "gold",
                        linewidth = 3,
                        alpha = 1) +
  geom_sf(data = F1_circuit_melbourne$osm_lines,
        color = "grey70",
        linewidth = 1.5,
        alpha = 1,
        inherit.aes = FALSE) +
  # geom_sf(data = F1_albert_park_lake$osm_multipolygons,
  #       inherit.aes = FALSE,
  #       fill = "cornflowerblue",
  #       color = "lightskyblue",
  #       size = 0.1,
  #       alpha = 0.5) +
  # geom_sf(data = gunn_island$osm_multipolygons,
  #       inherit.aes = FALSE,
  #       fill = "forestgreen",
  #       color = "forestgreen",
  #       size = 0.1,
  #       alpha = 0.5) +
  # geom_sf(data = mud_island$osm_polygons,
  #       inherit.aes = FALSE,
  #       fill = "forestgreen",
  #       color = "forestgreen",
  #       size = 0.1,
  #       alpha = 0.5) +
# geom_sf(data = trees_melbourne$osm_points,
#         inherit.aes = FALSE,
#         color = cols_olive,
#         linewidth = 2,
#         size = tree_size,
#         alpha = 0.75) +
coord_sf(xlim = c(144.9615, 144.979),
         ylim = c(-37.855, -37.8375),
         expand = FALSE) + 
  geom_point(data = oz_circuit_map_sf,
             inherit.aes = FALSE,
             aes(x = lng, y = lat),
             size = 8,
             alpha = 0.5,
             colour = 'deeppink') +
  theme_void() +
  theme(plot.background = element_rect(fill = 'white'),
        #plot.caption = element_text(family = 'lato', hjust = 0.5, size = 12, color = "#694b37", margin = margin(t = -25)),
        plot.title = element_text(size = 20, family = "lato", face="bold", hjust=0.5, colour = "#694b37"), plot.subtitle = element_text(family = "lato", size = 16, colour = "#694b37", hjust=0.5),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        panel.border = element_rect(colour = "#694b37", fill=NA, size=1)) +
  enter_appear() +
  transition_states(order, transition_length = 1, state_length = 2)
#geom_sf_label(data = coordinates_sf_turnberry, aes(label = place), nudge_x = 0.6, nudge_y = 0.35, family = 'ultra', colour = 'black', size = 6, label.size = 0.5) +
#labs(caption = "Design: Allan James | @allanjames1506") +
#annotate("label", y = -37.8545, x = 144.97, label = "Design: Allan James | @allanjames1506", lineheight = 0.75, family = 'lato',  size = 4.5, color = "#694b37", vjust = 0.5) +
#labs(title = "ALBERT PARK GRAND PRIX CIRCUIT", subtitle = "37.842°S / 144.950°E")

main_map_animate
animate(main_map_animate)
animate(main_map_animate, nframes = 172)

anim_save("./04_gifs/first_saved_animation_main_map_animate.gif", height = 372, width = 538, units = "px")

# 5 inset maps (development)----
# https://ggplot2-book.org/maps

# *5.1 Australia map----
sf_aus <- ozmap("states")

oz_capitals <- tibble::tribble( 
  ~city,           ~lat,     ~lon,
  "Sydney",    -33.8688, 151.2093,  
  "Melbourne", -37.8136, 144.9631, 
  "Brisbane",  -27.4698, 153.0251, 
  "Adelaide",  -34.9285, 138.6007, 
  "Perth",     -31.9505, 115.8605, 
  "Hobart",    -42.8821, 147.3272, 
  "Canberra",  -35.2809, 149.1300, 
  "Darwin",    -12.4634, 130.8456, 
)

albert_park <- tibble::tribble(
  ~location,         ~lat,   ~lon,
  "Albert Park", -37.8545, 144.97,
)

ozmaps::abs_ced$NAME

# *5.2 Melbourne map----
melbourne_map <- ozmaps::abs_ced %>% filter(NAME %in% c("Banks", "Barton", "Bennelong", "Berowra", "Blaxland",
                                                        "Bradfield", "Calare", "Chifley", "Cook", "Cowper",
                                                        "Cunningham",  "Dobell", "Eden-Monaro", "Farrer", "Fowler",
                                                        "Gilmore", "Grayndler", "Greenway", "Hughes", "Hume",
                                                        "Hunter", "Kingsford Smith", "Lindsay", "Lyne", "Macarthur",
                                                        "Mackellar", "Macquarie", "McMahon", "Mitchell", "New England",
                                                        "Newcastle", "North Sydney", "Page", "Parkes", "Parramatta",
                                                        "Paterson", "Reid", "Richmond", "Riverina", "Robertson",
                                                        "Shortland", "Sydney", "Warringah", "Watson", "Wentworth",
                                                        "Werriwa", "Whitlam", "Aston", "Ballarat", "Bendigo",
                                                        "Bruce", "Calwell", "Casey", "Chisholm", "Cooper",
                                                        "Corangamite", "Corio", "Deakin", "Dunkley", "Flinders",
                                                        "Fraser", "Gellibrand", "Gippsland", "Goldstein", "Gorton",
                                                        "Higgins", "Holt", "Hotham", "Indi", "Isaacs",
                                                        "Jagajaga", "Kooyong", "La Trobe", "Lalor", "Macnamara",
                                                        "Mallee", "Maribyrnong", "McEwen", "Melbourne", "Menzies",
                                                        "Monash", "Nicholls", "Scullin", "Wannon", "Wills",
                                                        "Blair", "Bonner", "Bowman", "Brisbane", "Capricornia",
                                                        "Dawson", "Dickson", "Fadden", "Fairfax", "Fisher",
                                                        "Flynn", "Forde", "Griffith", "Groom", "Herbert",
                                                        "Hinkler", "Kennedy", "Leichhardt", "Lilley", "Longman",
                                                        "Maranoa", "McPherson", "Moncrieff", "Moreton", "Oxley",
                                                        "Petrie", "Rankin", "Ryan", "Wide Bay", "Wright",
                                                        "Adelaide", "Barker", "Boothby", "Grey", "Hindmarsh",
                                                        "Kingston", "Makin", "Mayo", "Spence", "Sturt",
                                                        "Brand", "Burt", "Canning", "Cowan", "Curtin",
                                                        "Durack", "Forrest", "Fremantle", "Hasluck", "Moore",
                                                        "O'Connor", "Pearce", "Perth", "Stirling", "Swan",
                                                        "Tangney", "Bass", "Braddon", "Clark", "Franklin",
                                                        "Lyons", "Lingiari", "Solomon", "Bean", "Canberra",
                                                        "Fenner"))

# merge local authority borders for melbourne
melbourne_map_union <- st_union(melbourne_map)

# define points for Melbourne and Albert Park
coordinates_melbourne <- data.frame(
  place = c('The City of Melbourne', 'Albert Park'),
  longitude = c(144.963, 144.97),
  latitude = c(-37.814, -37.85)
)

# project the coordinates
coordinates_sf_melbourne <- coordinates_melbourne |>  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)

melbourne_inset_map <- ggplot(melbourne_map_union) + 
  geom_sf(aes(), fill = 'floralwhite', show.legend = FALSE) + 
  #coord_sf(xlim = c(144.9055, 145.0283), ylim = c(-37.8345, -37.7735)) + 
  coord_sf(xlim = c(144.8, 145.0), ylim = c(-37.88, -37.8)) +
  geom_point(data = albert_park, mapping = aes(x = lon, y = lat), colour = "forestgreen", size = 2) +
  geom_point(data = oz_capitals %>% filter(city == 'Melbourne'), mapping = aes(x = lon, y = lat), colour = "sienna2", size = 20, alpha = 0.9) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightskyblue")) +
  geom_sf_label(data = coordinates_sf_melbourne, aes(label = place)) +
  ggspatial::annotation_scale(
    location = "tr",
    pad_y = unit(0.2, "in"),
    bar_cols = c("grey60", "white"),
    text_family = "lato"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.6, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "lato"
    )
  )
  #geom_sf_label(aes(label = location), label.padding = unit(1, "mm")) 
  
melbourne_inset_map

# define points for Melbourne and Albert Park
coordinates_melbourne2 <- data.frame(
  place = 'Melbourne',
  longitude = 144.963,
  latitude = -36.65
)

# project the coordinates
coordinates_sf_melbourne2 <- coordinates_melbourne2 |>  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)

# *5.3 Australia inset map----
australia_inset_map <- ggplot() + 
  geom_sf(data = sf_aus, fill = c('grey70', 'floralwhite', 'grey70', 'grey70', 'grey70', 'grey70', 'grey70', 'grey70', 'grey70')) +
  xlim(112, 155) +
  # geom_hline(yintercept = -37.88, lty = 1, colour = "red") +
  # geom_hline(yintercept = -37.8, lty = 1, colour = "red") +
  # geom_vline(xintercept = 144.75, lty = 1, colour = "red") +
  # geom_vline(xintercept = 145.05, lty = 1, colour = "red") +
  labs() +
  #theme_bw() +
  #geom_point(data = oz_capitals %>% filter(city == 'Melbourne'), mapping = aes(x = lon, y = lat), colour = "sienna2", size = 8) + 
  #coord_sf(expand = FALSE) +
  theme_void() + 
  theme(panel.background = element_rect(fill = "cornflowerblue")) 
  #geom_sf_text(data = coordinates_sf_melbourne2, aes(label = place), size=6)


# *5.4 Melbourne with Australia insetted----
main_inset <- melbourne_inset_map + inset_element(australia_inset_map, 
                                                  left = 0.01, 
                                                  bottom = 0.4, 
                                                  right = unit(1, 'npc') - unit(120, 'mm'), 
                                                  top = unit(1, 'npc') - unit(15, 'mm'))


main_map + inset_element(australia_inset_map,
                         left = 0.05, 
                         bottom = 0.2, 
                         right = 0.4, 
                         top = 0.4,
                         align_to = 'full')


