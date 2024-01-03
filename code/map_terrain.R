#---
# map_terrain.R
#
# This Rscript: generate a map displaying
# * the terrain of Vietnam as highlighted part
# * the terrain of neighboring landscapes as background
#
# Dependencies...
# data/VNM_gis/VNM_adm0.shp
#
# Produces...
# figures/map_terrain.pdf
# figures/map_terrain.png
#---

# install package `lehuynh`
# if(!requireNamespace("devtools", quietly = TRUE)) {
#         install.packages("devtools")
# }
# devtools::install_github("le-huynh/lehuynh")

library(tidyverse)
library(ggmap)
library(sf)
library(here)
library(lehuynh)

# Step 1: Specify the boundaries of the map drawing rectangle #---------
# set plot boundaries
# coordinate data can be obtained from several websites, for example, [bboxfinder.com](http://bboxfinder.com/)
longitude <- c(100.854490, 116.147458)
latitude <- c(6.945000, 23.812699)

# Step 2: Get the stamen map of the plot #--------------
# enable Stadia Maps services in R using API key
ggmap::register_stadiamaps(key = "YOUR-API-KEY", write = FALSE)

# Note: increase `zoom` to get higher resolution
background_map <- ggmap::get_stadiamap(bbox = c(longitude[1],
                                                latitude[1],
                                                longitude[2],
                                                latitude[2]),
                                       maptype = "stamen_terrain_background",
                                       zoom = 7)

# Step 3: Read the polygonal file to outline the boundary of Vietnam #--------
# data can be downloaded from [diva-gis](https://www.diva-gis.org/gdata) as .zip file
# corresponding file format = SHAPEFILE (.shp), level = 0 = country
target_extent <- sf::st_read(here::here("data/VNM_gis/VNM_adm0.shp"))

# Step 4: Convert shapefile to be compatible with 'ggplot2'#-------
target_shape <- # convert coordinates into correct CRS
                st_transform(target_extent, crs = 4326) %>%
                # convert data type to be compatible with 'ggplot2'
                as_Spatial()

# Step 5: Create the map using 'ggplot2' #---------
map <- ggmap(background_map) +
        geom_polygon(data = target_shape,
                     aes(x = long, y = lat, group = group),
                     colour = 'white',
                     fill = 'gray87',
                     alpha = 0.65,
                     size = 0.5) +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              plot.margin = margin(t = 0.1,
                                   r = 0.1,
                                   b = 0.1,
                                   l = 0.1,
                                   unit = "in"))

# Step 6: Incorporate cities and islands into the map #-----------
# generate dataframe containing coordinate data and site names
site_df <- data.frame(site_name = c("Ha Noi",
                                    "Ho Chi Minh City",
                                    "Paracel Islands \n(Hoang Sa, VIETNAM)",
                                    "Spratly Islands \n(Truong Sa, VIETNAM)"),
                      site_latitude = c(21.028511, 10.823020, 16.83517, 8.64464),
                      site_longitude = c(105.804817, 106.629650, 112.33873, 111.91969))

# mapping using ggplot2
map <- map +
        # add the areas of islands as large circles on the map
        geom_point(data = site_df %>% filter(str_detect(site_name, "Islands")),
                   mapping = aes(x = site_longitude,
                                 y = site_latitude),
                   size = 16,
                   color = "coral",
                   alpha = 0.3) +
        # add the cities and primary islands as points
        geom_point(mapping = aes(x = site_longitude,
                                 y = site_latitude),
                   data = site_df,
                   size = 4,
                   colour = "coral1") +
        # add names of cities and islands as text labels
        geom_text(mapping = aes(x = site_longitude,
                                y = site_latitude + 0.5,
                                label = site_name),
                  data = site_df,
                  size = 5)

# Step 7: Add the map title #-------------------------------------------------
map <- map +
        geom_text(mapping = aes(x = 112.1, y = 23, label = "MAP OF VIETNAM"),
                  size = 10,
                  color = "black")

# Step 8: Save the map #-------------------------------------------------
lehuynh::ggsave_elsevier(
        filename = "figures/map_terrain.pdf",
        plot = map,
        width = "full_page",
        height = 210)

lehuynh::ggsave_elsevier(
        filename = "figures/map_terrain.png",
        plot = map,
        width = "full_page",
        height = 210)

