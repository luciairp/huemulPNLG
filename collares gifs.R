# Visualizacion gif con movevis ####
rm(list=ls())

library(dplyr)
library(moveVis)
library(lubridate)
library(raster)
library(sf)

data <- read.csv("data_huemul.CSV",header=T,sep=";",dec=".")

#filtros
#filtrar datos regionalmente
data <- data %>% filter(lat < -49) %>% 
  filter(lat > -50) %>% 
  filter(lon < -72.4) %>% 
  filter(lon > -73.2)

# fecha y hora en formato legible
data <- data %>% 
  mutate(fechahora = paste(date, time)) %>% 
  mutate(fechahora = ymd_hms(fechahora))

unique(data$id) #10 aparatos

#filtrar comienzo para cada collar
#cargo datos
datacollares <- read.csv("datoscollares.csv", header=T)
datacollares <- as_tibble(datacollares)
datacollares <- datacollares %>% 
  mutate(fechahora = paste(Fecha, Horaimpacto)) %>% 
  mutate(inicio = dmy_hm(fechahora)) %>% 
  mutate(id = IDCollar) %>% 
  dplyr::select(id, inicio)

#ahora filter cada dato respecto a inicio de cada collar
data <- data %>% 
  left_join(datacollares,by= "id") %>% 
  filter(fechahora > inicio)

# peli
crs_wgs84 <- st_crs(4326)
huemul <- df2move(data, proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", 
                  x = "lon", y = "lat", time = "fechahora", track_id = "id")

m <- align_move(huemul, res = "mean", unit = "hours")


# para extensión:
range(data$lat)
range(data$lon)
extent <- c(-73.0,-72.4,-49.5,-49.1)
extent <- extent(extent)

frames <- frames_spatial(m, path_colours = c("blue", "orange", "lightblue",
                                             "black", "gray", "red", "violet",
                                             "green", "pink", "yellow"),
                         map_service = "osm", map_type = "streets", 
                         map_res = 0.2, alpha = 1, 
                         equidistant = F, path_legend = F
                         #,ext = extent
                         ) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% 
  add_scalebar(position = "bottomright") %>% 
  add_timestamps(m, type = "label") %>% 
  add_progress()  

animate_frames(frames, fps = 40, out_file = "huemul.gif")
