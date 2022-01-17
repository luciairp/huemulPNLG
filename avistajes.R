library(tidyverse)
guess_encoding("planilla_base_huemul.csv")
datos <- read.csv("planilla_base_huemul.csv", header = T,
                  sep = "," , dec = ",")

datos <- as_tibble(datos)

library(sf)
library(tmap)

datos.sf <- st_as_sf(datos, coords=c("Longitud","Latitud"), crs=4326)

pnlg <- st_read("Limite PNLG -.shp")
pnlg_kml <- st_read("Limite PNLG-.kml")

qplot(datos$Longitud, datos$Latitud)

bbox_new <- st_bbox(datos.sf) # current bounding box
xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[1] <- bbox_new[1] + (0.3 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.1 * xrange) # xmax - right
bbox_new[2] <- bbox_new[2] + (0.5 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.1 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

tmap_options(check.and.fix = TRUE)

sf::st_is_valid(lg_norte)
sf::st_make_valid(lg_norte)


mapa <- tm_shape(pnlg_kml)+
  tm_polygons(col = "red")+
  tm_shape(datos.sf)+
  tm_dots()

datos.sf.yr <- datos.sf %>% 
  group_by(a.o) %>%
  summarise(
    n = n()
  ) %>% 
  ungroup()

mapa_yr <- tm_shape(pnlg_kml_crop)+
  tm_polygons(col = "white")+
  tm_shape(datos.sf.yr)+
  tm_dots(size = "n")+
  tm_facets(by = "a.o", free.coords = F)

pnlg_kml_crop <- st_crop(pnlg_kml,bbox_new)


# transectas --------------------------------------------------------------


datos <- read.csv("CAMP H 2014 (sin editar).csv", header = F,
                 skip = 4)
head(datos)
datos <- select(datos, "V1", "V3", "V4", "V11") %>% 
  transmute(ID = V1,
         Long = V3,
         Lat = V4,
         DateTime = V11)
datos.sf <- st_as_sf(datos, coords=c("Long","Lat"), crs=4326)
qtm(datos.sf)
