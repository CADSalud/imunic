

library(ProjectTemplate)
load.project()

library(RColorBrewer)
library(viridisLite)
library(classInt)

library(ggmap)
library(mxmaps)
load("cache/tab_delitos_u.RData")

blups <- brewer.pal(9, "BuPu")
vir <- viridis(9)
mag <- magma(9)


# Get maps ----
cdmx <- c(-99.13, 19.43)
cdmx_gm <- get_map(location = cdmx,     #Long y lat del centro del mapa que buscamos
                   source = "google",           #Fuente, tb OpenStreetView
                   maptype = "watercolor",         #Tipo. También "satellite", "roadmap"
                   zoom = 10)
ggmap(cdmx_gm)

cdmx_gm <- get_map(location = cdmx,     #Long y lat del centro del mapa que buscamos
                   source = "stamen",           #Fuente, tb OpenStreetView
                   maptype = "toner",         #Tipo. También "satellite", "roadmap"
                   zoom = 12)
ggmap(cdmx_gm)

mx_gm <- get_map(location = c(right=-85, left=-121, bottom=13, top=33),
                 # maptype = "watercolor",
                 maptype = "stamen",
                 source = "toner")
ggmap(mx_gm)


# Ejemplo de mapas municipios ----
mxmunicipio.map %>% head
mxmunicipio.map$region %>% n_distinct()
mxmunicipio.map$id %>% n_distinct()

tab_map <- mxmunicipio.map %>% 
  as.tibble() %>% 
  mutate(state_code = parse_number(str_sub(id, 1, 2)), 
         mun_code = parse_number(str_sub(id, 3, 5)) ) %>% 
  left_join(tab_delitos_u, 
            spread(indicador, value), 
            by = c("state_code", "mun_code"))
tab_map

ggmap(mx_gm, 
      base_layer = ggplot(aes(long, lat), 
                          data =filter(tab_map, indicador == "prop_seguridad"))) + 
  geom_polygon(aes(group = group, fill = value), 
               alpha = .08)

qmplot(long, lat,
       data = filter(tab_map, indicador == "prop_seguridad"),
       geom = "polygon", fill = value)


# Trying tmap ----
library(sp)
library(tmap)

data(Europe)
summary(Europe)
class(Europe)
Europe$name

tm_shape(Europe) +
  tm_fill(col = "pop_est", 
          style = "quantile",
          palette = blups) +
  tm_borders(col = "burlywood4")
save_tmap(filename = "tmap_prueba.html")

tm_shape(Europe, 
         projection = "robin") +
  tm_fill(col = "economy", 
          style = "quantile") +
  tm_bubbles(size = "pop_est_dens", 
             style = "quantile") +
  tm_borders(col = "burlywood4") +
  tm_style_classic()

save_tmap(filename = "graphs/tmap_prueba.pdf")


# cartogram and spatial dep----
library(cartogram)
library(spdep)


library(maps)
library(maptools)
library(spdep)
library(classInt) ## Will be used for plotting maps later 
library(RColorBrewer)
library(tmap)

# Shape files ----

# Municipios (2458)
# spatial data frame 
dir("data/mex_muncs_shapes/")
shp_mun_rgdal <-  rgdal::readOGR("data/mex_muncs_shapes/merge.shp")
class(shp_mun_rgdal)

str(shp_mun_rgdal, max.level = 2)
summary(shp_mun_rgdal)

plot(shp_mun_rgdal)
shp_mun_rgdal@data %>% head

tm_shape(shp_mun_rgdal) + 
  tm_borders(col = "gray70") +  
  # tm_text(text = "CVE_ENT", size = 0.5) +
  tm_credits("Source: ACS 2014 5-year Estimates, \n accessed via acs package", 
             position = c("right", "bottom"))



# Estados (32)
# spatial data frame 
dir("data/mex_edos_shapes")
shp_edo_rgdal <-  rgdal::readOGR("data/mex_edos_shapes/Mex_Edos.shp")
class(shp_edo_rgdal)

str(shp_edo_rgdal, max.level = 2)
summary(shp_edo_rgdal)

plot(shp_edo_rgdal)

shp_edo_rgdal$state_code <- 1:32
plot(shp_edo_rgdal)
plot(shp_edo_rgdal[shp_edo_rgdal$state_code == 1, ], 
     col = "red", add = TRUE)
plot(shp_edo_rgdal[shp_edo_rgdal$state_code == 9, ], 
     col = "red", add = TRUE)
plot(shp_edo_rgdal[shp_edo_rgdal$state_code == 15, ], 
     col = "blue", add = TRUE)

shp_edo_rgdal@data


one <- shp_edo_rgdal@polygons[[1]]
length( shp_edo_rgdal@polygons)


# merge
any(duplicated(shp_edo_rgdal$state_code))
all(shp_edo_rgdal$state_code %in% tab_delitos$state_code)
all(tab_delitos$state_code %in% shp_edo_rgdal$state_code)

shp_delitos <- merge(shp_edo_rgdal, tab_delitos, by.x = "idunion", by.y = "idunion")
summary(shp_delitos)

tm_shape(shp_edo_rgdal) + 
  tm_borders() + 
  tm_bubbles(size = "state_code") + 
  tm_text(text = "NOM_ENT", size = 0.5) +
  tm_credits("Source: ACS 2014 5-year Estimates, \n accessed via acs package", 
             position = c("right", "bottom"))






