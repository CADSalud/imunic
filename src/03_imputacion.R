
library(ProjectTemplate)
load.project()


library(mxmaps)
library(tmap)
library(sp)
library(spdep)
library(gstat)
library(automap)

library(RColorBrewer)
library(viridisLite)
# library(R2BayesX)

blups <- brewer.pal(9, "BuPu")
vir <- viridis(9)
mag <- magma(9)


# 1. Datos ----

# Códigos de entidad y municipio
load("cache/tab_cods_estmun.RData")
tab_cods_estmun %>% head


# Delitos por municipio
load("cache/tab_delitos_u.RData")
tab_delitos_u$indicador %>% unique()

tab_delitos <- tab_delitos_u %>% 
  spread(indicador, value)
tab_delitos %>% summary

# Estados (32)
# spatial data frame 
dir("data/mex_edos_shapes")
shp_edo_rgdal <-  rgdal::readOGR("data/mex_edos_shapes/Mex_Edos.shp") %>% 
  merge(tab_cods_estmun %>% 
          dplyr::select(NOM_ENT, state_code) %>% 
          unique(), by = "NOM_ENT" )
class(shp_edo_rgdal)
shp_edo_rgdal@data %>% head

# Municipios (2457)
# spatial data frame 
dir("data/mex_muncs_shapes/")
shp_mun_rgdal <-  rgdal::readOGR("data/mex_muncs_shapes/merge.shp") 

# duplicados
any(duplicated(shp_mun_rgdal$CVEGEO))
shp_mun_rgdal <- shp_mun_rgdal[-1962, ]
shp_mun_rgdal

class(shp_mun_rgdal)
shp_mun_rgdal$mun_code <- parse_number(shp_mun_rgdal$CVE_MUN)
shp_mun_rgdal$state_code <- parse_number(shp_mun_rgdal$CVE_ENT)

# union
shp_mun_rgdal <- shp_mun_rgdal %>% 
  merge(tab_cods_estmun, by = c("state_code", "mun_code") ) %>% 
  merge(tab_delitos, by = c("state_code", "mun_code") ) 
  
shp_mun_rgdal@data %>% head
names(shp_mun_rgdal)


# 2. Graficas ----
tm_shape(shp_mun_rgdal) + 
  tm_fill(col = "prop") + 
  tm_shape(shp_edo_rgdal) + 
  tm_borders() +
  # tm_text(text = "state_code") + #, col = "white", remove.overlap =T
  tm_credits("Fuente: INEGI", 
             position = c("right", "bottom")) 
 save_tmap(filename = "graphs/tmap_inseguridad.png")


# 3. Pruebas ----

mun_nb <- poly2nb(shp_mun_rgdal) # neighbor list muns
mun_centers <- coordinates(shp_mun_rgdal) # mun centers
plot(mun_nb, mun_centers)
# spplot(shp_mun_rgdal, zcol = "prop_seguridad")

mun_centers %>% head()
mun_centers %>% dim()
dim(mun_centers)

# numero de observaciones 
shp_mun_rgdal$state_code %>% length()
shp_mun_rgdal$mun_code %>% length()
shp_mun_rgdal@data %>% dim
sum(apply(!is.na(shp_mun_rgdal@data), 1, sum) == 0 ) # 0 renglones NA

shp_mun_rgdal@polygons %>% length()
shp_mun_rgdal@polygons[[1]] %>% str(max.level = 2)
shp_mun_rgdal@plotOrder

mun_centers %>% head()
shp_mun_rgdal@data %>% head()

tab_coords <- mun_centers %>% 
  as_data_frame() %>% 
  mutate(CVEGEO = shp_mun_rgdal$CVEGEO) %>% 
  rename(long = V1, lat = V2)

# merge de coordenadas 
shp_mun_rgdal$CVEGEO[duplicated(shp_mun_rgdal$CVEGEO)]
any(duplicated(shp_mun_rgdal$CVEGEO))
all(shp_mun_rgdal$CVEGEO %in% tab_coords$CVEGEO)
all(tab_coords$CVEGEO %in% shp_mun_rgdal$CVEGEO)
setdiff(tab_coords$CVEGEO, shp_mun_rgdal$CVEGEO)

shp_mun_rgdal <- shp_mun_rgdal %>% 
  merge(tab_coords)


# 4. Impuntacion ----

imp_shp <- shp_mun_rgdal#[shp_mun_rgdal$state_code %in% c(9, 15), ]
imp_mun_nb <- poly2nb(imp_shp) # neighbor list muns
imp_mun_centers <- coordinates(imp_shp) # mun centers


plot(imp_mun_nb, imp_mun_centers)
tm_shape(imp_shp) + 
  tm_fill(col = "prop_seguridad")

# moran.test(imp_shp$prop_seguridad, 
#            nb2listw( poly2nb(imp_shp) ))


# faltantes
nas_vec <- is.na(imp_shp$prop_seguridad)
sum(nas_vec)
sum(nas_vec)/length(nas_vec)



# variograma de faltante sy no faltantes
plot(variogram(prop_seguridad ~ 1, imp_shp[!nas_vec, ]))
mod_vgm <- variogram(prop_seguridad ~ long + lat, imp_shp[!nas_vec, ])
plot(mod_vgm)


# 4a. autokrige
mod_autok <- autoKrige(formula = prop_seguridad ~ V1 + V2, 
                     input_data = imp_shp[!nas_vec, ], 
                     new_data  = imp_shp[nas_vec, ],
                     model = "Mat")


# 4b. kriging manual -----

# Eyeball the variogram and estimate the initial parameters
nugget <- .02
psill <- .03-.02
range <- 600

# Fit the variogram
v_model <- fit.variogram(
  mod_vgm, 
  model = vgm(
    model = "Ste",
    nugget = nugget,
    psill = psill,
    range = range,
    kappa = 0.5
  )
)

# Show the fitted variogram on top of the binned variogram
plot(mod_vgm, model = v_model)
print(v_model)

# Set the trend formula and the new data
km <- krige(prop_seguridad ~ long + lat, imp_shp[!nas_vec, ], newdata = imp_shp[nas_vec, ], model = v_model)
names(km)

# Plot the predicted values
spplot(km, "var1.pred")







