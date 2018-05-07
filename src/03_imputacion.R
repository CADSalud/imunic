
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



# Tabla de indicadores intercensal ----
load("cache/inds_list.RData")
tab_intercensal_inds <- inds_list %>% 
  bind_rows() %>% 
  filter(indicadores %in% c("pob_total",
                            "pob_mujeres", "pob_hombres",
                            "pob_menos12", "pob_mas75",
                            "pob_trabaja", 
                            "estudiante", "pob_15mas_sinedubasica",
                            "pob_perte_indigena", 
                            "pob_seg_popular","pob_seg_imss", 
                            "ingreso_q50", 
                            "pob_edad_(15,20]", "pob_edad_(20,25]")) %>% 
  spread(indicadores, valor) %>% 
  mutate(pob_edad_16a25 = `pob_edad_(15,20]`+`pob_edad_(20,25]`) %>% 
  dplyr::select(-`pob_edad_(15,20]`, -`pob_edad_(20,25]`) %>% 
  rename(state_code = ENT, 
         mun_code = MUN) %>% 
  mutate(state_code = parse_number(state_code),
         mun_code = parse_number(mun_code)) 
names(tab_intercensal_inds)
summary(tab_intercensal_inds)


# Delitos por municipio ----
load("cache/tab_delitos_u.RData")
tab_delitos_inds <- tab_delitos_u %>% 
  filter(!is.na(value)) %>% 
  mutate(indicador = gsub("[(]violacion)|[(]secuestro)", "", x = indicador) %>% 
           str_replace_all(string = ., ":", "") %>% 
           str_replace_all(string = ., "/", "") %>% 
           str_replace_all(string = ., "[.]", "") %>% 
           str_replace_all(string = ., "   ", " ") %>% 
           str_replace_all(string = ., "  ", " ") %>% 
           str_replace_all(string = ., " ", "_")) %>% 
  filter(indicador %in% c("scrt_homicidios_dolosos", 
                          "scrt_priv_de_la_libertad_secuestro",
                          "scrt_robo_comun_cs_violencia",
                          "vicper_robo_calle_transporte",
                          "vichgr_homicidio",
                          "vichgr_desap_forzada",
                          "vichgr_secuestro",
                          "prop_seguridad")) %>% 
  spread(indicador, value) 
names(tab_delitos_inds)
summary(tab_delitos_inds)


# Union con estados y municipios ----
dim(tab_delitos_inds)
dim(tab_intercensal_inds)
tab_data <- tab_cods_estmun %>% 
  left_join(tab_intercensal_inds,
            by = c("state_code", "mun_code")) %>% 
  left_join(tab_delitos_inds, 
            by = c("state_code", "mun_code"))
dim(tab_data) # 2457
names(tab_data)
filter(tab_data, is.na(pob_trabaja)) %>% 
  data.frame()
summary(tab_data)


# Estados (32)  ----
# spatial data frame 
dir("data/mex_edos_shapes")
shp_edo_rgdal <-  rgdal::readOGR("data/mex_edos_shapes/Mex_Edos.shp") %>% 
  merge(tab_cods_estmun %>% 
          dplyr::select(NOM_ENT, state_code) %>% 
          unique(), by = "NOM_ENT" )
class(shp_edo_rgdal)
shp_edo_rgdal@data %>% head

# Municipios (2457) ----
# spatial data frame 
dir("data/mex_muncs_shapes/")
shp_mun_rgdal <-  rgdal::readOGR("data/mex_muncs_shapes/merge.shp") 

# duplicados
any(duplicated(shp_mun_rgdal$CVEGEO))
shp_mun_rgdal <- shp_mun_rgdal[-1962, ]
shp_mun_rgdal
any(duplicated(shp_mun_rgdal$CVEGEO))

class(shp_mun_rgdal)
shp_mun_rgdal$mun_code <- parse_number(shp_mun_rgdal$CVE_MUN)
shp_mun_rgdal$state_code <- parse_number(shp_mun_rgdal$CVE_ENT)

# coordenadas
tab_coords <- coordinates(shp_mun_rgdal)  %>% 
  as_data_frame() %>% 
  mutate(CVEGEO = shp_mun_rgdal$CVEGEO) %>% 
  rename(long = V1, lat = V2)

# union
shp_mun_rgdal <- shp_mun_rgdal %>% 
  merge(tab_cods_estmun ) %>% 
  merge(tab_data ) %>% 
  merge(tab_coords)
  
shp_mun_rgdal@data %>% head
names(shp_mun_rgdal)
summary(shp_mun_rgdal)





# 2. Graficas ----
tm_shape(shp_mun_rgdal) + 
  tm_fill(col = "pob_perte_indigena") + 
  # tm_shape(shp_edo_rgdal) + 
  # tm_borders() +
  # tm_text(text = "state_code") + #, col = "white", remove.overlap =T
  tm_credits("Fuente: INEGI", 
             position = c("left", "bottom")) 
# save_tmap(filename = "graphs/tmap_inseguridad.png")


# 3. Pruebas ----
mun_centers <- coordinates(shp_mun_rgdal) # mun centers
mun_nb <- poly2nb(shp_mun_rgdal) # neighbor list muns
plot(mun_nb, mun_centers)

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




# 4. Imputacion ----
library(mi)
library(bayesplot)

# Análisis de faltantes
df_data <- shp_mun_rgdal@data %>% 
  dplyr::select(mun_code, state_code,
                estudiante:lat, 
                -prop_seguridad)
class(df_data)
head(df_data)
dim(df_data)

mdf_data <- missing_data.frame(df_data)
show(mdf_data)
summary(mdf_data)
image(mdf_data)
hist(mdf_data)

mdf_data <- change(mdf_data, 
              y = c("estudiante", "pob_15mas_sinedubasica", "pob_perte_indigena", 
                    "pob_seg_imss", "pob_seg_popular", "pob_trabaja"), 
              what = "type",
              to = rep("count", 6))

mdf_data <- change(mdf_data, 
              y = c("ingreso_q50", 
                    "vichgr_desap_forzada", "vichgr_homicidio",
                    "vichgr_secuestro", "vicper_robo_calle_transporte"), 
              what = "transformation",
              to = rep("logshift", 5) ) # no negativas

imps_mi <- mi(mdf_data,
              n.iter = 40,
              n.chains = 4,
              max.minutes = 10)
show(imps_mi)
round(mipply(imps_mi, mean, to.matrix = TRUE), 3)

Rhats(imps_mi) 
mcmc_rhat(Rhats(imps_mi))
imps_mi <- mi(imps_mi, n.iter = 5)

plot(imps_mi)
hist(imps_mi)
image(imps_mi)
summary(imps_mi)

cache("imps_mi")



analysis_miss <- pool(estudiante ~ pob_menos12 + 
                        long + lat,
                 data = imps_mi, 
                 m = 5)
display(analysis_miss)

comp_imps_mi <- mi::complete(imps_mi, m = 4)
summary(comp_imps_mi)
str(comp_imps_mi)
comp_imps_mi[[1]] %>% head
comp_imps_mi[[3]] %>% 
  filter(state_code == 1, mun_code== 8)


df_imps <- lapply(comp_imps_mi, function(sub){
  sub %>% 
    select(mun_code, state_code, estudiante:vicper_robo_calle_transporte) %>% 
    gather(variable, value, estudiante:vicper_robo_calle_transporte) %>% 
    as_tibble()
  }) %>% 
  bind_rows() %>% 
  group_by(mun_code, state_code, variable) %>% 
  summarise(mean = mean(value),
            median = median(value)) %>% 
  ungroup() %>% 
  left_join(comp_imps_mi[[1]] %>% 
              dplyr::select(state_code, mun_code, starts_with('missing')) %>% 
              gather(variable, value, -c(state_code, mun_code)) %>% 
              mutate(variable = str_replace_all(variable, "missing_", "")) %>% 
              as_tibble(), 
            by = c("mun_code", "state_code", "variable")) %>% 
  rename(missing_imp = value) %>% 
  mutate(missing_imp = fct_explicit_na( factor(missing_imp), "Sin faltantes") )
df_imps

df_data_imp <- df_data %>% 
  select(mun_code, state_code, estudiante:vicper_robo_calle_transporte) %>% 
  gather(variable, value, estudiante:vicper_robo_calle_transporte) %>% 
  as_tibble() %>% 
  left_join(df_imps)

filter(df_data_imp, mun_code == 8, state_code == 1)



# Revisar





# Datos Kriging ----
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



# 
# # Suavizamiento ----
# # Make neighbor list
# borough_nb <- poly2nb(london_ref)
# borough_centers <- coordinates(london_ref)
# 
# # Show the connections
# plot(london_ref); plot(borough_nb, borough_centers, add = T)
# 
# # Map the total pop'n
# spplot(london_ref, zcol = "TOTAL_POP")
# 
# # Run a Moran I test on total pop'n
# moran.test(
#   london_ref$TOTAL_POP, 
#   nb2listw(borough_nb)
# )
# 
# 
# borough_nb <- poly2nb(london)
# borough_gra <- nb2gra(borough_nb)
# 
# # Fit spatial model
# flu_spatial <- bayesx(
#   Flu_OBS ~ HealthDeprivation + sx(i, bs = "spatial", map = borough_gra),
#   offset = log(london$TOTAL_POP),
#   family = "poisson", data = data.frame(london), 
#   control = bayesx.control(seed = 17610407)
# )
# 
# # Summarize the model
# summary(flu_spatial)
# summary(flu_spatial)
# 
# # Map the fitted spatial term only
# london$spatial <- fitted(flu_spatial, term = "sx(i):mrf")[, "Mean"]
# spplot(london, zcol = "spatial")
# 
# # Map the residuals
# london$spatial_resid <- residuals(flu_spatial)[, "mu"]
# spplot(london, zcol = "spatial_resid")
# 
# # Test residuals for spatial correlation
# moran.mc(london$spatial_resid, nb2listw(borough_nb), 999)
# 
# 
# 
# library(gstat)
# miss <- is.na(ca_geo$pH)
# 
# # Make a variogram of the non-missing data
# plot(variogram(pH ~ 1, ca_geo[!miss, ]))
# 
# 
# # The pH depends on the coordinates
# ph_vgm <- variogram(pH ~ x + y, ca_geo[!miss, ])
# plot(ph_vgm)
# 
# # Eyeball the variogram and estimate the initial parameters
# nugget <- .16
# psill <- .28-.16
# range <- 10000
# 
# # Fit the variogram
# v_model <- fit.variogram(
#   ph_vgm, 
#   model = vgm(
#     model = "Ste",
#     nugget = nugget,
#     psill = psill,
#     range = range,
#     kappa = 0.5
#   )
# )
# 
# # Show the fitted variogram on top of the binned variogram
# plot(ph_vgm, model = v_model)
# print(v_model)
# 
# 
# # Set the trend formula and the new data
# km <- krige(pH ~ x + y, ca_geo[!miss, ], newdata = ca_geo[miss, ], model = v_model)
# names(km)
# 
# # Plot the predicted values
# spplot(km, "var1.pred")
# 
# # Compute the probability of alkaline samples, and map
# km$pAlkaline <- 1 - pnorm(7, mean = km$var1.pred, sd = sqrt(km$var1.var))
# spplot(km, "pAlkaline")
# 
# 
# plot(geo_bounds); points(ca_geo)
# 
# # Find the corners of the boundary
# bbox(geo_bounds)
# 
# # Define a 2.5km square grid over the polygon extent. The first parameter is
# # the bottom left corner.
# grid <- GridTopology(c(537853, 5536290), c(2500, 2500), c(72, 48))
# 
# # Create points with the same coordinate system as the boundary
# gridpoints <- SpatialPoints(grid, proj4string = CRS(projection(geo_bounds)))
# plot(gridpoints)
# 
# # Crop out the points outside the boundary
# cropped_gridpoints <- crop(gridpoints, geo_bounds)
# plot(cropped_gridpoints)
# 
# # Convert to SpatialPixels
# spgrid <- SpatialPixels(cropped_gridpoints)
# coordnames(spgrid) <- c("x", "y")
# plot(spgrid)
# 
# 
# # Do kriging predictions over the grid
# ph_grid <- krige(pH ~ x + y, ca_geo[!miss, ], newdata = spgrid, model = v_model)
# 
# # Calc the probability of pH exceeding 7
# ph_grid$pAlkaline <- 1 - pnorm(7, mean = ph_grid$var1.pred, sd = sqrt(ph_grid$var1.var))
# 
# # Map the probability of alkaline samples
# spplot(ph_grid, zcol = "pAlkaline")
# 
# 
# # Kriging with linear trend, predicting over the missing points
# ph_auto <- autoKrige(
#   pH ~ x + y, 
#   input_data = ca_geo[!miss, ], 
#   new_data = ca_geo[miss, ], 
#   model = "Mat"
# )
# 
# # Plot the variogram, predictions, and standard error
# plot(ph_auto)
# 
# 
# # Auto-run the kriging
# ph_auto_grid <- autoKrige(pH ~ x + y, input_data = ca_geo[!miss, ], new_data = spgrid)
# 
# # Remember predictions from manual kriging
# plot(ph_grid)
# 
# # Plot predictions and variogram fit
# plot(ph_auto_grid)
# 
# # Compare the variogram model to the earlier one
# v_model
# ph_auto_grid$var_model



