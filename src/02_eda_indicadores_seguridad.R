
library(ProjectTemplate)
load.project()

library(ggcorrplot)
library(tmap)

load("cache/tab_est.RData")
load("cache/tab_mun.RData")


# colores
# blups <- brewer.pal(9, "BuPu")
# vir <- viridis(9)
# mag <- magma(9)



# EDA Estatal ----
tab_est %>% 
  data.frame() %>% 
  head()
tab_est

# Correlación entre delitos
cor(tab_est %>% 
      dplyr::select(starts_with("im_secr_"))) %>%
  ggcorrplot() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Correlación entre delitos",
          "Secretariado")

cor(tab_est %>% 
      dplyr::select(starts_with("im_vict_")) %>% 
      na.omit()) %>%
  ggcorrplot() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Correlación entre delitos", 
          "ENVIPE")


# Delitos reportados y problemas 
names(tab_est)
cor(tab_est %>% 
      dplyr::select(starts_with("im_prob_")) %>% 
      na.omit()) %>%
  ggcorrplot() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Correlación entre delitos", 
          "ENVIPE")

vars_sel <- tidyselect::vars_select(names(tab_est), starts_with("im_vict"))
cor_mat <- lapply(vars_sel, function(nom_var){
  cor(tab_est %>%
        dplyr::select(starts_with("im_prob_"),
                      nom_var))[nom_var, ] %>%
    reshape2::melt(na.rm = TRUE) %>%
    rownames_to_column("secretariado") %>%
    mutate(envipe = nom_var)
}) %>%
  bind_rows()

colors_manual <- viridis::plasma(n = 6)[c(2, 3, 6)]
cor_mat %>%
  filter(secretariado != envipe) %>%
  ggplot(aes(x = secretariado, y = envipe, fill = value)) +
  geom_tile(color = "gray") +
  scale_fill_gradient2(low = colors_manual[1], high = colors_manual[3],
                       mid = colors_manual[2], midpoint = 0,
                       limit = c(-1, 1), name = "Corr") +
  geom_text( aes(label = round(value, 2)) ) +
  theme(axis.text.x = ggplot2::element_text(angle = 90, size = 12,
                                            vjust = 1, hjust = 1),
        axis.text.y = ggplot2::element_text(size = 12)) +
  coord_fixed() +
  xlab(NULL) + ylab(NULL) +
  ggtitle( "Correlación de variables",
           "Secretariado y ENVIPE 2015")

# Delitos reportados y percepción 
names(tab_est)
cor(tab_est %>% 
      dplyr::select(starts_with("im_perc_")) %>% 
      na.omit()) %>%
  ggcorrplot() 



# EDA Municipal ----
tab_mun %>% summary()


tab_gg <- tab_mun %>% 
  gather(variable, value, -c(ENT, MUN, NOM_ENT)) %>% 
  group_by(NOM_ENT, ENT, variable) %>% 
  summarise( n_nas = sum(is.na(value)),
             n_obs = n(), 
             prop = n_nas/n_obs) %>% 
  ungroup() %>% 
  mutate(variable = str_replace_all(variable, "im_", "") %>% 
           str_replace(., "_", ":")) %>% 
  separate(col = variable, c("fuente", 'variable_nom'), sep = ":") 
tab_gg

tab_gg %>% 
  ggplot(aes(x = variable_nom, y = factor(NOM_ENT), 
             fill = prop)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_grid(~fuente, space = "free_x", scales = "free")

tab_gg %>% 
  filter(fuente == "intrc")



# Mapa municipios (2457) ----
# spatial data frame 
dir("data/mex_muncs_shapes/")
shp_mun_rgdal <-  rgdal::readOGR("data/mex_muncs_shapes/merge.shp") 
shp_mun_rgdal@data %>% head()

any(duplicated(shp_mun_rgdal$CVEGEO))
shp_mun_rgdal <- shp_mun_rgdal[-1962, ]
any(duplicated(shp_mun_rgdal$CVEGEO))

class(shp_mun_rgdal)
shp_mun_rgdal$mun_code <- parse_number(shp_mun_rgdal$CVE_MUN)
shp_mun_rgdal$state_code <- parse_number(shp_mun_rgdal$CVE_ENT)

# poligonos
# load("cache/map_polygons_mun.RData")
# length(shp_mun_rgdal@polygons)
# shp_mun_rgdal %>% str(max.level = 2)
# 
# map_polygons_mun <- lapply(1:length(shp_mun_rgdal), function(i){
#   shp_mun_rgdal@polygons[[i]]@Polygons[[1]]@coords %>% 
#     as.tibble() %>% 
#     mutate(mun_code = shp_mun_rgdal@data$mun_code[i], 
#            state_code = shp_mun_rgdal@data$state_code[i], 
#            CVEGEO = shp_mun_rgdal@data$CVEGEO[i])
#   }) %>% 
#   bind_rows() %>% 
#   rename(long = V1, lat = V2) 
# map_polygons_mun %>% head
# cache("map_polygons_mun")

# ggplot() + 
#   geom_polygon(data = map_polygons_mun, 
#                aes(x=long, y = lat, group = CVEGEO, 
#                    fill = "gray20") )




# union
im_shp_mun_rgdal <- shp_mun_rgdal %>% 
  sp::merge(tab_mun %>% 
          rename(state_code = ENT, 
                 mun_code = MUN) %>% 
          data.frame(), 
        by = c('state_code', 'mun_code') )

im_shp_mun_rgdal@data %>% head
im_shp_mun_rgdal@data %>% dim # 2457 muns - 42 vars

names(im_shp_mun_rgdal)
summary(im_shp_mun_rgdal)

cache("im_shp_mun_rgdal")

# Algunas graficas
names(im_shp_mun_rgdal)[str_detect(names(im_shp_mun_rgdal), "im_")]

# gg_nom <- 
  tm_shape(im_shp_mun_rgdal) + 
  tm_fill(col = "im_vict_secuest") 
# save_tmap(gg_nom, filename = "graphs/gg_nom.png")

  
  
# coordenadas
tab_coords <- coordinates(shp_mun_rgdal)  %>% 
    as_data_frame() %>% 
    mutate(CVEGEO = shp_mun_rgdal$CVEGEO) %>% 
    rename(long = V1, lat = V2)
  