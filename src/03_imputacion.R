
library(ProjectTemplate)
load.project()

library(sp)
library(spdep)
library(gstat)
library(mi)
library(bayesplot)
library(automap)
library(tmap)
library(R2BayesX)


# Datos ----
load("cache/tab_cods_estmun.RData")
tab_cods_estmun

load("cache/tab_mun.RData")
tab_mun %>% dim
tab_mun %>% summary()

load("cache/im_shp_mun_rgdal.RData")
im_shp_mun_rgdal@data %>% head


# Imputación multiple
df_imp <- im_shp_mun_rgdal@data
df_imp %>% head
summary(df_imp)

tab_coords <- coordinates(im_shp_mun_rgdal)  %>%
  as_data_frame() %>%
  mutate(state_code = im_shp_mun_rgdal$state_code,
         mun_code = im_shp_mun_rgdal$mun_code) %>%
  rename(long = V1, lat = V2)


tab_imp <- tab_mun %>% 
  dplyr::select(starts_with("im_perc"),
                starts_with("im_prob"),
                starts_with("im_intrc"),
                starts_with("im_secr"),
                state_code = ENT, 
                mun_code = MUN, 
                -im_intrc_pol_habs, 
                -im_intrc_jov_ecoact,
                -im_secr_robo_cs,
                -im_secr_robo_sv,
                # -im_prob_robesc,
                -im_intrc_jov_ocup_pea,
                -im_intrc_jov_estudian,
                -NOM_ENT) %>% 
  filter(!is.na(im_intrc_ind_ingreso),
         !is.na(im_intrc_linbienmin),
         !is.na(im_intrc_jov_nini)) %>% 
  left_join(tab_coords)
tab_imp %>% summary()

# 1. Imputacion multiple----

# Análisis de faltantes
df_data <- tab_imp %>% 
  data.frame()
class(df_data)
head(df_data)
dim(df_data) # 2422 municipios

df_data$im_perc_robos %>% summary()
df_data$im_perc_disparo*100

mdf_data <- missing_data.frame(df_data)
show(mdf_data)
summary(mdf_data)
image(mdf_data)
hist(mdf_data)

mdf_data <- change(mdf_data,
              y = c("im_perc_robos", "im_perc_pandill", "im_perc_secuest",
                    "im_perc_homicid", "im_perc_prostit", "im_perc_pirater",
                    "im_perc_disparo", "im_perc_extors", "im_perc_invaspr",
                    "im_perc_pocciud", "im_perc_ventdrog", 
                    "im_prob_robos", "im_prob_delinc", "im_prob_alumbr", 
                    "im_prob_pandill", "im_prob_robesc"),
              what = "transformation",
              to = rep("logshift", 16) ) # no negativas
show(mdf_data)

# imps_mi <- mi(mdf_data,
#               n.iter = 8,
#               n.chains = 3,
#               max.minutes = 10)
show(imps_mi)

round(mipply(imps_mi, mean, to.matrix = TRUE), 3)
Rhats(imps_mi)
mcmc_rhat(Rhats(imps_mi))
# imps_mi <- mi(imps_mi, n.iter = 8)
# cache("imps_mi")
load("cache/imps_mi.RData")

comp_imps_mi <- mi::complete(imps_mi, m = 4)
summary(comp_imps_mi)
str(comp_imps_mi)
comp_imps_mi[[1]] %>% head
comp_imps_mi[[3]][1, ]
# sub <- comp_imps_mi[[1]] 

df_imps <- lapply(comp_imps_mi, function(sub){
    sub %>% 
      select(state_code, mun_code, starts_with("im_")) %>% 
      gather(variable, value, -c("state_code", "mun_code")) %>%
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
  rename(missing_imp = value) 

df_imps %>% head()
df_data %>% head()

tab_imp_res <- df_imps %>% 
  left_join(df_data %>% 
              gather(variable, obs, -c(state_code, mun_code, long, lat)) %>% 
              dplyr::select(-long, -lat), 
            by = c("mun_code", "state_code", "variable"))
tab_imp_res

# cache("tab_imp_res")
load('cache/tab_imp_res.RData')

vars_selec <- unique(tab_imp_res$variable)[grep(pattern = "im_prob|im_perc", 
                                                unique(tab_imp_res$variable))]
vars_selec

tab_imp_res %>% 
  filter( variable %in% vars_selec) %>% 
  gather(tipo, valor, c(mean, median, obs)) %>% 
  ggplot(aes( x = valor, fill = tipo)) + 
  geom_density(alpha = .4) + 
  facet_wrap(~variable, scales = "free") + 
  scale_x_log10()

tab_imp_res %>% 
  filter( variable %in% vars_selec) %>% 
  gather(tipo, valor, c(mean, median, obs)) %>% 
  filter(tipo %in% c("mean", "obs")) %>% 
  ggplot(aes( x = valor, fill = tipo)) + 
  geom_histogram(bins = 25, position = "stack") + 
  facet_wrap(~variable, scales = "free") + 
  scale_x_log10()

tab_imp_res %>% 
  filter( variable %in% vars_selec) %>% 
  gather(tipo, valor, c(mean, median, obs)) %>% 
  mutate(valor_imp = ifelse(missing_imp == T, valor, NA)) %>% 
  ggplot(aes( x = tipo, y = valor)) + 
  geom_point(aes(y = valor_imp), 
             position = "jitter",
             color = "gray50",
             alpha = .5, size =.3) +
  geom_violin(aes(fill = tipo), alpha = .5) + 
  facet_wrap(~variable, scales = "free") +
  scale_y_log10()

tab_imp_res %>% 
  filter( variable %in% vars_selec) %>% 
  gather(tipo, valor, c(mean, median, obs)) %>% 
  filter(tipo %in% c("mean", "obs")) %>% 
  filter(variable == "im_perc_homicid") %>% 
  ggplot(aes( x = tipo, y = valor, 
              fill = tipo)) + 
  geom_violin(alpha = .3) +
  geom_boxplot( alpha = .5, width = .5) +
  facet_wrap(~state_code, scales = "free", 
             nrow = 4) + 
  scale_y_log10()


tab <- tab_imp_res %>% 
  filter( variable %in% vars_selec) %>% 
  gather(tipo, valor, c(mean, median, obs)) %>% 
  group_by(state_code, variable, tipo) %>% 
  summarise(prom = mean(valor, na.rm = T), 
            median = median(valor, na.rm = T),
            q25 = quantile(valor, .25, na.rm = T),
            q75 = quantile(valor, .75, na.rm = T)) %>% 
  ungroup %>% 
  gather(cuant, valor, prom:q75) %>% 
  filter(tipo != "median") %>% 
  unite(tipo_cuant, c(tipo, cuant)) %>% 
  spread(tipo_cuant, valor) %>% 
  left_join(tab_cods_estmun %>% 
              dplyr::select(state_code, NOM_ENT) %>% 
              unique())

tab %>% 
  ggplot(aes(x = obs_prom, y = mean_prom)) + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_errorbar(aes(ymin = mean_q25, 
                     ymax = mean_q75), 
                 alpha = .3) + 
  geom_errorbarh(aes(xmin = obs_q25, 
                     xmax = obs_q75), 
                 alpha = .3) + 
  geom_point(color = "blue") + 
  facet_wrap(~variable, scales = "free") + 
  geom_text(aes(label = NOM_ENT),
            size = 2, check_overlap = T)



# 2. Kriging ----
tab_krige <- tab_imp_res %>% 
  filter( variable %in% vars_selec) %>% 
  dplyr::select(mun_code, state_code, variable, mean) %>% 
  mutate(variable = paste0("imp_", variable)) %>% 
  spread(variable, mean) %>% 
  right_join(tab_cods_estmun %>% 
               dplyr::select(state_code, mun_code), 
             by = c("mun_code", "state_code")) %>% 
  left_join(tab_coords,
            by = c("mun_code", "state_code"))
tab_krige

# Mapas 
names(tab_krige)
im_shp_mun_rgdal <- im_shp_mun_rgdal %>% 
  merge(tab_krige)
im_shp_mun_rgdal@data %>% head()
names(im_shp_mun_rgdal)

# Shape polygon dataframe
imp_shp <- im_shp_mun_rgdal
names(imp_shp)

imp_shp@data %>% head()
imp_shp@data %>% summary()
class(imp_shp)

imp_mun_nb <- poly2nb(imp_shp) # neighbor list muns
imp_mun_centers <- coordinates(imp_shp) # mun centers
imp_mun_gra <- nb2gra(imp_mun_nb)



# Shape points dataframe
imp_shp_pts <- SpatialPointsDataFrame(imp_shp, data = imp_shp@data)


# variable interpolacion
names(imp_shp)
col_name <- 'imp_im_perc_robos'

# mapas
tm_shape(imp_shp) +
  tm_fill(col = str_replace(col_name, "imp_", ""),
          style = "quantile") 
tm_shape(imp_shp) +
  tm_fill(col = col_name, 
          style = "quantile", 
          title.col = "Robos") + 
  tm_bubbles(size = col_name, 
             col = col_name,
             style = "quantile", 
             legend.size.show = FALSE, 
             title.col = "Robos")


# faltantes
nas_vec <- is.na(imp_shp@data[, col_name])
sum(nas_vec)
sum(nas_vec)/length(nas_vec)


# Prueba con una variable
# variograma de faltante sy no faltantes 
col_name <- 'imp_im_perc_robos'
mod_vgm <- variogram(as.formula(paste("log(", col_name, ")", 
                                      "~ long + lat")), 
                     imp_shp_pts[!is.na(imp_shp_pts@data[, col_name]), ])
plot(mod_vgm)
head(mod_vgm)

fit_vgm <- fit.variogram(mod_vgm, 
                         model = vgm(psill = .9-.7, model = "Sph",
                                     range = 5.5, nugget = .7))
plot(mod_vgm, fit_vgm) 

# auto krige
mod_autok <- autoKrige(formula = as.formula(paste("log(", col_name, 
                                                  ") ~ long + lat")),
                       # log(imp_im_perc_robos) ~ long + lat, 
                       input_data = imp_shp_pts[!nas_vec, ],
                       new_data  = imp_shp_pts[nas_vec, ])
summary(mod_autok)
plot(mod_autok)

mod_autok %>% str
mod_autok$krige_output@data %>% head

imp_shp_pts@data %>% head
kg_col_name <- paste0("kg_", col_name)

imp_shp_pts@data[, kg_col_name] <- imp_shp_pts@data[, col_name]
imp_shp_pts@data[nas_vec, kg_col_name] <- exp(mod_autok$krige_output@data$var1.pred)
data_res <- imp_shp_pts@data %>% 
  dplyr::select_('state_code', 'mun_code', kg_col_name)



# Función para todas las variables 
# auto krige function by colname 
vars_imp <- imp_shp_pts@data %>% 
  dplyr::select(starts_with("imp_")) %>% 
  names()
vars_imp

kg_tbls_mod <- lapply(vars_imp, function(col_name){
  # col_name <- "imp_im_prob_robos"
  print(col_name)
  
  mod_autok <- autoKrige(formula = as.formula(paste("log(", col_name, 
                                                    ") ~ long + lat")),
                         input_data = imp_shp_pts[!nas_vec, ],
                         new_data  = imp_shp_pts[nas_vec, ])
  # summary(mod_autok)
  kg_col_name <- paste0("kg_", col_name)
  
  imp_shp_pts@data[, kg_col_name] <- 
    imp_shp_pts@data[, col_name]
  imp_shp_pts@data[nas_vec, kg_col_name] <- 
    exp(mod_autok$krige_output@data$var1.pred)
  data_res <- imp_shp_pts@data %>% 
    dplyr::select_('state_code', 'mun_code', kg_col_name) %>% 
    as_tibble()
  
  # Fit spatial model
  smooth_spatial <- bayesx(
    formula = as.formula( paste("log(", kg_col_name, 
            ") ~ sx( bs = 'spatial', map = imp_mun_gra)")),
    data = data.frame(data_res), 
    control = bayesx.control(seed = 17610407))
  
  data_res
  
})
kg_tbls_mod %>% length()
vars_imp %>% length()

imp_shp_kg <- imp_shp %>% 
  merge(kg_tbls_mod %>% 
          reduce(.f = left_join,
                 by = c("state_code", "mun_code")) ) 
imp_shp_kg %>% head
# cache("imp_shp_kg")
load("cache/imp_shp_kg.RData")


tm_shape(imp_shp_kg) +
  tm_fill(col = "kg_imp_im_prob_robescu", 
          style = "quantile")

tm <- tm_shape(imp_shp_kg) +
  tm_fill(col = c("im_prob_robescu", 
                  "imp_im_prob_robescu", 
                  "kg_imp_im_prob_robescu"),
          style = "quantile",
          title = "Percepción")
          # title = c("Observado", "Imputación", "Interpolación")) 
save_tmap(tm = tm, filename = "graphs/prb_tmap_kg.png", 
          width = 15, height = 6)



