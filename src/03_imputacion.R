
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


# Shape data para imputacion
df_imp <- im_shp_mun_rgdal@data
df_imp %>% head
summary(df_imp)

# coordenadas en df del centro del municipio
tab_coords <- coordinates(im_shp_mun_rgdal)  %>%
  as_data_frame() %>%
  mutate(state_code = im_shp_mun_rgdal$state_code,
         mun_code = im_shp_mun_rgdal$mun_code) %>%
  rename(long = V1, lat = V2)

# tabla de variable a imputar
tab_imp <- tab_mun %>% 
  dplyr::select(starts_with("im_perc"),
                starts_with("im_prob"),
                starts_with("im_intrc"),
                starts_with("im_vict"),
                # starts_with("im_secre"),
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



# 1. Imputacion multiple ----
# (no es necesario correr si ya existe cache/tab_imp_res.RData)

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
                   y = "im_vict_secuest",
                   what = "family",
                   to = "gaussian")

mdf_data <- change(mdf_data,
              y = c("im_perc_robos", "im_perc_pandill", "im_perc_secuest",
                    "im_perc_homicid", "im_perc_prostit", "im_perc_pirater",
                    "im_perc_disparo", "im_perc_extors", "im_perc_invaspr",
                    "im_perc_pocciud", "im_perc_ventdrog", 
                    "im_prob_robos", "im_prob_delinc", "im_prob_alumbr", 
                    "im_prob_pandill", "im_prob_robesc", 
                    "im_vict_robo", "im_vict_agrfis", "im_vict_secuest"),
              what = "transformation",
              to = rep("logshift", 19) ) # no negativas
show(mdf_data)

# imps_mi <- mi(mdf_data,
#               n.iter = 8,
#               n.chains = 3,
#               max.minutes = 10)
show(imps_mi)

# correr hasta convergencia de cadenas
round(mipply(imps_mi, mean, to.matrix = TRUE), 3)
Rhats(imps_mi)
mcmc_rhat(Rhats(imps_mi))
# imps_mi <- mi(imps_mi, n.iter = 8)

# guarda el objeto de imputaciones
# cache("imps_mi")
# load("cache/imps_mi.RData")

# construye listas de simulaciones
comp_imps_mi <- mi::complete(imps_mi, m = 4)
summary(comp_imps_mi)
str(comp_imps_mi)
comp_imps_mi[[1]] %>% head
comp_imps_mi[[3]][1, ]
# sub <- comp_imps_mi[[1]] 

# resumen de cadenas de simulaciones 
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

# tabla con simulacion
tab_imp_res <- df_imps %>% 
  left_join(df_data %>% 
              gather(variable, obs, -c(state_code, mun_code, long, lat)) %>% 
              dplyr::select(-long, -lat), 
            by = c("mun_code", "state_code", "variable"))
tab_imp_res
# cache("tab_imp_res")


# 2. Análisis de simulaciones por municipio ----
# Tabla de simulaciones
load('cache/tab_imp_res.RData')
vars_selec <- c("im_vict_agrfis","im_vict_secuest", "im_vict_robo",
                unique(tab_imp_res$variable)[grep(pattern = "im_prob|im_perc", 
                                                unique(tab_imp_res$variable))])
vars_selec


# Evaluacion grafica de simualciones
# (no necesario)
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



# 3. Kriging de varlores faltantes ----
load('cache/tab_imp_res.RData')
vars_selec <- unique(tab_imp_res$variable)[grep(pattern = "im_prob|im_perc", 
                                                unique(tab_imp_res$variable))]
vars_selec


# tabla a imputar
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

# merge de shape y tabla para imputar
names(tab_krige)
im_shp_mun_rgdal <- im_shp_mun_rgdal %>%
  merge(tab_krige)
im_shp_mun_rgdal@data %>% head()
names(im_shp_mun_rgdal)

# shape a imputar
imp_shp <- im_shp_mun_rgdal
names(imp_shp)
class(imp_shp)

imp_shp@data %>% head()
imp_shp@data %>% summary()


# caracteristicas
imp_mun_nb <- poly2nb(imp_shp) # neighbor list muns
imp_mun_gra <- nb2gra(imp_mun_nb)
imp_mun_coo <- coordinates(imp_shp) # mun centers

# Shape poligonos a puntos para kriging
imp_shp_pts <- SpatialPointsDataFrame(imp_shp, data = imp_shp@data)




# 3a. Prueba con una variable ----
# (no es necesario correr)
col_name <- 'imp_im_perc_robos'

# faltantes de la columna
nas_vec <- is.na(imp_shp@data[, col_name])
sum(nas_vec)
sum(nas_vec)/length(nas_vec)

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
# plot(mod_autok)

mod_autok %>% str
mod_autok$krige_output %>% str
mod_autok$krige_output@data %>% head

imp_shp_pts@data %>% head
kg_col_name <- str_replace_all(col_name, "imp_", "kg_")

imp_shp_pts@data[, kg_col_name] <- imp_shp_pts@data[, col_name]
imp_shp_pts@data[nas_vec, kg_col_name] <- exp(mod_autok$krige_output@data$var1.pred)
head(imp_shp_pts@data)

data_res <- imp_shp_pts@data %>%
  dplyr::select_('state_code', 'mun_code', kg_col_name)
data_res %>% head

imp_shp_prb <- imp_shp %>% 
  merge(data_res,
        by = c("state_code", "mun_code")) 
imp_shp_prb %>% head






# 3b. Kriging en todas las variables  ----
imp_shp_pts <- SpatialPointsDataFrame(imp_shp, data = imp_shp@data)

# variables a imputar
vars_imp <- imp_shp_pts@data %>% 
  dplyr::select(starts_with("imp_")) %>% 
  names()
vars_imp

# lapply por cada variable
kg_tbls_mod <- lapply(vars_imp, function(col_name){
  # col_name <- "imp_im_prob_robos"
  print(col_name)
  
  # valores faltantes de columna
  nas_vec <- is.na(imp_shp@data[, col_name])
  sum(nas_vec)
  sum(nas_vec)/length(nas_vec)

  # auto krige por colname 
  mod_autok <- autoKrige(formula = as.formula(paste("log(", col_name, 
                                                    ") ~ long + lat")),
                         input_data = imp_shp_pts[!nas_vec, ],
                         new_data  = imp_shp_pts[nas_vec, ])
  # summary(mod_autok)
  
  # nombre de variable
  kg_col_name <- str_replace_all(col_name, "imp_", "kg_")
  
  # reemplazar nas
  imp_shp_pts@data[, kg_col_name] <- 
    imp_shp_pts@data[, col_name]
  imp_shp_pts@data[nas_vec, kg_col_name] <- 
    exp(mod_autok$krige_output@data$var1.pred)
  
  # dataframe final
  data_res <- imp_shp_pts@data %>% 
    dplyr::select_('state_code', 'mun_code', kg_col_name) %>% 
    as_tibble()
  
  return(data_res)
})
kg_tbls_mod %>% length()
vars_imp %>% length()

# merge de datos de imputados
imp_shp_kg <- imp_shp %>% 
  merge(kg_tbls_mod %>% 
          reduce(.f = left_join,
                 by = c("state_code", "mun_code")) ) 
imp_shp_kg %>% head

# cache("imp_shp_kg")
load("cache/imp_shp_kg.RData")





# 4. Smoothing ----

# 4a. Varias pruebas una variable ----
#(no es necesario correr esto)
kg_col_name <- 'kg_im_perc_robos'

# Krige (points)
imp_shp_pts@data %>% head()
proj4string(imp_shp_pts)
coordinates(imp_shp_pts)

imp_shp_pts_fit <- imp_shp_pts
imp_shp_pts_fit$kg_im_perc_robos <- NULL
imp_shp_pts_fit@data %>% head()


mod_autok <- autoKrige(formula = as.formula(paste("log(", kg_col_name, 
                                                  ") ~ long + lat")),
                       input_data = imp_shp_pts,
                       new_data = imp_shp_pts_fit)
mod_autok$krige_output %>% head()
mod_autok$krige_output %>% dim()

df_akg <- imp_shp_pts@data %>% 
  dplyr::select(state_code, mun_code) 
df_akg[, "fit_kg"] <- mod_autok$krige_output$var1.pred %>% exp

head(df_akg)
class(imp_shp_prb)

imp_shp_prb <- imp_shp_prb %>% 
  merge(df_akg)
imp_shp_prb@data %>% head
qplot(imp_shp_prb@data$kg_im_perc_robos, imp_shp_prb@data$fit_kg)

tm_shape(imp_shp_prb) +
  tm_fill(col = 'fit_kg',
          style = "quantile")

var_vgm <- variogram(as.formula(paste("log(", kg_col_name, 
                                      ") ~ long + lat")), 
                     locations = imp_shp_pts)
var_fit <- fit.variogram(var_vgm, model=  vgm("Sph") ) # fit model
plot(var_vgm, var_fit)
var_krg <- krige(formula =formu,
                 data = data.frame(imp_shp_pts),
                 locations = imp_shp_pts,
                 model = var_fit)



# knn: Nearest Neighbours
ids_mun <- row.names(as(imp_shp_prb, "data.frame"))
imp_mun_coo <- coordinates(imp_shp_prb) 

knn50 <- knn2nb(knearneigh(imp_mun_coo, k = 4), row.names = ids_mun)
knn50 <- include.self(knn50)
# Creating the localG statistic for each of counties, 
localGvalues <- localG(x = as.numeric( (imp_shp_prb$kg_im_perc_robos)), 
                       listw = nb2listw(knn50, style = "S"),
                       zero.policy = TRUE)
str(localGvalues)

qplot(as.numeric(localGvalues), log(imp_shp_prb$kg_im_perc_robos)) + 
  geom_abline(slope = 1, intercept = 0) 

class(imp_shp_prb)
imp_shp_prb$smo_im_perc_robos <- exp(localGvalues)
imp_shp_prb@data %>% head()
imp_shp_prb$smo_im_perc_robos %>% summary()

tm_shape(imp_shp_prb) +
  tm_fill(col = 'smo_im_perc_robos',
          style = "quantile")

imp_shp_prb@data %>% 
  filter(smo_im_perc_robos > 10) %>% 
  dplyr::select(state_code, mun_code, NOMGEO, 
                kg_im_perc_robos, smo_im_perc_robos)




# bayesx: markov random field
imp_mun_nb_prb <- poly2nb(imp_shp_prb) # neighbor list muns
imp_mun_gra_prb <- nb2gra(imp_mun_nb_prb) # mun centers

sm_col_name <- "smo_im_perc_robos"
df_shp <- data.frame(imp_shp_prb) %>% 
  rownames_to_column('ids') %>% 
  mutate(ids = parse_number(ids)) %>% 
  dplyr::select_("state_code", "mun_code", "lat", "long", 
                 "ids", kg_col_name)

df_shp$ids %>% summary()
df_shp$ids %>% n_distinct()

formu <- as.formula(paste("log(kg_im_perc_robos)~ 1 +", 
                          "+ sx(long) + sx(lat)",
                          "+ sx(state_code, bs = 'spatial', map = imp_mun_gra_prb)",
                          "+ sx(mun_code, bs = 'spatial', map = imp_mun_gra_prb)"))
formu
mod_spatial <- bayesx(formula = formu,
                      family = "gaussian", 
                      data = df_shp, 
                      control = bayesx.control(seed = 17610407)) 
summary(mod_spatial)

mod_spatial$fitted.values %>% head()
qplot(mod_spatial$fitted.values[, 1], df_shp$kg_im_perc_robos)

df_shp[, 'fit_mod'] <- exp(mod_spatial$fitted.values[, 1])
df_shp %>% head()
df_shp %>% dim()

imp_shp_tmap <- imp_shp_prb %>% 
  merge(df_shp)

imp_shp_tmap %>% class()
imp_shp_tmap@data %>% head()

tm_shape(imp_shp_tmap) +
  tm_fill(col = 'fit_mod',
          style = "quantile")





# linear regression
imp_mun_nb_prb <- poly2nb(imp_shp_prb) # neighbor list muns
imp_mun_gra_prb <- nb2gra(imp_mun_nb_prb) # mun centers

sm_col_name <- "smo_im_perc_robos"
df_shp <- data.frame(imp_shp_prb) %>% 
  dplyr::select_("state_code", "mun_code", "lat", "long", 
                 kg_col_name)
head(df_shp)

formu <- as.formula(paste("log(", kg_col_name,
                          ")~ long + lat + (1|state_code)"))
mod_lmer <- lm(formula = log(kg_im_perc_robos) ~ long + lat ,
                 data = df_shp) 
summary(mod_lmer)

qplot(exp(fitted(mod_lmer)), df_shp$kg_im_perc_robos)

df_shp[, 'fit'] <- exp(fitted(mod_lmer))
df_shp %>% head()

imp_shp_prb <- imp_shp_prb %>% 
  merge(df_shp)

imp_shp_prb %>% class()
imp_shp_prb@data %>% head()

tm_shape(imp_shp_prb) +
  tm_fill(col = 'fit',
          style = "quantile")











# 4b smoothing completa ----

load("cache/imp_shp_kg.RData")
# polygons
class(imp_shp_kg)

ids_mun <- row.names(as(imp_shp_kg, "data.frame"))
imp_mun_coo <- coordinates(imp_shp_kg) 

knn_mod <- knn2nb(knearneigh(imp_mun_coo, k = 10), row.names = ids_mun)
knn_mod <- include.self(knn_mod)


vars_selec_kg <- names(imp_shp_kg@data)[str_detect(names(imp_shp_kg@data), 
                                                   "kg_")]
vars_selec_kg

smo_tbls_mod <- lapply(vars_selec_kg, function(col_name){
  # col_name <- 'kg_im_perc_robos'
  var_vec <- as.numeric(imp_shp_kg[, col_name]@data[, 1])
  
  localgmod <- localG(x = var_vec, 
                         listw = nb2listw(knn_mod, style = "S"),
                         zero.policy = TRUE)
  
  # dataframe final
  data_res <- imp_shp_kg@data %>% 
    dplyr::select_('state_code', 'mun_code') %>% 
    as_tibble()
  data_res[, str_replace(col_name, "kg_", "smo_")] <- exp(localgmod)
  
  return(data_res)
})
smo_tbls_mod %>% length()
vars_selec_kg %>% length()

# merge de datos de imputados
imp_shp_smo <- imp_shp_kg %>% 
  merge(smo_tbls_mod %>% 
          reduce(.f = left_join,
                 by = c("state_code", "mun_code")) ) 
imp_shp_smo %>% head


# cache("imp_shp_smo")
load("cache/imp_shp_smo.RData")


# 5. Evaluación gráfica ----

names(imp_shp_kg)
col_name <- 'kg_im_perc_robos'
# col_name <- 'kg_im_vict_robo'

tm_shape(imp_shp_kg) +
  tm_fill(col = col_name, 
          style = "quantile")

tm_shape(imp_shp_smo) +
  tm_fill(col = gsub("kg_", "smo_", col_name),
          style = "quantile")


# Scatter plot
df_smo <- imp_shp_smo@data %>% 
  as_tibble()



c(gsub("kg_", "", vars_selec_kg), 
  gsub("kg_", "smo_", vars_selec_kg))

tab <- df_smo %>% 
  dplyr::select(state_code, mun_code, 
                starts_with("im_"),
                starts_with("smo_")) %>% 
  gather(var, val, -c(state_code, mun_code)) %>% 
  filter(var %in% c(gsub("kg_", "", vars_selec_kg), 
                    gsub("kg_", "smo_", vars_selec_kg))) %>% 
  mutate(tipo = str_detect(var, "smo_"),
         var = gsub("im_|smo_", "", var)) %>% 
  spread(tipo, val)

tab %>% 
  ggplot(aes(x = `FALSE`, y = `TRUE`)) + 
  geom_point()+ 
  facet_wrap(~var, scales = "free") + 
  geom_text(aes(label = state_code), check_overlap = T)

tab_cods_estmun %>% 
  dplyr::select(state_code, NOM_ENT) %>% 
  unique %>% 
  arrange(state_code) %>% 
  print(n = Inf)



# Algunas graficas

tm <- tm_shape(imp_shp_kg) +
  tm_fill(col = c("im_prob_robescu", 
                  "imp_im_prob_robescu", 
                  "kg_im_prob_robescu",
                  "smo_im_prob_robescu"),
          style = "quantile",
          title = c("Observado", "Imputación", "Interpolación"))
save_tmap(tm = tm, filename = "graphs/prb_tmap_kg.png", 
          width = 15, height = 6)

