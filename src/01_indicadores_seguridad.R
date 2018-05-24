

library(ProjectTemplate)
load.project()

library(ggcorrplot)
library(imputeTS)

# Códigos de entidad y municipio
load("cache/tab_cods_estmun.RData")
tab_cods_estmun %>% head


#  0. Delitos secretariado ----
load("cache/tab_secre_inds.RData")
tab_secre_inds$indicadores %>% unique()

tab_secre <- tab_secre_inds %>% 
  rename(ENT = state_code, 
         MUN = mun_code, 
         valor = total) %>% 
  mutate(ENT = parse_number(ENT),
         MUN = parse_number(MUN))
summary(tab_secre)


# 0. Delitos envipe ----
doc_vars <- read_csv("docs/envipe_variables_tiempo.csv") %>% 
  gather(year, pregunta, `2013`:`2016`)
doc_vars %>% 
  filter(year == 2015) %>% 
  print(n = Inf)

tab_vars <- doc_vars %>% 
  filter(year == 2015) %>% 
  unite(variable, c(Tema, Descripción), sep = "_")


vars_victima <- c("víctima persona_robo calle", 
  "víctima persona_agresión física", 
  "víctima persona_secuestro", 
  "víctima persona_abuso sexual", 
  "víctima persona_violación sexual")

vars_percepcion <- c("percepción_pandillerismo", 
  "percepción_venta droga", 
  "percepción_robos asaltos",
  "percepción_invasión de predios",
  "percepción_prostitución",
  "percepción_secuestros",
  "percepción_extorsiones",
  "percepción_homicidios",
  "percepción_prostitución",
  "percepción_disparos",
  "percepción_policia vs ciudadanos", 
  "percepción_venta prods pirata")

vars_problemas <- c("problemas_pandillerismo violento",
  "problemas_falta alumbrado",
  "problemas_baches o fugas de agua",
  "problemas_pandillerismo violento",
  "problemas_robos",
  "problemas_delincuencia cerca escuelas")

variables_selec <- c(vars_percepcion, vars_problemas)
variables_selec

indis_l <- list()


# 0. Delitos municipal ----
load("cache/tab_pvic2.RData")
tab_pvic2$variable %>% unique()

tab_victima <- tab_pvic2 %>% 
  filter(year_file == 2016) %>% 
  inner_join(tab_vars %>% 
               filter(variable %in% vars_victima) %>% 
               dplyr::select(variable = pregunta, 
                             indicadores = variable), 
             by = "variable") %>% 
  filter(year_file == 2016, 
         value == 1) %>% 
  dplyr::select(ENT, MUN, valor =  n_ele, indicadores)




# 0. Percepción estatal envipe ----
load("cache/tab_pvic1.RData")

tab_percprob <- tab_pvic1 %>% 
  filter(year_file == 2016) %>% 
  inner_join(tab_vars %>% 
               filter(variable %in% c(vars_percepcion, 
                                      vars_problemas)) %>% 
               dplyr::select(variable = pregunta, 
                             indicadores = variable), 
             by = "variable") %>% 
  filter(value == 1) %>% 
  dplyr::select(ENT, MUN, valor =  n_ele, indicadores)





# 0. Indicadores intercensal ----
load("cache/inds_list.RData")
names(inds_list)
length(inds_list) # 22

inds_list %>% names
inds_list$pob_municipio

tab_interc <- inds_list %>% 
  bind_rows() %>% 
  dplyr::select(-COBERTURA) %>% 
  mutate(ENT = parse_number(ENT),
         MUN = parse_number(MUN))
summary(tab_interc)
tab_interc


# 0. Imputación estatal victimas ----

tab_imp_vic <- tab_pvic2 %>% 
  mutate(year = year_file -1) %>% 
  inner_join(doc_vars %>% 
               unite(variable, c(Tema, Descripción), sep = "_") %>% 
               filter(variable %in% vars_victima) %>% 
               rename(indicadores = variable, variable = pregunta) %>% 
               mutate(year = parse_number(year)),
             by = c("variable", "year")) %>% 
  filter(value == 1) %>% 
  group_by(ENT, year, indicadores) %>% 
  summarise(valor = sum(n_ele)) %>%
  ungroup %>% 
  complete(ENT, year, indicadores) %>% 
  group_by(ENT, indicadores) %>% 
  mutate(nas = sum(is.na(valor)),
         valor_imp = ifelse(nas > 2, 
                            mean(valor, na.rm = T), 
                            na.interpolation(valor)) ) %>% 
  ungroup()
cache("tab_imp_vic")





# 1. Tabla estatal ----
tab_est <- tab_percprob %>% 
  bind_rows(tab_secre) %>% 
  bind_rows(tab_interc) %>% 
  group_by(ENT, indicadores) %>% 
  summarise(valor = sum(valor, na.rm = T)) %>% 
  ungroup() %>% 
  bind_rows(tab_imp_vic %>% 
              filter(year == 2015) %>% 
              dplyr::select(ENT, indicadores, valor = valor_imp)) %>% 
  spread(indicadores, valor) %>% 
  mutate(im_ind_ingreso  = ingreso_q25/ingreso_q75, 
         im_jov_estudian = (jov_acteco_estud)/pob_15a24,
         im_jov_ecoact   = (jov_acteco_ocupado + jov_acteco_desocup)/pob_15a24,
         im_jov_nini     = (pob_15a24 - (jov_acteco_ocupado + jov_acteco_estud))/pob_15a24,
         im_jov_ocup_pea = jov_acteco_ocupado/(jov_acteco_ocupado + jov_acteco_desocup),
         im_jov_ocup_pob = jov_acteco_ocupado/pob_15a24,
         im_pol_habs     = trab_seguridad_pub/pob_total, 
         im_madres_sol   = madres_solteras/madres_nosolteras,
         im_linbien      = pob_bajo_lineabien/pob_total,
         im_linbienmin   = pob_bajo_lineabienmin/pob_total,
         # percepcion envipe
         im_perc_robos   = `percepción_robos asaltos`/pob_total,
         im_perc_robos   = `percepción_pandillerismo`/pob_total,
         im_perc_robos   = `percepción_secuestros`/pob_total,
         im_perc_robos   = `percepción_homicidios`/pob_total,
         im_perc_robos   = `percepción_prostitución`/pob_total,
         im_perc_robos   = `percepción_venta prods pirata`/pob_total,
         im_perc_robos   = `percepción_disparos`/pob_total,
         im_perc_robos   = `percepción_extorsiones`/pob_total,
         im_perc_robos   = `percepción_invasión de predios`/pob_total,
         im_perc_robos   = `percepción_policia vs ciudadanos`/pob_total,
         im_perc_robos   = `percepción_venta droga`/pob_total,
         
         
         
         im_prob_robos   = `problemas_robos`/pob_total,
         im_prob_delinc  = `problemas_delincuencia cerca escuelas`/pob_total,
         im_prob_robescu = `problemas_robos`/pob_total,
         im_prob_alumbr  = `problemas_falta alumbrado`/pob_total,
         im_prob_pandill = `problemas_pandillerismo violento`/pob_total,
         
         im_secr_robo_cv = `robo_comun_con_violencia`/pob_total,
         im_secr_robo_sv = `robo_comun_sin_violencia`/pob_total,
         im_secr_robo_cs = (`robo_comun_con_violencia` + `robo_comun_sin_violencia`)/
           pob_total,
         im_secr_secuest = `secuestro`/pob_total,
         im_secr_violac  = `violacion`/pob_total,
         
         im_vict_robo    = `víctima persona_robo calle`/pob_total,
         im_vict_agrfis  = `víctima persona_agresión física`/pob_total,
         im_vict_secuest = `víctima persona_secuestro`/pob_total,
         im_vict_violac  = `víctima persona_violación sexual`/pob_total,
         im_vict_abusex   = `víctima persona_abuso sexual`/pob_total,
         
         im_pob_total    = pob_total) %>% 
  left_join(tab_cods_estmun %>% 
              dplyr::select(NOM_ENT, ENT = state_code) %>% 
              unique, 
            by = "ENT")
tab_est %>% data.frame() %>% head()
summary(tab_est)
cache("tab_est")





# tab_mun <- tab_percprob %>% 
#   bind_rows(tab_victima) %>% 
#   bind_rows(tab_secre) %>% 
#   bind_rows(tab_interc) %>% 
#   spread(indicadores, valor) %>% 
#   dplyr::select(ENT, MUN, starts_with("im_"))
# tab_mun %>% data.frame() %>% head()
# summary(tab_mun)



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



corr_mat_est <- cor_pmat(tab_est %>% 
                    dplyr::select(starts_with("im")))
corr_mat_est[, 1:4] %>% head
corr_mat_est %>% dim
ggcorrplot(corr_mat_est, method = "circle") + 
  theme(axis.text.x = element_text(angle = 90))

cor_pmat(tab_est %>% 
           dplyr::select(starts_with("im_vict"))) %>%
  ggcorrplot(method = "circle") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Correlación entre victimas")


names(tab_est %>% 
        dplyr::select(starts_with("im")))

cor(tab_est$im_robo_cv_secr,tab_est$im_robo_sv_secr)



tab_est %>% 
  ggplot(aes(x = im_vict_robo, y = im_secr_robo_sv) ) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, 
              size = 1, color = 'red')  + 
  geom_text_repel(aes(label  = NOM_ENT))



tab_est %>% 
  ggplot(aes(x = `robo_comun_con_violencia`/pob_total, 
             y = (robo_comun_sin_violencia)/
               pob_total)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  geom_text_repel(aes(label  = NOM_ENT))

tab_est %>% 
  ggplot(aes(x = `víctima persona_robo calle`/pob_total, 
             y = (robo_comun_con_violencia)/
               pob_total)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, 
              size = 1, color = 'red')  + 
  geom_text_repel(aes(label  = NOM_ENT))

tab_est %>% 
  ggplot(aes(x = `víctima persona_robo calle`/pob_total, 
             y = `percepción_robos asaltos`/pob_total)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  geom_text_repel(aes(label  = NOM_ENT))

tab_est %>% 
  ggplot(aes(x = (robo_comun_con_violencia + robo_comun_sin_violencia)/
               pob_total, 
             y = `percepción_robos asaltos`/pob_total)) + 
  geom_point() + 
  geom_text_repel(aes(label  = NOM_ENT)) + 
  geom_smooth(method = "lm", se = F) 

