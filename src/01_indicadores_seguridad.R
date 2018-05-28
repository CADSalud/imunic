

library(ProjectTemplate)
load.project()


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
  # "víctima persona_abuso sexual", 
  # "víctima persona_violación sexual",
  "víctima persona_secuestro")

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

vars_problemas <- c(
  "problemas_falta alumbrado",
  # "problemas_baches o fugas de agua",
  "problemas_pandillerismo violento",
  "problemas_robos",
  "problemas_delincuencia cerca escuelas")


# 0. Victimización envipe ----
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


# 0a. Imputación estatal victimas ----
tab_impest_vic <- tab_pvic2 %>% 
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
  tidyr::complete(ENT, year, indicadores) %>% 
  group_by(ENT, indicadores) %>% 
  mutate(nas = sum(is.na(valor)),
         valor_imp = ifelse(nas > 2, 
                            mean(valor, na.rm = T), 
                            na.interpolation(valor)) ) %>% 
  ungroup()

summary(tab_impest_vic)



# 0b. Imputación municpal victimas ----
tab_nasmun_vict <- tab_pvic2 %>% 
  mutate(year = year_file -1) %>% 
  inner_join(doc_vars %>% 
               unite(variable, c(Tema, Descripción), sep = "_") %>% 
               filter(variable %in% c(vars_victima) ) %>% 
               rename(indicadores = variable, 
                      variable = pregunta) %>% 
               mutate(year = parse_number(year)),
             by = c("variable", "year")) %>% 
  filter(value == 1) %>% 
  group_by(MUN, ENT, year, indicadores) %>% 
  summarise(valor = sum(n_ele)) %>%
  ungroup %>% 
  tidyr::complete(MUN, ENT, year, indicadores) %>% 
  group_by(MUN, ENT, indicadores) %>% 
  mutate(nas = sum(is.na(valor))) %>% 
  ungroup %>% 
  arrange(ENT, MUN, indicadores, year)
tab_nasmun_vict
tab_nasmun_vict %>% filter(nas == 2)

# Interpolación o promedio si solo hay un punto
tab_impmun_vict <- tab_nasmun_vict %>% 
  filter(nas < 4) %>% 
  group_by(MUN, ENT, indicadores) %>% 
  mutate(valor_imp = ifelse(nas > 2, 
                            mean(valor, na.rm = T), 
                            na.interpolation(valor)) ) %>% 
  ungroup()
summary(tab_impmun_vict)

# graficas
gg_l_victmun <- lapply(vars_victima, function(var_nom){
  tab_impmun_vict %>% 
    filter(indicadores == var_nom) %>% 
    ggplot(aes(x = year, y = valor_imp, 
               group = MUN)) + 
    geom_line(alpha = .5) + 
    geom_point(aes(color = is.na(valor))) + 
    scale_color_manual(values = c("gray50", "red")) +
    theme(axis.text.x = element_text(angle = 90)) + 
    facet_wrap(~ENT, nco = 8, scales = "free") +
    ggtitle(gsub(pattern = ".*[_]", "", var_nom))
})
gg_l_victmun %>% length()
gg_l_victmun[[2]]




# 0. Percepción y problemas envipe ----
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



# 0a. Imputación municpal problemas y percepcion ----
tab_nasmun_pp <- tab_pvic1 %>% 
  mutate(year = year_file -1) %>% 
  inner_join(doc_vars %>% 
               unite(variable, c(Tema, Descripción), sep = "_") %>% 
               filter(variable %in% c(vars_problemas, 
                                      vars_percepcion) ) %>% 
               rename(indicadores = variable, 
                      variable = pregunta) %>% 
               mutate(year = parse_number(year)),
             by = c("variable", "year")) %>% 
  filter(value == 1) %>% 
  group_by(MUN, ENT, year, indicadores) %>% 
  summarise(valor = sum(n_ele)) %>%
  ungroup %>% 
  tidyr::complete(MUN, ENT, year, indicadores) %>% 
  group_by(MUN, ENT, indicadores) %>% 
  mutate(nas = sum(is.na(valor))) %>% 
  ungroup %>% 
  arrange(ENT, MUN, indicadores, year)
tab_nasmun_pp
tab_nasmun_pp %>% filter(nas == 2)

# Imputación
tab_impmun_pp <- tab_nasmun_pp %>% 
  filter(nas < 4) %>% 
  group_by(MUN, ENT, indicadores) %>% 
  mutate(valor_imp = ifelse(nas > 2, 
                            mean(valor, na.rm = T), 
                            na.interpolation(valor)) ) %>% 
  ungroup()
summary(tab_impmun_pp)
# cache("tab_impmun_pp")


# Graficas
gg_l_probsmun <- lapply(vars_problemas, function(var_nom){
  tab_impmun_pp %>% 
    filter(indicadores == var_nom) %>% 
    ggplot(aes(x = year, y = valor_imp, 
               group = MUN)) + 
    geom_line(alpha = .5) + 
    geom_point(aes(color = is.na(valor))) + 
    scale_color_manual(values = c("gray50", "red")) +
    theme(axis.text.x = element_text(angle = 90)) + 
    facet_wrap(~ENT, nco = 8, scales = "free") +
    ggtitle(gsub(pattern = ".*[_]", "", var_nom))
})
gg_l_probsmun %>% length()
gg_l_probsmun[[4]]



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





# 1. Tabla estatal ----
tab_est <- tab_percprob %>% 
  bind_rows(tab_secre) %>% 
  bind_rows(tab_interc) %>% 
  group_by(ENT, indicadores) %>% 
  summarise(valor = sum(valor, na.rm = T)) %>% 
  ungroup() %>% 
  bind_rows(tab_impest_vic %>% 
              filter(year == 2015) %>% 
              dplyr::select(ENT, indicadores, valor = valor_imp)) %>% 
  spread(indicadores, valor) %>% 
  mutate(im_intrc_ind_ingreso  = ingreso_q25/ingreso_q75, 
         im_intrc_jov_estudian = (jov_acteco_estud)/pob_15a24,
         im_intrc_jov_ecoact   = (jov_acteco_ocupado + jov_acteco_desocup)/pob_15a24,
         im_intrc_jov_nini     = (pob_15a24 - (jov_acteco_ocupado + jov_acteco_estud))/pob_15a24,
         im_intrc_jov_ocup_pea = jov_acteco_ocupado/(jov_acteco_ocupado + jov_acteco_desocup),
         im_intrc_jov_ocup_pob = jov_acteco_ocupado/pob_15a24,
         im_intrc_pol_habs     = trab_seguridad_pub/pob_total, 
         im_intrc_madres_sol   = madres_solteras/madres_nosolteras,
         im_intrc_linbien      = pob_bajo_lineabien/pob_total,
         im_intrc_linbienmin   = pob_bajo_lineabienmin/pob_total,
         # percepcion envipe
         im_perc_robos   = `percepción_robos asaltos`/pob_total,
         im_perc_pandill = `percepción_pandillerismo`/pob_total,
         im_perc_secuest = `percepción_secuestros`/pob_total,
         im_perc_homicid = `percepción_homicidios`/pob_total,
         im_perc_prostit = `percepción_prostitución`/pob_total,
         im_perc_pirater = `percepción_venta prods pirata`/pob_total,
         im_perc_disparo = `percepción_disparos`/pob_total,
         im_perc_extors  = `percepción_extorsiones`/pob_total,
         im_perc_invaspr = `percepción_invasión de predios`/pob_total,
         im_perc_pocciud = `percepción_policia vs ciudadanos`/pob_total,
         im_perc_ventdrog = `percepción_venta droga`/pob_total,
         # problemas locales
         im_prob_robos   = `problemas_robos`/pob_total,
         im_prob_delinc  = `problemas_delincuencia cerca escuelas`/pob_total,
         im_prob_robescu = `problemas_robos`/pob_total,
         im_prob_alumbr  = `problemas_falta alumbrado`/pob_total,
         im_prob_pandill = `problemas_pandillerismo violento`/pob_total,
         # denuncias del secretariado
         im_secr_robo_cv = 1e5*`robo_comun_con_violencia`/pob_total,
         im_secr_robo_sv = 1e5*`robo_comun_sin_violencia`/pob_total,
         im_secr_robo_cs = 1e5*(`robo_comun_con_violencia` + `robo_comun_sin_violencia`)/pob_total,
         im_secr_secuest = 1e5*`secuestro`/pob_total,
         im_secr_violac  = 1e5*`violacion`/pob_total,
         # envipe victimas
         im_vict_robo    = `víctima persona_robo calle`/pob_total,
         im_vict_agrfis  = `víctima persona_agresión física`/pob_total,
         im_vict_secuest = `víctima persona_secuestro`/pob_total,
         # poblacion total
         im_pob_total    = pob_total) %>% 
  left_join(tab_cods_estmun %>% 
              dplyr::select(NOM_ENT, ENT = state_code) %>% 
              unique, 
            by = "ENT")
tab_est %>% data.frame() %>% head()
summary(tab_est)
cache("tab_est")


# 1. Tabla municipal ----



  
  
tab_mun <- tab_impmun_pp %>%
  filter(year == 2015) %>% 
  dplyr::select(MUN, ENT, indicadores, valor = valor_imp) %>%
  bind_rows(tab_impmun_vict %>% 
              filter(year == 2015) %>% 
              dplyr::select(MUN, ENT, indicadores, valor = valor_imp)) %>%
  bind_rows(tab_secre) %>%
  bind_rows(tab_interc) %>%
  spread(indicadores, valor) %>%
  mutate(im_intrc_ind_ingreso  = ingreso_q25/ingreso_q75, 
         im_intrc_jov_estudian = (jov_acteco_estud)/pob_15a24,
         im_intrc_jov_ecoact   = (jov_acteco_ocupado + jov_acteco_desocup)/pob_15a24,
         im_intrc_jov_nini     = (pob_15a24 - (jov_acteco_ocupado + jov_acteco_estud))/pob_15a24,
         im_intrc_jov_ocup_pea = jov_acteco_ocupado/(jov_acteco_ocupado + jov_acteco_desocup),
         im_intrc_jov_ocup_pob = jov_acteco_ocupado/pob_15a24,
         im_intrc_pol_habs     = trab_seguridad_pub/pob_total, 
         im_intrc_madres_sol   = madres_solteras/madres_nosolteras,
         im_intrc_linbien      = pob_bajo_lineabien/pob_total,
         im_intrc_linbienmin   = pob_bajo_lineabienmin/pob_total,
         # percepcion envipe
         im_perc_robos   = `percepción_robos asaltos`/pob_total,
         im_perc_pandill = `percepción_pandillerismo`/pob_total,
         im_perc_secuest = `percepción_secuestros`/pob_total,
         im_perc_homicid = `percepción_homicidios`/pob_total,
         im_perc_prostit = `percepción_prostitución`/pob_total,
         im_perc_pirater = `percepción_venta prods pirata`/pob_total,
         im_perc_disparo = `percepción_disparos`/pob_total,
         im_perc_extors  = `percepción_extorsiones`/pob_total,
         im_perc_invaspr = `percepción_invasión de predios`/pob_total,
         im_perc_pocciud = `percepción_policia vs ciudadanos`/pob_total,
         im_perc_ventdrog = `percepción_venta droga`/pob_total,
         # problemas locales
         im_prob_robos   = `problemas_robos`/pob_total,
         im_prob_delinc  = `problemas_delincuencia cerca escuelas`/pob_total,
         im_prob_robescu = `problemas_robos`/pob_total,
         im_prob_alumbr  = `problemas_falta alumbrado`/pob_total,
         im_prob_pandill = `problemas_pandillerismo violento`/pob_total,
         # denuncias del secretariado
         im_secr_robo_cv = 1e5*`robo_comun_con_violencia`/pob_total,
         im_secr_robo_sv = 1e5*`robo_comun_sin_violencia`/pob_total,
         im_secr_robo_cs = 1e5*(`robo_comun_con_violencia` + `robo_comun_sin_violencia`)/pob_total,
         im_secr_secuest = 1e5*`secuestro`/pob_total,
         im_secr_violac  = 1e5*`violacion`/pob_total,
         # envipe victimas
         im_vict_robo    = `víctima persona_robo calle`/pob_total,
         im_vict_agrfis  = `víctima persona_agresión física`/pob_total,
         im_vict_secuest = `víctima persona_secuestro`/pob_total,
         # poblacion total
         im_pob_total    = pob_total) %>%
  dplyr::select(ENT, MUN, starts_with("im_")) %>%
  left_join(tab_cods_estmun %>% 
              dplyr::select(NOM_ENT, 
                            ENT = state_code,
                            MUN = mun_code), 
            by = c("ENT", "MUN")) 

tab_mun %>% data.frame() %>% head()
summary(tab_mun)
cache("tab_mun")


