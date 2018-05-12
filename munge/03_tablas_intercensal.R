

# Tablas de indicadores intercensal 2015

library(tidyverse)
library(bigrquery)

load("cache/inds_list.RData")
# inds_list <- list()
names(inds_list)

query_exec("SELECT * FROM [imunic-196018:intercensal.persona_append] LIMIT 3",
           project = "imunic-196018")




# Afiliación a servicios de salud----
tab <- query_exec("SELECT ENT, MUN, COBERTURA, DHSERSAL1, DHSERSAL2, sum(FACTOR), count(FACTOR) FROM [imunic-196018:intercensal.persona_append] GROUP BY ENT, MUN, COBERTURA, DHSERSAL1, DHSERSAL2",
           project = "imunic-196018") %>% 
  as_tibble() %>% 
  rename(n_facexp = f0_,
         n = f1_)
tab %>% head()


# Población con seguro social ( IMSS) ----
inds_list$`seguro_imss` <- tab %>% 
  mutate(cond = DHSERSAL1 %in% c(2) + DHSERSAL2 %in% c(2)) %>% 
  filter(cond > 0) %>% 
  group_by(ENT, MUN, COBERTURA) %>% 
  summarise(pob_seg_imss = sum(n_facexp)) %>% 
  ungroup() %>% 
  gather(indicadores, valor, -c(ENT, MUN, COBERTURA))
inds_list$`seguro_imss`

# Población con seguro popular  ----
inds_list$`seguro_popular` <- tab %>% 
  mutate(cond = DHSERSAL1 %in% c(1) + DHSERSAL2 %in% c(1)) %>% 
  filter(cond > 0) %>% 
  group_by(ENT, MUN, COBERTURA) %>% 
  summarise(pob_seg_popular = sum(n_facexp)) %>% 
  ungroup() %>% 
  gather(indicadores, valor, -c(ENT, MUN, COBERTURA))
inds_list$`seguro_popular`

# Población con IMSS, ISSTE, PEMEX, etc ----
inds_list$`seguro_social` <- tab %>% 
  mutate(cond = DHSERSAL1 %in% c(2, 3, 4, 5) + DHSERSAL2 %in% c(2, 3, 4, 5)) %>% 
  filter(cond > 0) %>% 
  group_by(ENT, MUN, COBERTURA) %>% 
  summarise(pob_seg_social = sum(n_facexp)) %>% 
  ungroup() %>% 
  gather(indicadores, valor, -c(ENT, MUN, COBERTURA))
inds_list$`seguro_social`

# Población con seguro privado ----
inds_list$`seguro_privado` <- tab %>% 
  mutate(cond = DHSERSAL1 %in% c(6) + DHSERSAL2 %in% c(6)) %>% 
  filter(cond > 0) %>% 
  group_by(ENT, MUN, COBERTURA) %>% 
  summarise(pob_seg_privado = sum(n_facexp)) %>% 
  ungroup() %>% 
  gather(indicadores, valor, -c(ENT, MUN, COBERTURA))
inds_list$`seguro_privado`



# Pertenencia indigena ----
tab <- query_exec("SELECT ENT, MUN, COBERTURA, PERTE_INDIGENA, sum(FACTOR), count(FACTOR) FROM [imunic-196018:intercensal.persona_append] GROUP BY ENT, MUN, COBERTURA, PERTE_INDIGENA",
                  project = "imunic-196018") %>% 
  as_tibble() %>% 
  rename(n_facexp = f0_,
         n = f1_)
tab

inds_list$`perte_indigena` <- tab %>% 
  mutate(cond = PERTE_INDIGENA %in% c(1, 2) ) %>% 
  filter(cond > 0) %>% 
  group_by(ENT, MUN, COBERTURA) %>% 
  summarise(pob_perte_indigena = sum(n_facexp)) %>% 
  ungroup() %>% 
  gather(indicadores, valor, -c(ENT, MUN, COBERTURA))
inds_list$`perte_indigena`



# Educacion ----
# tab <- query_exec("SELECT ENT, MUN, COBERTURA, EDAD, NIVACAD, ESCOLARI, ASISTEN, sum(FACTOR), count(FACTOR) FROM [imunic-196018:intercensal.persona_append] GROUP BY ENT, MUN, COBERTURA, EDAD, NIVACAD, ESCOLARI, ASISTEN",
#                   project = "imunic-196018", max_pages = Inf) %>%
#   as_tibble() %>%
#   rename(n_facexp = f0_,
#          n = f1_) %>%
#   mutate(edad_num = parse_number(EDAD)) %>%
#   mutate(edad_num =  ifelse(edad_num == 999, NA, edad_num),
#          edad_rango = cut(edad_num, breaks = seq(0,110, by = 5), include.lowest = T))
# tab
# saveRDS(tab, "cache/tab_edu.Rds")
tab <- read_rds("cache/tab_edu.Rds")


# más de 15 años sin edu basica ----
inds_list$`sin_edubasica` <- tab %>% 
  filter(edad_num > 15) %>% 
  filter(parse_number(NIVACAD) < 2 ) %>% 
  group_by(ENT, MUN, COBERTURA) %>% 
  summarise(pob_15mas_sinedubasica = sum(n_facexp)) %>% 
  ungroup() %>% 
  gather(indicadores, valor, -c(ENT, MUN, COBERTURA))
inds_list$`sin_edubasica`


# asisten a la escuela por rango edad ----
inds_list$`pob_asistenescuela_escedad` <- tab %>% 
  filter(edad_num >= 3, 
         edad_num <= 26, 
         ASISTEN == "5") %>% 
  mutate(edad_rango = cut(edad_num, 
                          breaks = c(3, 5, 12, 15, 18, 26),
                          labels = c("03_05", "06_12", "13_15", "16_18", "19_26"),
                          include.lowest = T)) %>% 
  group_by(ENT, MUN, COBERTURA, edad_rango) %>% 
  summarise(pob = sum(n_facexp)) %>% 
  ungroup() %>% 
  mutate(edad_rango = paste0("pob_asistenesc_", edad_rango)) %>% 
  spread(edad_rango, pob) %>% 
  gather(indicadores, valor, -c(ENT, MUN, COBERTURA))
inds_list$`pob_asistenescuela_escedad`


# población por rango edad escuela ----
inds_list$`pob_escedad` <- tab %>% 
  filter(edad_num >= 3, 
         edad_num <= 26) %>% 
  mutate(edad_rango = cut(edad_num, 
                          breaks = c(3, 5, 12, 15, 18, 26),
                          labels = c("03_05", "06_12", "13_15", "16_18", "19_26"),
                          include.lowest = T)) %>% 
  group_by(ENT, MUN, COBERTURA, edad_rango) %>% 
  summarise(pob = sum(n_facexp)) %>% 
  ungroup() %>% 
  mutate(edad_rango = paste0("pob_", edad_rango)) %>% 
  spread(edad_rango, pob) %>% 
  gather(indicadores, valor, -c(ENT, MUN, COBERTURA))
inds_list$`pob_escedad`


# poblacion edad ----
tab <- query_exec("SELECT ENT, MUN, COBERTURA, EDAD, sum(FACTOR), count(FACTOR) FROM [imunic-196018:intercensal.persona_append] GROUP BY ENT, MUN, COBERTURA, EDAD",
                             project = "imunic-196018", max_pages = Inf) %>%
             as_tibble() %>%
             rename(n_facexp = f0_,
                    n = f1_) %>%
             mutate(edad_num = parse_number(EDAD)) %>%
             mutate(edad_num =  ifelse(edad_num == 999, NA, edad_num),
                    edad_rango = cut(edad_num, breaks = seq(0,110, by = 5), include.lowest = T))

# población vulnerable mayores 75 años ----
inds_list$`pob_vul_mas75` <- tab %>% 
  filter(edad_num >= 75) %>% 
  group_by(ENT, MUN, COBERTURA) %>% 
  summarise( pob_mas75 = sum(n_facexp)) %>% 
  ungroup() %>% 
  gather(indicadores, valor, -c(ENT, MUN, COBERTURA))
inds_list$`pob_vul_mas75`


# población edad ----
inds_list$`pob_ranedad` <- tab %>% 
  filter(edad_num <= 75) %>% 
  group_by(ENT, MUN, COBERTURA, edad_rango) %>% 
  summarise( pob = sum(n_facexp)) %>% 
  ungroup() %>% 
  mutate(edad_rango = paste0("pob_edad_", edad_rango)) %>% 
  spread(edad_rango, pob) %>% 
  gather(indicadores, valor, -c(ENT, MUN, COBERTURA))
inds_list$`pob_ranedad`


# entidad y municipio union ----
tab <- query_exec("SELECT ENT, MUN, COBERTURA, sum(FACTOR), count(FACTOR) FROM [imunic-196018:intercensal.persona_append] GROUP BY ENT, MUN, COBERTURA",
           project = "imunic-196018") %>% 
  as_tibble() %>% 
  rename(pob_total = f0_,
         n = f1_) 

inds_list$`pob_municipio` <- tab %>% 
  dplyr::select(-n) %>% 
  gather(indicadores, valor, -c(ENT, MUN, COBERTURA))
inds_list$`pob_municipio`


# Acta de nacimiento ----
tab <- query_exec("SELECT ENT, MUN, COBERTURA, ACTA_NAC, sum(FACTOR), count(FACTOR) FROM [imunic-196018:intercensal.persona_append]  GROUP BY ENT, MUN, COBERTURA, ACTA_NAC",
                  project = "imunic-196018") %>% 
  as_tibble() %>% 
  rename(n_facexp = f0_,
         n = f1_) 

inds_list$`pob_actanacim` <- tab %>% 
  filter(ACTA_NAC == 1) %>% 
  dplyr::select(ENT:COBERTURA, pob_actanacim = n_facexp) %>% 
  gather(indicadores, valor, -c(ENT, MUN, COBERTURA))
inds_list$`pob_actanacim`


# Sexo ----
tab <- query_exec("SELECT ENT, MUN, COBERTURA, SEXO, sum(FACTOR), count(FACTOR) FROM [imunic-196018:intercensal.persona_append]  GROUP BY ENT, MUN, COBERTURA, SEXO",
                  project = "imunic-196018") %>% 
  as_tibble() %>% 
  rename(n_facexp = f0_,
         n = f1_) 

inds_list$`pob_sexo` <- tab %>% 
  mutate(indicadores = paste0("pob_", 
                             fct_recode(SEXO, 
                                        mujeres = "3",
                                        hombres = "1")) ) %>% 
  select(ENT, MUN, COBERTURA, indicadores, valor = n_facexp)
inds_list$`pob_sexo`


# Madres solteras ----
tab <- query_exec("SELECT ENT, MUN, COBERTURA, SITUA_CONYUGAL, HIJOS_SOBREVIV, sum(FACTOR), count(FACTOR) FROM [imunic-196018:intercensal.persona_append]  GROUP BY ENT, MUN, COBERTURA, SITUA_CONYUGAL, HIJOS_SOBREVIV", project = "imunic-196018", max_pages = Inf) %>% 
  as_tibble() %>% 
  rename(n_facexp = f0_,
         n = f1_) 

inds_list$`madres_solteras` <-
  tab %>% 
    mutate(madres_solteras = HIJOS_SOBREVIV > 0 & 
             SITUA_CONYUGAL %in% c(2, 3, 4, 6), 
           madres_nosolteras = HIJOS_SOBREVIV > 0 & 
             SITUA_CONYUGAL %in% c(1, 5), 
           mujeres_nosolteras = SITUA_CONYUGAL %in% c(1, 5)) %>% 
    dplyr::select(ENT:COBERTURA, n_facexp, madres_solteras:mujeres_nosolteras) %>% 
    gather(indicadores, value, -c(ENT, MUN, COBERTURA, n_facexp)) %>% 
    filter(value) %>% 
    group_by(ENT, MUN, COBERTURA, indicadores) %>% 
    summarise(valor = sum(n_facexp)) %>% 
    ungroup()
inds_list$`madres_solteras`



# Ingreso ----
library(spatstat)
tab <- query_exec("SELECT ENT, MUN, COBERTURA, CONACT, INGTRMEN, sum(FACTOR), count(FACTOR) FROM [imunic-196018:intercensal.persona_append] GROUP BY ENT, MUN, COBERTURA, CONACT, INGTRMEN", project = "imunic-196018", max_pages = Inf) %>% 
  as_tibble() %>% 
  rename(n_facexp = f0_,
         n = f1_) 

inds_list$`pob_otrostrabajo` <- tab %>% 
  mutate(CONACT = parse_number(CONACT), 
         INGTRMEN = ifelse(INGTRMEN == '999999', NA, parse_number(INGTRMEN)),
         amas_casa = CONACT %in% c(33),
         incapacitado = CONACT %in% c(34),
         econom_ocupado = CONACT %in% c(10:16),
         econom_desocup = CONACT %in% c(20),
         econom_notrabajo = CONACT %in% c(35),
         estudiante = CONACT %in% c(31)) %>% 
  gather(indicadores, value, amas_casa:estudiante) %>% 
  filter(value) %>% 
  group_by(ENT, MUN, COBERTURA, indicadores) %>% 
  summarise(valor = sum(n_facexp))
inds_list$`pob_otrostrabajo`



inds_list$`pob_trabajaingreso` <- tab %>% 
  mutate(CONACT = parse_number(CONACT), 
         INGTRMEN = ifelse(INGTRMEN == '999999', NA, parse_number(INGTRMEN)),
         econom_activos = CONACT %in% c(10:16)) %>% 
  filter(econom_activos) %>% 
  group_by(ENT, MUN, COBERTURA) %>% 
  summarise(ingreso_q50 = spatstat::weighted.quantile(x = INGTRMEN, 
                                             w = n_facexp, 
                                             probs = .5, na.rm = TRUE),
            ingreso_q25 = spatstat::weighted.quantile(x = INGTRMEN, 
                                                      w = n_facexp, 
                                                      probs = .25, na.rm = TRUE),
            ingreso_q75 = spatstat::weighted.quantile(x = INGTRMEN, 
                                                      w = n_facexp, 
                                                      probs = .75, na.rm = TRUE),
            ingreso_q10 = spatstat::weighted.quantile(x = INGTRMEN, 
                                                      w = n_facexp, 
                                                      probs = .10, na.rm = TRUE),
            ingreso_q90 = spatstat::weighted.quantile(x = INGTRMEN, 
                                                      w = n_facexp, 
                                                      probs = .90, na.rm = TRUE),
            pob_trabaja = sum(n_facexp)) %>% 
  ungroup %>% 
  gather(indicadores, valor, -c(ENT, MUN, COBERTURA))
inds_list$`pob_trabajaingreso`
  

pob_trabajaingreso_ent <- 
  tab %>% 
  mutate(CONACT = parse_number(CONACT), 
         INGTRMEN = ifelse(INGTRMEN == '999999', NA, parse_number(INGTRMEN)),
         econom_activos = CONACT %in% c(10:16)) %>% 
  filter(econom_activos) %>% 
  group_by(ENT) %>% 
  summarise(ingreso_q50 = spatstat::weighted.quantile(x = INGTRMEN, 
                                                      w = n_facexp, 
                                                      probs = .5, na.rm = TRUE),
            ingreso_q25 = spatstat::weighted.quantile(x = INGTRMEN, 
                                                      w = n_facexp, 
                                                      probs = .25, na.rm = TRUE),
            ingreso_q75 = spatstat::weighted.quantile(x = INGTRMEN, 
                                                      w = n_facexp, 
                                                      probs = .75, na.rm = TRUE),
            ingreso_q10 = spatstat::weighted.quantile(x = INGTRMEN, 
                                                      w = n_facexp, 
                                                      probs = .10, na.rm = TRUE),
            ingreso_q90 = spatstat::weighted.quantile(x = INGTRMEN, 
                                                      w = n_facexp, 
                                                      probs = .90, na.rm = TRUE),
            pob_trabaja = sum(n_facexp))
pob_trabajaingreso_ent

cache("pob_trabajaingreso_ent")



# Trabajo jovenes ----
qry <- paste0( "SELECT ENT, MUN, COBERTURA, CONACT, sum(FACTOR), count(FACTOR) ",
              "FROM [imunic-196018:intercensal.persona_append] WHERE EDAD in (",
              paste("'", 15:24, "'", sep = "", collapse = ","),
              ") GROUP BY ENT, MUN, COBERTURA, CONACT")
tab <- query_exec(qry, project = "imunic-196018", max_pages = Inf) %>% 
  as_tibble() %>% 
  rename(n_facexp = f0_,
         n = f1_) 

inds_list$`pob_jovenestrabajo` <- tab %>% 
  mutate(CONACT = parse_number(CONACT), 
         jov_acteco_hogar = CONACT %in% c(33),
         jov_acteco_incap = CONACT %in% c(34),
         jov_acteco_ocupado = CONACT %in% c(10:16),
         jov_acteco_desocup = CONACT %in% c(20),
         jov_acteco_notrabajo = CONACT %in% c(35),
         jov_acteco_estud = CONACT %in% c(31)) %>% 
  gather(indicadores, value, jov_acteco_hogar:jov_acteco_estud) %>% 
  filter(value) %>% 
  group_by(ENT, MUN, COBERTURA, indicadores) %>% 
  summarise(valor = sum(n_facexp))
inds_list$`pob_jovenestrabajo`


# Población de jovenes onu 15 a 24 ----
qry <- paste0( "SELECT ENT, MUN, COBERTURA, sum(FACTOR), count(FACTOR) ",
               "FROM [imunic-196018:intercensal.persona_append] WHERE EDAD in (",
               paste("'", 15:24, "'", sep = "", collapse = ","),
               ") GROUP BY ENT, MUN, COBERTURA")
tab <- query_exec(qry, project = "imunic-196018", max_pages = Inf) %>% 
  as_tibble() %>% 
  rename(n_facexp = f0_,
         n = f1_) 

inds_list$`pob_jovenes` <- tab %>% 
  mutate(indicadores = 'pob_15a24', 
         valor = n_facexp) %>% 
  dplyr::select(-n_facexp, -n)
inds_list$`pob_jovenes`




# población mayor de 12 años ----
tab <- query_exec("SELECT ENT, MUN, COBERTURA, EDAD, sum(FACTOR), count(FACTOR) FROM [imunic-196018:intercensal.persona_append] GROUP BY ENT, MUN, COBERTURA, EDAD", project = "imunic-196018", max_pages = Inf) %>% 
  as_tibble() %>% 
  rename(n_facexp = f0_,
         n = f1_) 

inds_list$`pob_edadesespc` <- tab %>% 
  mutate(EDAD = ifelse(EDAD == '999', NA, parse_number(EDAD))) %>% 
  group_by(ENT, MUN, COBERTURA) %>% 
  summarise(pob_12omas = sum(n_facexp*(EDAD >= 12), na.rm = T), 
            pob_15omas = sum(n_facexp*(EDAD >= 15), na.rm = T), 
            pob_15o35 = sum(n_facexp*(EDAD >= 15 & EDAD <= 35), na.rm = T),
            pob_menos12 = sum(n_facexp*(EDAD < 12), na.rm = T)) %>% 
  ungroup() %>% 
  gather(indicadores, valor, -c(ENT, MUN, COBERTURA))
inds_list$`pob_edadesespc`


# Linea de bienestar ----
linea_bienestar <- readxl::read_xlsx("docs/Lineas_Bienestar_marzo2018.xlsx", 
                                     sheet = "tabla", skip = 1, col_names = T) %>% 
  gather(var, value, -c(year, mes)) %>% 
  separate(col = var, into = c('linea', 'loc'), sep = "_") %>% 
  group_by(year, linea, loc) %>% 
  summarise(ing_prom = round(mean(value)) ) %>% 
  ungroup
linea_bienestar

# linea de bienestar
# qry <- paste0( "SELECT ENT, MUN, COBERTURA, TAMLOC, INGTRMEN, ",
qry <- paste0( "SELECT ENT, MUN, COBERTURA, sum(FACTOR) as valor, ",
               "(TAMLOC in ('1') and integer(INGTRMEN) < ",
               filter(linea_bienestar, loc == "rural", linea == "b")$ing_prom,
               ") or ",
               "(TAMLOC in ('2', '3', '4', '5') and integer(INGTRMEN) < ",
               filter(linea_bienestar, loc == "urbano", linea == "b")$ing_prom,
               ") as LIN_BIEN ",
               "FROM [imunic-196018:intercensal.persona_append] ",
               "WHERE NOT INGTRMEN in ('999998') ",
               "GROUP BY ENT, MUN, COBERTURA, LIN_BIEN")
               # "LIMIT 10")
tab <- query_exec(qry, project = "imunic-196018", max_pages = Inf) %>% 
  as_tibble()
tab

inds_list$`pob_lineabien` <- tab %>% 
  filter(LIN_BIEN == T) %>% 
  mutate(indicadores = "pob_bajo_lineabien") %>% 
  dplyr::select(-LIN_BIEN)
inds_list$`pob_lineabien`


# linea de bienestar minima
qry <- paste0( "SELECT ENT, MUN, COBERTURA, sum(FACTOR) as valor, ",
               "(TAMLOC in ('1') and integer(INGTRMEN) < ",
               filter(linea_bienestar, loc == "rural", linea == "bm")$ing_prom,
               ") or ",
               "(TAMLOC in ('2', '3', '4', '5') and integer(INGTRMEN) < ",
               filter(linea_bienestar, loc == "urbano", linea == "bm")$ing_prom,
               ") as LIN_BIEN_MIN ",
               "FROM [imunic-196018:intercensal.persona_append] ",
               "WHERE NOT INGTRMEN in ('999998') ",
               "GROUP BY ENT, MUN, COBERTURA, LIN_BIEN_MIN")
tab <- query_exec(qry, project = "imunic-196018", max_pages = Inf) %>% 
  as_tibble()
tab

inds_list$`pob_bajo_lineabienmin` <- tab %>% 
  filter(LIN_BIEN_MIN == T) %>% 
  mutate(indicadores = "pob_bajo_lineabienmin") %>% 
  dplyr::select(-LIN_BIEN_MIN)
inds_list$`pob_bajo_lineabienmin`




# Ocupación policias ----
# qry <- paste0( "SELECT ENT_PAIS_TRAB, MUN_TRAB, OCUPACION_C, sum(FACTOR) as valor, ",
qry <- paste0( "SELECT ACTIVIDADES_C, OCUPACION_C, sum(FACTOR) as valor, ",
               "FROM [imunic-196018:intercensal.persona_append] ",
               "WHERE OCUPACION_C in ('531') ",
               "and  (ACTIVIDADES_C in ('9312','9313', '9314', '9319')) ",
               "GROUP BY ACTIVIDADES_C,  OCUPACION_C")
tab <- query_exec(qry, project = "imunic-196018", max_pages = Inf) %>% 
  as_tibble()
tab
tab$valor %>% sum()




# guardar ----
names(inds_list)
length(inds_list) # 21

cache("inds_list")

sapply(inds_list, dim)




# Vivienda ----

query_exec("SELECT * FROM [imunic-196018:intercensal.vivienda_append] LIMIT 3",
           project = "imunic-196018")


tab <- query_exec("SELECT ENT, MUN, COBERTURA, DHSERSAL1, DHSERSAL2, sum(FACTOR), count(FACTOR) FROM [imunic-196018:intercensal.persona_append] GROUP BY ENT, MUN, COBERTURA, DHSERSAL1, DHSERSAL2",
                  project = "imunic-196018") %>% 
  as_tibble() %>% 
  rename(n_facexp = f0_,
         n = f1_)
tab %>% head()

