

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

# guardar
names(inds_list)
cache("inds_list")

sapply(inds_list, dim)


tab_indic_intercensal <- inds_list %>% 
  bind_rows() %>% 
  spread(indicadores, valor)
tab_indic_intercensal

summary(tab_indic_intercensal)

tab_indic_intercensal %>% 
  filter(COBERTURA == 3)
