
library(ProjectTemplate)
load.project()


# Tablas resúmen ----
load("cache/tab_pvic1.RData")
load("cache/tab_pvic2.RData")


# datos victimización ----
tab_1 <- tab_pvic2 %>% 
  group_by(ENT, MUN, Tema, Descripción, value) %>% 
  summarise(n = sum(n), 
            n_ele = sum(n_ele),
            n_hog = sum(n_hog),
            n_viv = sum(n_viv)) %>% 
  ungroup() %>% 
  filter(!str_detect(Descripción, "num"),
         value == 1) %>% 
  rename(state_code = ENT, mun_code = MUN) %>% 
  mutate(nfac = ifelse(Tema == "víctima hogar", 
                       n_hog, 
                       n_ele)) %>% 
  dplyr::select(state_code, mun_code,
                Tema, Descripción, 
                valor = nfac) %>% 
  mutate(Descripción = str_trim(str_replace(tolower(Descripción), "sí", "")))


# datos victimización ----
tab_2 <- tab_pvic1 %>% 
  filter(Tema == "inseguridad") %>% 
  group_by(ENT, MUN, Tema, Descripción, value) %>% 
  summarise(n = sum(n), 
            nfac = sum(n_ele)) %>% 
  group_by(ENT, MUN, Tema, Descripción ) %>% 
  mutate(prop = nfac/sum(nfac)) %>% 
  ungroup() %>% 
  filter(value == 2) %>% 
  rename(state_code = ENT, mun_code = MUN) %>% 
  dplyr::select(state_code, mun_code,
                Tema, Descripción, valor = prop) 


tab_3 <- tab_pvic1 %>% 
  filter(Tema %in% c("importancia", 'percepción', "problemas")) %>% 
  group_by(ENT, MUN, Tema, Descripción, value) %>% 
  summarise(n = sum(n), 
            n_ele = sum(n_ele)) %>% 
  group_by(ENT, MUN, Tema, Descripción) %>% 
  mutate(prop = n_ele/sum(n_ele)) %>% 
  ungroup() %>% 
  filter(value == 1) %>% 
  rename(state_code = ENT, mun_code = MUN) %>% 
  dplyr::select(state_code, mun_code,
                Tema, Descripción, valor = prop)



# Union de tabla ----
tab_1 %>% 
  bind_rows(tab_2) %>% 
  bind_rows(tab_3)

