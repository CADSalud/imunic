
library(ProjectTemplate)
load.project()


# Tablas resúmen ----
load("cache/tab_pvic1.RData")
load("cache/tab_pvic2.RData")
load("cache/tab_secre.RData")


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



# secretariado ----
tab_4 <- tab_secre %>% 
  ungroup() %>% 
  mutate_at(.vars = c('state_code', 'mun_code'), .funs = parse_number) %>% 
  unite(ind, c(modalidad, tipo)) %>% 
  mutate(Descripción = fct_recode(ind, 
                    `violación sexual` = "DELITOS SEXUALES (VIOLACION)_VIOLACION",
                    `homicidio` = "HOMICIDIOS_DOLOSOS", 
                    `secuestro` = "PRIV. DE LA LIBERTAD (SECUESTRO)_SECUESTRO",
                    `robo` = "ROBO COMUN_C/S VIOLENCIA"), 
         Tema = "secretariado") %>% 
  ungroup() %>% 
  dplyr::select(-ind) %>% 
  rename(valor = total)
tab_4



# 1. Union de tabla ----
tab_union_indicadores <- tab_1 %>% 
  bind_rows(tab_2) %>% 
  bind_rows(tab_3) %>% 
  bind_rows(tab_4) %>% 
  complete(state_code, mun_code, nesting(Tema, Descripción)) %>% 
  ungroup


tab_union_indicadores$Descripción %>% unique()
tab_union_indicadores$Tema %>% unique()

cache("tab_union_indicadores")


tab_union_indicadores %>% 
  group_by(state_code, mun_code) %>% 
  tally()



library(mxmaps)

tab_union_indicadores$Tema %>% unique()

tab_union_indicadores %>% 
  filter(Tema == "problemas") %>% 
  .$Descripción %>% unique %>% write.table(row.names = F)



mxmunicipio.map %>% 
  as.tibble() %>% 
  mutate(state_code = parse_number(str_sub(id, 1, 2)), 
         mun_code = parse_number(str_sub(id, 3, 5)) ) %>% 
  full_join(tab_union_indicadores %>% 
              filter(Tema == "inseguridad") ,
            by = c("state_code", "mun_code")) %>% 
  ggplot() +  
  geom_polygon(aes(x = long, 
                   y = lat,
                   group = group, 
                   fill = valor),  
               color = "white", 
               size = .001) + 
  facet_wrap(~Tema + Descripción) + 
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(direction = "vertical")) +
  coord_equal()



# 2. Indicadores Intercensal ----

load("cache/tab_union_indicadores.RData")
load("cache/inds_list.RData")



tab_indic_intercensal <- inds_list %>% 
  bind_rows() %>% 
  spread(indicadores, valor) %>% 
  mutate(ind_prop_preesc = pob_asistenesc_03_05/pob_03_05,
         ind_prop_primar = pob_asistenesc_06_12/pob_06_12,
         ind_prop_secund = pob_asistenesc_13_15/pob_13_15,
         ind_prop_prepar = pob_asistenesc_16_18/pob_16_18,
         ind_prop_univer = pob_asistenesc_19_26/pob_19_26, 
         ind_pob_sineducbasica = pob_15mas_sinedubasica,
         ind_pob_mas75 = pob_mas75,
         ind_pob_infantil = pob_03_05 + pob_06_12,
         ind_pob_juvenil = pob_19_26,
         ind_prop_indigena = pob_perte_indigena/pob_total,
         ind_prop_seguropopu = pob_seg_popular/pob_total,
         ind_prop_seguropriv = pob_seg_privado/pob_total,
         ind_prop_seguroimss = pob_seg_imss/pob_total) %>% 
  dplyr::select(ENT, MUN, starts_with("ind_")) %>% 
  gather(indicadores, valor, -c(ENT, MUN)) %>% 
  left_join(read_csv("docs/intercensal_inds.csv")) %>% 
  rename(state_code = ENT, 
         mun_code = MUN) %>% 
  mutate(state_code = parse_number(state_code),
         mun_code = parse_number(mun_code)) %>% 
  dplyr::select(-indicadores)

tab_indic_intercensal
tab_indic_intercensal %>% head
tab_union_indicadores %>% head


cache("tab_indic_intercensal")

# Datos para rmd ----
tab_intercensal_rmd <- inds_list %>% 
  bind_rows() %>% 
  spread(indicadores, valor) %>% 
  mutate(ind_prop_preesc = pob_asistenesc_03_05/pob_03_05,
         ind_prop_primar = pob_asistenesc_06_12/pob_06_12,
         ind_prop_secund = pob_asistenesc_13_15/pob_13_15,
         ind_prop_prepar = pob_asistenesc_16_18/pob_16_18,
         ind_prop_univer = pob_asistenesc_19_26/pob_19_26, 
         ind_pob_sineducbasica = pob_15mas_sinedubasica,
         ind_pob_mas75 = pob_mas75,
         ind_pob_infantil = pob_03_05 + pob_06_12,
         ind_pob_juvenil = pob_19_26,
         ind_prop_indigena = pob_perte_indigena/pob_total,
         ind_prop_seguropopu = pob_seg_popular/pob_total,
         ind_prop_seguropriv = pob_seg_privado/pob_total,
         ind_prop_seguroimss = pob_seg_imss/pob_total) %>% 
  rename(state_code = ENT, 
         mun_code = MUN) %>% 
  mutate(state_code = parse_number(state_code),
         mun_code = parse_number(mun_code)) 
tab_intercensal_rmd

cache("tab_intercensal_rmd")
