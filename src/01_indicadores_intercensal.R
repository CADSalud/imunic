
library(ProjectTemplate)
load.project()

# Indicadores Intercensal ----

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
