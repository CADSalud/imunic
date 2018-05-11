

library(ProjectTemplate)
load.project()


# Códigos de entidad y municipio
load("cache/tab_cods_estmun.RData")
tab_cods_estmun %>% head




# Tabla de indicadores intercensal ----
load("cache/inds_list.RData")
names(inds_list)
length(inds_list) # 21

tab_ind <- inds_list %>% 
  bind_rows() %>% 
  spread(indicadores, valor)
summary(tab_ind)

tt <- tab_ind %>% 
  mutate(im_ingreso = ingreso_q25/ingreso_q75, 
         im_jov_estudian = (jov_acteco_estud)/pob_15a24,
         im_jov_ecoact = (jov_acteco_ocupado + jov_acteco_desocup)/pob_15a24,
         im_jov_nini = (pob_15a24 - (jov_acteco_ocupado + jov_acteco_estud))/pob_15a24,
         im_jov_ocup_pea = jov_acteco_ocupado/(jov_acteco_ocupado + jov_acteco_desocup),
         im_jov_ocup_pob = jov_acteco_ocupado/pob_15a24,
         im_linbien = pob_bajo_lineabien/pob_total,
         im_linbienmin = pob_bajo_lineabienmin/pob_total) %>% 
  dplyr::select(ENT:COBERTURA, starts_with("im_"))
tt %>% data.frame %>% head
tt %>% summary()
