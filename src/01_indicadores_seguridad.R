

library(ProjectTemplate)
load.project()


# Códigos de entidad y municipio
load("cache/tab_cods_estmun.RData")
tab_cods_estmun %>% head


# Percepción estatal envipe ----
load("cache/tab_pvic1.RData")
tab_pvic1 %>% 
  filter(variable %in% tab_vars$pregunta, 
         year_file == 2017) %>% 
  unite(tema, c(Tema, Descripción), sep = "_") %>% 
  group_by(ENT, tema) %>% 
  mutate(total = sum(n_ele)) %>% 
  group_by(ENT, variable, tema, value) %>% 
  summarise(prop = sum(n_ele)/unique(total)) %>% 
  filter(value == 1) %>% 
  ungroup %>% 
  mutate(tema = str_replace_all(tema, " ", "_")) %>% 
  dplyr::select(ENT, tema, prop) %>% 
  spread(tema, prop)





# Tabla de indicadores intercensal ----
load("cache/inds_list.RData")
names(inds_list)
length(inds_list) # 22

inds_list %>% names
inds_list$pob_municipio

tab_ind <- inds_list %>% 
  bind_rows() %>% 
  dplyr::select(-COBERTURA) %>% 
  spread(indicadores, valor) %>% 
  left_join(inds_list$pob_municipio %>% 
              dplyr::select(ENT:COBERTURA))
summary(tab_ind)
tab_ind


tt <- tab_ind %>% 
  mutate(im_ind_ingreso  = ingreso_q25/ingreso_q75, 
         im_jov_estudian = (jov_acteco_estud)/pob_15a24,
         im_jov_ecoact   = (jov_acteco_ocupado + jov_acteco_desocup)/pob_15a24,
         im_jov_nini     = (pob_15a24 - (jov_acteco_ocupado + jov_acteco_estud))/pob_15a24,
         im_jov_ocup_pea = jov_acteco_ocupado/(jov_acteco_ocupado + jov_acteco_desocup),
         im_jov_ocup_pob = jov_acteco_ocupado/pob_15a24,
         im_pol_habs     = trab_seguridad_pub/pob_total, 
         im_madres_sol   = madres_solteras/madres_nosolteras,
         im_pob_total    = pob_total,
         im_linbien      = pob_bajo_lineabien/pob_total,
         im_linbienmin   = pob_bajo_lineabienmin/pob_total) %>% 
  dplyr::select(ENT, MUN, COBERTURA, starts_with("im_"))
tt %>% data.frame %>% head
tt %>% summary()


tt %>% filter(ENT == '09') %>% 
  data.frame()
