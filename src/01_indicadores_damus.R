library(ProjectTemplate)
load.project()



# prueba 
query_exec("SELECT * FROM [imunic-196018:damus.bd_imunac_login] limit 3", 
           project = "imunic-196018")



# Datos de damus ----
imunac <- query_exec("SELECT * FROM [imunic-196018:damus.bd_imunac_login]", 
                              project = "imunic-196018", 
                              page_size = 7000) %>% 
  as_tibble() %>% 
  rename(state_code = cve_ent, mun_code = cve_mun)
imunac



# Indicadores por municipio y promedio por entidad ----
tab_damus_rmd <- imunac %>% 
  dplyr::select(state_code,mun_code, 
                sb_agua_potable, sb_almacenamiento_agua, sb_electricidad, 
                sb_combustible_cocina, sb_material_vivienda, sb_saneamiento, 
                sb_manejo_residuos, sb_telecomunicaciones, 
                sb_espacio, dt_empleo_uso_suelo) %>% 
  gather(indicador, mun, -c(state_code, mun_code)) %>% 
  group_by(indicador, state_code) %>% 
  mutate(proment = mean(mun)) %>% 
  gather(tpo, valor, c(mun, proment)) %>% 
  unite(indicadores, c(indicador, tpo)) %>% 
  spread(indicadores, valor) %>% 
  ungroup()
  
tab_damus_rmd
save(tab_damus_rmd, file = "cache/tab_damus_rmd.RData")
