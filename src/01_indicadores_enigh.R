library(ProjectTemplate)
load.project()



# prueba 
query_exec("SELECT * FROM [imunic-196018:enigh.enigh2016mun] limit 3", 
           project = "imunic-196018", 
           page_size = 7000)



# Datos de Enigh ----
concentrado2016 <- query_exec("SELECT * FROM [imunic-196018:enigh.concentrado2016]", 
                              project = "imunic-196018", 
                              page_size = 7000)

concentrado2016 %>% 
  as_tibble()

# Indicadores por municipio ----
enigh2016mun <- concentrado2016 %>% 
  mutate(clave_geo = stringi::stri_pad_left(ubica_geo,
                                            width = 9,
                                            pad = '0'),
         ent = str_sub(clave_geo,1,2),
         mun = str_sub(clave_geo,3,5),
         hog_arriba_lbm = ifelse(ing_cor/tot_integ>3438.54055122975,1,0)) %>% 
  # Bienestar Mínimo (Canasta alimentaria) promedio anual 2016 (promedio rural y urbano)
  group_by(ent,mun) %>% 
  summarise(hogares = sum(factor),
            salud_prom = Hmisc::wtd.mean(salud, 
                                         weights = factor), 
            salud_med = Hmisc::wtd.quantile(salud, 
                                            weights = factor,
                                            probs = c(0.5)),
            educ_prom = Hmisc::wtd.mean(educacion, 
                                        weights = factor), 
            educ_med = Hmisc::wtd.quantile(educacion, 
                                           weights = factor,
                                           probs = c(0.5)),
            ingr_prom = Hmisc::wtd.mean(ing_cor, 
                                        weights = factor), 
            ingr_med = Hmisc::wtd.quantile(ing_cor, 
                                           weights = factor,
                                           probs = c(0.5)),
            phog_arriba_lbm = sum(hog_arriba_lbm*factor)/hogares)




# Indicadores por entidad ----
enigh2016ent <- concentrado2016 %>% 
  mutate(clave_geo = stringi::stri_pad_left(ubica_geo,
                                            width = 9,
                                            pad = '0'),
         ent = str_sub(clave_geo,1,2),
         mun = str_sub(clave_geo,3,5),
         hog_arriba_lbm = ifelse(ing_cor/tot_integ>3438.54055122975,1,0)) %>% 
  # Bienestar Mínimo (Canasta alimentaria) promedio anual 2016 (promedio rural y urbano)
  group_by(ent) %>% 
  summarise(hogares = sum(factor),
            salud_prom = Hmisc::wtd.mean(salud, 
                                         weights = factor), 
            salud_med = Hmisc::wtd.quantile(salud, 
                                            weights = factor,
                                            probs = c(0.5)),
            educ_prom = Hmisc::wtd.mean(educacion, 
                                        weights = factor), 
            educ_med = Hmisc::wtd.quantile(educacion, 
                                           weights = factor,
                                           probs = c(0.5)),
            ingr_prom = Hmisc::wtd.mean(ing_cor, 
                                        weights = factor), 
            ingr_med = Hmisc::wtd.quantile(ing_cor, 
                                           weights = factor,
                                           probs = c(0.5)),
            phog_arriba_lbm = sum(hog_arriba_lbm*factor)/hogares)







tab_mun <- enigh2016mun %>% 
  rename(state_code = ent, 
         mun_code = mun) %>% 
  ungroup()

names_ent <- function(x){paste0(x, "_ent")}
tab_ent <- enigh2016ent %>% 
  rename_all(.funs = names_ent) %>% 
  rename(state_code = ent_ent) %>% 
  ungroup()


# Tabla para RMD ----
tab_enigh2016_rmd <- tab_mun %>% 
  left_join(tab_ent, by = "state_code") %>% 
  mutate(state_code = parse_number(state_code),
         mun_code = parse_number(mun_code))
tab_enigh2016_rmd

save(tab_enigh2016_rmd, file = "cache/tab_enigh2016_rmd.RData")
