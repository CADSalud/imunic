library(ProjectTemplate)
load.project()

concentrado2016 <- query_exec("SELECT * FROM [imunic-196018:enigh.concentrado2016]", 
                              project = "imunic-196018", 
                              page_size = 7000)

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

