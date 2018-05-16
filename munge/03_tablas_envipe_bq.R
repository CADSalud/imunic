
library(tidyverse)
library(bigrquery)





# variables----
doc_vars <- read_csv("docs/envipe_variables_tiempo.csv") %>% 
  gather(year, pregunta, `2013`:`2016`)
doc_vars
doc_vars$Tema %>% unique()

doc_keys <- read_csv("docs/envipe_keys_union.csv") %>% 
  gather(year, pregunta, `2013`:`2016`)
doc_keys


# pruebas query
query_exec("SELECT * FROM [imunic-196018:envipe.tper_vic2_2016] LIMIT 5", 
           project = "imunic-196018")

query_exec("SELECT * FROM [imunic-196018:envipe.tper_vic1_2014] LIMIT 5", 
           project = "imunic-196018")

# Variables de victimas persona por año

# Percepción ----
tab_pvic1_l <- lapply(2014:2017, function(year_num){
  # year_num <- 2015
  print(year_num)
  
  doc_sub <- doc_vars %>% 
    filter(Tema %in% c("inseguridad", "importancia", 
                       "percepción", "problemas") , 
           year == year_num-1)
  doc_sub
  
  key_vars_tper <- doc_keys %>% 
    filter(Tabla == "tper_vic1", 
           year == year_num-1) %>% 
    .$pregunta
  key_vars_tper
  
  qry_tper <- paste0("SELECT year_file, FAC_ELE, FAC_HOG, ",
                     key_vars_tper,
                     ", ",
                     paste(doc_sub$pregunta, collapse = ", "), 
                     " FROM [imunic-196018:envipe.tper_vic1_",
                     year_num, "]")
  tab_tper <- query_exec(query = qry_tper, 
             project = "imunic-196018") %>% 
    as_tibble()
  
  qry_viv <- paste0("SELECT * FROM [imunic-196018:envipe.tvivienda_",
                    year_num , "]")
  tab_tviv <- query_exec(query = qry_viv, 
             project = "imunic-196018") %>% 
    as_tibble()

  tab <- tab_tper %>% 
    left_join(tab_tviv)
  
  if(year_num == 2017){
    tab <- tab %>% 
      dplyr::rename(ENT = CVE_ENT, MUN = CVE_MUN)
  }
  tab %>% 
    gather(variable, value, starts_with("AP4_")) %>% 
    group_by(year_file, ENT, MUN, variable, value) %>% 
    summarise(n = n(), 
              n_ele = sum(FAC_ELE),
              n_hog = sum(FAC_HOG), 
              n_viv = sum(FAC_VIV)) %>% 
    ungroup %>% 
    left_join(doc_sub %>% 
                dplyr::select(-year) %>% 
                unique(), 
              by = c("variable" = "pregunta"))
})

tab_pvic1 <- tab_pvic1_l %>% 
  bind_rows() %>% 
  ungroup

cache("tab_pvic1")




# Incidencia ----

tab_pvic2_l <- lapply(2014:2017, function(year_num){
  # year_num <- 2015
  print(year_num)
  
  doc_sub <- doc_vars %>% 
    filter(Tema %in% c("víctima hogar", "víctima persona") , 
           year == year_num-1)
  doc_sub
  
  key_vars_tper <- doc_keys %>% 
    filter(Tabla == "tper_vic2", 
           year == year_num-1) %>% 
    .$pregunta
  key_vars_tper
  
  qry_tper <- paste0("SELECT year_file, FAC_ELE, FAC_HOG, ",
                     key_vars_tper,
                     ", ",
                     paste(doc_sub$pregunta, collapse = ", "), 
                     " FROM [imunic-196018:envipe.tper_vic2_",
                     year_num, "]")
  tab_tper <- query_exec(query = qry_tper, 
                         project = "imunic-196018") %>% 
    as_tibble()
  
  qry_viv <- paste0("SELECT * FROM [imunic-196018:envipe.tvivienda_",
                    year_num , "]")
  tab_tviv <- query_exec(query = qry_viv, 
                         project = "imunic-196018") %>% 
    as_tibble()
  
  tab <- tab_tper %>% 
    left_join(tab_tviv)
  
  if(year_num == 2017){
    tab <- tab %>% 
      dplyr::rename(ENT = CVE_ENT, MUN = CVE_MUN)
  }
  tt <- tab %>% 
    gather_('variable', 'value', doc_sub$pregunta) %>% 
    group_by(year_file, ENT, MUN, variable, value) %>% 
    summarise(n = n(), 
              n_ele = sum(FAC_ELE),
              n_hog = sum(FAC_HOG), 
              n_viv = sum(FAC_VIV)) %>% 
    ungroup %>% 
    left_join(doc_sub %>% 
                dplyr::select(-year) %>% 
                unique(), 
              by = c("variable" = "pregunta"))
})


tab_pvic2 <- tab_pvic2_l %>% 
  bind_rows() %>% 
  ungroup

cache("tab_pvic2")



# Tabla percepcion ----
variables_selec <- c("percepción_pandillerismo", 
  "percepción_venta droga", 
  "percepción_robos asaltos",
  "percepción_policia vs ciudadanos")#,
  # "problemas_pandillerismo violento",
  # "importancia_Narco",
  # "importancia_Desempleo",
  # "importancia_Impunidad")


tab_vars <- doc_vars %>% 
  filter(year == 2016) %>% 
  unite(variable, c(Tema, Descripción), sep = "_") %>% 
  filter(variable %in% variables_selec)




