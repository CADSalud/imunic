

# library(ProjectTemplate)
# load.project()

# library(knitr)
# library(rmarkdown)

# knit(input = "reports/ficha_template.Rmd")
# render(input = "reports/ficha_template.Rmd", output_format = "pdf_document")

# Municipios ----

tablas_load_fun <- function(municipios_selec, state_code_selec){
  require(tidyverse)
  require(here)
  
  # Para cargar datos
  setwd(here("."))
  print(getwd())
  
  load("cache/tab_cods_estmun.RData")
  tab_estmun_selec <- tab_cods_estmun %>% 
    filter(NOM_MUN == municipios_selec, 
           state_code == state_code_selec)
  
  
  # Envipe
  load("cache/tab_union_indicadores.RData")
  tab_envipe <- tab_union_indicadores %>% 
    right_join(tab_estmun_selec, 
               by = c("state_code", "mun_code"))
  
  
  # Intercensal
  load("cache/tab_intercensal_rmd.RData")
  tab_interc <- tab_intercensal_rmd %>% 
    right_join(tab_estmun_selec, 
               by = c("state_code", "mun_code"))
  
  
  # Intercensal entidad
  tab_estatal <- tab_intercensal_rmd %>% 
    filter(state_code == tab_estmun_selec$state_code) %>% 
    dplyr::select(-c(state_code, mun_code, COBERTURA)) %>% 
    summarise_all(.funs = sum)
  
  # Ingreso entidad
  load("cache/pob_trabajaingreso_ent.RData")
  tab_ingreso_ent <- pob_trabajaingreso_ent %>% 
    filter(parse_number(ENT) == tab_estmun_selec$state_code)
  
  tablas <- list(tab_envipe = tab_envipe, 
                 tab_interc = tab_interc, 
                 tab_estatal = tab_estatal, 
                 entidad = tab_estmun_selec, 
                 tab_ingreso_ent = tab_ingreso_ent)
}

