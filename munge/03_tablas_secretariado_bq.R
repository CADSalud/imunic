

library(tidyverse)
library(bigrquery)

# pruebas query
query_exec("SELECT * FROM [imunic-196018:secretariado.vm_fuerocomun_municipios2013] LIMIT 5", 
           project = "imunic-196018")


vars_mod <- c("HOMICIDIOS", "ROBO COMUN", 
  "DELITOS SEXUALES (VIOLACION)",
  "PRIV. DE LA LIBERTAD (SECUESTRO)")


# Secretariado
tab_secre_l <- lapply(2013:2016, function(year_num){
  # year_num <- 2015
  print(year_num)
  
  qry_secr <- paste0("SELECT state_code, mun_code, modalidad, tipo, count",
                     " FROM [imunic-196018:secretariado.vm_fuerocomun_municipios",
                     year_num, 
                     "] WHERE modalidad IN (", 
                     paste("'", vars_mod, "'", collapse = ", ", sep = ""),
                     ")")
  tab_secr <- query_exec(query = qry_secr, 
                         project = "imunic-196018", 
                         max_pages = Inf) %>% 
    as_tibble %>% 
    mutate(count = parse_number(count), 
           year_num) %>% 
    group_by(year_num, state_code, mun_code, modalidad, tipo) %>% 
    summarise(count = sum(parse_number(count), na.rm = T)) %>% 
    ungroup
})



tab_secre <- tab_secre_l %>% 
  bind_rows() %>% 
  mutate(tipo = fct_recode(tipo, 
                           `C/S VIOLENCIA` = 'CON VIOLENCIA',
                           `C/S VIOLENCIA` = 'SIN VIOLENCIA') ) %>% 
  filter(tipo != "CULPOSOS") %>% 
  group_by(state_code, mun_code, modalidad, tipo) %>% 
  summarise(total = sum(count, na.rm = T))
tab_secre

cache("tab_secre")
