
library(bigrquery)

tab_cods_estmun <- query_exec("SELECT ENT, NOM_ENT, MUN, NOM_MUN, COUNT(ENT), FROM [imunic-196018:intercensal.persona_append] GROUP BY ENT, NOM_ENT, MUN, NOM_MUN",
           project = "imunic-196018") %>% 
  as_tibble() %>% 
  mutate(state_code = parse_number(ENT), 
         mun_code = parse_number(MUN)) %>% 
  dplyr::select(-f0_, -ENT, -MUN) 
tab_cods_estmun

cache("tab_cods_estmun")
