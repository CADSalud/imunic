
library(ProjectTemplate)
load.project()

load("cache/tab_delitos_u.RData")

library(mxmaps)

mxmunicipio.map %>% head
mxmunicipio.map$region %>% n_distinct()
mxmunicipio.map$id %>% n_distinct()
tab_delitos_u %>% nrow()

# union con mapa ----
tab_gg <- mxmunicipio.map %>% 
  as.tibble() %>% 
  mutate(state_code = parse_number(str_sub(id, 1, 2)), 
         mun_code = parse_number(str_sub(id, 3, 5)) ) %>% 
  left_join(tab_delitos_u, 
            spread(indicador, value), 
            by = c("state_code", "mun_code"))
tab_gg


# Mapa de municipios e indicadores ----
names(tab_gg)
tab_gg %>% 
  ggplot() +  
  geom_polygon(aes(x = long, 
                   y = lat,
                   group = group, 
                   fill = prop_seguridad),  
               color = "white", 
               size = .01)


tab_gg %>% 
  dplyr::select(long:mun_code, 
                `scrt_robo comun : c/s violencia`, 
                `vicper_robo_calle_transporte`, 
                `vicper_robo_otro`) %>% 
  filter(state_code == 9) %>% 
  gather(delito, value, `scrt_robo comun : c/s violencia`:`vicper_robo_otro`) %>% 
  ggplot() +  
  geom_polygon(aes(x = long, 
                   y = lat,
                   group = group, 
                   fill = value),  
               color = "white", 
               size = .001) + 
  facet_wrap(~delito)



tab_gg %>% 
  dplyr::select(long:mun_code, 
                `scrt_delitos sexuales (violacion) : violacion`, 
                `vicper_violación_sexual`) %>% 
  filter(state_code) %>% 
  gather(delito, value, 
         `scrt_delitos sexuales (violacion) : violacion`:`vicper_violación_sexual`) %>% 
  ggplot() +  
  geom_polygon(aes(x = long, 
                   y = lat,
                   group = group, 
                   fill = value),  
               color = "white", 
               size = .001) + 
  facet_wrap(~delito)



tab_inds <- tab_delitos_urec %>% 
  filter(gpo_indicador == "robo") 

tab_gg <- mxmunicipio.map %>% 
  as.tibble() %>% 
  mutate(state_code = parse_number(str_sub(id, 1, 2)), 
         mun_code = parse_number(str_sub(id, 3, 5)) ) %>% 
  left_join(tab_inds) %>% 
  filter(state_code %in% c(4))
tab_gg

tab_gg %>% 
  ggplot() +  
  geom_polygon(aes(x = long, 
                   y = lat,
                   group = group, 
                   fill = value),  
               color = "white", 
               size = .001) + 
  facet_wrap(~indicador) + 
  theme(legend.position = "bottom")
