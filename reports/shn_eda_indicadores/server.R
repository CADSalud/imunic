#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(mxmaps)

# load("../../cache/tab_delitos_urec.RData")
load("../../cache/tab_union_indicadores.RData")
theme_set(theme_minimal(base_size = 16))

tab_delitos_urec <-  tab_union_indicadores %>% 
  filter(Tema %in% c("víctima hogar", 
                     "víctima persona", 
                     "secretariado")) 

tab_importancia <-  tab_union_indicadores %>% 
  filter(Tema == "importancia") 

shinyServer(function(input, output) {
   
  
  StateSelec <- reactive({
    if(input$estado.input == "all"){
      state_selec <- unique(tab_delitos_urec$state_code)  
    }
    if(input$estado.input != "all"){
      state_selec <- unique(input$estado.input)
    }
    state_selec
  })
  
  TabInds <- reactive({
    state_selec <- StateSelec()
    
    indicador_selec <- input$indicador.input
    if(input$indicador.input == "robo"){
      indicador_selec <- c("robo", "robo calle", "robo otro")
    }
    
    tab_inds <- tab_delitos_urec %>% 
      filter(Descripción %in% indicador_selec ) 
    tab_gg <- mxmunicipio.map %>% 
      as.tibble() %>% 
      mutate(state_code = parse_number(str_sub(id, 1, 2)), 
             mun_code = parse_number(str_sub(id, 3, 5)) ) %>% 
      full_join(tab_inds,
                by = c("state_code", "mun_code")) %>% 
      filter(state_code %in% state_selec) 
  })
  
  output$map_inds_gg <- renderPlot({
    tab_gg <- TabInds() 
    tab_gg %>% 
      ggplot() +  
      geom_polygon(aes(x = long, 
                       y = lat,
                       group = group, 
                       fill = valor),  
                   color = "white", 
                   size = .001) + 
      facet_wrap(~Tema + Descripción) + 
      theme(legend.position = "bottom") + 
      guides(fill = guide_legend(direction = "vertical")) +
      coord_equal()
  })
  
  
  output$map_importancia_gg <- renderPlot({
    state_selec <- StateSelec()
    
    mxmunicipio.map %>% 
      as.tibble() %>% 
      mutate(state_code = parse_number(str_sub(id, 1, 2)), 
             mun_code = parse_number(str_sub(id, 3, 5)) ) %>% 
      full_join(tab_union_indicadores %>% 
                  filter(Tema == "importancia") %>% 
                  filter(Descripción == input$importancia.input),
                by = c("state_code", "mun_code")) %>% 
      filter(state_code %in% state_selec) %>% 
      ggplot() +  
      geom_polygon(aes(x = long, 
                       y = lat,
                       group = group, 
                       fill = valor),  
                   color = "white", 
                   size = .001) + 
      facet_wrap(~ Descripción) + 
      theme(legend.position = "bottom") + 
      guides(fill = guide_legend(direction = "vertical")) +
      coord_equal()
  })
  
  output$map_percepcion_gg <- renderPlot({
    state_selec <- StateSelec()
    
    mxmunicipio.map %>% 
      as.tibble() %>% 
      mutate(state_code = parse_number(str_sub(id, 1, 2)), 
             mun_code = parse_number(str_sub(id, 3, 5)) ) %>% 
      full_join(tab_union_indicadores %>% 
                  filter(Tema == "percepción") %>% 
                  filter(Descripción == input$percepcion.input),
                by = c("state_code", "mun_code")) %>% 
      filter(state_code %in% state_selec) %>% 
      ggplot() +  
      geom_polygon(aes(x = long, 
                       y = lat,
                       group = group, 
                       fill = valor),  
                   color = "white", 
                   size = .001) + 
      facet_wrap(~ Descripción) + 
      theme(legend.position = "bottom") + 
      guides(fill = guide_legend(direction = "vertical")) +
      coord_equal()
  })
  
  output$map_problemas_gg <- renderPlot({
    state_selec <- StateSelec()
    
    mxmunicipio.map %>% 
      as.tibble() %>% 
      mutate(state_code = parse_number(str_sub(id, 1, 2)), 
             mun_code = parse_number(str_sub(id, 3, 5)) ) %>% 
      full_join(tab_union_indicadores %>% 
                  filter(Tema == "problemas") %>% 
                  filter(Descripción == input$problemas.input),
                by = c("state_code", "mun_code")) %>% 
      filter(state_code %in% state_selec) %>% 
      ggplot() +  
      geom_polygon(aes(x = long, 
                       y = lat,
                       group = group, 
                       fill = valor),  
                   color = "white", 
                   size = .001) + 
      facet_wrap(~ Descripción) + 
      theme(legend.position = "bottom") + 
      guides(fill = guide_legend(direction = "vertical")) +
      coord_equal()
  })
 
  
  
   output$map_percinseg_gg <- renderPlot({
    state_selec <- StateSelec()
    
    mxmunicipio.map %>% 
      as.tibble() %>% 
      mutate(state_code = parse_number(str_sub(id, 1, 2)), 
             mun_code = parse_number(str_sub(id, 3, 5)) ) %>% 
      full_join(tab_union_indicadores %>% 
                  filter(Tema == "inseguridad") ,
                by = c("state_code", "mun_code")) %>% 
      filter(state_code %in% state_selec) %>% 
      ggplot() +  
      geom_polygon(aes(x = long, 
                       y = lat,
                       group = group, 
                       fill = valor),  
                   color = "white", 
                   size = .001) + 
      facet_wrap(~ Descripción) + 
      theme(legend.position = "bottom") + 
      guides(fill = guide_legend(direction = "vertical")) +
      coord_equal()
  })
  
})
