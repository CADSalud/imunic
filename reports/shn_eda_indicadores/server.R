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
library(RColorBrewer)
library(plotly)

# load("../../cache/tab_delitos_urec.RData")
load("../../cache/tab_union_indicadores.RData")
load("../../cache/tab_indic_intercensal.RData")
load("../../cache/tab_cods_estmun.RData")
theme_set(theme_minimal(base_size = 16))

tab_cods_estmun %>% 
  arrange(state_code, mun_code) %>% 
  unique() %>% 
  data.frame()

# Tabla de codigos
tab_cods_estmun <- tab_cods_estmun %>% 
  rename(Entidad = NOM_ENT, Municipio = NOM_MUN)

# Tabla intercensal
tab_indic_intercensal <- tab_indic_intercensal%>% 
  # nombres de estados y municipios
  right_join(tab_cods_estmun, 
             by = c("state_code", "mun_code"))

# Tabla de indicadores envipe
tab_union_indicadores <- tab_union_indicadores %>% 
  # nombres de estados y municipios
  right_join(tab_cods_estmun, 
             by = c("state_code", "mun_code"))

# Tabla de indicadores 
tab_delitos_urec <-  tab_union_indicadores %>% 
  filter(Tema %in% c("víctima hogar", 
                     "víctima persona", 
                     "secretariado")) %>% 
  bind_rows(tab_indic_intercensal)



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
  
  # INDICADORES ----
  TabInds <- reactive({
    state_selec <- StateSelec()
    
    indicador_selec <- input$indicador.input
    if(input$indicador.input == "robo"){
      indicador_selec <- c("robo", "robo calle", "robo otro")
    }
    
    tab_inds <- tab_delitos_urec %>% 
      filter(Descripción %in% indicador_selec ) 
    tab_inds
  })
  
  output$tab_inds <- renderDataTable({
    state_selec <- StateSelec()
    tab <- TabInds() %>% 
      mutate(valor = round(valor, 3)) %>% 
      spread(Descripción, valor)
    if( length(state_selec) == 1 ){
      tab <- tab %>% 
        filter(state_code == state_selec)
    }
    tab %>% 
      dplyr::select(-Tema, -state_code, -mun_code)
  }, options = list(autoWidth = TRUE))
  
  output$map_inds_gg <- renderPlot({
    state_selec <- StateSelec()
    tab_gg <- mxmunicipio.map %>% 
      as.tibble() %>% 
      mutate(state_code = parse_number(str_sub(id, 1, 2)), 
             mun_code = parse_number(str_sub(id, 3, 5)) ) %>% 
      full_join(TabInds(),
                by = c("state_code", "mun_code")) %>% 
      filter(state_code %in% state_selec) 
    
    tab_gg %>% 
      ggplot() +  
      geom_polygon(aes(x = long, 
                       y = lat,
                       group = group, 
                       fill = valor),  
                   color = "white", 
                   size = .001) + 
      facet_wrap(~Tema + Descripción) + 
      scale_fill_continuous(low = "#ffffb2", high = "#253494") + 
      theme(legend.position = "bottom") + 
      guides(fill = guide_legend(direction = "vertical")) +
      coord_equal()
  })
  
  # IMPORTANCIA ----
  output$tab_importancia <- renderDataTable({
    state_selec <- StateSelec()
    tab <- tab_union_indicadores %>% 
      filter(Tema == "importancia") %>% 
      mutate(valor = round(valor, 3)) %>% 
      spread(Descripción, valor)
    if( length(state_selec) == 1 ){
      tab <- tab %>% 
        filter(state_code == state_selec)
    }
    tab %>% 
      dplyr::select(-Tema, -state_code, -mun_code)
  }, options = list(autoWidth = TRUE))
  
  output$map_importancia_gg <- renderPlot({
    state_selec <- StateSelec()
    gg <- mxmunicipio.map %>% 
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
      scale_fill_continuous(low = "#ffffb2", high = "#006837") + 
      theme(legend.position = "bottom") + 
      guides(fill = guide_legend(direction = "vertical")) +
      coord_equal()
    # ggplotly(gg)
    gg
  })
  
  
  # PERCEPCIÓN ----
  output$tab_percepcion <- renderDataTable({
    state_selec <- StateSelec()
    tab <- tab_union_indicadores %>% 
      filter(Tema == "percepción") %>% 
      mutate(valor = round(valor, 3)) %>% 
      spread(Descripción, valor)
    if( length(state_selec) == 1 ){
      tab <- tab %>% 
        filter(state_code == state_selec)
    }
    tab %>% 
      dplyr::select(-Tema, -state_code, -mun_code)
  }, options = list(autoWidth = TRUE))
  
  
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
      scale_fill_continuous(low = "#ffffb2", high = "#7a0177") + 
      theme(legend.position = "bottom") + 
      guides(fill = guide_legend(direction = "vertical")) +
      coord_equal()
  })
  
  # PROBLEMAS ----
  output$tab_problemas <- renderDataTable({
    state_selec <- StateSelec()
    tab <- tab_union_indicadores %>% 
      filter(Tema == "problemas") %>% 
      mutate(valor = round(valor, 3)) %>% 
      spread(Descripción, valor)
    if( length(state_selec) == 1 ){
      tab <- tab %>% 
        filter(state_code == state_selec)
    }
    tab %>% 
      dplyr::select(-Tema, -state_code, -mun_code)
  }, options = list(autoWidth = TRUE))
  
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
      scale_fill_continuous(low = "#ffffb2", high = "#980043") + 
      theme(legend.position = "bottom") + 
      guides(fill = guide_legend(direction = "vertical")) +
      coord_equal()
  })
 
  
  # INSEGURIDAD ----
  output$tab_percinseg <- renderDataTable({
    state_selec <- StateSelec()
    tab <- tab_union_indicadores %>% 
      filter(Tema == "inseguridad") %>% 
      mutate(valor = round(valor, 3)) %>% 
      spread(Descripción, valor)
    if( length(state_selec) == 1 ){
      tab <- tab %>% 
        filter(state_code == state_selec)
    }
    tab %>% 
      dplyr::select(-Tema, -state_code, -mun_code)
  }, options = list(autoWidth = TRUE))
  
  
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
      scale_fill_continuous(low = "#fff7bc", high = "#f03b20") + 
      theme(legend.position = "bottom") + 
      guides(fill = guide_legend(direction = "vertical")) +
      coord_equal()
  })
  
})
