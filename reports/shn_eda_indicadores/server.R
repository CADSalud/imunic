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

load("../../cache/tab_delitos_urec.RData")
theme_set(theme_minimal(base_size = 16))

shinyServer(function(input, output) {
   
  TabGG <- reactive({
    
    if(input$estado.input == "all"){
      state_selec <- unique(tab_delitos_urec$state_code)  
    }
    if(input$estado.input != "all"){
      state_selec <- unique(input$estado.input)
    }
    
    
    
    tab_inds <- tab_delitos_urec %>% 
      filter(gpo_indicador == input$indicador.input)
    tab_gg <- mxmunicipio.map %>% 
      as.tibble() %>% 
      mutate(state_code = parse_number(str_sub(id, 1, 2)), 
             mun_code = parse_number(str_sub(id, 3, 5)) ) %>% 
      left_join(tab_inds,
                by = c("state_code", "mun_code")) %>% 
      filter(state_code %in% state_selec)
    tab_gg
  })
  
  output$map_gg <- renderPlot({
    tab_gg <- TabGG() 
    tab_gg %>% 
      ggplot() +  
      geom_polygon(aes(x = long, 
                       y = lat,
                       group = group, 
                       fill = value),  
                   color = "white", 
                   size = .001) + 
      facet_wrap(~indicador) + 
      theme(legend.position = "bottom") + 
      coord_equal()
  })
  
})
