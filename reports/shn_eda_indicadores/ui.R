#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("EDA: Indicadores"),
  
  br(),
  
  wellPanel(fixedRow(column(5, selectInput(inputId = "estado.input", 
                        label = "Estado",
                        selected = "all",
                        choices = c("0 NACIONAL" = "all", 
                                    "1 AGUASCALIENTES" = 1,
                                    "2 BAJA CALIFORNIA" = 2,
                                    "3 BAJA CALIFORNIA SUR" = 3,
                                    "4 CAMPECHE" = 4,
                                    "5 COAHUILA" = 5,
                                    "6 COLIMA" = 6,
                                    "7 CHIAPAS" = 7,
                                    "8 CHIHUAHUA" = 8,
                                    "9 CIUDAD DE MEXICO" = 9,
                                    "10 DURANGO" = 10, 
                                    "11 GUANAJUATO" = 11, 
                                    "12 GUERRERO" = 12, 
                                    "13 HIDALGO" = 13, 
                                    "14 JALISCO" = 14, 
                                    "15 MEXICO" = 15, 
                                    "16 MICHOACAN" = 16, 
                                    "17 MORELOS" = 17, 
                                    "18 NAYARIT" = 18, 
                                    "19 NUEVO LEON" = 19, 
                                    "20 OAXACA" = 20, 
                                    "21 PUEBLA" = 21, 
                                    "22 QUERETARO" = 22, 
                                    "23 QUINTANA ROO" = 23, 
                                    "24 SAN LUIS POTOSI" = 24, 
                                    "25 SINALOA" = 25, 
                                    "26 SONORA" = 26, 
                                    "27 TABASCO" = 27, 
                                    "28 TAMAULIPAS" = 28,
                                    "29 TLAXCALA" = 29,
                                    "30 VERACRUZ" = 30,
                                    "31 YUCATAN" = 31,
                                    "32 ZACATECAS" = 32)))
                     )),
  
  # INDICADORES ----
  tabsetPanel(
    tabPanel("Indicadores", 
             wellPanel(fixedRow(
               column(5,
                      selectInput(inputId = "indicador.input", 
                                  label = "Indicador",
                                  selected = "Prop. población indigena",
                                  choices =c(
                    "violacion sexual" = "violación sexual",
                    "homicidio" = "homicidio",
                    "secuestro" = "secuestro",
                    "desaparicion forzada" = "desparición",
                    "robo" = "robo",
                    "amenazas" = "amenazas",
                    "extorsión" = "extorsión",
                    "fraude bancario" = "fraude bancario",
                    "fraude consumidor" = "fraude consumidor",
                    "abuso sexual" = "abuso sexual",
                    "agresión física" = "agresión física",
                    "otros" = "otros",
                    "Prop. asisten preescolar"   = "Prop. asisten preescolar",
                    "Prop. asisten primaria"     = "Prop. asisten primaria",
                    "Prop. asisten secundaria"   = "Prop. asisten secundaria",
                    "Prop. asisten preparatoria" = "Prop. asisten preparatoria",
                    "Prop. asisten universidad"  = "Prop. asisten universidad",
                    "Población 15 o más sin educación básica" = "Población 15 o más sin educación básica",
                    "Población más de 75 años" = "Población más de 75 años",
                    "Población infantil" = "Población infantil",
                    "Población juvenil" = "Población juvenil",
                    "Prop. población indigena" = "Prop. población indigena",
                    "Prop. con seguro popular" = "Prop. con seguro popular",
                    "Prop. con seguro privado" = "Prop. con seguro privado",
                    "Prop. con seguro imss" = "Prop. con seguro miss"))
               )
             )),
             
             # Show a plot of the generated distribution
             mainPanel(
               tabsetPanel(
                 tabPanel("Mapa", 
                          plotOutput("map_inds_gg", width = "1000px", height = "700px")),
                 tabPanel("Tabla", 
                          dataTableOutput("tab_inds"))
               )
             )
    ),  # indicador panel
    
    
    # IMPORTANCIA ----
    tabPanel("Importancia",
             wellPanel(fixedRow(
               column(5,
                      selectInput(inputId = "importancia.input", 
                                  label = "Tema de importancia",
                                  selected = "",
                                  choices =c("Agua",
                                    "Corrupción",
                                    "Desastres",
                                    "Desempleo",
                                    "Educación",
                                    "Impunidad",
                                    "Inseguridad",
                                    "Narco",
                                    "Pobreza",
                                    "Precios",
                                    "Salud",
                                    "Otro",
                                    "No sabe"
                                    ))
               )
             )),
             mainPanel(
               tabsetPanel(
                 tabPanel("Mapa", 
                          plotOutput("map_importancia_gg", width = "1000px", height = "700px")),
                 tabPanel("Tabla", 
                          dataTableOutput("tab_importancia"))
               )
             )
    ),   # importancia panel
    
    # PERCEPCIÓN ----
    tabPanel("Percepción",
             wellPanel(fixedRow(
               column(5,
                      selectInput(inputId = "percepcion.input", 
                                  label = "Tema de percepción",
                                  selected = "",
                                  choices =c("alcohol en la calle",
                                             "consumo drogas",
                                             "disparos",
                                             "extorsiones",
                                             "homicidios",
                                             "invasión de predios",
                                             "ninguna",
                                             "no sabe",
                                             "pandillerismo",
                                             "policia vs ciudadanos",
                                             "prostitución",
                                             "riñas entre vecinos",
                                             "robos asaltos",
                                             "secuestros",
                                             "venta droga",
                                             "venta ilegal alcohol",
                                             "venta prods pirata"))
               )
             )),
             mainPanel(
               tabsetPanel(
                 tabPanel("Mapa", 
                          plotOutput("map_percepcion_gg", width = "1000px", height = "700px")),
                 tabPanel("Tabla", 
                          dataTableOutput("tab_percepcion"))
               )
             )
    ),   # percepción panel
    
    # PROBLEMAS ----
    tabPanel("Problemas",
             wellPanel(fixedRow(
               column(5,
                      selectInput(inputId = "problemas.input", 
                                  label = "Tema de problema",
                                  selected = "",
                                  choices =c("baches o fugas de agua",
                                             "delincuencia cerca escuelas",
                                             "falta agua",
                                             "falta alumbrado",
                                             "pandillerismo violento",
                                             "robos"))
               )
             )),
             mainPanel(
               tabsetPanel(
                 tabPanel("Mapa", 
                          plotOutput("map_problemas_gg", width = "1000px", height = "700px")),
                 tabPanel("Tabla", 
                          dataTableOutput("tab_problemas"))
               )
             )
    ),   # problema panel
    
    
    # INSEGURIDAD ----
    tabPanel("Percepción Inseguridad",
             mainPanel(
               tabsetPanel(
                 tabPanel("Mapa", 
                          plotOutput("map_percinseg_gg", width = "1000px", height = "700px")),
                 tabPanel("Tabla", 
                          dataTableOutput("tab_percinseg"))
               )
             )
    )   # problema panel
    
  ) # tabset panel
))
