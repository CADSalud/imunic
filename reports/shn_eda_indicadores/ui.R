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
                        choices = c("0 México" = "all", 
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
  
  tabsetPanel(
    tabPanel("Indicador", 
             wellPanel(fixedRow(
               column(5,
                      selectInput(inputId = "indicador.input", 
                                  label = "Indicador",
                                  selected = "",
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
                                    "otros" = "otros"))
               )
             )),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("map_inds_gg", width = "1000px", height = "700px")
             )
    ),  # indicador panel
    
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
               plotOutput("map_importancia_gg", width = "1000px", height = "700px")
             )
    ),   # importancia panel
    
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
               plotOutput("map_percepcion_gg", width = "1000px", height = "700px")
             )
    ),   # percepción panel
    
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
               plotOutput("map_problemas_gg", width = "1000px", height = "700px")
             )
    ),   # problema panel
    
    tabPanel("Percepción Inseguridad",
             mainPanel(
               plotOutput("map_percinseg_gg", width = "1000px", height = "700px")
             )
    )   # problema panel
    
  ) # tabset panel
))
