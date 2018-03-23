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
  
  # Application title
  br(),
  div(style="text-align:center",
      img(src="logo-CAD.png", width="100") ),
  br(),
  
  
  titlePanel("Indicadores por Municipio"),
  h3("Visualización nacional."),
  
  br(),
  h4("Selecciona un el nivel de visualización."),
  
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
             h3("Indicadores de seguridad, educación, población, entre otros."),
             wellPanel(fixedRow(
               column(5,
                      selectInput(inputId = "indicador.input", 
                                  label = "Selecciona un indicador",
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
             h6("Fuente: Encuesta Nacional de Victimización y Percepción sobre Seguridad Pública 2014 a 2017."),
             h6("Fuente: Encuesta Intercensal 2015."),
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
             h3("¿Cuáles son los tres temas que le preocupan más?"),
             h4("Proporción de personas entre los encuestados que mencionó el tema entre los tres que más le preocupan."),
             
             wellPanel(fixedRow(
               column(5,
                      selectInput(inputId = "importancia.input", 
                                  label = "Selecciona un tema de importancia",
                                  selected = "",
                                  choices =c(
                                    "Pobreza" = "Pobreza",
                                    "Desempleo" = "Desempleo",
                                    "Narcotráfico" = "Narco",
                                    "Aumento de precios" = "Precios",
                                    "Corrupción" = "Corrupción",
                                    "Inseguridad" = "Inseguridad",
                                    "Desastres naturales" = "Desastres",
                                    "Escasez de agua" = "Agua",
                                    "Educación" = "Educación",
                                    "Impunidad" = "Impunidad",
                                    "Salud" = "Salud",
                                    "Otro" = "Otro",
                                    "No sabe" = "No sabe"
                                    ))
               )
             )),
             h6("Fuente: Encuesta Nacional de Victimización y Percepción sobre Seguridad Pública 2014 a 2017."),
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
             h3("¿Sabe usted o ha escuchado si en los alrededores de su vivienda suceden las siguientes situaciones?"),
             h4("Proporción de personas entre los encuestados que mencionó saber que suceden las situaciones"),
             
             
             wellPanel(fixedRow(
               column(5,
                      selectInput(inputId = "percepcion.input", 
                                  label = "Selecciona una situación",
                                  selected = "",
                                  choices =c("consume alcohol en la calle" = "alcohol en la calle",
                                             "existe pandillerismo" = "pandillerismo",
                                             "hay riñas entre vecinos" = "riñas entre vecinos",
                                             "existe venta ilegal de alcohol" = "venta ilegal alcohol",
                                             "se venden prods. pirata" = "venta prods pirata",
                                             "hay invasión de predios" = "invasión de predios",
                                             "se consume drogas" = "consumo drogas",
                                             "existen robos/asaltos frecuentes" = "robos asaltos",
                                             "se vende droga" = "venta droga",
                                             "ha habido disparos frecuentes" = "disparos",
                                             "hay prostitución" = "prostitución",
                                             "ha habido secuestros" = "secuestros",
                                             "ha habido homicidios" = "homicidios",
                                             "ha habido extorsiones/cobro de piso" = "extorsiones",
                                             "policia vs ciudadanos" = "policia vs ciudadanos",
                                             "ninguna" = "ninguna",
                                             "no sabe" = "no sabe"
                                             ))
               )
             )),
             h6("Fuente: Encuesta Nacional de Victimización y Percepción sobre Seguridad Pública 2014 a 2017."),
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
             h3("¿En su colonia/municipio han tenido los siguientes problemas?"),
             h4("Proporción de personas entre los encuestados que mencionó que han tenido el problema en la colonia."),
             
             
             wellPanel(fixedRow(
               column(5,
                      selectInput(inputId = "problemas.input", 
                                  label = "Selecciona un problema",
                                  selected = "",
                                  choices =c("baches o fugas de agua",
                                             "delincuencia cerca escuelas",
                                             "falta agua",
                                             "falta alumbrado",
                                             "pandillerismo violento",
                                             "robos"))
               )
             )),
             h6("Fuente: Encuesta Nacional de Victimización y Percepción sobre Seguridad Pública 2014 a 2017."),
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
             h3("En términos de delincuencia, ¿considera que vivir en el municipio es inseguro?"),
             h4("Proporción de personas entre los encuestados que consideran inseguro vivier en el municipio."),
             h6("Fuente: Encuesta Nacional de Victimización y Percepción sobre Seguridad Pública 2014 a 2017."),
             # ¿En términos de delincuencia, considera que vivir en
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
  
  # 
  # # footer----
  # br(),
  # HTML('<p style="text-align:center">
  #      <b> Creado por </b>
  #      <br>
  #      <img src="logo-CAD.png", width="90", height="35">
  #      </p>')
  
)
)
