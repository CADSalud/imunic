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
  
  wellPanel(fixedRow(
    column(5,
           selectInput(inputId = "indicador.input", 
                       label = "Indicador",
                       selected = "",
                       choices =c(
                         "percepcion inseguridad" = "percepcion inseguridad",
                         "violacion" = "violacion",
                         "homicidios" = "homicidios",
                         "secuestro" = "secuestro",
                         "robo" = "robo",
                         "vicper_amenazas" = "vicper_amenazas",
                         "vicper_extorsión" = "vicper_extorsión",
                         "vicper_fraude_bancario" = "vicper_fraude_bancario",
                         "vicper_fraude_consumidor" = "vicper_fraude_consumidor",
                         "vicper_hostigamiento_sexual" = "vicper_hostigamiento_sexual",
                         "vicper_lesiones_físicas" = "vicper_lesiones_físicas",
                         "vicper_otros" = "vicper_otros",
                         "desaparicion forzada" = "desaparicion forzada"))
      ),
    column(5, selectInput(inputId = "estado.input", 
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
  
  # Show a plot of the generated distribution
  mainPanel(
     plotOutput("map_gg", width = "1000px", height = "600px")
  )
  
))
