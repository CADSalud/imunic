
library(ProjectTemplate)
reload.project()


library(knitr)
library(rmarkdown)



# Funcion generadora de reportes
pdf_report_fun <- function(ent_code, munc_code){
  load("cache/tab_cods_estmun.RData")
  tab_cods_estmun
  
  tab <- tab_cods_estmun %>% 
    filter(mun_code == munc_code) %>% 
    filter(state_code == ent_code)
  
  # Crear vector de entidad y municipio 
  sink(file = "reports/src_mun/vector_municipio.R")
  cat("municipios_selec_vec <- c(", paste0("'", tab$NOM_MUN, "'"),
      ",", tab$state_code, ")")
  sink()
  
  # Crear RMD 
  knit(input = "reports/src_mun/ficha_intercenvipe.Rmd") 
  render(input = paste0("reports/src_mun/ficha_intercenvipe.Rmd"), 
         output_format = "pdf_document")
  
  # Mover pdf con nombre correcto
  command_cp <- paste0("cp reports/src_mun/ficha_intercenvipe.pdf reports/src_mun/pdfs/",
                       "ficha_intercenvipe_", ent_code, "-", tolower(munc_code), ".pdf")
  print(command_cp)
  system(command = command_cp)
  "fin"
}

# pdf_report_fun(ent_code = 15, munc_code = 74)
# pdf_report_fun(ent_code = 9, munc_code = 10)
# pdf_report_fun(ent_code = 9, munc_code = 4)



# Excel de gira mañana --- 
read_csv('docs/homologadora_edomex_muns.csv') %>% 
  filter(Fecha == "Miercoles 11 de Abril") %>%
  # filter(Fecha == "Martes 10 de Abril") %>% 
  rowwise() %>%
  do(pdf_report_fun(ent_code = .$state_code, munc_code = .$mun_code))

tab_cods_estmun %>% 
  filter(state_code == 15) %>% 
  filter(str_detect(NOM_MUN, "Tec"))
