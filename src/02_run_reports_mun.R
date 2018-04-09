
library(ProjectTemplate)
load.project()


library(knitr)
library(rmarkdown)



# Entidad
entidad <- "Álvaro Obregón"
municipio <- 9

# Crear vector de entidad y municpio
sink(file = "reports/src_mun/vector_municipio.R")
cat("municipios_selec_vec <- c(", paste0("'", entidad, "'"),
    ",", municipio, ")")
sink()

# Crear RMD 
knit(input = "reports/src_mun/ficha_intercenvipe.Rmd")
render(input = "reports/src_mun/ficha_intercenvipe.Rmd", output_format = "pdf_document")


