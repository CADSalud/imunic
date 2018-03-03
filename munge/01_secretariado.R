

# datos del secretariado
library(tidyverse)
library(lubridate)

# I. Incidencia delictiva vieja metodología (hasta dic 2017) ----

# 1. Fuero comun: ----
# a) dv 
# 10,219,968
fcomun_mun <- read_csv("data/vieja_metodologia/fuero-comun-municipios.csv") %>% 
  mutate(date = ymd(paste0(date, "-01")) )

m# 2. Fuero federal: ----

# # b) secretariado
# incdel_ffed <- readxl::read_excel("data/secretariado_estatal/Datos_abiertos_Incidencia_delictiva_Fuero_federal.xls")
# incdel_ffed %>%
#   rename(year = ANO) %>%
#   gather(month, count, ENERO:DICIEMBRE) %>%
#   mutate(month_num = factor(str_sub(month, 1, 3),
#                             c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN",
#                               "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")) %>%
#            as.numeric,
#          date = dmy(paste("01", month_num, year, sep = "-")) ) %>%
#   group_by(ENTIDAD, `CLAVE MARCO GEOESTADISTICO NAL`, TIPO, SUBTIPO, date) %>%
#   summarise(count = sum(count, na.rm = T)) %>%
#   ungroup
# 
# incdel_ffed$TIPO %>% unique()


# 3. Victimas estatal: ----

# a) dv 
victimas <- read_csv("data/vieja_metodologia/victimas.csv")
victimas

victimas$modalidad %>% unique()


# II. Incidencia delictiva nueva metodología (desde 2017) ----

# 1. Fuero comun: ----
# a) dv 
# 6,812,568
fcomun_mun_nm <- read_csv("data/nueva_metodologia/nm-fuero-comun-municipios.csv") %>% 
  mutate(date = ymd(paste0(date, "-01")) )


fcomun_mun_nm$bien_juridico %>% unique()
fcomun_mun_nm$tipo %>% unique()





# State ----


fcomun_mun %>% 
  dplyr::select(state_code:municipio) %>% 
  unique()

fcomun_mun %>% 
  dplyr::select(state_code, state) %>% 
  unique %>% 
  unite(estado, state_code, state, sep = " ") %>% 
  write.csv(row.names = F)



fcomun_mun$date %>% summary()
year.num <- 2011
fcomun_mun %>% 
  filter(year(date) == year.num) %>% dim
  # write_csv(paste0("data/vm_fuero-comun-municipios",year.num,".csv"))

# 2011: 1,092,960
# 2012: 1,117,512
# 2013: 1,117,512
# 2014: 1,500,048
# 2015: 1,500,048
# 2016: 1,945,944
# 2017: 1,945,944


fcomun_mun_nm$date %>% summary()
year.num <- 2017
fcomun_mun_nm %>% 
  filter(year(date) == year.num) %>% dim
  # write_csv(paste0("data/nm_fuero-comun-municipios",year.num,".csv"))


# 2015: 2,270,856
# 2016: 2,270,856
# 2017: 2,270,856

library(bigrquery)

query_exec("SELECT count(state_code) FROM [imunic-196018:secretariado.nm_fuerocomun_municipios2017]", 
           project = "imunic-196018")

query_exec("SELECT * FROM [imunic-196018:secretariado.nm_fuerocomun_municipios2017] LIMIT 3",
           project = "imunic-196018")




# tab_pob <- fcomun_mun %>% 
#   mutate(year = year(date)) %>% 
#   filter(month(date) == 12) %>% 
#   group_by(state_code, mun_code, 
#            state, municipio, year) %>% 
#   summarise(poblacion = unique(population))
# 
# tab_pob %>% 
#   write.csv("../../cursos_r/intro_tidyverse/data/poblacion_municipal.csv", row.names = F)

