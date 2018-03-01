
# desaparición forzada

library(tidyverse)
library(lubridate)

desfor_comun <- read.csv("data/secretariado_rnped/rnped_comun.csv") %>% 
  as_tibble() %>% 
  mutate(fuerocomun_desapfecha_fmt = dmy(fuerocomun_desapfecha), 
         date = ymd(date))

names(desfor_comun) <- str_replace(names(desfor_comun), "fuerocomun_", "")
desfor_comun %>% data.frame() %>% head()

desfor_federal <- read.csv("data/secretariado_rnped/rnped_federal.csv") %>% 
  as_tibble() %>% 
  mutate(fuerofederal_ultimafecha_fmt = dmy(fuerofederal_ultimafecha))

names(desfor_federal) <- str_replace(names(desfor_federal), "fuerofederal_", "")
desfor_federal %>% data.frame() %>% head()

cache("desfor_comun")
cache("desfor_federal")
