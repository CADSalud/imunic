

# datos de envipe
library(tidyverse)
library(lubridate)
library(foreign)

# 2017 (info durante 2016)
df_envipe_pervic1 <- read.dbf("data/envipe/bd_envipe2017_dbf/TPer_Vic1.dbf") %>% 
  as.tibble()
names(df_envipe_pervic1)

df_envipe_pervic2 <- read.dbf("data/envipe/bd_envipe2017_dbf/TPer_Vic2.dbf") %>% 
  as.tibble()
names(df_envipe_pervic2)

cache("df_envipe_pervic1")
cache("df_envipe_pervic2")
