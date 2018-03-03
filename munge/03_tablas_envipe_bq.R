
library(tidyverse)
library(bigrquery)

query_exec("SELECT * FROM [imunic-196018:envipe.tper_vic2_2016] LIMIT 5", 
           project = "imunic-196018")

# Variables de victimas persona por año