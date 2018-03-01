

library(tidyverse)
library(foreign)

# 2014 a 2017 (info durante 2013 a 2016)
envipe_vic1_year_l <- lapply(2014:2017, function(year_num){
  path <- paste0("data/envipe/bd_envipe", year_num,
                 "_dbf/TPer_Vic1.dbf")
  print(path)
  read.dbf(file = path) %>% 
    as.tibble() %>% 
    mutate(year_file = year_num, 
           FAC_HOG = parse_number(FAC_HOG), 
           FAC_ELE = parse_number(FAC_ELE),
           FAC_HOG_AM = parse_number(FAC_HOG_AM),
           FAC_ELE_AM = parse_number(FAC_ELE_AM))
  })
names(envipe_vic1_year_l) <- 2014:2017
envipe_vic1_year_l[[1]] %>% head
envipe_vic1_year_l[[1]] %>% dim


envipe_vic2_year_l <- lapply(2014:2017, function(year_num){
  vivienda <- read.dbf(file = paste0("data/envipe/bd_envipe", year_num,
                                  "_dbf/TVivienda.dbf")) %>% 
    as.tibble()
  
  path <- paste0("data/envipe/bd_envipe", year_num,
                 "_dbf/TPer_Vic2.dbf")
  print(path)
  tt <- read.dbf(file = path) %>% 
    as.tibble() 
  
  tt %>% 
    left_join(vivienda) %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate(year_file = year_num, 
           FAC_HOG = parse_number(FAC_HOG), 
           FAC_ELE = parse_number(FAC_ELE),
           FAC_HOG_AM = parse_number(FAC_HOG_AM),
           FAC_ELE_AM = parse_number(FAC_ELE_AM))
})

names(envipe_vic2_year_l) <- 2014:2017
envipe_vic2_year_l[[1]] %>% head
envipe_vic2_year_l[[1]] %>% dim

cache("envipe_vic1_year_l")
cache("envipe_vic2_year_l")


# to csv ----


envipe_vic1_year_l$`2014` %>% 
  mutate_all(as.character) %>% 
  mutate(AREAM = fct_explicit_na(AREAM, "NA")) %>% 
  write_csv("data/tper_vic1_2014.csv")

envipe_vic2_year_l$`2017` %>% 
  mutate_all(as.character) %>% 
  write_csv("data/tper_vic2_2017.csv")
