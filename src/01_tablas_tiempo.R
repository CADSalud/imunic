
library(ProjectTemplate)
load.project()



tab_vic_hogar <- envipe_vic2_year %>% 
  dplyr::select(CONTROL, VIV_SEL,
                ID_PER, ID_VIV, ID_HOG, 
                UPM, HOGAR, 
                CVE_ENT, CVE_MUN,
                starts_with("FAC_"),
                year_file, 
                AP6_10_1, AP6_10_2, 
                AP6_15_1, AP6_15_2, 
                AP6_20_1, AP6_20_2) %>% 
  gather(codigo, val, starts_with("AP6_")) %>% 
  separate(codigo, into = c("cod", "delito", "preg"), sep = "_") %>% 
  mutate(preg = factor(preg, 1:2, c("integrante_bin", "cuantos"))) %>% 
  spread(preg, val) %>% 
  mutate(FAC_HOG = parse_number(FAC_HOG),
         cuantos = parse_number(cuantos),
         integrante_bin = parse_number(integrante_bin)) %>% 
  filter(integrante_bin == 1) %>% 
  group_by(year_file, CVE_ENT, CVE_MUN, delito) %>% 
  summarise(hogar_n = sum(integrante_bin), 
            cuantos_mean = median(cuantos), 
            hogar_fach = sum(integrante_bin*FAC_HOG)) %>% 
  ungroup()
tab_vic_hogar

tab_vic_hogar %>% 
  mutate(delito = factor(delito, 
                         c(10, 15, 20), 
                         c("secuestro", "desap_forzada", "homicidio"))) %>% 
  group_by(year_file, CVE_ENT, CVE_MUN, delito) %>% 
  summarise(cnt_vichgrp = sum(hogar_fach)*mean(cuantos_mean)) %>% 
  ungroup() %>% 
  mutate(CVE_ENT = parse_number(CVE_ENT), 
         CVE_MUN = parse_number(CVE_MUN)) %>% 
  rename(state_code = CVE_ENT, 
         mun_code = CVE_MUN) %>% 
  mutate(delito = paste0("vichgr_", delito)) %>% 
  spread(delito, cnt_vichgrp)
