
library(ProjectTemplate)
load.project()

load("cache/df_envipe_pervic1.RData")
load("cache/df_envipe_pervic2.RData")
load("cache/fcomun_mun.RData")

tab_vic_hogar <- df_envipe_pervic2 %>% 
  dplyr::select(ID_VIV:HOGAR, 
                CVE_ENT:CVE_MUN, 
                AP6_10_1, AP6_10_2,
                AP6_15_1, AP6_15_2,
                AP6_20_1, AP6_20_2,
                starts_with("FAC_")) %>% 
  gather(codigo, val, starts_with("AP6_")) %>% 
  separate(codigo, into = c("cod", "delito", "preg"), sep = "_") %>% 
  mutate(preg = factor(preg, 1:2, c("integrante_bin", "cuantos"))) %>% 
  spread(preg, val) %>% 
  mutate(FAC_HOG = parse_number(FAC_HOG),
         cuantos = parse_number(cuantos),
         integrante_bin = parse_number(integrante_bin)) %>% 
  filter(integrante_bin == 1) %>% 
  group_by(CVE_ENT, NOM_ENT, CVE_MUN, delito) %>% 
  summarise(hogar_n = sum(integrante_bin), 
            cuantos_mean = median(cuantos), 
            hogar_fach = sum(integrante_bin*FAC_HOG)) %>% 
  ungroup()
tab_vic_hogar

tab_vic_per <- df_envipe_pervic2 %>% 
  dplyr::select(CVE_ENT:CVE_MUN, 
                starts_with("AP7_3_"),
                starts_with("FAC_ELE")) %>% 
  gather(delito, value, starts_with("AP7_3_")) %>% 
  mutate(factor = parse_number(FAC_ELE)) %>% 
  group_by(CVE_ENT, NOM_ENT, CVE_MUN, delito, value) %>% 
  summarise(n = n(),
            nfac = sum(factor, na.rm = T)) %>% 
  ungroup()
tab_vic_per


tab_incdel <- fcomun_mun %>% 
  filter(year(date) == 2016) %>% 
  filter(modalidad %in% c("HOMICIDIOS", 
                          "ROBO COMUN", 
                          "DELITOS SEXUALES (VIOLACION)",
                          "PRIV. DE LA LIBERTAD (SECUESTRO)")) %>% 
  group_by(state_code, state, mun_code, modalidad, tipo) %>% 
  summarise(count = sum(count, na.rm = F), 
            pop = max(population, na.rm = F)) %>% 
  ungroup()
tab_incdel

tab_incdel %>% 
  dplyr::select(modalidad, tipo) %>% 
  unique()

tab_incdel %>% 
  dplyr::select(state_code, state) %>% 
  unique()

tab_vic_hogar %>% 
  dplyr::select(CVE_ENT, NOM_ENT) %>% 
  unique() %>%
  mutate(CVE_ENT = parse_number(CVE_ENT)) %>% 
  left_join(tab_incdel %>% 
              mutate(CVE_ENT = parse_number(state_code)) %>% 
              dplyr::select(CVE_ENT, state_code, state) %>% 
              unique()) %>% 
  data.frame()




# Municipal ----

# percepción de municipio ----
tabj_perc <- df_envipe_pervic1 %>% 
  dplyr::select(ID_VIV:HOGAR, 
                CVE_ENT:CVE_MUN, 
                AP4_3_2, 
                starts_with("FAC_")) %>% 
  group_by(CVE_ENT, CVE_MUN, AP4_3_2) %>% 
  # 1 seguro, 2 inseguro, 9 no sabe
  summarise(nfac = sum(parse_number(FAC_ELE))) %>% 
  group_by(CVE_ENT, CVE_MUN) %>%
  mutate(pop_envp = sum(nfac), 
         prop_seguridad = nfac/pop_envp) %>% 
  ungroup() %>% 
  filter(AP4_3_2 == 2) %>% 
  mutate(CVE_ENT = parse_number(CVE_ENT), 
         CVE_MUN = parse_number(CVE_MUN)) %>% 
  rename(state_code = CVE_ENT, 
         mun_code = CVE_MUN) %>% 
  dplyr::select(state_code, mun_code, prop_seguridad)
tabj_perc

# incidencia delitiva secretariado: ----
tabj_scrt <- tab_incdel %>% 
  filter(tipo != "CULPOSOS") %>% 
  unite(mod_tipo, c('modalidad', 'tipo'), sep = " : ") %>% 
  mutate(mod_tipo = fct_recode(mod_tipo, 
                               `ROBO COMUN : C/S VIOLENCIA` = 'ROBO COMUN : CON VIOLENCIA',
                               `ROBO COMUN : C/S VIOLENCIA` = 'ROBO COMUN : SIN VIOLENCIA')) %>%
  group_by(state_code, mun_code, mod_tipo) %>% 
  summarise(cnt_scrt = sum(count)) %>% 
  ungroup %>% 
  mutate(mod_tipo = paste0("scrt_", tolower(mod_tipo))) %>% 
  spread(mod_tipo, cnt_scrt)
tabj_scrt


# delitos hogares envipe: ----
tabj_nvphgr <- tab_vic_hogar %>% 
  mutate(delito = factor(delito, 
                         c(10, 15, 20), 
                         c("secuestro", "desap_forzada", "homicidio"))) %>% 
  group_by(CVE_ENT, CVE_MUN, delito) %>% 
  summarise(cnt_vichgrp = sum(hogar_fach)*mean(cuantos_mean)) %>% 
  ungroup() %>% 
  mutate(CVE_ENT = parse_number(CVE_ENT), 
         CVE_MUN = parse_number(CVE_MUN)) %>% 
  rename(state_code = CVE_ENT, 
         mun_code = CVE_MUN) %>% 
  mutate(delito = paste0("vichgr_", delito)) %>% 
  spread(delito, cnt_vichgrp)
tabj_nvphgr


# delitos hogares persona: ----
tabj_nvppers <- tab_vic_per %>% 
  filter(value == 1) %>% 
  mutate(delito = factor(parse_number(str_sub(delito, -2, -1)), 
                         c(5:15),
                         c("robo_calle_transporte", 
                           "robo_otro", 
                           "fraude_bancario", 
                           "fraude_consumidor", 
                           "extorsión", 
                           "amenazas", 
                           "lesiones_físicas", 
                           "secuestro", 
                           "hostigamiento_sexual", 
                           "violación_sexual", 
                           "otros")) ) %>% 
  group_by(CVE_ENT, CVE_MUN, delito) %>% 
  summarise(cnt_envpper = sum(nfac)) %>% 
  ungroup() %>% 
  mutate(CVE_ENT = parse_number(CVE_ENT), 
         CVE_MUN = parse_number(CVE_MUN)) %>% 
  rename(state_code = CVE_ENT, 
         mun_code = CVE_MUN) %>% 
  mutate(delito = paste0("vicper_", delito)) %>% 
  spread(delito, cnt_envpper)
tabj_nvppers



# Union de tablas

# union de tablas (2,457) ----
tab_delitos_u <- tabj_perc %>% 
  full_join(tabj_scrt, by = c("state_code", "mun_code")) %>% 
  full_join(tabj_nvppers, by = c("state_code", "mun_code")) %>% 
  full_join(tabj_nvphgr, by = c("state_code", "mun_code")) %>% 
  gather(indicador, value, -c(state_code:mun_code))
tab_delitos_u
cache("tab_delitos_u")


tab_delitos_u$indicador %>% unique()
tab_delitos_urec <- tab_delitos_u %>% 
  mutate(gpo_indicador = fct_recode(indicador, 
                          violacion = "scrt_delitos sexuales (violacion) : violacion",
                          violacion = 'vicper_violación_sexual',
                          homicidios = "scrt_homicidios : dolosos",
                          homicidios = "vichgr_homicidio",
                          robo = "vicper_robo_calle_transporte",
                          robo = "vicper_robo_otro",
                          robo = "scrt_robo comun : c/s violencia" ,
                          secuestro = "vicper_secuestro", 
                          secuestro = "vichgr_secuestro", 
                          secuestro = "scrt_priv. de la libertad (secuestro) : secuestro", 
                          `desaparicion forzada` = "vichgr_desap_forzada",
                          `percepcion inseguridad` = 'prop_seguridad'))
cache("tab_delitos_urec")

# # Homicidio municipio ----
# tab1 <- tab_incdel %>% 
#   filter(tipo == "DOLOSOS") %>% 
#   rename(incdel = count)
# 
# tab2 <- tab_vic_hogar %>% 
#   filter(delito == "20") %>% 
#   dplyr::select(CVE_ENT, CVE_MUN, 
#                 hogar_fach, cuantos_mean) %>% 
#   mutate(CVE_MUN = parse_number(CVE_MUN),
#          CVE_ENT = parse_number(CVE_ENT), 
#          incdelh_prom = hogar_fach*cuantos_mean) %>% 
#   rename(state_code = CVE_ENT, 
#          mun_code = CVE_MUN)
# 
# tab1 %>% 
#   full_join(tab2, 
#             by = c("state_code", "mun_code")) %>% 
#   # mutate(incdelh_prom = ifelse(is.na(incdelh_prom), 0, incdelh_prom)) %>% 
#   ggplot(aes(x = incdel, 
#              y = incdelh_prom)) + 
#   geom_point() + 
#   geom_abline(slope = 1, intercept = 0) + 
#   ggrepel::geom_text_repel(aes(label = paste(state, mun_code)),
#                            size = 2) +
#   ylab("envipe") + 
#   xlab("secretariado")
# 
# 
# 
# 
# 
# # Homicidio estatal ----
# tab1 <- tab_incdel %>% 
#   filter(tipo == "DOLOSOS") %>% 
#   rename(incdel = count) %>% 
#   group_by(state_code, state) %>% 
#   summarise(incdel = sum(incdel), 
#             pop = sum(pop)) %>% 
#   ungroup()
# 
# tab2 <- tab_vic_hogar %>% 
#   filter(delito == "20") %>% 
#   dplyr::select(CVE_ENT, CVE_MUN, 
#                 hogar_fach, cuantos_mean) %>% 
#   mutate(CVE_MUN = parse_number(CVE_MUN),
#          CVE_ENT = parse_number(CVE_ENT), 
#          incdelh_prom = hogar_fach*cuantos_mean) %>% 
#   rename(state_code = CVE_ENT, 
#          mun_code = CVE_MUN) %>% 
#   group_by(state_code) %>% 
#   summarise(incdelh_prom = sum(incdelh_prom, na.rm = F), 
#             hogar_fach = sum(hogar_fach, na.rm = F), 
#             cuantos_mean = mean(cuantos_mean)) %>% 
#   ungroup()
# 
# tab1 %>% 
#   full_join(tab2, 
#             by = c("state_code")) %>% 
#   mutate(incdelh_prom = ifelse(is.na(incdelh_prom), 0, incdelh_prom)) %>% 
#   ggplot(aes(x = incdel, y = incdelh_prom)) + 
#   geom_point() + 
#   geom_abline(slope = 1, intercept = 0) + 
#   ggrepel::geom_text_repel(aes(label = state), 
#                            size = 3) + 
#   ylab("envipe\n(personas por hogar)") + 
#   xlab("secretariado") 
# 
# 
# 
# 
# 
# # percepción: -----
# 
# df_envipe_pervic1 %>% 
#   dplyr::select(CVE_ENT:CVE_MUN, 
#                 starts_with("AP4_5_"),
#                 starts_with("FAC_ELE")) %>% 
#   gather(llave, val, starts_with("AP4_5_")) %>%
#   filter(val == 1) %>% 
#   group_by(CVE_ENT, CVE_MUN, llave) %>% 
#   summarise(n = n(),
#             hog = sum(parse_number(val)*parse_number(FAC_ELE))) %>% 
#   filter(llave == "AP4_5_05", 
#          CVE_ENT =="09") %>% 
#   group_by(CVE_ENT) %>% 
#   mutate(sum(hog))
