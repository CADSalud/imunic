


library(ProjectTemplate)
load.project()

library(sp)
library(spdep)
library(gstat)
library(tmap)
library(FactoMineR)


# Datos ----
load("cache/tab_cods_estmun.RData")
tab_cods_estmun %>% head

load("cache/tab_mun.RData")
tab_mun %>% dim
tab_mun %>% summary()

load("cache/imp_shp_smo.RData")
imp_shp_smo %>% class
imp_shp_smo@data %>% head


imp_shp_smo@data %>% names

# variables
vars <- c("im_perc_robos", "im_perc_pandill", "im_perc_secuest",
  "im_perc_homicid", "im_perc_prostit", "im_perc_pirater",
  "im_perc_disparo", "im_perc_extors", "im_perc_invaspr",
  "im_perc_pocciud", "im_perc_ventdrog", 
  "im_prob_robos", "im_prob_delinc", "im_prob_alumbr", 
  "im_prob_pandill",
  "im_vict_robo", "im_vict_agrfis", 
  "im_vict_secuest")

#¿Sabe usted o ha escuchado si en los alrededores de su vivienda suceden o se dan las siguientes situaciones?
# ¿En su (COLONIA/LOCALIDAD) han tenido problemas de...?
vars_names <- c("Percepción robos frecuentes", "Percepción pandillas violentas", 
                "Percepción secuestros", "Percepción homicidios", 
                "Percepción prostitución", "Percepción venta piratería",
                "Percepción disparos frecuentes", "Percepción extorsiones", 
                "Percepción invasión predios",
                "Percepción violencia policia contra ciudadanos",
                "Percepción venta droga", 
                "Problema robos", "Problema delincuencia cerca escuelas",
                "Problema falta alumbrado", "Problema pandillas violentas", 
                "Víctimas robos", "Víctimas agresión física", 
                "Víctimas secuestros")

length(vars)
length(vars_names)

cbind(vars_names, vars)


# graficas ----

imp_shp_eda <- imp_shp_smo %>% 
  merge(tab_cods_estmun)
imp_shp_eda$NOM_ENT

Tmap_Comp_fun <- function(var_nom){
  # var_nom <- vars[2]
  print(var_nom)
  
  vars_selec <- c(var_nom, 
                  str_replace(var_nom, "im_", "imp_im_"), 
                  str_replace(var_nom, "im_", "kg_im_"), 
                  str_replace(var_nom, "im_", "smo_im_"))
  vars_name_selec <- vars_names[which(vars == var_nom)]
  panel_labels_name <- c("Observado", "Imputación", 
                    "Interpolación", "Suavizamiento")
  imp_shp_eda@data[, vars_selec] %>% head
  
  
  # Mapa
  tm_l <- lapply(1:4, function(i){
    tm <- tm_shape(imp_shp_eda) +
      tm_fill(col = vars_selec[i],
              style = "quantile",
              title = vars_name_selec) + 
      tm_layout(panel.show = TRUE,
                panel.labels = panel_labels_name[i],
                legend.outside = F)
    tm
  })
  
  file_nom <- paste0("graphs/eda/oiis_", 
                    str_replace(var_nom, "im_", ""),
                    ".png")
  png(filename = file_nom, width = 1200, height = 900)
  tmap_arrange(tm_l[[1]], tm_l[[2]], 
               tm_l[[3]], tm_l[[4]], 
               ncol = 2, asp = NA)
  dev.off()
  
  # Dispersión 
  gg1 <- imp_shp_eda@data[!is.na(imp_shp_eda@data[,vars_selec[1]]), ] %>% 
    ggplot(aes_string(x = vars_selec[1], y = vars_selec[4])) + 
    geom_point(alpha = .5, 
               aes(color = CVE_ENT)) + 
    geom_text(aes(label = abbreviate(NOM_ENT)), alpha = .5, 
              nudge_x = .2, check_overlap = T) + 
    xlab("Observado") + 
    ylab("Suavizamiento") + 
    theme(legend.position = "none") + 
    ggtitle(vars_name_selec, 
            "Observado vs Suavizamiento")

  gg2 <- imp_shp_eda@data %>% 
    ggplot(aes_string(x = vars_selec[3], y = vars_selec[4])) + 
    geom_point(alpha = .5, 
               aes(color = CVE_ENT)) + 
    geom_text(aes(label = abbreviate(NOM_ENT)), alpha = .5, 
              nudge_x = .2, check_overlap = T) + 
    xlab("Interpolación") + 
    ylab("Suavizamiento") + 
    theme(legend.position = "none") + 
    ggtitle(vars_name_selec,
            "Interpolación vs Suavizamiento")
  file_nom <- paste0("graphs/eda/scat_", 
                     str_replace(var_nom, "im_", ""),
                     ".png")
  png(filename = file_nom, width = 800, height = 400)
  gridExtra::grid.arrange(gg1, gg2, nrow = 1)
  dev.off()
  
  "fin"
}

# Tmap_Comp_fun(vars[2])
map(vars, Tmap_Comp_fun)



# Datos municipios
df_mun_smo <- imp_shp_eda@data %>% 
  as_tibble() %>% 
  mutate(var_code = paste(state_code, mun_code, 
                          abbreviate(NOM_ENT), abbreviate(NOMGEO), 
                          sep = "_")) %>% 
  dplyr::select(state_code, mun_code, NOM_ENT, NOMGEO,
                var_code, starts_with("kg_"))
df_mun_smo


tab <- df_mun_smo %>% 
  dplyr::select(var_code, 
                starts_with("kg_im_perc"),
                starts_with("kg_im_prob"), 
                -kg_im_prob_alumbr) %>% 
  data.frame()
rownames(tab) <- df_mun_smo$var_code
tab %>% head


# PCA
pca_fg <- PCA(tab, graph = F)
summary(pca_fg)

pca_fg$var$coord %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  arrange(Dim.1)

pca_fg$ind$coord %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  arrange(Dim.1)

hcl_muns <- hclust(dist(pca_fg$ind$coord[, 1:2]), method = "ward.D")
# plot(hcl_muns, cex = .2)

df_mun_smo$gpos_10 <- paste("gpo", cutree(hcl_muns, k = 10))
df_mun_smo$gpos_5 <- paste("gpo", cutree(hcl_muns, k = 5))

df_mun_smo$gpos_cut <- cut_number(pca_fg$ind$coord[, 2], n = 6) %>% 
  as.numeric() %>% 
  paste("gpo", .)


tab_gpos <- df_mun_smo %>% 
  dplyr::select(starts_with("kg_im_perc"),
                starts_with("kg_im_prob"),
                -kg_im_prob_alumbr,
                starts_with("gpos_"),
                var_code) %>% 
  gather(var, val, -c(var_code, gpos_10, gpos_5)) %>% 
  group_by(gpos_5, var) %>% 
  dplyr::summarise(prom = mean(val)) %>% 
  spread(gpos_5, prom) %>% 
  data.frame()
rownames(tab_gpos) <- tab_gpos$var
tab_gpos <- tab_gpos[, -1]
CA(tab_gpos)


tm <- imp_shp_eda %>% 
  merge(df_mun_smo %>% 
          dplyr::select(state_code, mun_code, 
                        starts_with("gpos_"))) %>% 
  tm_shape() +
  tm_fill(col = 'gpos_cut', 
          palette = viridis_pal()(6))
save_tmap(tm, "graphs/gpos_cuantiles.png")


# princomp
pca_pr <- princomp(tab)
summary(pca_pr)

pca_pr$loadings[, 1:3] %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() 

pca_pr$scores[, 1:3] %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  arrange(Comp.1)

df_scores <- pca_pr$scores[, 1:3] %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble()

hcl_muns <- hclust(dist(pca_pr$scores[, 1:2]), method = "ward.D")
# plot(hcl_muns, cex = .2)

df_mun_smo$gpos_10 <- cutree(hcl_muns, k = 10)
df_mun_smo$gpos_5 <- cutree(hcl_muns, k = 5)

tab_gpos <- df_mun_smo %>% 
  dplyr::select(starts_with("kg_im_perc"),
                starts_with("kg_im_prob"),
                starts_with("gpos_"),
                var_code) %>% 
  gather(var, val, -c(var_code, gpos_10, gpos_5)) %>% 
  group_by(gpos_5, var) %>% 
  dplyr::summarise(prom = mean(val)) %>% 
  spread(gpos_5, prom) %>% 
  data.frame()
rownames(tab_gpos) <- tab_gpos$var
tab_gpos <- tab_gpos[, -1]
CA(tab_gpos)


