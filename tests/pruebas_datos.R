

library(ProjectTemplate)
load.project()

# diego valle mortality
library(mxmortalitydb)
mxmortalitydb::injury.intent %>% head


# solo df y homicidios
df_homicides <- read_csv("data/clean-data/cuadrantes-hoyodecrimen.csv", n_max = 10)
df_homicides

df_homicides$crime %>% unique()
df_homicides$zona %>% unique()

