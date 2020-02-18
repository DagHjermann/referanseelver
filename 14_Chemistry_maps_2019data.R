# ---
# title: "Referanseelver - kart kjemiske parametre"
# author: "DHJ"
# date: "22 februar 2019"
# output: html_document
# ---

# For report: Based on excel file given by �yvind Garmo  ("Middelverdier 2018.xlsx")
# 
# 1)	Figur med to kartpaneler som viser regional fordeling for TotP og TotN (absoluttverdier)
# 2)	Figur med tre kartpaneler som viser regional fordeling for pH, ANC og LAl (absoluttverdier)
# 3)	Figur med fire kartpaneler som viser regional fordeling av vannregionspesifikke stoffer (absoluttverdier)
# 4)	Figur med fire kartpaneler som viser regional fordeling av vannregionspesifikke stoffer (tilstandsklasser)
# 5)	Figur med fire kartpaneler som viser regional fordeling av prioriterte stoffer (absoluttverdier)
# 6)	Figur med fire kartpaneler som viser regional fordeling av prioritrte stoffer stoffer (tilstandsklasser)
#    
# For kartene med tilstandsklasser kan du godt bruke «vanndirektivfargene» blått, grønt, gult, oransje og rødt for hhv. kl I, II, III, IV og V.

# Options and libraries ----

library(dplyr)
library(ggplot2)
library(readxl)
library(viridis)
library(cowplot)     # plot_grid (https://cran.r-project.org/web/packages/cowplot/vignettes/plot_grid.html)

## Plot functions ----
source("04_Chemistry_maps_functions.R")

factor2char <- function(var) { levels(var)[var] }


#
# Data ----
#
#
# 2019 data
#
df_chem <- readxl::read_excel("Input_2019data/Referanseelver yearly means for maps.xlsx", sheet = 1) 

# Remove "-avg" in variable names
names(df_chem) <- sub("-avg", "", names(df_chem))


# The hard way
if (FALSE){
  df_chem <- readxl::read_excel("Input_2019data/Vannkjemi_Resultater 2019.xlsx", sheet = "Ordnet") 
  df_stationmeta <- read_excel("Input_2019data/Stations and catchment data all rivers.xlsx", sheet = 1) 
  df_stationmeta$Lengdegrad <- as.numeric(df_stationmeta$Long)
  df_stationmeta$Breddegrad <- as.numeric(df_stationmeta$Lat)
  df_chem <- df_chem %>% 
    left_join(df_stationmeta %>% select(Kode, Lengdegrad, Breddegrad), by = c("StationCode" = "Kode"))
}



names(df_chem)


# P and N ----


summ_variable("TotP")
summ_variable("TotN")

gg1 <- plot_map_abs("TotP", c(1, 2, 5, 10, 20, 50))
gg2 <- plot_map_abs("TotN", c(20, 50, 100, 200, 500, 1000))
gg_comb <- plot_grid(gg1, gg2, labels = c('(a)', '(b)'))
gg_comb

ggsave("Figures_2019data/14_01_P_and_N_abs.png", gg_comb, width = 10, height = 5, dpi = 500)


# Vannregionspesifikke stoffer ----


summ_variable("Cu")
summ_variable("Cr")
summ_variable("Zn")
summ_variable("As")
gg1 <- plot_map_abs("Cu", c(0.05, 0.1, 0.2, 0.5, 1, 2, 5, 7.92))
gg2 <- plot_map_abs("Cr", c(0.01, 0.03, 0.1, 0.3, 1, 3.49))
gg3 <- plot_map_abs("Zn", c(0.01, 0.03, 0.1, 0.3, 1, 3, 10, 15.95))
gg4 <- plot_map_abs("As", c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 1.56))
gg_comb <- plot_grid(gg1, gg2, gg3, gg4, nrow = 2, labels = c('(a) ', '(b) ', '(c) ', '(d) '))
gg_comb

ggsave("Figures_2019data/14_02_vannregionspesifikke_absolutt.png", 
       gg_comb, width = 10, height = 10, dpi = 500)

# Reverse color scale
gg1 <- plot_map_abs("Cu", c(0.05, 0.1, 0.2, 0.5, 1, 2, 5, 7.92), direction = -1)
gg2 <- plot_map_abs("Cr", c(0.01, 0.03, 0.1, 0.3, 1, 3.49), direction = -1)
gg3 <- plot_map_abs("Zn", c(0.01, 0.03, 0.1, 0.3, 1, 3, 10, 15.95), direction = -1)
gg4 <- plot_map_abs("As", c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 1.56), direction = -1)
gg_comb <- plot_grid(gg1, gg2, gg3, gg4, nrow = 2, labels = c('(a) ', '(b) ', '(c) ', '(d) '))
ggsave("Figures_2019data/14_02_vannregionspesifikke_absolutt_rev.png", 
       gg_comb, width = 10, height = 10, dpi = 500)


#
# Prioriterte stoffer ----
#

summ_variable("Cd")
summ_variable("Pb")
summ_variable("Ni")
summ_variable("Hg")
gg1 <- plot_map_abs("Cd", c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.13))
gg2 <- plot_map_abs("Pb", c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 4.08))
gg3 <- plot_map_abs("Ni", c(0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 8.47))
gg4 <- plot_map_abs("Hg", c(0.5, 0.7, 1, 2, 5.25))
gg_comb <- plot_grid(gg1, gg2, gg3, gg4, nrow = 2, labels = c('(a) ', '(b) ', '(c) ', '(d) '))
gg_comb

ggsave("Figures_2019data/14_03_Prioriterte_absolutt.png", gg_comb, width = 10, height = 10, dpi = 500)
# gg_comb

# Reverse color scale
gg1 <- plot_map_abs("Cd", c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.13), direction = -1)
gg2 <- plot_map_abs("Pb", c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 4.08), direction = -1)
gg3 <- plot_map_abs("Ni", c(0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 8.47), direction = -1)
gg4 <- plot_map_abs("Hg", c(0.5, 0.7, 1, 2, 5.25), direction = -1)
gg_comb <- plot_grid(gg1, gg2, gg3, gg4, nrow = 2, labels = c('(a) ', '(b) ', '(c) ', '(d) '))
ggsave("Figures_2019data/14_03_Prioriterte_absolutt_rev.png", gg_comb, width = 10, height = 10, dpi = 500)


# pH, ANC, labile Al ----

#
# 2018 only
#

df_chem <- df_chem %>%
  rename(`Labilt\naluminium` = lAl)

# colnames(df_chem)
summ_variable("pH")
summ_variable("ANC")
summ_variable("Labilt\naluminium")
gg1 <- plot_map_abs("pH", c(5.21, 6.5, 7, 7.5, 8.06), log = FALSE, direction = -1, high_on_top = FALSE)
gg2 <- plot_map_abs("ANC", c(20.9, 50, 100, 200, 500, 1000, 2513), direction = -1, high_on_top = FALSE)
gg3 <- plot_map_abs("Labilt\naluminium", c(6, 10, 20, 72), direction = 1)
gg_comb <- plot_grid(gg1, gg2, gg3, nrow = 2, labels = c('(a) ', '(b) ', '(c) '))
gg_comb

ggsave("Figures_2019data/14_05_pH_og_Alu.png", gg_comb, width = 10, height = 10, dpi = 500)
# gg_comb

# Reverse scale
gg1 <- plot_map_abs("pH", c(5.21, 6.5, 7, 7.5, 8.06), log = FALSE, direction = 1, high_on_top = FALSE)
gg2 <- plot_map_abs("ANC", c(20.9, 50, 100, 200, 500, 1000, 2513), direction = 1, high_on_top = FALSE)
gg3 <- plot_map_abs("Labilt\naluminium", c(6, 10, 20, 72), direction = -1)
gg_comb <- plot_grid(gg1, gg2, gg3, nrow = 2, labels = c('(a) ', '(b) ', '(c) '))
ggsave("Figures_2019data/14_05_pH_og_Alu_rev.png", gg_comb, width = 10, height = 10, dpi = 500)

