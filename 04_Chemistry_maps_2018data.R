# ---
# title: "Referanseelver - kart kjemiske parametre"
# author: "DHJ"
# date: "22 februar 2019"
# output: html_document
# ---

# For report: Based on excel file given by Øyvind Garmo  ("Middelverdier 2018.xlsx")
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
# 2018 data
#
df_chem <- readxl::read_excel("Input_2018data/Middelverdier 2018.xlsx") 

df_stationmeta <- read_excel("Input_2018data/Stasjonsoversikt 2018 l?pende oppdatert_.xlsx", sheet = 1) %>% 
  rename(STATION_CODE = `Aquamonitor stasjonskode`,
         Rapportnavn = `Kortnavn/Rapportnavn`)
df_stationmeta$Lengdegrad <- as.numeric(df_stationmeta$Lengdegrad)
df_stationmeta$Breddegrad <- as.numeric(df_stationmeta$Breddegrad)

#
# 2017 data
#
df1 <- read_excel("../../Referanseelver_2018/Input_data/Kart til Dag.xlsx", sheet = 1) # TotP og TotN
df2 <- read_excel("../../Referanseelver_2018/Input_data/Kart til Dag.xlsx", sheet = 2) # pH, ANC og LAl 
df3 <- read_excel("../../Referanseelver_2018/Input_data/Kart til Dag.xlsx", sheet = 3) # vannregionspesifikke stoffer (Cu et al)
#df4 <- read_excel("../../Referanseelver_2018/Input_data/Kart til Dag.xlsx", sheet = 4)
df5 <- read_excel("../../Referanseelver_2018/Input_data/Kart til Dag.xlsx", sheet = 5) # prioriterte stoffer (Hg et al
#df6 <- read_excel("../../Referanseelver_2018/Input_data/Kart til Dag.xlsx", sheet = 6)


# df_colors <- read_excel("../Input_data/Fargekoder RGB for tilstandsklassifisering.xlsx", sheet = 2)
# class_colors <- with(df_colors, rgb(R/255, G/255, B/255))

map_norway <- map_data("world", "Norway")
# Test colors:
# pie(rep(1,6), col = class_colors)


# Add coordinates to data
df_chem <- df_chem %>% 
  left_join(df_stationmeta %>% select(Rapportnavn, Lengdegrad, Breddegrad))


# P and N ----

#
# 2018 only
#
summ_variable("TOTP-?g/l")
summ_variable("TOTN-?g/l")
gg1 <- plot_map_abs("TOTP-?g/l", c(2, 5, 10, 20, 30, 50, 104))
gg2 <- plot_map_abs("TOTN-?g/l", c(50, 100, 200, 300, 600, 1217))
gg_comb <- plot_grid(gg1, gg2, labels = c('(a)', '(b)'))
ggsave("Figures_2018data/04_01_P_and_N_abs.png", gg_comb, width = 10, height = 5, dpi = 500)

#
# 2017 (df1) + 2018
#

# Combine data sets
pars <- c("Rapportnavn", "Lengdegrad", "Breddegrad", "TOTP-?g/l", "TOTN-?g/l")
df_comb1 <- df_chem[,pars] %>% mutate(Year = 2018)
df_comb2 <- df1 %>%
  rename("TOTP-?g/l" = "TOTP (?g/l)",
         "TOTN-?g/l" = "TOTN (?g/l)", 
         Breddegrad = Latitude, Lengdegrad = Longitude)
df_comb2 <- df_comb2[,pars] %>% mutate(Year = 2017)
df_comb <- bind_rows(df_comb1, df_comb2)

# Plot  
summ_variable("TOTP-?g/l", data = df_comb)
summ_variable("TOTN-?g/l", data = df_comb)
gg1 <- plot_map_abs("TOTP-?g/l", c(1, 2, 5, 10, 20, 30, 50, 104), data = df_comb, yearshape = TRUE)
gg2 <- plot_map_abs("TOTN-?g/l", c(50, 100, 200, 300, 600, 1217), data = df_comb, yearshape = TRUE)
gg_comb <- plot_grid(gg1, gg2, labels = c('(a)', '(b)'))
ggsave("Figures_2018data/04_01b_P_and_N_abs.png_incl2017.png", gg_comb, width = 10, height = 5, dpi = 500)


# Vannregionspesifikke stoffer ----

#
# 2018 only
#
summ_variable("Cu-?g/l")
summ_variable("Cr-?g/l")
summ_variable("Zn-?g/l")
summ_variable("As-?g/l")
gg1 <- plot_map_abs("Cu-?g/l", c(0.05, 0.1, 0.2, 0.5, 1, 2.74))
gg2 <- plot_map_abs("Cr-?g/l", c(0.02, 0.05, 0.1, 0.2, 0.53))
gg3 <- plot_map_abs("Zn-?g/l", c(0.02, 0.05, 0.1, 0.2, 0.5, 1, 2.98))
gg4 <- plot_map_abs("As-?g/l", c(0.02, 0.05, 0.1, 0.2, 0.51))
gg_comb <- plot_grid(gg1, gg2, gg3, gg4, nrow = 2, labels = c('(a) ', '(b) ', '(c) ', '(d) '))
ggsave("Figures_2018data/04_02_vannregionspesifikke_absolutt.png", gg_comb, width = 10, height = 10, dpi = 500)
# gg_comb

#
# 2017 (df3) + 2018
#

# Combine data sets
pars <- c("Rapportnavn", "Lengdegrad", "Breddegrad", "Cu-?g/l", "Cr-?g/l", "Zn-?g/l", "As-?g/l")
df_comb1 <- df_chem[,pars] %>% mutate(Year = 2018)
df_comb2 <- df3 %>%
  rename("Cu-?g/l" = "Cu",
         "Cr-?g/l" = "Cr",
         "Zn-?g/l" = "Zn",
         "As-?g/l" = "As", 
         Breddegrad = Latitude, Lengdegrad = Longitude)
df_comb2 <- df_comb2[,pars] %>% mutate(Year = 2017)
df_comb <- bind_rows(df_comb1, df_comb2)

# Plot  
summ_variable("Cu-?g/l", data = df_comb)
summ_variable("Cr-?g/l", data = df_comb)
summ_variable("Zn-?g/l", data = df_comb)
summ_variable("As-?g/l", data = df_comb)
gg1 <- plot_map_abs("Cu-?g/l", c(0.05, 0.1, 0.2, 0.5, 1, 2.74), data = df_comb, yearshape = TRUE)
gg2 <- plot_map_abs("Cr-?g/l", c(0.02, 0.05, 0.1, 0.2, 0.5, 1, 1.21), data = df_comb, yearshape = TRUE)
gg3 <- plot_map_abs("Zn-?g/l", c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 6.63), data = df_comb, yearshape = TRUE)
gg4 <- plot_map_abs("As-?g/l", c(0.01, 0.02, 0.05, 0.1, 0.2, 0.53), data = df_comb, yearshape = TRUE)
gg_comb <- plot_grid(gg1, gg2, gg3, gg4, nrow = 2, labels = c('(a) ', '(b) ', '(c) ', '(d) '))
ggsave("Figures_2018data/04_02b_vannregionspesifikke_absolutt_incl2017.png", gg_comb, width = 10, height = 10, dpi = 500)
# gg_comb


#
# Prioriterte stoffer ----
#

summ_variable("Cd-?g/l")
summ_variable("Pb-?g/l")
summ_variable("Ni-?g/l")
summ_variable("Hg-?g/l")
gg1 <- plot_map_abs("Cd-?g/l", c(0.001, 0.002, 0.005, 0.01, 0.02, 0.045))
gg2 <- plot_map_abs("Pb-?g/l", c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.54))
gg3 <- plot_map_abs("Ni-?g/l", c(0.05, 0.1, 0.2, 0.5, 1, 2, 4.11))
gg4 <- plot_map_abs("Hg-?g/l", c(0.5, 0.7, 1, 1.25))
gg_comb <- plot_grid(gg1, gg2, gg3, gg4, nrow = 2, labels = c('(a) ', '(b) ', '(c) ', '(d) '))
ggsave("Figures_2018data/04_03_Prioriterte_absolutt.png", gg_comb, width = 10, height = 10, dpi = 500)
# gg_comb


#
# 2017 (df5) + 2018
#

# Combine data sets
pars <- c("Rapportnavn", "Lengdegrad", "Breddegrad", "Cd-?g/l", "Pb-?g/l", "Ni-?g/l", "Hg-?g/l")
df_comb1 <- df_chem[,pars] %>% mutate(Year = 2018)
df_comb2 <- df5 %>%
  rename("Cd-?g/l" = "Cd",
         "Pb-?g/l" = "Pb",
         "Ni-?g/l" = "Ni",
         "Hg-?g/l" = "Hg", 
         Breddegrad = Latitude, Lengdegrad = Longitude)
df_comb2 <- df_comb2[,pars] %>% mutate(Year = 2017)
df_comb <- bind_rows(df_comb1, df_comb2)

# Plot  
summ_variable("Cd-?g/l", data = df_comb)
summ_variable("Pb-?g/l", data = df_comb)
summ_variable("Ni-?g/l", data = df_comb)
summ_variable("Hg-?g/l", data = df_comb)
summ_variable("Hg-?g/l", data = df_comb %>% filter(`Hg-?g/l` < 146.5))
gg1 <- plot_map_abs("Cd-?g/l", c(0.001, 0.002, 0.005, 0.01, 0.02, 0.045), data = df_comb, yearshape = TRUE)
gg2 <- plot_map_abs("Pb-?g/l", c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 0.83), data = df_comb, yearshape = TRUE)
gg3 <- plot_map_abs("Ni-?g/l", c(0.05, 0.1, 0.2, 0.5, 1, 2, 4.11), data = df_comb, yearshape = TRUE)
# Hg has a real extreme value - 4a: plot including the extreme:
gg4a <- plot_map_abs("Hg-?g/l", c(0.5, 1, 3, 10, 30, 146.5), data = df_comb, yearshape = TRUE)
# 4B: plot expluding the extreme, then add it
gg4b <- plot_map_abs("Hg-?g/l", c(0.5, 1, 2, 3, 4.3), data = df_comb %>% filter(`Hg-?g/l` < 146.5), yearshape = TRUE)
df_extreme <- df_comb %>% filter(`Hg-?g/l` >= 146.5) %>% as.data.frame()
gg4b <- gg4b + 
  geom_point(data = df_extreme, pch = 16, size = 3, color = "red") +
  annotate("text", x = df_extreme$Lengdegrad, y = df_extreme$Breddegrad, label = "146.4", hjust = -0.2) 
gg_comb <- plot_grid(gg1, gg2, gg3, gg4b, nrow = 2, labels = c('(a) ', '(b) ', '(c) ', '(d) '))
ggsave("Figures_2018data/04_03b_Prioriterte_absolutt_incl2017.png", gg_comb, width = 10, height = 10, dpi = 500)
# gg_comb


# pH, ANC, labile Al ----

#
# 2018 only
#

# colnames(df_chem)
summ_variable("pH")
summ_variable("ANC2-?Ekv/L")
summ_variable("Al/L-?g/l")
gg1 <- plot_map_abs("pH", c(6.116, 6.5, 7, 7.5, 8.06), log = FALSE, direction = -1)
gg2 <- plot_map_abs("ANC2-?Ekv/L", c(27.33, 50, 100, 200, 500, 1000, 2492), direction = -1)
gg3 <- plot_map_abs("Al/L-?g/l", c(0.417, 1, 2, 5, 10, 20, 31.25), direction = 1)
gg_comb <- plot_grid(gg1, gg2, gg3, nrow = 2, labels = c('(a) ', '(b) ', '(c) '))
ggsave("Figures_2018data/04_05_pH_og_Alu.png", gg_comb, width = 10, height = 10, dpi = 500)
# gg_comb


#
# 2017 (df2) + 2018
#

# Combine data sets
pars <- c("Rapportnavn", "Lengdegrad", "Breddegrad", "pH", "ANC2-?Ekv/L", "Al/L-?g/l")
df_comb1 <- df_chem[,pars] %>% mutate(Year = 2018)
df_comb2 <- df2 %>%
  rename("ANC2-?Ekv/L" = "ANC (?Ekv/L)",
         "Al/L-?g/l" = "LAl (?g/l)", 
         Breddegrad = Latitude, Lengdegrad = Longitude)
df_comb2 <- df_comb2[,pars] %>% mutate(Year = 2017)
df_comb <- bind_rows(df_comb1, df_comb2)

# Plot  
summ_variable("pH", data = df_comb)
summ_variable("ANC2-?Ekv/L", data = df_comb)
summ_variable("Al/L-?g/l", data = df_comb)
gg1 <- plot_map_abs("pH", c(5.306, 5.5, 6, 6.5, 7, 7.5, 8.06), log = FALSE, direction = -1, data = df_comb, yearshape = TRUE)
gg2 <- plot_map_abs("ANC2-?Ekv/L", c(7.56, 20, 50, 100, 200, 500, 1000, 2493), direction = -1, data = df_comb, yearshape = TRUE)
gg3 <- plot_map_abs("Al/L-?g/l", c(0.417, 1, 2, 5, 10, 20, 50, 81), direction = 1, data = df_comb, yearshape = TRUE)
gg_comb <- plot_grid(gg1, gg2, gg3, nrow = 2, labels = c('(a) ', '(b) ', '(c) '))
ggsave("Figures_2018data/04_05b_pH_og_Alu_incl2017.png", gg_comb, width = 10, height = 10, dpi = 500)
