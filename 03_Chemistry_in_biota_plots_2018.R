
# 1. Libraries ----

library(dplyr)
library(ggplot2)
library(readxl)
library(viridis)
library(cowplot)
library(niRvana)
library(lsmeans)   # lsmeans(), see script 03fun
library(multcompView)

source("03fun_Chemistry_in_biota_plots_functions.R")

# 2. Data ----

df_chem <- readRDS("2018data/02_df_chem.rds") %>%
  filter(!is.na(Rapportnavn))

df_eqs <- read_excel("Input_2018data/EQS data med navn - korrigert.xlsx")
df_eqs <- df_eqs[,1:7]
df_eqs <- rename(df_eqs, EQS = `EQS (µg/kg)`)


# 3. Make plots ----

save_plots <- FALSE

### PBDE (bromated flage retardants)

subset(df_chem, NAME %in% "BDE6S") %>% head(2)

dflist <- perform_logtukeytest("BDE6S")
brks <- c(0.005, 0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3)
ylabel <- expression(Sum~PBDE6~(mu*g/kg~w.w.))

gg1 <- make_tukeyplot_log(dflist, ylab = ylabel, ybreaks = brks, letterposition = -1)
gg2 <- make_tukeyplot_log(dflist, ylab = ylabel, ybreaks = brks, 
                          extra_limit = 44, letterposition = 0.5, linecolors = rev(c("#e31a1c", "#ff7f00")))
if (save_plots){
  ggsave("Figures/03_Tukeyplot_SumPBDE6.png", gg2, width = 9, height = 4, dpi = 500)
}
gg2


### Kvikksølv
subset(df_chem, NAME %in% "Kvikksølv") %>% head(2)   # unit MG_P_KG
sel <- df_chem$NAME %in% "Kvikksølv" & df_chem$UNIT %in% "MG_P_KG"; sum(sel)       # Convert to UG per kg
df_chem$VALUE[sel] <- df_chem$VALUE[sel]*1000
df_chem$UNIT[sel] <- "UG_P_KG"

dflist <- perform_logtukeytest("Kvikksølv", y_nudge = 0.15)
gg1 <- make_tukeyplot_log(dflist, ylab = expression(Kvikksølv~(mu*g/kg~w.w.)), 
                          ybreaks = c(20, 30, 50, 100, 200, 300, 500),
                          extra_limit = 500, letterposition = -1)
if (save_plots){
  ggsave("Figures/03_Tukeyplot_Hg.png", gg1, width = 9, height = 4, dpi = 500)
} else {
  gg1
}

### PFOS
dflist <- perform_logtukeytest("Perfluoroktylsulfonat (PFOS)", y_nudge = 0.15)
gg1 <- make_tukeyplot_log(dflist, ylab = expression(PFOS~(mu*g/kg~w.w.)), 
                          ybreaks = c(0.5, 1, 2, 3, 5, 10, 30, 60), letterposition = -1)
if (save_plots){
  ggsave("Figures/03_Tukeyplot_PFOS.png", gg1, width = 9, height = 4, dpi = 500)
} else {
  gg1
}


### PFOA
dflist <- perform_logtukeytest("Perfluoroktansyre (PFOA)", y_nudge = 0.15)
gg1 <- make_tukeyplot_log(dflist, ylab = expression(PFOA~(mu*g/kg~w.w.)), 
                          ybreaks = c(0.5, 0.75, 1, 5, 10, 50, 91), letterposition = 20)
if (save_plots){
  ggsave("Figures/03_Tukeyplot_PFOA.png", gg1, width = 9, height = 4, dpi = 500)
} else {
  gg1
}


### Dioxins

#
# NOTE: Last year, zero values (values beloq LOQ) were set to 7.50e-08 (half of LOQ = 1.50e-07)
#
sel <- df_chem$NAME %in% "WHO (2005)-PCDD/F+PCB TEQ (ekskl LOQ)" & df_chem$UNIT %in% "PG_P_G"; sum(sel)
summary(df_chem$VALUE[sel])
# head(df_chem[sel,], 2)                               # Is: PG_P_G = picogram (1 millionth UG) per gram
df_chem$VALUE[sel] <- df_chem$VALUE[sel]/1000          # Convert to UG per kilogram
df_chem$UNIT[sel] <- "UG_P_KG"

# Run debug of 'perform_logtukeytest', and sort(df1$VALUE_JMO) in the start, to find that A = 1.50e-07
#   is the smallest value > 0
# debugonce(perform_logtukeytest)
# dflist <- perform_logtukeytest("WHO (2005)-PCDD/F+PCB TEQ (ekskl LOQ)", y_nudge = 0.5, A = 1.50e-07)

dflist <- perform_logtukeytest("WHO (2005)-PCDD/F+PCB TEQ (ekskl LOQ)", y_nudge = 0.5)

# Browse[2]> sort(df1$VALUE_JMO)
#  [1] 0.00e+00 1.50e-07 1.65e-07 1.85e-07 2.07e-07 4.11e-07 4.27e-07 5.55e-07 7.27e-07 3.05e-06 3.26e-06
# [12] 3.87e-06 7.22e-05 8.59e-05 9.10e-05 1.00e-04 1.19e-04 1.23e-04 1.45e-04 1.47e-04 1.81e-04 1.83e-04
gg1 <- make_tukeyplot_log(dflist, ylab = expression(Dioksiner~(mu*g/kg~w.w.)), 
                          ybreaks = c(1E-7, 3E-7, 1E-6, 3E-6, 1E-5, 3E-5, 1E-4, 3E-4, 1E-3, 3E-3, 0.0065),                                     letterposition = -1)
gg1
if (save_plots){
  ggsave("Figures/03_Tukeyplot_dioksin.png", gg1, width = 9, height = 4, dpi = 500)
} else {
  gg1
}




### MCCP

dflist <- perform_logtukeytest("MCCP eksl. LOQ")
gg <- make_tukeyplot_log(dflist, ylab = expression(MCCP~uten~LOQ~(mu*g/kg~w.w.)), 
                         ybreaks = c(1, 2, 3, 10, 20, 30, 50, 100, 200, 300), letterposition = 30)
if (save_plots){
  ggsave("Figures/03_Tukeyplot_MCCP.png", gg, width = 9, height = 4, dpi = 500)
} else {
  gg
}



### SCCP

dflist <- perform_logtukeytest("SCCP eksl. LOQ", y_nudge = 0.4)
gg <- make_tukeyplot_log(dflist, ylab = expression(SCCP~uten~LOQ~(mu*g/kg~w.w.)), 
                         ybreaks = c(1, 2, 3, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 6000), 
                         letterposition = -1)
gg
if (save_plots){
  ggsave("Figures/03_Tukeyplot_SCCP.png", gg, width = 9, height = 4, dpi = 500)
} else {
  gg
}



### Sum PCB 7
# Zero values (values beloq LOQ) are set to 0.11 (half of LOQ)

# debugonce(perform_logtukeytest)
# sort(df1$VALUE_JMO)
#  [1] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.22 0.22 0.23 0.23 0.24 0.25 0.26 0.27 0.28 0.30 0.32 0.41 0.46
# [22] 0.53 0.57 1.16 1.35
# dat_back <- dat 
sel <- with(df_chem, NAME %in% "Sum PCB 7" & VALUE == 0)
sum(sel) # 0

# MG_P_KG
table(subset(df_chem, NAME %in% "Sum PCB 7")$UNIT)

# Convert to UG per kg
sel <- df_chem$NAME %in% "Sum PCB 7" & df_chem$UNIT %in% "MG_P_KG"; sum(sel)
df_chem$VALUE[sel] <- df_chem$VALUE[sel]*1000
df_chem$UNIT[sel] <- "UG_P_KG"

summary(subset(df_chem, NAME %in% "Sum PCB 7")$VALUE)
dflist <- perform_logtukeytest("Sum PCB 7", y_nudge = 0.4)
gg <- make_tukeyplot_log(dflist, ylab = expression(Sum~PCB~7~(mu*g/kg~w.w.)), 
                         ybreaks = c(0.1, 0.2, 0.3, 0.5, 1, 2, 5, 10), letterposition = -1)
gg
if (save_plots){
  ggsave("Figures/03_Tukeyplot_PCB7.png", gg, width = 9, height = 4, dpi = 500)
} else {
  gg
}





