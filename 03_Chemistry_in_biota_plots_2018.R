

#
# Chemistry plots (including PAH metabol.) 
#


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

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 2. Data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Chemistry (part 3)
df_chem <- readRDS("2018data/02_df_chem.rds") %>%
  filter(!is.na(Rapportnavn))

df_eqs <- read_excel("Input_2018data/EQS data med navn - korrigert.xlsx")
df_eqs <- df_eqs[,1:7]
df_eqs <- rename(df_eqs, EQS = `EQS (µg/kg)`)

sel <- df_eqs$NAME %in% "SumPBDE6"; sum(sel)
df_eqs$NAME[sel] <- "BDE6S"

sel <- df_eqs$NAME %in% "Sum PCB 7"; sum(sel)
df_eqs$EQS[sel] <- 0.6                         # Update PCB EQS from 1 to 0.6

# Special data set for PFAS sum (not entered into Aquamonitor)
df_chem_pfas <- readxl::read_excel("Input_2018data/PFOS data with count.xlsx")
# Pick Sum_PFAS only; set column names so they fit with df_chem
df_chem_pfas <- df_chem_pfas %>%
  rename(SAMPLE_NO = `Blandprøve nr.`, Sample_weight = `vekt prøve`) %>%
  select(Rapportnavn, SAMPLE_NO, Sample_weight, PFOS, PFOA, Sum_PFAS) %>%
  tidyr::gather("NAME", "VALUE", PFOS:Sum_PFAS) %>%
  mutate(NAME = case_when(
    NAME %in% "PFOS" ~ "Perfluoroktylsulfonat (PFOS)",
    NAME %in% "PFOA" ~ "Perfluoroktansyre (PFOA)",
    TRUE ~ NAME)
  )

# PAH metabolites (part 4)
df_pah <- read_excel("Input_2018data/PAHm stacked excel.xlsx") %>%
  filter(!is.na(Rapportnavn))

# xtabs(~NAME, df_chem)

# Length and weight (part 5)
df_fish <- read_excel("Input_2018data/2019 Refelver Fish data.xlsx")

# Metadata for stations (actually only 'Rapportnavn', for adding to df_fish)
df_stationmeta <- read_excel("Input_2018data/Stasjonsoversikt 2018 løpende oppdatert_.xlsx", sheet = 1) %>% 
  rename(`Station Code` = `Aquamonitor stasjonskode`,
         Rapportnavn = `Kortnavn/Rapportnavn`)
sel <- df_stationmeta$`Station Code` %in% "F_212-1729_Lah"; sum(sel)   # Fix one station code
df_stationmeta$`Station Code`[sel] <- "F_212-1729_Láh"

# Check if we find all station codes
a <- unique(df_fish$`Station Code`)
b <- df_stationmeta$`Station Code`
a %in% b

# Add Rapportnavn
df_fish <- df_fish %>%
  left_join(df_stationmeta %>% select(`Station Code`, Rapportnavn))

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3a. Make plots, BDE and Hg ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

save_plots <- TRUE
save_plots <- FALSE

### PBDE (bromated flage retardants)

subset(df_chem, NAME %in% "BDE6S") %>% head(2)

dflist <- perform_logtukeytest("BDE6S")
brks <- c(0.005, 0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 1, 3, 10, 30, 44)
ylabel <- expression(Sum~PBDE6~(mu*g/kg~w.w.))

# debugonce(make_tukeyplot_log)
gg1 <- make_tukeyplot_log(dflist, ylab = ylabel, ybreaks = brks, letterposition = -1)
gg2 <- make_tukeyplot_log(dflist, ylab = ylabel, ybreaks = brks, 
                          extra_limit = 44, letterposition = 0.5, linecolors = c("#e31a1c", "#ff7f00"))
if (save_plots){
  ggsave("Figures_2018data/03_Tukeyplot_SumPBDE6.png", gg2, width = 9, height = 4, dpi = 500)
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
  ggsave("Figures_2018data/03_Tukeyplot_Hg.png", gg1, width = 9, height = 4, dpi = 500)
} else {
  gg1
}

### Fat
subset(df_chem, NAME %in% "Fettinnhold") %>% head(2)   # unit MG_P_KG
dflist <- perform_logtukeytest("Fettinnhold", y_nudge = 0.15)
gg1 <- make_tukeyplot_ordinary(dflist, ylab = expression(Fettinnhold~(plain("%"))),
                               ybreaks = seq(2, 12, 2), include_letters = FALSE)
gg1

if (save_plots){
  ggsave("Figures_2018data/03_Tukeyplot_Fett.png", gg1, width = 9, height = 4, dpi = 500)
} else {
  gg1
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3b. Make plots, PFAS ----
#
# separate data set, and separate function 'make_tukeyplot_log_pfas'
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

debugonce()

# save_plots <- TRUE
dflist <- perform_logtukeytest("Perfluoroktylsulfonat (PFOS)", y_nudge = 0.15, data = df_chem_pfas)
gg1 <- make_tukeyplot_log_pfas(dflist, ylab = expression(Perfluoroktylsulfonat~(PFOS)~(mu*g/kg~w.w.)), 
                               ybreaks = c(0.5, 0.75, 1, 5, 10, 50, 62), letterposition = 90)
if (save_plots){
  ggsave("Figures_2018data/03_Tukeyplot_PFOS.png", gg1, width = 9, height = 4, dpi = 500)
} else {
  gg1
}

dflist <- perform_logtukeytest("Perfluoroktansyre (PFOA)", y_nudge = 0.15, data = df_chem_pfas)
gg1 <- make_tukeyplot_log_pfas(dflist, ylab = expression(Perfluoroktansyre~(PFOA)~(mu*g/kg~w.w.)), 
                               ybreaks = c(0.5, 0.75, 1, 5, 10, 50, 91), letterposition = 50)
if (save_plots){
  ggsave("Figures_2018data/03_Tukeyplot_PFOA.png", gg1, width = 9, height = 4, dpi = 500)
} else {
  gg1
}

dflist <- perform_logtukeytest("Sum_PFAS", y_nudge = 0.15, data = df_chem_pfas)
gg1 <- make_tukeyplot_log_pfas(dflist, ylab = expression(Sum~PFAS~(mu*g/kg~w.w.)), 
                               ybreaks = c(0.5, 0.75, 1, 5, 10, 50, 62), letterposition = 90)
if (save_plots){
  ggsave("Figures_2018data/03_Tukeyplot_PFASsum.png", gg1, width = 9, height = 4, dpi = 500)
} else {
  gg1
}





### 3c. Dioxins ----

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
  ggsave("Figures_2018data/03_Tukeyplot_dioksin.png", gg1, width = 9, height = 4, dpi = 500)
} else {
  gg1
}


#
# 3d. MCCP, SCCP ----
#


### MCCP

dflist <- perform_logtukeytest("MCCP eksl. LOQ")
gg <- make_tukeyplot_log(dflist, ylab = expression(MCCP~uten~LOQ~(mu*g/kg~w.w.)), 
                         ybreaks = c(1, 2, 3, 10, 20, 30, 50, 100, 200, 300), letterposition = 30)
if (save_plots){
  ggsave("Figures_2018data/03_Tukeyplot_MCCP.png", gg, width = 9, height = 4, dpi = 500)
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
  ggsave("Figures_2018data/03_Tukeyplot_SCCP.png", gg, width = 9, height = 4, dpi = 500)
} else {
  gg
}



#
# 3e. Sum PCB 7 ----
#
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
  ggsave("Figures_2018data/03_Tukeyplot_PCB7.png", gg, width = 9, height = 4, dpi = 500)
} else {
  gg
}



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 4. PAH metabolites ----
#
# Separate file, and not used functions for plots
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Assessment levels

df_assessment <- tibble(
  Limit = c("BAC low", "BAC high", "EAC low", "EAC high"),
  `1-OH-fenantren` = c(0.8, 3.7, 262, 1832),
  `1-OH-pyren` = c(13, 21, 483, 909)
)


# head(df_pah)
# colnames(df_pah)

cols <- c("Rapportnavn", "PrøveID",
          "Result halfLOD 1-OH-fenantren", "Result halfLOD 1-OH-pyren", "Result halfLOD 3-OH-benzo(a)pyren",
          "Result>LOD 1-OH-fenantren", "Result>LOD 1-OH-pyren", "Result>LOD 3-OH-benzo(a)pyren")

df_pah2 <- df_pah[,cols] %>%
  rename("1-OH-fenantren" = "Result halfLOD 1-OH-fenantren",
         "1-OH-pyren" = "Result halfLOD 1-OH-pyren", 
         "3-OH-benzo(a)pyren" = "Result halfLOD 3-OH-benzo(a)pyren") %>%
  mutate(`LOD 1-OH-fenantren`  = ifelse(is.na(`Result>LOD 1-OH-fenantren`), "Under LOD", "Over LOD"),
         `LOD 1-OH-pyren`  = ifelse(is.na(`Result>LOD 1-OH-pyren`), "Under LOD", "Over LOD"),
         `LOD 3-OH-benzo(a)pyren`  = ifelse(is.na(`Result>LOD 3-OH-benzo(a)pyren`), "Under LOD", "Over LOD")
  )

# Add an extra level, just in order to have some space in the left side for printing BAC and EAC
df_pah2$Rapportnavn <- factor(df_pah2$Rapportnavn, levels = c("", unique(df_pah2$Rapportnavn)))

# Make 'Pos_x' variable for "exact jittering" = 
#   using geom moving all points away from the centre line in steps of 0.06 (look at Pos_x data to understand)
df_pah2 <- df_pah2 %>%
  group_by(Rapportnavn) %>%
  mutate(N = n(), 
         I = as.numeric(substr(PrøveID,5,5)),
         Pos_x = (I - (N+1)/2)*0.06)

# Medians (to be plotted as lines)
df_medians <- df_pah2 %>%
  group_by(Rapportnavn) %>%
  summarise_if(is.numeric, median)

#
# For improved log-axis:
#
makelabels <- function(labels, breaks){
  brk_lab <- breaks 
  brk_lab[!as.character(brk) %in% labels] <- ""
  brk_lab
}


# 4a. Plot '1-OH-fenantren' ----


# Defining tick marks
brk <- c(seq(1,10,1), seq(20,100,10), seq(200,1000,100), 2000, 3000)

gg <- ggplot(df_pah2, aes(Rapportnavn, `1-OH-fenantren`)) + 
  geom_point(data = df_medians, pch = "_", size = 15, color = "purple") +
  # geom_point(position = position_jitter(width = 0.2, height = 0)) +    # good old jittering
  geom_point(aes(shape = `LOD 1-OH-fenantren`), position = position_nudge(x = df_pah2$Pos_x)) +             # exact jittering
  scale_shape_manual(values = c(16, 6)) +
  scale_x_discrete(drop = FALSE) +
  scale_y_log10(breaks = brk, labels = makelabels(c(1,3,10,30,100,300,1000,3000), brk), limits = c(1,3100)) +
  geom_hline(yintercept = df_assessment$`1-OH-fenantren`, color = c(rep("orange",2),rep("red",2))) +
  annotate("text", label = df_assessment$Limit, x = 1, y = df_assessment$`1-OH-fenantren`, 
           size = 4, hjust = 0.5, vjust = -0.25, color = c(rep("orange",2),rep("red",2)))  +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.2)) +
  labs(x = "", y = "1-OH-fenantren (µg/kg)") +
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(16, 16, 16, 16),
        legend.text = element_text(size = rel(0.8)),
        legend.key.height=unit(0.8,"line"),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_blank(),
        axis.ticks.x = element_blank()
  )

gg
ggsave("Figures_2018data/03_PAHmet_1-OH-fenantren.png", gg, width = 7, height = 5, dpi = 500)



# 4B. Plot '1-OH-pyren' ----
# (exactly equal, except the variable name,  y axis breaks and y limits)

# Defining tick marks
summary(df_pah2$`1-OH-pyren`)
brk <- c(seq(0.1,1,0.1), seq(2,10,1), seq(20,100,10), seq(200,1000,100), 2000, 3000)


# Plot '1-OH-pyren'
gg <- ggplot(df_pah2, aes(Rapportnavn, `1-OH-pyren`)) + 
  geom_point(data = df_medians, pch = "_", size = 15, color = "purple") +
  # geom_point(position = position_jitter(width = 0.2, height = 0)) +    # good old jittering
  geom_point(aes(shape = `LOD 1-OH-pyren`), position = position_nudge(x = df_pah2$Pos_x)) +             # exact jittering
  scale_shape_manual(values = c(16, 6)) +
  scale_x_discrete(drop = FALSE) +
  scale_y_log10(breaks = brk, labels = makelabels(c(1,3,10,30,100,300,1000,3000), brk), limits = c(0.2, 2200)) +
  geom_hline(yintercept = df_assessment$`1-OH-pyren`, color = c(rep("orange",2),rep("red",2))) +
  annotate("text", label = df_assessment$Limit, x = 1, y = df_assessment$`1-OH-pyren`, 
           size = 4, hjust = 0.5, vjust = -0.25, color = c(rep("orange",2),rep("red",2)))  +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.2)) +
  labs(x = "", y = "1-OH-pyren (µg/kg)") +
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(16, 16, 16, 16),
        legend.text = element_text(size = rel(0.8)),
        legend.key.height=unit(0.8,"line"),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_blank(),
        axis.ticks.x = element_blank()
  )

gg

ggsave("Figures_2018data/03_PAHmet_1-OH-pyren.png", gg, width = 7, height = 5, dpi = 500)


#
# 4C. Plot '3-OH-benzo(a)pyren' ----
# (exactly equal, except the variable name,  y axis breaks and y limits, and  no lines for limit values)
#

# Defining tick marks
summary(df_pah2$`3-OH-benzo(a)pyren`)
brk <- c(seq(0.5,1,0.1), seq(2,10,1))

gg <- ggplot(df_pah2, aes(Rapportnavn, `3-OH-benzo(a)pyren`)) + 
  geom_point(data = df_medians, pch = "_", size = 15, color = "purple") +
  # geom_point(position = position_jitter(width = 0.2, height = 0)) +    # good old jittering
  geom_point(aes(shape = `LOD 3-OH-benzo(a)pyren`), position = position_nudge(x = df_pah2$Pos_x)) +             # exact jittering
  scale_shape_manual(values = c(16, 6)) +
  scale_x_discrete(drop = FALSE) +
  scale_y_log10(breaks = brk, labels = makelabels(c(0.5, 1, 3, 10), brk), limits = c(0.5, 35)) +
  # geom_hline(yintercept = df_assessment$`3-OH-benzo(a)pyren`, color = c(rep("orange",2),rep("red",2))) +   # No limit values
  # annotate("text", label = df_assessment$Limit, x = 1, y = df_assessment$`3-OH-benzo(a)pyren`,             # No limit values
  #          size = 4, hjust = 0.5, vjust = -0.25, color = c(rep("orange",2),rep("red",2)))  +               # No limit values
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.2)) +
  labs(x = "", y = "3-OH-benzo(a)pyren (µg/kg)") +
  theme(legend.position = c(.95, 1),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(16, 16, 16, 16),
        legend.text = element_text(size = rel(0.8)),
        legend.key.height=unit(0.8,"line"),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_blank(),
        axis.ticks.x = element_blank()
  )

gg

ggsave("Figures_2018data/03_PAHmet_3-OH-benzo(a)pyren.png", gg, width = 7, height = 5, dpi = 500)




#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 5. Make plots, length and weight ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# rename one variable

df_fish <- df_fish %>%
  rename(Sampleno = `Blandprøve nr.`)

# Length  
df_cld <- make_tukeydf(df_fish %>% rename(VALUE = `Lengde (cm)`))
df_cld$Sampleno = 2

gg <- ggplot(df_fish, aes(Sampleno, `Lengde (cm)`,
                          fill = as.factor(Sampleno), shape = as.factor(Sampleno))) + 
  geom_point(position = position_jitter(width = .3, height = 0)) + 
  scale_shape_manual("Prøvenr", values = c(24,22,25)) +
  scale_fill_manual("Prøvenr", values = c('#c51b8a','#fa9fb5','#fde0dd')) +
  geom_text(data = df_cld, aes(y = 23, label = .group), color = "black") +
  facet_grid(.~Rapportnavn) +
  scale_x_continuous(breaks = c(1,2,3)) +
  labs(x = "Prøvenummer", y = "Lengde (cm)") +
  theme_bw() +
  theme(strip.text=element_text(angle=90, hjust=0.5, vjust=0), strip.background = element_rect(fill = "white"))
gg
ggsave("Figures_2018data/03_Tukeyplot_fish_length.png", gg, width = 7, height = 4, dpi = 500)

# Weight  
# Exactly equal except replaced variable name , and y = 145 in geom_text
df_cld <- make_tukeydf(df_fish %>% rename(VALUE = `Vekt (g)`))
df_cld$Sampleno = 2

gg <- ggplot(df_fish, aes(Sampleno, `Vekt (g)`,
                          fill = as.factor(Sampleno), shape = as.factor(Sampleno))) + 
  geom_point(position = position_jitter(width = .3, height = 0)) + 
  scale_shape_manual("Prøvenr", values = c(24,22,25)) +
  scale_fill_manual("Prøvenr", values = c('#c51b8a','#fa9fb5','#fde0dd')) +
  geom_text(data = df_cld, aes(y = 145, label = .group), color = "black") +
  facet_grid(.~Rapportnavn) +
  scale_x_continuous(breaks = c(1,2,3)) +
  labs(x = "Prøvenummer", y = "Vekt (g)") +
  theme_bw() +
  theme(strip.text=element_text(angle=90, hjust=0.5, vjust=0), strip.background = element_rect(fill = "white"))
gg
ggsave("Figures_2018data/03_Tukeyplot_fish_weight.png", gg, width = 7, height = 4, dpi = 500)



