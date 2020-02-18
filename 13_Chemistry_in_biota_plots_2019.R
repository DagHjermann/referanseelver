

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

# OLD STUFF (2018 data)
# For 2019 data, we use separate files for each contaminant (in case Merete has QC'ed)

if (FALSE){
  
  # Chemical data
  df_chem <- readRDS("2018data/02_df_chem.rds") %>%
    filter(!is.na(Rapportnavn))
  
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
  
  }

df_eqs <- read_excel("Input_2018data/EQS data med navn - korrigert.xlsx")
df_eqs <- df_eqs[,1:7]
df_eqs <- rename(df_eqs, EQS = `EQS (µg/kg)`)

sel <- df_eqs$NAME %in% "SumPBDE6"; sum(sel)
df_eqs$NAME[sel] <- "BDE6S"

sel <- df_eqs$NAME %in% "Sum PCB 7"; sum(sel)
df_eqs$EQS[sel] <- 0.6                         # Update PCB EQS from 1 to 0.6

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3a. Make plots, BDE and Hg ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

save_plots <- TRUE
save_plots <- FALSE

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
### PBDE (brominated flame retardants)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# 2018:
# dflist <- perform_logtukeytest("BDE6S")

fn <- "Input_2019data/JMP/052b PBDE split.xlsx"

name <- "BDE6"  # this is not the name in data, but the name in EQS file
dflist <- readxl::read_excel(fn) %>%
  rename(VALUE = sumBDE_6, SAMPLE_NO = Stasjonsnr) %>%
  mutate(NAME = name) %>%
  perform_logtukeytest(name, data = .)

brks <- c(0.005, 0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 1, 3, 10, 30, 44)
ylabel <- expression(Sum~PBDE6~(mu*g/kg~w.w.))

gg1 <- make_tukeyplot_log(dflist, ylab = ylabel, ybreaks = brks, letterposition = -1)
gg2 <- make_tukeyplot_log(dflist, ylab = ylabel, ybreaks = brks, 
                          extra_limit = 44, letterposition = 1, linecolors = c("#e31a1c", "#ff7f00"))
if (save_plots){
  ggsave("Figures_2019data/03_Tukeyplot_SumPBDE6.png", gg2, width = 9, height = 4, dpi = 500)
}
gg2

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
### Kvikksølv
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# subset(df_chem, NAME %in% "Kvikksølv") %>% head(2)   # unit MG_P_KG
# sel <- df_chem$NAME %in% "Kvikksølv" & df_chem$UNIT %in% "MG_P_KG"; sum(sel)       # Convert to UG per kg
# df_chem$VALUE[sel] <- df_chem$VALUE[sel]*1000
# df_chem$UNIT[sel] <- "UG_P_KG"

fn <- "Input_2019data/JMP/051 Kvikksølv.xlsx"
# readxl::read_excel(fn) %>% names()
# readxl::read_excel(fn) %>% head()
# readxl::read_excel(fn) %>% View()

# 2018:
# dflist <- perform_logtukeytest("Kvikksølv", y_nudge = 0.15)

name <- "Kvikksølv"  # this is not the name in data, but the name in EQS file
dflist <- readxl::read_excel(fn) %>%
  rename(NAME = Analyse, VALUE = `Result halfLOD`, SAMPLE_NO = `Stasjonsnr.`) %>%
  mutate(VALUE = VALUE*1000,       # rescale from mg to ug
         NAME = name) %>%
  perform_logtukeytest(name, data = .)

gg1 <- make_tukeyplot_log(dflist, ylab = expression(Kvikksølv~(mu*g/kg~w.w.)), 
                          ybreaks = c(20, 30, 50, 100, 200, 300, 500),
                          extra_limit = 500, letterposition = -1)
if (save_plots){
  ggsave("Figures_2019data/03_Tukeyplot_Hg.png", gg1, width = 9, height = 4, dpi = 500)
} else {
  gg1
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
### Fat
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

fn <- "Input_2019data/JMP/050 fisk fettprosent.xlsx"
# readxl::read_excel(fn) %>% names()
# readxl::read_excel(fn) %>% head()
# readxl::read_excel(fn) %>% View()

# 2018:
# dflist <- perform_logtukeytest("Fettinnhold", y_nudge = 0.15)

name <- "Fettinnhold"  # this is not the name in data, but the name in EQS file
dflist <- readxl::read_excel(fn) %>%
  rename(NAME = Analyse, VALUE = `Result halfLOD`, SAMPLE_NO = `Stasjonsnr`) %>%
  mutate(NAME = ) %>%
  perform_logtukeytest(name, data = .)


gg1 <- make_tukeyplot_ordinary(dflist, ylab = expression(Fettinnhold~(plain("%"))),
                               ybreaks = seq(2, 20, 2), include_letters = FALSE)

if (save_plots){
  ggsave("Figures_2019data/03_Tukeyplot_Fett.png", gg1, width = 9, height = 4, dpi = 500)
} else {
  gg1
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3b. Make plots, PFAS ----
#
# separate data set, and separate function 'make_tukeyplot_log_pfas'
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

fn <- "Input_2019data/JMP/053b PFOS_PFOA split.xlsx"
#
# Note that we get some warnings due to dates in "Stadium"
# Doesn't matter
#
#
# readxl::read_excel(fn) %>% names()
# readxl::read_excel(fn) %>% names() %>% .[23]
# readxl::read_excel(fn) %>% head()
# readxl::read_excel(fn) %>% View()

# "Result halfLOD Perfluoroktansyre (PFOA)" "Result halfLOD Perfluoroktylsulfonat (PFOS)"    

# Must also contain variable Sample_weight (in addition to VALUE, NAME, Rapportnavn)
#
# PFOS
#

# 2018:
# dflist <- perform_logtukeytest("Perfluoroktylsulfonat (PFOS)", y_nudge = 0.15, data = df_chem_pfas)

name <- "Perfluoroktylsulfonat (PFOS)"
dflist <- readxl::read_excel(fn) %>%
  rename(VALUE = `Result halfLOD Perfluoroktylsulfonat (PFOS)`, 
         Sample_weight = `Lever (g)`,
         SAMPLE_NO = `Stasjonsnr`) %>%
  mutate(NAME = name) %>%
  perform_logtukeytest(name, data = ., y_nudge = 0.15)


gg1 <- make_tukeyplot_log_pfas(dflist, ylab = expression(Perfluoroktylsulfonat~(PFOS)~(mu*g/kg~w.w.)), 
                               ybreaks = c(0.5, 0.75, 1, 5, 10), letterposition = 12)
if (save_plots){
  ggsave("Figures_2019data/03_Tukeyplot_PFOS.png", gg1, width = 9, height = 4, dpi = 500)
} else {
  gg1
}


#
# PFOA
# Empty column in data ???
#
name <- "Perfluoroktansyre (PFOA)"
dflist <- readxl::read_excel(fn) %>% 
  rename(VALUE = `Result halfLOD Perfluoroktansyre (PFOA)`, 
         Sample_weight = `Lever (g)`,
         SAMPLE_NO = `Stasjonsnr`) %>% 
  mutate(NAME = name) %>% select(VALUE, NAME, Rapportnavn) %>% View()
  perform_logtukeytest(name, data = ., y_nudge = 0.15)


dflist <- perform_logtukeytest("Perfluoroktansyre (PFOA)", y_nudge = 0.15, data = df_chem_pfas)
gg1 <- make_tukeyplot_log_pfas(dflist, ylab = expression(Perfluoroktansyre~(PFOA)~(mu*g/kg~w.w.)), 
                               ybreaks = c(0.5, 0.75, 1, 5, 10, 50, 91), letterposition = 50)
if (save_plots){
  ggsave("Figures_2019data/03_Tukeyplot_PFOA.png", gg1, width = 9, height = 4, dpi = 500)
} else {
  gg1
}


#
# Sum PFOS + PFOA , meaningless when no PFOA
#
dflist <- perform_logtukeytest("Sum_PFAS", y_nudge = 0.15, data = df_chem_pfas)
gg1 <- make_tukeyplot_log_pfas(dflist, ylab = expression(Sum~PFAS~(mu*g/kg~w.w.)), 
                               ybreaks = c(0.5, 0.75, 1, 5, 10, 50, 62), letterposition = 90)
if (save_plots){
  ggsave("Figures_2019data/03_Tukeyplot_PFASsum.png", gg1, width = 9, height = 4, dpi = 500)
} else {
  gg1
}




#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
### 3c. Dioxins ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

fn <- "Input_2019data/JMP/062 dioxiner.xlsx"
# readxl::read_excel(fn) %>% names()
# readxl::read_excel(fn) %>% head()
# readxl::read_excel(fn) %>% View()

# 2018:
# dflist <- perform_logtukeytest("WHO (2005)-PCDD/F+PCB TEQ (ekskl LOQ)", y_nudge = 0.5)

name <- "WHO (2005)-PCDD/F+PCB TEQ (ekskl LOQ)"  # this is not the name in data, but the name in EQS file
dflist <- readxl::read_excel(fn) %>%
  rename(NAME = Analyse, VALUE = `Result halfLOD`, SAMPLE_NO = `Stasjonsnr`) %>%
  mutate(VALUE = VALUE/1000,       # rescale from mg to ug
         NAME = name) %>%
  perform_logtukeytest(name, data = .)

gg1 <- make_tukeyplot_log(dflist, ylab = expression(Dioksiner~(mu*g/kg~w.w.)), 
                          ybreaks = c(1E-7, 3E-7, 1E-6, 3E-6, 1E-5, 3E-5, 1E-4, 
                                      3E-4, 1E-3, 3E-3, 0.0065),
                          letterposition = 0.001)

if (save_plots){
  ggsave("Figures_2019data/03_Tukeyplot_dioksin.png", gg1, width = 9, height = 4, dpi = 500)
} else {
  gg1
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3d. MCCP, SCCP ----
#
# These are both in the "collection file"
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


fn <- "Input_2019data/JMP/012 Stacked data with station names.xlsx"

df_all <- readxl::read_excel(fn) %>% # View()
  rename(NAME = Komponenter, VALUE = `Result>LOD`, SAMPLE_NO = `Stasjonsnr`)

        # rescale from mg to ug

# unique(df_all$NAME)
# df_all %>% select(NAME, Rapportnavn, VALUE, SAMPLE_NO) %>% View()
# names(df_all)
  
# names of MCCP and SCCP are alreaday correct
    
# 2018:
# dflist <- perform_logtukeytest("MCCP eksl. LOQ")

### MCCP

dflist <- perform_logtukeytest("MCCP eksl. LOQ", data = df_all)

gg <- make_tukeyplot_log(dflist, ylab = expression(MCCP~uten~LOQ~(mu*g/kg~w.w.)), 
                         ybreaks = c(1, 2, 3, 10, 20, 30, 50, 100, 200, 300), letterposition = 20)
if (save_plots){
  ggsave("Figures_2019data/03_Tukeyplot_MCCP.png", gg, width = 9, height = 4, dpi = 500)
} else {
  gg
}


### SCCP

# Doesn't work because there are data from only one station.....
# dflist <- perform_logtukeytest("SCCP eksl. LOQ", y_nudge = 0.4, data = df_all)


gg <- ggplot(df1, aes(SAMPLE_NO, VALUE, fill = as.factor(SAMPLE_NO))) +
  geom_point(aes(shape = Prøvevekt), size = rel(3)) +       # special for PFAS
  geom_hline(yintercept = limits, colour = linecolors[1], linetype = 2, size = 1) +
  facet_grid(.~Rapportnavn) +
  scale_shape_manual("Prøvevekt", values = c(4,16,0)) +   # see symbol overview above
  scale_fill_manual("Prøvenr", values = c('#c51b8a','#fa9fb5','#fde0dd')) +
  scale_x_continuous(breaks = c(1,2,3), limits = c(0.5,3.5)) +
  scale_y_log10(breaks = ybreaks, labels = no_extra_digits) +
  labs(x = xlab, y = ylab) +
  theme_bw() +
  theme(strip.text=element_text(angle=90, hjust=0.5, vjust=0), strip.background = element_rect(fill = "white")) +
  theme(legend.position = "none")



gg <- make_tukeyplot_log(dflist, ylab = expression(SCCP~uten~LOQ~(mu*g/kg~w.w.)), 
                         ybreaks = c(1, 2, 3, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 6000), 
                         letterposition = -1)
gg
if (save_plots){
  ggsave("Figures_2019data/03_Tukeyplot_SCCP.png", gg, width = 9, height = 4, dpi = 500)
} else {
  gg
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3e. Sum PCB 7 ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# subset(df_all, NAME == "Sum PCB 7")$Enhet %>% unique()
# mg/kg

dflist <- perform_logtukeytest("Sum PCB 7", y_nudge = 0.4, 
                               data = df_all %>% mutate(VALUE = VALUE*1000))  # convert to ug/kg

gg <- make_tukeyplot_log(dflist, ylab = expression(Sum~PCB~7~(mu*g/kg~w.w.)), 
                         ybreaks = c(0.1, 0.2, 0.3, 0.5, 1, 2, 5, 10), letterposition = 4)
gg
if (save_plots){
  ggsave("Figures_2019data/03_Tukeyplot_PCB7.png", gg, width = 9, height = 4, dpi = 500)
} else {
  gg
}



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# 4. PAH metabolites ----
#
# Separate file, and not used functions for plots
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Assessment levels

df_assessment <- tibble(
  Limit = c("BAC low", "BAC high", "EAC low", "EAC high"),
  `1-OH-fenantren` = c(0.8, 3.7, 262, 1832),
  `1-OH-pyren` = c(13, 21, 483, 909)
)



fn <- "Input_2019data/JMP/055c PAHm split.xlsx"
df_pah <- readxl::read_excel(fn)
# readxl::read_excel(fn) %>% names()
# readxl::read_excel(fn) %>% head()
# readxl::read_excel(fn) %>% View()


# head(df_pah)
# colnames(df_pah)

# cols <- c("Rapportnavn", "PrøveID",
#           "Result halfLOD 1-OH-fenantren", "Result halfLOD 1-OH-pyren", "Result halfLOD 3-OH-benzo[a]pyren",
#           "Result>LOD 1-OH-fenantren", "Result>LOD 1-OH-pyren", "Result>LOD 3-OH-benzo[a]pyren")

df_pah <- df_pah %>%
  rename(PrøveID = `BIO SAMPLE Nr. 2`)

cols <- c("Rapportnavn", "PrøveID",
          "Result halfLOD 1-OH-fenantren", "Result halfLOD 1-OH-pyren", "Result halfLOD 3-OH-benzo[a]pyren",
          "Result>LOD 1-OH-fenantren", "Result>LOD 1-OH-pyren", "Result>LOD 3-OH-benzo[a]pyren")


df_pah2 <- df_pah[,cols] %>%
  rename("1-OH-fenantren" = "Result halfLOD 1-OH-fenantren",
         "1-OH-pyren" = "Result halfLOD 1-OH-pyren", 
         "3-OH-benzo[a]pyren" = "Result halfLOD 3-OH-benzo[a]pyren") %>%
  mutate(`LOD 1-OH-fenantren`  = ifelse(is.na(`Result>LOD 1-OH-fenantren`), "Under LOD", "Over LOD"),
         `LOD 1-OH-pyren`  = ifelse(is.na(`Result>LOD 1-OH-pyren`), "Under LOD", "Over LOD"),
         `LOD 3-OH-benzo[a]pyren`  = ifelse(is.na(`Result>LOD 3-OH-benzo[a]pyren`), "Under LOD", "Over LOD")
  )

# Add an extra level, just in order to have some space in the left side for printing BAC and EAC
df_pah2$Rapportnavn <- factor(df_pah2$Rapportnavn, levels = c("", unique(df_pah2$Rapportnavn)))

# Make 'Pos_x' variable for "exact jittering" = 
#   using geom moving all points away from the centre line in steps of 0.06 (look at Pos_x data to understand)

# 2018 code
# df_pah2 <- df_pah2 %>%
#   group_by(Rapportnavn) %>%
#   mutate(N = n(), 
#          I = as.numeric(substr(PrøveID,5,5)),
#          Pos_x = (I - (N+1)/2)*0.06)

df_pah2 <- df_pah2 %>%
  group_by(Rapportnavn) %>%
  mutate(N = n(), 
         I = PrøveID,
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

# From 2018 to 2019: added more lower valliues in brk, makelabels and lower first limit in scale_y_log10()

# Defining tick marks
brk <- c(0.25, 0.5, seq(1,10,1), seq(20,100,10), seq(200,1000,100), 2000, 3000)

gg <- ggplot(df_pah2, aes(Rapportnavn, `1-OH-fenantren`)) + 
  geom_point(data = df_medians, pch = "_", size = 15, color = "purple") +
  # geom_point(position = position_jitter(width = 0.2, height = 0)) +    # good old jittering
  geom_point(aes(shape = `LOD 1-OH-fenantren`), position = position_nudge(x = df_pah2$Pos_x)) +             # exact jittering
  scale_shape_manual(values = c(16, 6)) +
  scale_x_discrete(drop = FALSE) +
  scale_y_log10(breaks = brk, labels = makelabels(c(0.25,1,3,10,30,100,300,1000,3000), brk), limits = c(0.15,3100)) +  # c(1,3100)
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

ggsave("Figures_2019data/03_PAHmet_1-OH-fenantren.png", gg, width = 7, height = 5, dpi = 500)



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
  scale_y_log10(breaks = brk, labels = makelabels(c(0.1, 0.3, 1,3,10,30,100,300,1000,3000), brk), 
                limits = c(0.08, 2200)) +
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

ggsave("Figures_2019data/03_PAHmet_1-OH-pyren.png", gg, width = 7, height = 5, dpi = 500)


#
# 4C. Plot '3-OH-benzo[a]pyren' ----
# (exactly equal, except the variable name,  y axis breaks and y limits, and  no lines for limit values)
#

# Defining tick marks
summary(df_pah2$`3-OH-benzo[a]pyren`)
brk <- c(seq(0.5,1,0.1), seq(2,10,1))

gg <- ggplot(df_pah2, aes(Rapportnavn, `3-OH-benzo[a]pyren`)) + 
  geom_point(data = df_medians, pch = "_", size = 15, color = "purple") +
  # geom_point(position = position_jitter(width = 0.2, height = 0)) +    # good old jittering
  geom_point(aes(shape = `LOD 3-OH-benzo[a]pyren`), position = position_nudge(x = df_pah2$Pos_x)) +             # exact jittering
  scale_shape_manual(values = c(16, 6)) +
  scale_x_discrete(drop = FALSE) +
  scale_y_log10(breaks = brk, labels = makelabels(c(0.5, 1, 3, 10), brk), limits = c(0.5, 35)) +
  # geom_hline(yintercept = df_assessment$`3-OH-benzo[a]pyren`, color = c(rep("orange",2),rep("red",2))) +   # No limit values
  # annotate("text", label = df_assessment$Limit, x = 1, y = df_assessment$`3-OH-benzo[a]pyren`,             # No limit values
  #          size = 4, hjust = 0.5, vjust = -0.25, color = c(rep("orange",2),rep("red",2)))  +               # No limit values
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.2)) +
  labs(x = "", y = "3-OH-benzo[a]pyren (µg/kg)") +
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

ggsave("Figures_2019data/03_PAHmet_3-OH-benzo[a]pyren.png", gg, width = 7, height = 5, dpi = 500)




#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 5. Make plots, length and weight ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o



fn <- "Input_2019data/JMP/011 Fiskedata with station names.xlsx"
df_fish <- readxl::read_excel(fn)
# readxl::read_excel(fn) %>% names()
# readxl::read_excel(fn) %>% head()
# readxl::read_excel(fn) %>% View()

# rename one variable

# 2018:
# df_fish <- df_fish %>%
#   rename(Sampleno = `Blandprøve nr.`)

df_fish <- df_fish %>%
  rename(Sampleno = Stasjonsnr)

# Length  
df_cld <- make_tukeydf(df_fish %>% rename(VALUE = `Lengde (cm)`))
df_cld$Sampleno = 2

gg <- ggplot(df_fish, aes(Sampleno, `Lengde (cm)`,
                          fill = as.factor(Sampleno), shape = as.factor(Sampleno))) + 
  geom_point(position = position_jitter(width = .3, height = 0)) + 
  scale_shape_manual("Prøvenr", values = c(24,22,25)) +
  scale_fill_manual("Prøvenr", values = c('#c51b8a','#fa9fb5','#fde0dd')) +
  geom_text(data = df_cld, aes(y = 35, label = .group), color = "black") +
  facet_grid(.~Rapportnavn) +
  scale_x_continuous(breaks = c(1,2,3)) +
  labs(x = "Prøvenummer", y = "Lengde (cm)") +
  theme_bw() +
  theme(strip.text = element_text(angle=90, hjust=0.5, vjust=0, size = rel(1.1)), 
        strip.background = element_rect(fill = "white"),
        axis.title = element_text(size = rel(1.1))
        )
gg

ggsave("Figures_2019data/03_Tukeyplot_fish_length.png", gg, width = 8.5, height = 5, dpi = 500)

# Weight  
# Exactly equal except replaced variable name , and y = 145 in geom_text
df_cld <- make_tukeydf(df_fish %>% rename(VALUE = `Vekt (g)`))
df_cld$Sampleno = 2

gg <- ggplot(df_fish, aes(Sampleno, `Vekt (g)`,
                          fill = as.factor(Sampleno), shape = as.factor(Sampleno))) + 
  geom_point(position = position_jitter(width = .3, height = 0)) + 
  scale_shape_manual("Prøvenr", values = c(24,22,25)) +
  scale_fill_manual("Prøvenr", values = c('#c51b8a','#fa9fb5','#fde0dd')) +
  geom_text(data = df_cld, aes(y = 365, label = .group), color = "black") +
  facet_grid(.~Rapportnavn) +
  scale_x_continuous(breaks = c(1,2,3)) +
  labs(x = "Prøvenummer", y = "Vekt (g)") +
  theme_bw() +
  theme(strip.text = element_text(angle=90, hjust=0.5, vjust=0, size = rel(1.1)), 
        strip.background = element_rect(fill = "white"),
        axis.title = element_text(size = rel(1.1))
  )
gg

ggsave("Figures_2019data/03_Tukeyplot_fish_weight.png", gg, width = 8.5, height = 5, dpi = 500)



