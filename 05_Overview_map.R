
#
# Script based on "01_Make_map.R" in "C:\Data\Referanseelver_2018\06_Overview_maps" (used the first year)
#


# SKAL FIKSES!!!
# 01 Stabburselv, 05 Komagelv, 07 Lapjohhka, 08 Sametielva, 11 Smeddalselva, 12 Raundalselva, 15 Utla, 18 Smådøla, 22 Kjaglielva, 24 Mistra, 28 Lomma
# skal fære **med** miljøgifter i 2018

library(tidyverse)  # filter, group_by, summary, etc.
library(readxl)     # read_excel()
library(rkt)        # rkt() - computes the Mann-Kendall tests and Theil-Sens slope estimator
library(maps)       # Norway map
library(viridis)    # scale_fill_viridis()
library(lubridate)  # ymd_hm(), year(), month()
library(cowplot)    # plot_grid()

map_norway <- map_data("world", "Norway")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
#  ---- Station data 2017 ---- 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# 2017 stations (plus planned 2018 stations)
df_stations2017 <- read_excel("Input_2017data/Referanseelver_rapportnavn_2018.xlsx")
#  Coordinate data ---- 
df_stations2 <- read_excel("Input_2017data/Stasjonsoversikt_2018_01_12.xlsx")
df_stations2$Lat <- as.numeric(df_stations2$Breddegrad)
df_stations2$Lon <- as.numeric(df_stations2$Lengdegrad)
colnames(df_stations2)[1] <- "Ecoregion"

df_stations2017 <- df_stations2017 %>% 
  left_join(df_stations2[,c("Aquamonitor St. kode", "Lat", "Lon", "Prøvetakingsår", "Miljøgifter")],
            by = c("Aquamonitor_stasjonskode" = "Aquamonitor St. kode")
  ) %>%
  select(`Øko-region`, Kommune, Aquamonitor_stasjonskode, Rapportnavn, Lat, Lon, Prøvetakingsår, Miljøgifter)
xtabs(~Prøvetakingsår, df_stations2017)

df_stations2017 %>% filter(Prøvetakingsår %in% "Hvert år") %>% pull(Rapportnavn)
# [1] "42. Døråe (Ø)"  "43. Atna04 (Ø)" "44. Atna03 (Ø)" "45. Atna11 (Ø)"

xtabs(~addNA(Miljøgifter), df_stations2017)
df_stations2017 %>% filter(Miljøgifter %in% "X") %>% pull(Rapportnavn)
# [1] "05. Rotsund (N)"        "06. Flakstadvåg (N)"    "08. Kobbvåg (N)"        "12. Eiteråga (M)"       "15. Sanddøla (M)"      
# [6] "23. Størdalselva (M)"   "37. Lislefjøddåi (S)"   "39. Rørholtfjorden (S)" "41. Molandsåna (S)"     "42. Døråe (Ø)"         


df_stations2017 %>% filter(Miljøgifter %in% "-") %>% pull(Rapportnavn)
# [1] "01. Skillefjordelva (F)" "02. Kobbholet (F)"       "03. Rostaelva (F)"       "04. Divielva (F)"        "07. Mammakjosen (N)"    
# [6] "09. Kongsvikosen (M)"    "10. Gjeddåga (M)"        "11. Simskardelva (M)"    "13. Susna (M)"           "14. Imsa (M)"           
# [11] "16. Luru (M)"            "17. Homla (M)"           "18. Nordåa (M)"          "19. Nordfolda (M)"       "20. Nødalselva (M)"     
# [16] "21. Bolåselva (M)"       "22. Leiråa (M)"          "24. Breineset (M)"       "25. Underdalselvi (V)"   "26. Kalstadelva (V)"    
# [21] "27. Hildalselvi (V)"     "28. Hålandselva (V)"     "29. Øydgardselva (V)"    "30. Skjeggedalsåna (S)"  "31. Vatnedalselva (S)"  
# [26] "32. Geiskeliåni (S)"     "33. Berdalsbekken (S)"   "34. Aslestadåi (S)"      "35. Daleåa (S)"          "36. Vesterdalsåni (S)"  
# [31] "38. Farsjø (S)"          "40. Sandvatn (S)"        "43. Atna04 (Ø)"          "44. Atna03 (Ø)"          "45. Atna11 (Ø)"         
# [36] "46. Leppa (Ø)"           "47. Rørvannet (Ø)"      

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
#  ---- Station data 2018 ---- 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

stations_chem <- c("01. Stabburselva (F)", "05. Komagelva (F)",
                    "07. Láhpojohka (F)", "08. Sametielva (F)", "11. Smeddalselvi (V)", "12. Raundalselva (V)", 
                    "15. Utla (V)", "18. Smådøla (Ø)", "22. Kjaglielva (Ø)", "24. Mistra (Ø)", "28. Lomma (Ø)")

df_stations2018 <- read_excel("Input_2018data/Stasjonsoversikt 2018 løpende oppdatert_.xlsx") %>%
  mutate(Lat = as.numeric(Breddegrad), Lon = as.numeric(Lengdegrad), 
         Prøvetakingsår = "2018",
         Miljøgifter = ifelse(`Kortnavn/Rapportnavn` %in% stations_chem, "X", "-")) %>%
  rename(Aquamonitor_stasjonskode = `Aquamonitor stasjonskode`,
         Rapportnavn = `Kortnavn/Rapportnavn`) %>%
  filter(!Rapportnavn %in% c("31. Døråe (Ø)", "32. Atna03 (Ø)", "33. Atna04 (Ø)", "34. Atna11 (Ø)")) %>%  # already in 2017 data
  filter(!is.na(Rapportnavn)) %>%
  select(`Øko-region`, Kommune, Aquamonitor_stasjonskode, Rapportnavn, Lat, Lon, Prøvetakingsår, Miljøgifter)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Combine data and plot ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Combine
df_stations <- bind_rows(df_stations2017, df_stations2018)

# Setting new factor names
df_stations$Miljøgifter2 <- factor(df_stations$Miljøgifter, levels = c("X","-"), labels = c("Med","Uten"))
levels(df_stations$Miljøgifter2)

# Setting "Kategori"
sel <- with(df_stations, Prøvetakingsår %in% "2017" & Miljøgifter2 %in% "Med")
df_stations$Kategori <- "Oddetallsår, med miljøgifter"
sel <- with(df_stations, Prøvetakingsår %in% "2017" & Miljøgifter2 %in% "Uten")
df_stations$Kategori[sel] <- "Oddetallsår, uten miljøgifter"
sel <- with(df_stations, Prøvetakingsår %in% "Hvert år" & Miljøgifter2 %in% "Med")
df_stations$Kategori[sel] <- "Hvert år, med miljøgifter"
sel <- with(df_stations, Prøvetakingsår %in% "Hvert år" & Miljøgifter2 %in% "Uten")
df_stations$Kategori[sel] <- "Hvert år, uten miljøgifter"
sel <- with(df_stations, Prøvetakingsår %in% "2018" & Miljøgifter2 %in% "Med")
df_stations$Kategori[sel] <- "Partallsår, med miljøgifter"
sel <- with(df_stations, Prøvetakingsår %in% "2018" & Miljøgifter2 %in% "Uten")
df_stations$Kategori[sel] <- "Partallsår, uten miljøgifter"
# dput(names(table(df_stations$Kategori)))
df_stations$Kategori <- factor(df_stations$Kategori, 
                               levels = c("Oddetallsår, med miljøgifter", "Oddetallsår, uten miljøgifter", 
                                          "Partallsår, med miljøgifter", "Partallsår, uten miljøgifter",
                                          "Hvert år, med miljøgifter", "Hvert år, uten miljøgifter"))
table(df_stations$Kategori)


df <- df_stations %>% filter(Prøvetakingsår %in% c("2017","2018","Hvert år"))
x <- as.numeric(df$Prøvetakingsår %in% "Hvert år")
df <- df[order(x),]
View(df)


gg <- ggplot(df,              
             aes(Lon, Lat, shape = Kategori, fill = Kategori, color = Kategori)) + 
  annotation_map(map_norway, fill = "grey60", color = "black") +
  geom_point(size = 3, stroke = 1.5) +
  scale_shape_manual(values = c(21, 21, 23, 23, 24, 24)) +
  scale_fill_manual(values = c('yellow2','yellow2','purple3','purple3','deeppink1','deeppink1')) +
  scale_color_manual(values = c('black','white','black','white','black','white')) +
  coord_map("lambert", parameters = c(64, 12)) +
  theme_bw() + 
  labs(x = "", y = "") +
  theme(legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.key = element_rect(fill = "grey60"))
gg

ggsave("Figures_2019data/05_Overview_map.png", gg, width = 8, height = 8, dpi = 500)

openxlsx::write.xlsx(df, "Figures_2019data/05_Overview_map.xlsx")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Check  ----
# Check that rivers are the same
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

df_2019 <- readxl::read_excel("Input_2019data/Stations and catchment data all rivers.xlsx", sheet = 1) 

# Too many FALSE:
# sel <- df_2019$Rapportnavn %in% df$Rapportnavn

sel1 <- df_2019$Kode %in% df$Aquamonitor_stasjonskode
mean(sel1) # 90 %
df_2019$Kode[!sel1] %>% sort()
# df_2019[!sel1,]

sel2 <- df$Aquamonitor_stasjonskode %in% df_2019$Kode
mean(sel2) # 90 %
df$Aquamonitor_stasjonskode[!sel2] %>% sort()
# df[!sel2,]


# "F_234_229_Más"       "N_161-227_Gje"       "N_177-16_Kon"        "O_002-218_Teg\r\n"   "O_002-305_Atn_DAN04" 
# "O_002-305_Atn_DAN11"  "V_050-82_Bjo\r\n"    "V_074-178_Utl\r\n"  

# [1] "F_234-229_Más"   "M_161-227_Gje"   "M_177-16_Kon"    "O_002-218_Teg"   "O_002-305_Atn11" 
# "O_002-305_Atn4"  "V_050-82_Bjo"    "V_074-178_Utl"  


