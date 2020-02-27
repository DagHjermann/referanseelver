
#
# Script based on "01_Make_map.R" in "C:\Data\Referanseelver_2018\06_Overview_maps" (used the first year)
#


# SKAL FIKSES!!!
# 01 Stabburselv, 05 Komagelv, 07 Lapjohhka, 08 Sametielva, 11 Smeddalselva, 12 Raundalselva, 15 Utla, 18 Smådøla, 22 Kjaglielva, 24 Mistra, 28 Lomma
# skal fære **med** miljøgifter i 2018

library(dplyr)      # filter, group_by, summary, etc.
library(ggplot2)
library(readxl)     # read_excel()
library(maps)       # Norway map

map_norway <- map_data("world", "Norway")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
#  ---- Station data ---- 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

df_stations <- read_excel("Input_2019data/Referanseelver_stasjoner_per_2019.xlsx")
  
table(df_stations$Fortsatt_med)

df <- df_stations %>%
  filter(Fortsatt_med %in% "ja") %>%       # 7 statins no longer included
  arrange(Prøvetakingsår %in% "Hvert år")  # Put 'Hvert år' last so they end up on top in map


#
# Save file ----
# 
# NOTE: this file was updated and subsequently put in 'Input_2019data'
#       That file is the one we use in the update of this script,
#       script 15 (used for 2019 report)
#
# openxlsx::write.xlsx(df_stations, "Referanseelver_stasjoner_per_2019.xlsx")


#
# Plot ----
#


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

ggsave("Figures_2019data/15_Overview_map.png", gg, width = 8, height = 8, dpi = 500)

