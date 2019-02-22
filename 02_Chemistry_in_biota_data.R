
# 1. Libraries ----

library(dplyr)
library(readxl)
library(niRvana)

# 2. Data ----

df_stationmeta <- read_excel("Input_2018data/Stasjonsoversikt 2018 løpende oppdatert_.xlsx", sheet = 1) %>% 
  rename(STATION_CODE = `Aquamonitor stasjonskode`,
         Rapportnavn = `Kortnavn/Rapportnavn`)

# Fix one
sel <- df_stationmeta$STATION_CODE %in% "F_212-1729_Lah"; sum(sel) 
df_stationmeta$STATION_CODE[sel] <- "F_212-1729_Láh"

# 3. Chemical data ----

set_credentials()

# Get a list of projects and the stations in the project
df_projects <- get_projects()  
df_stations <- get_stations_from_project("Overvåkning i referanseelver", ignore.case = FALSE)

# Get all specimens collected at these stations (20 seconds or so)
df_specimens <- get_specimens_from_stationdata(df_stations)
# str(df_specimens)

# Get method definition table (for the chemical methods)
df_methods <- get_nivabase_data("select METHOD_ID, NAME, UNIT, BASIS_ID from NIVADATABASE.METHOD_DEFINITIONS")

# Get data frame of chemical data in biota for all samples from a given measurement year (30 seconds or so)
df_chem_allcolumns <- get_biota_chemistry(
  years = 2018, 
  specimendata = df_specimens, 
  stationdata = df_stations,
  methoddata = df_methods, 
  report_samples = TRUE)

# We pick the most impoartan columns (i.e. we skip the index variables) 
df_chem <- df_chem_allcolumns %>%
  select(STATION_CODE, STATION_NAME, SAMPLE_DATE, TISSUE_NAME, LATIN_NAME,
         SAMPLE_NO, REPNO, NAME, VALUE, FLAG1, UNIT)

table(df_chem$SAMPLE_DATE)

# Only 2 dates: 2018-08-15, 2018-09-16

table(df_chem$STATION_CODE)

# 4. Add Rapportnavn / station metadata ----
nrow(df_chem)
df_chem <- df_chem %>%
  left_join(df_stationmeta %>% select(STATION_CODE, Rapportnavn, Lengdegrad, Breddegrad),
            by = "STATION_CODE")
nrow(df_chem)  # 3778

# Check whether there are lacking Rapportnavn
sel <- is.na(df_chem$Rapportnavn); sum(sel)
table(df_chem$STATION_CODE[sel])  #  S_017-17_Ror S_017-196_Far
table(df_chem$STATION_NAME[sel])
# These are not in the Excel file (df_stationmeta); they have the wrong date and vbelong to 2017: 
# Farsjø bekkefelt Rørholtfjorden bekkefelt 
#              377                      337 

# Remove Farsjø bekkefelt and Rørholtfjorden bekkefelt
df_chem <- df_chem[!sel,]
nrow(df_chem)  # 3064

# 5. Make sum variables ----

sumvars_check <- function(vars, data = df_chem){
  check <- data %>% 
    filter(NAME %in% pars & !is.na(VALUE)) %>%
    group_by(STATION_CODE, STATION_NAME, SAMPLE_DATE, TISSUE_NAME, LATIN_NAME,
             SAMPLE_NO, REPNO, NAME, UNIT)
  xtabs(~STATION_CODE + SAMPLE_NO, check)
}

sumvars_calc <- function(vars, data = df_chem){
  df_chem %>% 
    filter(NAME %in% pars & !is.na(VALUE)) %>%
    group_by(STATION_CODE, STATION_NAME, SAMPLE_DATE, TISSUE_NAME, LATIN_NAME,
             SAMPLE_NO, REPNO, UNIT,
             Rapportnavn, Lengdegrad, Breddegrad)  %>%
    summarize(VALUE = sum(VALUE), N = n(), OverLOQ = sum(is.na(FLAG1)))
}


## a. PBDE

pars <- c("BDE28", "BDE47", "BDE99", "BDE100", "BDE153", "BDE154")
sumvars_check(pars)

df_sum <- sumvars_calc(pars)
xtabs(~STATION_CODE + SAMPLE_NO, df_sum)
xtabs(N ~ STATION_CODE + SAMPLE_NO, df_sum)
xtabs(OverLOQ ~ STATION_CODE + SAMPLE_NO, df_sum)

df_sum$NAME <- "BDE6S"
df_sum$FLAG1 <- with(df_sum, ifelse(OverLOQ < 0.5*N, "<", as.character(NA)))
df_sum$OverLOQ <- NULL
df_sum$N <- NULL

# Add to dataset
if (!"BDE6S" %in% unique(df_chem$NAME))
  df_chem <- bind_rows(df_chem, df_sum)

nrow(df_chem)  # 3085

# 6. Save result ----
saveRDS(df_chem_allcolumns, "2018data/02_df_chem_allcolumns.rds")
saveRDS(df_chem, "2018data/02_df_chem.rds")


  



