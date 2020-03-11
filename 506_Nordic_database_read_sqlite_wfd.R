
install.packages("RSQLite")

library(DBI)
con <- dbConnect(RSQLite::SQLite(), "K:/Prosjekter/EEA ETC-ICM/EEA 2019/Task 1511 WFD/Data/wise-wfd-database_v01_r03/WISE_SOW.sqlite")

# Liste tabeller
dbListTables(con)

# Liste kolonner for en tabell
dbListFields(con, "SOW_SWB_SurfaceWaterBody")

# Vis 5 første norske vannforekomster
res <- dbSendQuery(con, "SELECT * FROM SOW_SWB_SurfaceWaterBody WHERE countryCode = 'NO' LIMIT 5")
dbFetch(res)

# Data for tre vannforekomster
res <- dbSendQuery(con, "SELECT * FROM SOW_SWB_SurfaceWaterBody WHERE euSurfaceWaterBodyCode IN ('NO002-252-L', 'NO002-3497-L', 'NO002-5013-L')")
dbFetch(res)

# eller
my_water_bodies <- c("NO002-252-L", "NO002-3497-L", "NO002-5013-L")

options(useFancyQuotes = FALSE)

sql <- paste0(
  "SELECT * FROM SOW_SWB_SurfaceWaterBody WHERE euSurfaceWaterBodyCode IN (",
  paste(sQuote(my_water_bodies), collapse = ","),
  ")"
)
res <- dbSendQuery(con, sql)
dbFetch(res)


