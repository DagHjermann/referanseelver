
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# READING "WIDE" AQUAMONITOR DATA ----
#
# Copied from '12_QA_2019_from_excel_functions.R'
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

AqMexport_read_waterchemistry <- function(filename, reformat_long = TRUE, remove_duplicates = TRUE, sheetname = "WaterChemistry"){
  # Read top line, only for the column names
  df_names <- read_excel(filename, sheet = sheetname, col_names = TRUE, n_max = 1)
  # Read data. All data is read as text (strings) and later converted, due to the "<" signs
  df_chem <- read_excel(filename, sheet = sheetname, col_names = FALSE, skip = 2, col_types = "text")
  # Set column names (copy them from df_names)
  if (ncol(df_names) == ncol(df_chem)){
    names(df_chem) <- names(df_names)
  } else {
    cat("Not same number of columns!\n")
  }
  # Convert these variables to numeric
  #for (col in c("ProjectId", "SampleDate", "Time", "Depth1", "Depth2")){
  #  df_chem[[col]] <- as.numeric(df_chem[[col]])
  #}
  # UNIX time
  # We just overwrite Time with UNIX time, which is usually just 00:00 anyway
  # df_chem$Time <- as.POSIXct((df_chem$SampleDate - 25569)*24*3600, origin = "1970-01-01", tz = "GMT")
  
  # Reformat data to long/narrow format (default option)
  if (reformat_long)
    df_chem <- AqMexport_reformat_long(df_chem, remove_duplicates = remove_duplicates)
  
  df_chem 
}

# Note hard-coded columns "ProjectId:Depth2"
AqMexport_reformat_long <- function(dat, remove_duplicates = FALSE){
  dat_long <- dat %>%
    pivot_longer(cols = -c(StationId:Måned), names_to = "Variable", values_to = "Value_chr") 
  # Get numeric data values
  x <- sub(",", ".", dat_long$Value_chr, fixed = TRUE)
  x <- sub(",", ".", x, fixed = TRUE)
  dat_long$Value <- as.numeric(sub("<", "", x))
  # Make less-than flag
  dat_long$Flag <- ifelse(grepl("<", dat_long$Value_chr, fixed = TRUE), "<", NA)
  # Get rid of duplicates (several observations, as there are several projects and each value isrepeated for each project)
  if (remove_duplicates)
    dat_long <- AqMexport_remove_duplicates(dat_long)
  dat_long
}

AqMexport_remove_duplicates <- function(dat){
  dat %>%
    filter(!is.na(Value)) %>%
    group_by(StationId, StationCode, StationName, Time, Depth1, Depth2, Variable) %>%
    summarise(ProjectId = paste(ProjectId, collapse = ","),
              ProjectName = paste(ProjectName, collapse = "; "),
              Value = first(Value), 
              Flag = first(Flag)) %>%
    ungroup()
}
