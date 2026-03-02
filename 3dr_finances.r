# Working directory - Make sure to adjust the working directory to the root of your project
setwd("/your/working/directory") # Adjust to your working directory, so the whole project can be run from the root folder and the paths to data and database are consistent across functions

# Libraries
library(pacman)
p_load(tidyverse, readsdmx, countrycode, nanoparquet)

# Definitions, data downloaded from OECD - Use OECD data explorer to find the relevant dataset and dimensions, then paste here the URL for the SDMX API call.
url_fdi_long <- "https://sdmx.oecd.org/public/rest/data/OECD.DAF.INV,DSD_FDI@DF_FDI_CTRY_IND_SUMM,/AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA.LE_FA_F+T_FA_F.USD_EXC.DI+DO.....W..C26.A.?startPeriod=2005&endPeriod=2024&dimensionAtObservation=AllDimensions"

# Do call and tidy
fdi_data <- read_sdmx(url_fdi_long) %>%
  select(ref_area = REF_AREA,
         period = TIME_PERIOD,
         unit = UNIT_MEASURE,
         measure = MEASURE,
         direction = MEASURE_PRINCIPLE,
         value = ObsValue) %>%
  mutate(
    measure = case_when(
      measure == "T_FA_F" ~ "FDI yearly financial flows",
      measure == "LE_FA_F" ~ "FDI yearly stock",
      TRUE ~ measure
    ),
    unit = case_when(
      unit == "USD_EXC" ~ "US dollars millions, exchange rate converted",
      TRUE ~ unit
    ),
    direction = case_when(
      direction == "DI" ~ "inward",
      direction == "DO" ~ "outward",
      TRUE ~ direction
    )
  )

# Write oecd fdi_data into a parquet, for efficient storage and later use in the analysis notebook.
write_parquet(fdi_data, "data/OECD/oecd_data.parquet")