# Working directory
setwd("your/working/directory") # Adjust to your working directory, so the whole project can be run from the root folder and the paths to data and database are consistent across functions

# Libraries
library(pacman)
p_load(tidyverse, nanoparquet, DBI, RSQLite, data.table, countrycode)

# 1a. Routes and load data
comtrade_path <- "data/chips_trade_2026.sqlite"
patents_path <- "data/Google-patents/patents_summary_2026.parquet"
ilostat_path <- "data/ILOSTAT/ilostat_data.parquet"
oecdfdi_path <- "data/OECD/oecd_data.parquet"
comtrade_cols <- c("period", "reporter_desc", "flow_desc", "partner_desc", "cmd_desc", "primary_value", "fobvalue", "cifvalue")
cols_query <- paste0(comtrade_cols, collapse = ", ")

# 1b. Create 'total' table in comtrade_path, binding rows from tables 'exports' and 'imports' in path 
con <- dbConnect(SQLite(), comtrade_path) 
query <- paste0(
            "CREATE TABLE total AS ", 
            "SELECT ", cols_query, " FROM exports ",
            "UNION ALL ",
            "SELECT ", cols_query, " FROM imports"
          )
dbExecute(con, query) # --------  IMPORTANT: If table already created, skip this query. IF NOT, EXECUTE THIS TO CREATE THE 'total' TABLE IN THE DATABASE, WHICH COMBINES IMPORTS AND EXPORTS DATA.

# 2. Load unique COMTRADE data, inspect and summarize into a new table 'comtrade_sum'
com_data <- dbReadTable(con, "total")
com_sum <- com_data %>%
  # Group by year, country reporting data and the type of flow they report (import or export)
  group_by(period, reporter_desc, flow_desc) %>%
  summarise(
    count = n(), # Count how many individual imports and exports partners each year
    total_val = sum(primary_value, na.rm = TRUE), # Calculate total value of imports and exports each year
    .groups = "drop"
  ) %>%
  # Pivot to have import and export values side by side
  pivot_wider(
    names_from = flow_desc,
    values_from = c(count, total_val),
    values_fill = 0
  ) %>%
  #  Formulas to calculate 'import dependency' based on FAO's formula for cereal dependency and net value of CHIPS trade
  mutate(
    dependency = ((count_Import - count_Export) / count_Import) * 100,
    net_value = total_val_Export - total_val_Import,
    reporter_desc = case_when(
      reporter_desc == "Rep. of Korea" ~ "South Korea",
      TRUE ~ reporter_desc
    )
  )


# 3. Load patents data and merge into a final dataset with previous data
pats <- read_parquet(patents_path) %>%
  mutate(reporter_desc = countrycode(country_code, "iso2c", "country.name.en"),

         # Manual correction for these two international offices         
         reporter_desc = case_when(
           country_code == "WO" ~ "World Intellectual Property Organization",
           country_code == "EP" ~ "European Patent Office",
           country_code == "US" ~ "USA", # This is particular to the USA, as it comes with this name from COMTRADE
           TRUE ~ reporter_desc  # Keep countrycode assigned name for other values
         )
        ) %>%
  select(-country_code)

# 4. Load employment and population data
ilo <- read_parquet(ilostat_path) %>%
  mutate(reporter = countrycode(ref_area, "iso3c", "country.name.en"),
         reporter = case_when(
           reporter == "United States" ~ "USA",
           TRUE ~ reporter
         ),
         indicator = case_when(
           indicator == "Working-age population by TOTAL sex and age (thousands)" &
            classif1 == "AGE_AGGREGATE_TOTAL" ~ "Working-age population TOTAL (thousands)",
           indicator == "Employment by TOTAL sex, education and economic activity - ISIC level 2 (thousands)" &
            classif1 == "EDU_AGGREGATE_ADV" ~ "Employed population in chips manufacture (ISIC2 = C26) with ADVANCED education (thousands)",
           indicator == "Employment by TOTAL sex, education and economic activity - ISIC level 2 (thousands)" &
            classif1 %in% c("EDU_AGGREGATE_BAS", "EDU_AGGREGATE_INT") ~ "Employed population in chips manufacture (ISIC2 = C26) with BASIC or INTERMEDIATE education (thousands)",
           indicator == "Employment by TOTAL sex, education and economic activity - ISIC level 2 (thousands)" &
             classif1 == "EDU_AGGREGATE_TOTAL" ~ "Employed population in chips manufacture (ISIC2 = C26) TOTAL (thousands)"
         ),
         time = as.double(time)) %>%
  filter(!is.na(indicator)) %>%
  select(reporter, period = time, indicator, value = obs_value) %>%
  group_by(reporter, period, indicator) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  mutate(
    `Percentage of working-age population involved in chips manufacture (ISIC = C26)` = 
      (`Employed population in chips manufacture (ISIC2 = C26) TOTAL (thousands)` / `Working-age population TOTAL (thousands)`) * 100
  )

# 5. Load foreign direct investment (FDI) data
fdi <- read_parquet(oecdfdi_path) %>%
  mutate(reporter = countrycode(ref_area, "iso3c", "country.name.en"),
         reporter = case_when(
           reporter == "United States" ~ "USA",
           TRUE ~ reporter
         ),
         indicator = case_when(
           unit == "US dollars millions, exchange rate converted" &
             measure == "FDI yearly financial flows" ~ paste("FDI yearly flows", direction, "in USD millions"),
           unit == "US dollars millions, exchange rate converted" &
             measure == "FDI yearly stock" ~ paste("FDI yearly stock", direction, "in USD millions")
          ),
         period = as.double(period)) %>%
  filter(!is.na(indicator) & !is.na(value)) %>%
  select(reporter, period, indicator, value) %>%
  pivot_wider(names_from = indicator, values_from = value)

# 6. Combine into final data and include a table in the sql file called 'trade-and-patents'
final_data <- pats %>%
  select(period = pub_year,
         reporter = reporter_desc,
         total_somehow_h10_pats = total_unique_patents,
         total_mainfield_h10_pats = total_mainfield_h10,
         total_globalfam_h10_pats = global_h10,
         total_foreignowned_h10_pats = foreign_owned_patents) %>%
  left_join((com_sum %>% select(period, 
                                reporter = reporter_desc,
                                trade_partners_balance = dependency,
                                trade_net_value = net_value)), 
            by = c("period" = "period", "reporter" = "reporter")) %>%
  left_join(ilo, by = c("period" = "period", "reporter" = "reporter")) %>%
  left_join(fdi, by = c("period" = "period", "reporter" = "reporter"))

dbWriteTable(con, "trade_and_patents", final_data, overwrite = TRUE)
write_csv(final_data, "data/final_output.csv") # We also save the final dataset as a .csv file, in case we want to use it outside of R or SQL, or just to have a backup of the data in a more accessible format.
dbListTables(con)
dbDisconnect(con)
message("Database updated and connection closed. Processing and merging of data concluded")
