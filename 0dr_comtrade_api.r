# Set wd
setwd("/your/working/directory") # Adjust to your working directory, so the whole project can be run from the root folder and the paths to data and database are consistent across functions

# Load libraries
library(pacman)
p_load(tidyverse, dplyr, comtradr, httr, countrycode, RSQLite, DBI)

# HS codes and descriptions
string_hs <- "8542" # Harmonization System (HS) code for: "Electronic integrated circuits; parts thereof" (https://www.tariffnumber.com/2026/8542). You can modify this to include more codes, but keep in mind that the more codes you include, the more API calls you will need to make, and the longer it will take to retrieve the data.
hs_codes <- comtradr::ct_get_ref_table('HS') %>%
  filter(str_starts(id, string_hs)) # Starts with 8542, HS codes follow a 'cascade' classification structure, retrieving every HS code associated with the main code of interest 8542

# Setting parameters for systematic API call in UN COMTRADE (UNCT),
# which bypasses daily download restriction through multiple tokens from accounts that support the project
comtrade_path <- "data/COMTRADE/" # Make sure 'data' folder exists in your working directory
cmdCodes <- hs_codes$id # Extract just codes
foci <- c("USA", # The USA and the EU countries as of 2026
                   "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
                   "Czechia", "Denmark", "Estonia", "Finland", "France",
                   "Germany", "Greece", "Hungary", "Ireland", "Italy",
                   "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
                   "Taiwan, Province of China", "Japan", "China", "Rep. of Korea") # Later, important Asian countries included
                   # Taiwan and South Korea weren't being recognized, the names inside comtradr are as presented above.
                   # Taiwan is available in comtradr only as partner, not reporter
countries <- comtradr::country_codes %>% # Include only US and EU countries
    filter(country %in% foci) %>%
    filter(is.na(exit_year) & group == FALSE & reporter == TRUE) %>% # Filter cases (countries) that no longer exist, are group of countries and are available as reporters
    pull(iso_3) %>% # Extract only ISO3 code for API call
    unique() # Unique to avoid repeating calls
partners <- comtradr::country_codes %>% 
  # Filter for active countries/territories
  filter(is.na(exit_year) & group == FALSE & partner == TRUE) %>% 
  # Keep only ISO3 codes consisting of exactly 3 letters
  filter(stringr::str_detect(iso_3, "^[A-Za-z]{3}$")) %>% 
  pull(iso_3) %>% 
  unique() # This extracts all valid partner codes in the COMTRADE database, so every trade interaction with the foci countries is captured, but you can modify according to your needs.
period <- 2000:2026 # Here you can adjust the years you want to retrieve, but keep in mind that the more years you include, the more API calls you will need to make, and the longer it will take to retrieve the data.
comtrade_cols <- c("period", "reporter_desc", "flow_desc", "partner_desc", "cmd_desc", "primary_value", "fob_value") # I used only these columns, but more are provided through the API. Exploration is encouraged.

# API keys to make the calls - Originally i had 3 keys, but asked for people to create free accounts to UN COMTRADE's API and share their keys for the development of this project
apikey1 <- "" # Free suscription to UN COMTRADE's API
apikey2 <- "" # Alternative free suscription to UN COMTRADE's API
apikey3 <- "" # Alternative free suscription to UN COMTRADE's API
apikey4 <- "" # Alternative free suscription to UN COMTRADE's API
apikey5 <- "" # Alternative free suscription to UN COMTRADE's API

# 1. Define synchronized time blocks
# This ensures that 2016-2026 is captured even if it's shorter than 12 years
starts <- seq(from = min(period), to = max(period), by = 12)
ends   <- starts + 11
ends[ends > max(period)] <- max(period) # Caps the last year at 2026

time_blocks <- data.frame(start = starts, end = ends)

# 2. Create combinations using a block index instead of independent dates
combinations <- expand.grid(
  reporter = split(countries, ceiling(seq_along(countries) / 6)),
  partner = split(partners, ceiling(seq_along(partners) / 6)),
  commodity_code = split(cmdCodes, ceiling(seq_along(cmdCodes) / 8)),
  time_idx = 1:nrow(time_blocks),
  flow_direction = c("import", "export")
) %>%
  # Sync dates based on the time_idx
  mutate(
    start_date = time_blocks$start[time_idx],
    end_date = time_blocks$end[time_idx]
  ) %>%
  # Optional: Keep the reverse order if you prefer downloading newer data first
  # We use arrange() instead of rev() to maintain logical integrity across rows
  arrange(desc(start_date)) %>%
  select(-time_idx)
            

# AUX FUNCTIONS
# Function to make API calls iteratively using comtradr and save results to CSV files
make_api_calls <- function(start_comb = 1, keys = c(apikey1, apikey2, apikey3)) {
    total_combinations <- nrow(combinations)
    num_keys <- length(keys)
    key_index <- 1  # Start with the first key
    api_call_counter <- 0  # Initialize call counter

    for (comb in start_comb:total_combinations) {
        # Extract parameters for the current combination
        current_reporters <- combinations$reporter[[comb]]
        current_cmd_codes <- combinations$commodity_code[[comb]]
        start_date <- combinations$start_date[[comb]]
        end_date <- combinations$end_date[[comb]]
        flow <- combinations$flow_direction[comb]
        combs_remaining <- total_combinations - comb

        # Switch the key after every 499 calls
        if (api_call_counter %% 499 == 0 && api_call_counter > 0) {
            key_index <- (key_index %% num_keys) + 1  # Move to the next key
            message("Switching to new key: Key ", keys[key_index])
        }
        set_primary_comtrade_key(keys[key_index])

        # Make the API call
        message("Making API call ", comb, " of ", total_combinations,
                " for flow: ", flow,
                ", reporters: ", paste(current_reporters, collapse = ", "),
                ", commodities: ", paste(current_cmd_codes, collapse = ", "),
                ", period: ", start_date, "-", end_date, " using key ", key_index)
        message("API calls remaining:", combs_remaining)
        api_call_counter <- api_call_counter + 1

        data <- tryCatch(
            ct_get_data(
                reporter = current_reporters,
                partner = combinations$partner[[comb]],
                commodity_code = current_cmd_codes,
                start_date = start_date,
                end_date = end_date,
                flow_direction = flow
            ),
            error = function(e) {
                if (grepl("Waiting \\d+s for retry backoff", e$message)) {
                    wait_time <- (as.numeric(gsub(".*Waiting (\\d+)s for retry backoff.*", "\\1", e$message))) + 600 # Wait for how long the platform states + 10 minutes just in case
                    message("Waiting for ", wait_time, " seconds before retrying...")
                    Sys.sleep(wait_time)
                    data <- ct_get_data(
                        reporter = current_reporters,
                        partner = "all_countries",
                        commodity_code = current_cmd_codes,
                        start_date = start_date,
                        end_date = end_date,
                        flow_direction = flow
                    )
                } else {
                    message("Error: ", e$message)
                    stop(e)
                }
            }
        )


        # Save the results to a CSV file
        if (!is.null(data)) {
            file_name <- paste0(comtrade_path, flow, "_", comb, ".csv")
            write.csv(data, file_name, row.names = FALSE)
            message("Data saved to ", file_name)
        }
    }
}
 

# Function to integrate data with error logging
integrate_data_sqlite <- function() {
  input_path <- "data/COMTRADE"
  db_path <- "data/chips_trade_2026.sqlite"
  
  ignored_log <- list()
  
  message(">>> Starting data integration into SQLite...")
  
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  all_files <- list.files(input_path, pattern = "\\.csv$", full.names = TRUE)
  total_files <- length(all_files)
  
  if (total_files == 0) {
    dbDisconnect(con)
    stop("No CSV files found in the specified directory.")
  }
  
  for (i in seq_along(all_files)) {
    f <- all_files[i]
    file_name <- basename(f)
    
    message(sprintf("[%d/%d] Processing file: %s", i, total_files, file_name))
    
    temp_data <- read_csv(f, show_col_types = FALSE)
    
    # 1. VALIDATION: Use normal assignment '=' or '<-' for local logging
    if (!"reporter_desc" %in% colnames(temp_data)) {
      message(paste("!!! File ignored due to missing columns:", file_name))
      ignored_log[[file_name]] <- "File missing reporter_desc column (possible API error)"
      next 
    }
    
    table_name <- ifelse(grepl("import", file_name, ignore.case = TRUE), "imports", "exports")
    
    tryCatch({
      dbWriteTable(con, table_name, temp_data, append = TRUE)
    }, error = function(e) {
      message(paste("!!! Skipping file due to error:", file_name))
      # 2. Here we use '<<-' to reach 'ignored_log' which is one level up
      ignored_log[[file_name]] <<- conditionMessage(e)
    })
  }
  
  message(">>> Creating indexes for optimized querying...")
  
  # 3. SAFETY: Only create the index if the table exists and has data
  if (dbExistsTable(con, "imports")) {
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_imp_reporter ON imports (reporter_desc)")
  }
  if (dbExistsTable(con, "exports")) {
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_exp_reporter ON exports (reporter_desc)")
  }
  
  dbDisconnect(con)
  
  if (length(ignored_log) > 0) {
    message(sprintf("\n>>> Integration complete. Total files ignored: %d", length(ignored_log)))
    return(ignored_log) 
  } else {
    message(">>> Integration complete. All files processed successfully.")
    return(NULL)
  }
}


## DATA RETRIEVAL

# Make API calls and integrate the data
# make_api_calls(start_comb = 1, keys = c(apikey1, apikey2, apikey3)) # Fill arguments with as many keys as you have declared before
# integrate_data_sqlite()