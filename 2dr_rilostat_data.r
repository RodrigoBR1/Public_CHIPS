# Working directory - Make sure to adjust the working directory to the root of your project
setwd("/your/working/directory") # Adjust to your working directory, so the whole project can be run from the root folder and the paths to data and database are consistent across functions

# Libraries
library(pacman)
p_load(Rilostat, tidyverse, countrycode, nanoparquet)

# Countries of interest - Adjust the list of countries according to your needs. 
foci <- c("USA", # The USA and the EU countries as of 2026
                   "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
                   "Czechia", "Denmark", "Estonia", "Finland", "France",
                   "Germany", "Greece", "Hungary", "Ireland", "Italy",
                   "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
                   "Taiwan, Province of China", "Japan", "China", "Rep. of Korea") # Later, important Asian countries included
                   # Taiwan and South Korea weren't being recognized, the names inside comtradr are as presented above.

# Transform into ISO3 codes - This ensures data retrieval from Rilostat is consistent, as the datasets use ISO3 codes for country identification.
foci_iso3 <- countrycode(foci, origin = "country.name", destination = "iso3c")

# List of datasets to download
sets <- c("EMP_TEMP_SEX_EDU_EC2_NB_A", # Employment by sex, education and economic activity - ISIC level 2 (thousands)
          "EMP_STEM_SEX_EDU_NB_A", # Employment in STEM occupations by sex and education (thousands)
          "EES_STEM_SEX_INS_NB_A", # Employees in STEM occupations by sex and public/private sector (thousands)
          "POP_XWAP_SEX_AGE_NB_A") # Working-age population by sex and age (thousands)

# Get ilo ToC with the datasets of interest
ilo <- get_ilostat_toc() %>%
        filter(id %in% sets)

# Download loop for each dataset in the list
for (i in seq_along(sets)) {
  # Generate name (A=65 in ASCII, B=66, etc.)
  var_name <- paste0("dat_", LETTERS[i])

  # Download and filter, then assign
  tmp <- get_ilostat(id = sets[i]) %>%
    filter(ref_area %in% foci_iso3)

  assign(var_name, tmp)
}

rm(tmp) # Clean up temporary variable

# Dataset 1: Identify information of interest in ISIC 2 level dataset
dat_A <- dat_A %>%
    left_join(ilo %>% select(indicator, indicator.label), by = "indicator") %>% 
    filter(str_detect(tolower(classif2), "c26") & # C corresponds to the manufacturing sector, 26 corresponds to manufacture of computer, electronic and optical products (including semiconductors)
           sex == "SEX_T") %>% # Only information for 'total' sex
    select(ref_area, 
           indicator = indicator.label,
           classif1, # Classif 2 excluded, because c26 cases were already filtered for (only manufacture of electronics)
           time,
           obs_value)

# Dataset 2: Only information for 'total' sex, number of employees in STEM by education
dat_B <- dat_B %>%
  left_join(ilo %>% select(indicator, indicator.label), by = "indicator") %>%
  filter(sex == "SEX_T") %>%
  select(ref_area, 
        indicator = indicator.label, 
        classif1, 
        time, 
        obs_value)

# Dataset 3: Only information for 'total' sex, STEM employees number by public or private employment
dat_C <- dat_C %>%
  left_join(ilo %>% select(indicator, indicator.label), by = "indicator") %>%
  filter(sex == "SEX_T") %>%
  select(ref_area, 
        indicator = indicator.label, 
        classif1, 
        time, 
        obs_value)

# Dataset 4:  Only information for 'total' sex, total working-age population
dat_D <- dat_D %>%
  left_join(ilo %>% select(indicator, indicator.label), by = "indicator") %>%
  filter(sex == "SEX_T" &
         classif1 == "AGE_AGGREGATE_TOTAL") %>%
  select(ref_area,
         indicator = indicator.label,
         classif1,
         time,
         obs_value)
  
# Combine datasets into one, ref_area, time, indicator and classif1 are main keys
rilostat_data <- bind_rows(dat_A, dat_B, dat_C, dat_D) %>%
  mutate(indicator = str_replace_all(indicator, "sex", "TOTAL sex")) %>% # Remember all data in this dataset refers to TOTAL cases, not differentiated by sex (which is possible from the source)
  distinct(ref_area, time, indicator, classif1, .keep_all = TRUE) %>% # Remove duplicates if any
  arrange(ref_area, time)

# Write rilostat_data into a parquet, for efficient storage and later use in the analysis notebook.
write_parquet(rilostat_data, "data/ILOSTAT/ilostat_data.parquet")