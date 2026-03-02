# Working directory
setwd("your/working/directory") # Adjust to your working directory, so the whole project can be run from the root folder and the paths to data and database are consistent across functions

# Libraries
library(pacman)
p_load(tidyverse, countrycode, scales, ggplot2)

# Load data
data <- read_csv("data/final_output.csv") %>%
        mutate(period = as.integer(period),
               reporter = as.character(reporter))

# Definitions
eu <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
        "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
        "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
        "Malta", "Netherlands", "Poland", "Portugal", "Romania",
        "Slovakia", "Slovenia", "Spain", "Sweden")

as <- c("China", "India", "Japan", "South Korea", "Indonesia", "Thailand", 
        "Vietnam", "Philippines", "Malaysia", "Singapore", "Pakistan", "Bangladesh", 
        "Sri Lanka", "Nepal", "Taiwan")

am <- c("USA", "Canada", "Mexico", "Brazil", "Argentina", "Colombia",
        "Chile", "Peru", "Venezuela", "Ecuador", "Colombia", "Bolivia", 
        "Paraguay", "Uruguay")

# Add continent variable
data <- data %>%
  mutate(continent = case_when(
    reporter %in% eu ~ "EU",
    reporter %in% as ~ "Asia",
    reporter %in% am ~ "America",
    TRUE ~ "Other"
  ))

rm(eu, as, am) # Clean up environment


#
##
### PLOTS TO TEST DATA
##
#

### -------------- EMPLOYMENT ON CHIPS MANUFACTURE -----------------------------
emp_tot <- data %>%
        select(
                period, reporter, continent,
                emp_tot = `Employed population in chips manufacture (ISIC2 = C26) TOTAL (thousands)`
                ) %>%
        mutate(emp_tot = as.numeric(emp_tot) * 1000) %>%
        filter(reporter %in% c("USA", "South Korea", "Austria", "Italy", "Cyprus", "France", "Greece") &
               !is.na(emp_tot)
                ) %>%
        ggplot() +
        geom_hline(yintercept = seq(0, 1200000, 50000), 
                   linetype = "dashed", color = "grey") +
        geom_vline(xintercept = c(2019, 2022, 2023), linetype = "dashed", color = c("#8e8e8e", "#6e6e6e", "#000000")) +
        geom_line(aes(x = period, y = emp_tot, group = reporter,
                        color = reporter), linewidth = 1) +
        geom_point(aes(x = period, y = emp_tot, group = reporter,
                        color = reporter, shape = continent), size = 3) +
        scale_shape_manual(values = c(16, 17, 15, 18)) +
        scale_y_continuous(breaks = seq(0, 1200000, 50000),
                          labels = scales::label_number(big.mark = " ")) +
        scale_x_continuous(breaks = seq(2000, 2025, 1)) +
        theme_minimal() +
        labs(x = "Year", y = "Population employed in chip manufacture (ISIC2 = C26)",
                color = "Country", shape = "Continent",
                caption = "Data source: ILOSTAT") +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1))
emp_tot


### -------------- FINANCIALIZATION OF CHIPS MANUFACTURE ------------------------
foci <- c("USA", "China", "South Korea", "Japan", "Taiwan", "Germany", "Netherlands", "France", "Italy")
rest <- setdiff(unique((data %>% filter(!is.na(`FDI yearly stock inward in USD millions`) | !is.na(`FDI yearly stock outward in USD millions`)))$reporter), foci) # Son todos EU

fin_stk_data <- data %>%
        select(period, reporter, continent,
               fin_stk_in = `FDI yearly stock inward in USD millions`,
               fin_stk_out = `FDI yearly stock outward in USD millions`) %>%
        filter(!is.na(fin_stk_in) | !is.na(fin_stk_out)) %>%
        bind_rows(
                data %>%
                        select(period, reporter, continent,
                               fin_stk_in = `FDI yearly stock inward in USD millions`,
                               fin_stk_out = `FDI yearly stock outward in USD millions`) %>%
                        mutate(fin_stk_in = as.numeric(fin_stk_in),
                               fin_stk_out = as.numeric(fin_stk_out)) %>%
                        filter(reporter %in% rest,
                               !is.na(fin_stk_in) | !is.na(fin_stk_out)) %>%
                        group_by(period) %>%
                        summarise(fin_stk_in = mean(fin_stk_in, na.rm = TRUE),
                                  fin_stk_out = mean(fin_stk_out, na.rm = TRUE),
                                  .groups = "drop") %>%
                        mutate(reporter = "Other EU countries²",
                               continent = "EU")
        ) %>%
        mutate(fin_stk_in = fin_stk_in * 1e6,
               fin_stk_out = fin_stk_out * 1e6,
               fin_stk_net = fin_stk_in - fin_stk_out) # Interpretation if negative: more FDI outwards than inwards, i.e. more financialization of the chip industry abroad than domestically. If positive, the opposite.

fin_stk <- fin_stk_data %>%
        filter(reporter %in% c(foci, "Other EU countries²") &
                !is.na(fin_stk_net)) %>%
        ggplot() +
        geom_hline(yintercept = seq(-70000000000, 50000000000, 10000000000),
                   linetype = "dashed", color = "grey") +
        geom_vline(xintercept = c(2019, 2022, 2023), linetype = "dashed", color = c("#8e8e8e", "#6e6e6e", "#000000")) +
        geom_line(aes(x = period, y = fin_stk_net, group = reporter,
                        color = reporter), linewidth = 1) +
        geom_point(aes(x = period, y = fin_stk_net, group = reporter,
                        color = reporter, shape = continent), size = 3) +
        scale_shape_manual(values = c(16, 17, 15, 18)) +
        scale_y_continuous(breaks = seq(-70000000000, 50000000000, 10000000000),
                           labels = scales::label_number(big.mark = " ")) +
        scale_x_continuous(breaks = seq(2000, 2025, 1)) +
        theme_minimal() +
        labs(x = "Year", y = "Net value in USD of FDI stock¹ in chip manufacture (ISIC2 = C26)", color = "Country", shape = "Continent",
             caption = paste("Data source: OECD
                             \n¹ Measured as FDI yearly stock inward minus FDI yearly stock outward, in USD. Positive values indicate receiving more FDI for the industry than putting abroad.
                             \n² Average of other EU countries:", paste(rest, collapse = ", "))) +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1),
              axis.title.y = element_text(size = 8),
              plot.caption = element_text(hjust = 0, size = 8))
fin_stk

fin_stk2 <- fin_stk_data %>%
        filter(reporter %in% c(rest) &
                !is.na(fin_stk_net)) %>%
        ggplot() +
        geom_hline(yintercept = seq(-30000000000, 100000000000, 10000000000),
                   linetype = "dashed", color = "grey") +
        geom_vline(xintercept = c(2019, 2022, 2023), linetype = "dashed", color = c("#8e8e8e", "#6e6e6e", "#000000")) +
        geom_line(aes(x = period, y = fin_stk_net, group = reporter,
                        color = reporter), linewidth = 1) +
        geom_point(aes(x = period, y = fin_stk_net, group = reporter,
                        color = reporter, shape = continent), size = 3) +
        scale_shape_manual(values = c(16, 17, 15, 18)) +
        scale_y_continuous(breaks = seq(-30000000000, 100000000000, 10000000000),
                           labels = scales::label_number(big.mark = " ")) +
        scale_x_continuous(breaks = seq(2000, 2025, 1)) +
        theme_minimal() +
        labs(x = "Year", y = "Net value in USD of FDI stock¹ in chip manufacture (ISIC2 = C26)", color = "Country", shape = "Continent",
             caption = "Data source: OECD
                       \n¹ Measured as FDI yearly stock inward minus FDI yearly stock outward, in USD. Positive values indicate receiving more FDI for the industry than putting abroad") +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1),
              axis.title.y = element_text(size = 8),
              plot.caption = element_text(hjust = 0, size = 8))
fin_stk2

rm(fin_stk_data, rest, foci) # Clean up environment


### -------------- INNOVATION AS PATENT PRODUCTION ON CHIP TECHNOLOGIES ---------
foci <- c("USA", "China", "Taiwan", "South Korea", "Germany", "Netherlands", "France", "Italy", "Japan")
temp_data <- data %>%
        filter((!is.na(total_globalfam_h10_pats) | !is.na(total_mainfield_h10_pats) | !is.na(total_foreignowned_h10_pats))) %>%
        filter(reporter != "European Patent Office" & reporter != "World Intellectual Property Organization")
rest <- setdiff(unique(temp_data$reporter), foci) # Son todos EU
rm(temp_data)

ino_data <- data %>%
        select(period, reporter, continent,
               total_globalfam_h10_pats, total_mainfield_h10_pats, total_foreignowned_h10_pats) %>%
        filter((!is.na(total_globalfam_h10_pats) | !is.na(total_mainfield_h10_pats) | !is.na(total_foreignowned_h10_pats)) &
               reporter != "European Patent Office" & reporter != "World Intellectual Property Organization") %>%
        bind_rows(
                data %>%
                        select(period, reporter, continent,
                               total_globalfam_h10_pats, total_mainfield_h10_pats, total_foreignowned_h10_pats) %>%
                        filter(reporter %in% rest &
                               (!is.na(total_globalfam_h10_pats) | !is.na(total_mainfield_h10_pats) | !is.na(total_foreignowned_h10_pats))) %>%
                        group_by(period) %>%
                        summarise(total_globalfam_h10_pats = mean(total_globalfam_h10_pats, na.rm = TRUE),
                                  total_mainfield_h10_pats = mean(total_mainfield_h10_pats, na.rm = TRUE),
                                  total_foreignowned_h10_pats = mean(total_foreignowned_h10_pats, na.rm = TRUE),
                                  .groups = "drop") %>%
                        mutate(reporter = "Other EU countries¹",
                               continent = "EU")
        ) %>%
        pivot_longer(cols = c(total_globalfam_h10_pats, total_mainfield_h10_pats, total_foreignowned_h10_pats),
                     names_to = "patent_type", values_to = "patent_count")

ino1 <- ino_data %>%
        filter(reporter %in% c(foci, "Other EU countries¹") &
               !is.na(patent_count) &
               patent_type == "total_globalfam_h10_pats"&
               period >= 2000) %>%
        ggplot() +
        geom_hline(yintercept = seq(0, 50000, 10000), linetype = "dashed", color = "grey") +
        geom_vline(xintercept = c(2019, 2022, 2023), linetype = "dashed", color = c("#8e8e8e", "#6e6e6e", "#000000")) +
        geom_line(aes(x = period, y = patent_count, group = reporter,
                        color = reporter), linewidth = 1) +
        geom_point(aes(x = period, y = patent_count, group = reporter,
                        color = reporter, shape = continent), size = 3) +
        scale_shape_manual(values = c(16, 17, 15, 18)) +
        scale_y_continuous(breaks = seq(0, 50000, 5000)) +
        scale_x_continuous(breaks = seq(2000, 2025, 1)) +
        theme_minimal() +
        labs(x = "Year", y = "Number of patents with family ID in chip technologies (CPC = H10)", color = "Country", shape = "Continent",
             caption = paste("Data source: Google Patents Public Data
                             \n¹ Average of other EU countries:", paste(rest, collapse = ", "))) +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1),
              axis.title.y = element_text(size = 8),
              plot.caption = element_text(hjust = 0, size = 8))
ino1

ino2 <- ino_data %>%
        filter(reporter %in% c(foci, "Other EU countries¹") &
               !is.na(patent_count) &
               patent_type == "total_foreignowned_h10_pats" &
               period >= 2000) %>%
        ggplot() +
        geom_hline(yintercept = seq(0, 50000, 10000), linetype = "dashed", color = "grey") +
        geom_vline(xintercept = c(2019, 2022, 2023), linetype = "dashed", color = c("#8e8e8e", "#6e6e6e", "#000000")) +
        geom_line(aes(x = period, y = patent_count, group = reporter,
                        color = reporter), linewidth = 1) +
        geom_point(aes(x = period, y = patent_count, group = reporter,
                        color = reporter, shape = continent), size = 3) +
        scale_shape_manual(values = c(16, 17, 15, 18)) +
        scale_y_continuous(breaks = seq(0, 50000, 5000)) +
        scale_x_continuous(breaks = seq(2000, 2025, 1)) +
        theme_minimal() +
        labs(x = "Year", y = "Number of patents  in chip technologies registered by foreign actors (CPC = H10)", color = "Country", shape = "Continent",
             caption = paste("Data source: Google Patents Public Data
                             \n¹ Average of other EU countries:", paste(rest, collapse = ", "))) +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1),
              axis.title.y = element_text(size = 8),
              plot.caption = element_text(hjust = 0, size = 8))
ino2

ino3 <- ino_data %>%
        filter(reporter %in% c(foci, "Other EU countries¹") &
               !is.na(patent_count) &
               patent_type == "total_mainfield_h10_pats" &
               period >= 2000) %>%
        ggplot() +
        geom_hline(yintercept = seq(0, 50000, 10000), linetype = "dashed", color = "grey") +
        geom_vline(xintercept = c(2019, 2022, 2023), linetype = "dashed", color = c("#8e8e8e", "#6e6e6e", "#000000")) +
        geom_line(aes(x = period, y = patent_count, group = reporter,
                        color = reporter), linewidth = 1) +
        geom_point(aes(x = period, y = patent_count, group = reporter,
                        color = reporter, shape = continent), size = 3) +
        scale_shape_manual(values = c(16, 17, 15, 18)) +
        scale_y_continuous(breaks = seq(0, 50000, 5000)) +
        scale_x_continuous(breaks = seq(2000, 2025, 1)) +
        theme_minimal() +
        labs(x = "Year", y = "Number of patents with main field in chip technologies (CPC = H10)", color = "Country", shape = "Continent",
             caption = paste("Data source: Google Patents Public Data
        \n¹ Average of other EU countries:", paste(rest, collapse = ", "))) +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1),
              axis.title.y = element_text(size = 8),
              plot.caption = element_text(hjust = 0, size = 8))
ino3

rm(ino_data, rest, foci) # Clean up environment


### -------------- TRADE BALANCE ON CHIPS ---------------------------------------
foci <- c("USA", "China", "South Korea", "Germany", "Netherlands", "France", "Italy", "Japan")
rest <- setdiff(unique((data %>% filter(!is.na(trade_partners_balance)))$reporter), foci) # Son todos EU


dep_data <- data %>%
  select(period, reporter, continent, trade_partners_balance, trade_net_value) %>%
  mutate(trade_partners_balance = as.numeric(trade_partners_balance),
         trade_net_value = as.numeric(trade_net_value)) %>%
  filter(!is.na(trade_partners_balance) | !is.na(trade_net_value)) %>%
  bind_rows(
    data %>%
      select(period, reporter, continent, trade_partners_balance, trade_net_value) %>%
      mutate(trade_partners_balance = as.numeric(trade_partners_balance),
             trade_net_value = as.numeric(trade_net_value)) %>%
      filter(reporter %in% rest,
             !is.na(trade_partners_balance) | !is.na(trade_net_value)) %>%
      group_by(period) %>%
      summarise(trade_partners_balance = mean(trade_partners_balance, na.rm = TRUE),
                trade_net_value = mean(trade_net_value, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(reporter = "Other EU countries²",
             continent = "EU")
  )

dep <- dep_data %>%
        select(period, reporter, continent, trade_partners_balance) %>%
        filter(reporter %in% c(foci, "Other EU countries²")) %>%
        ggplot() +
        geom_hline(yintercept = c(-100, -50), linetype = "dashed", color = "gray") +
        geom_vline(xintercept = c(2019, 2022, 2023), linetype = "dashed", color = c("#8e8e8e", "#6e6e6e", "#000000")) +
        geom_line(aes(x = period, y = trade_partners_balance, group = reporter,
                                color = reporter), linewidth = 1) +
        geom_point(aes(x = period, y = trade_partners_balance, group = reporter, 
                        color = reporter, shape = continent), size = 3) +
        scale_shape_manual(values = c(16, 17, 15, 18)) +
        scale_y_continuous(breaks = seq(-120, 20, 10)) +
        scale_x_continuous(breaks = seq(2000, 2025, 1)) +
        theme_minimal() +
        labs(x = "Year", y = "Trade partners balance¹", color = "Country", shape = "Continent",
        caption = paste("Data source: UN Comtrade
        \n¹ Measured inspired in FAO's cereal import dependency formula: ((import partners - export partners) / import partners) * 100
        \n² Average of other EU countries:", paste(rest, collapse = ", "))) +
        theme(legend.position = "bottom",
                plot.caption = element_text(hjust = 0, size = 8),
                axis.text.x = element_text(angle = 90, vjust = 1.2, hjust = 1))
dep

dep2 <- dep_data %>%
        select(period, reporter, continent, trade_partners_balance) %>%
        filter(reporter %in% c(rest)) %>%
        ggplot() +
        geom_hline(yintercept = c(-100, -50, 50, 100), linetype = "dashed", color = "gray") +
        geom_vline(xintercept = c(2019, 2022, 2023), linetype = "dashed", color = c("#8e8e8e", "#6e6e6e", "#000000")) +
        geom_line(aes(x = period, y = trade_partners_balance, group = reporter,
                                color = reporter), linewidth = 1) +
        geom_point(aes(x = period, y = trade_partners_balance, group = reporter,
                        color = reporter, shape = continent), size = 3) +
        scale_shape_manual(values = c(16, 17, 15, 18)) +
        scale_y_continuous(breaks = seq(-120, 100, 10)) +
        scale_x_continuous(breaks = seq(2000, 2025, 1)) +
        theme_minimal() +
        labs(x = "Year", y = "Trade partners balance¹", color = "Country", shape = "Continent",
        caption = "Data source: UN Comtrade | HS code 8542 (Electronic integrated circuits and micro assemblies)
        \n¹ Measured inspired in FAO's cereal import dependency formula: ((import partners - export partners) / import partners) * 100") +
        theme(legend.position = "bottom",
                plot.caption = element_text(hjust = 0, size = 8),
                axis.text.x = element_text(angle = 90, vjust = 1.2, hjust = 1))
dep2

dep3 <- dep_data %>%
        select(period, reporter, continent, trade_net_value) %>%
        filter(reporter %in% c(foci, "Other EU countries²")) %>%
        ggplot() +
        geom_hline(yintercept = c(-275000000000, 150000000000), linetype = "dashed", color = "gray") +
        geom_vline(xintercept = c(2019, 2022, 2023), linetype = "dashed", color = c("#8e8e8e", "#6e6e6e", "#000000")) +
        geom_line(aes(x = period, y = trade_net_value, group = reporter,
                                color = reporter), linewidth = 1) +
        geom_point(aes(x = period, y = trade_net_value, group = reporter,
                        color = reporter, shape = continent), size = 3) +
        scale_shape_manual(values = c(16, 17, 15, 18)) +
        scale_y_continuous(breaks = seq(-300000000000, 200000000000, 25000000000),
                           labels = scales::label_number(big.mark = " ")) +
        scale_x_continuous(breaks = seq(2000, 2025, 1)) +
        theme_minimal() +
        labs(x = "Year", y = "Net trade value in chip manufacture (USD)", color = "Country", shape = "Continent",
             caption = paste("Data source: UN Comtrade | HS code 8542 (Electronic integrated circuits and micro assemblies)
                             \n¹ Measured total value of exports minus total value of imports in products under and including HS code 8542
                             \n² Average of other EU countries:", paste(rest, collapse = ", "))) +
        theme(legend.position = "bottom",
              plot.caption = element_text(hjust = 0, size = 8),
              axis.text.x = element_text(angle = 90, vjust = 1.2, hjust = 1))
dep3

dep4 <- dep_data %>%
        select(period, reporter, continent, trade_net_value) %>%
        filter(reporter %in% c(rest)) %>%
        ggplot() +
        geom_hline(yintercept = c(-275000000000, 150000000000), linetype = "dashed", color = "gray") +
        geom_vline(xintercept = c(2019, 2022, 2023), linetype = "dashed", color = c("#8e8e8e", "#6e6e6e", "#000000")) +
        geom_line(aes(x = period, y = trade_net_value, group = reporter,
                                color = reporter), linewidth = 1) +
        geom_point(aes(x = period, y = trade_net_value, group = reporter,
                        color = reporter, shape = continent), size = 3) +
        scale_shape_manual(values = c(16, 17, 15, 18)) +
        scale_y_continuous(breaks = seq(-300000000000, 200000000000, 25000000000),
                           labels = scales::label_number(big.mark = " ")) +
        scale_x_continuous(breaks = seq(2000, 2025, 1)) +
        theme_minimal() +
        labs(x = "Year", y = "Net trade value in chip manufacture (USD)", color = "Country", shape = "Continent",
             caption = "Data source: UN Comtrade | HS code 8542 (Electronic integrated circuits and micro assemblies)
             \n¹ Measured total value of exports minus total value of imports in products under and including HS code 8542
             \n  Same scale as previous plot, to facilitate comparison") +
        theme(legend.position = "bottom",
              plot.caption = element_text(hjust = 0, size = 8),
              axis.text.x = element_text(angle = 90, vjust = 1.2, hjust = 1))
dep4

rm(dep_data, rest, foci) # Clean up environment


#
##
### ------------- SAVE PLOTS TO HIGH RESOLUTION IMAGE AND SVG ------------------------------------------------
##
#

# png
ggsave("plots/emp_tot.png", emp_tot, width = 10, height = 6, dpi = 300)
ggsave("plots/fin_stk.png", fin_stk, width = 10, height = 6, dpi = 300)
ggsave("plots/fin_stk2.png", fin_stk2, width = 10, height = 6, dpi = 300)
ggsave("plots/ino1.png", ino1, width = 10, height = 6, dpi = 300)
ggsave("plots/ino2.png", ino2, width = 10, height = 6, dpi = 300)
ggsave("plots/ino3.png", ino3, width = 10, height = 6, dpi = 300)
ggsave("plots/dep.png", dep, width = 10, height = 6, dpi = 300)
ggsave("plots/dep2.png", dep2, width = 10, height = 6, dpi = 300)
ggsave("plots/dep3.png", dep3, width = 10, height = 6, dpi = 300)
ggsave("plots/dep4.png", dep4, width = 10, height = 6, dpi = 300)

# svg
ggsave("plots/emp_tot.svg", emp_tot, width = 10, height = 6)
ggsave("plots/fin_stk.svg", fin_stk, width = 10, height = 6)
ggsave("plots/fin_stk2.svg", fin_stk2, width = 10, height = 6)
ggsave("plots/ino1.svg", ino1, width = 10, height = 6)
ggsave("plots/ino2.svg", ino2, width = 10, height = 6)
ggsave("plots/ino3.svg", ino3, width = 10, height = 6)
ggsave("plots/dep.svg", dep, width = 10, height = 6)
ggsave("plots/dep2.svg", dep2, width = 10, height = 6)
ggsave("plots/dep3.svg", dep3, width = 10, height = 6)
ggsave("plots/dep4.svg", dep4, width = 10, height = 6)
