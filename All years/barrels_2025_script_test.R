# barrels_2025_script.R
# Consolidated and adapted 2025 pipeline based on 2023 & 2024 scripts
# Produced automatically (edit as needed).

library(tidyverse)
library(readxl)
library(ggpubr)
library(agricolae)
library(multcomp)

# Set working directory to script location (works in RStudio)
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# --- Files ---
file_2023 <- "Barrel_Data_2023_FINAL.xlsx"
file_2024 <- "2024 Data_Clean.xlsx"
file_2025 <- "Copy of 2025 Data.xlsx"

# --- Read raw data ---
P2023 <- read_xlsx(file_2023, sheet = 1)
# some sheets
C2023_counts <- P2023 %>% dplyr::select(BARREL, SPECIES) %>% count(BARREL, SPECIES, name = "total")

C2024 <- read_xlsx(file_2024, sheet = 1)
P2024 <- read_xlsx(file_2024, sheet = 2)

C2025 <- read_xlsx(file_2025, sheet = 1)
P2025 <- read_xlsx(file_2025, sheet = 2)

# --- Helper: standardize counts for a year where counts are wide (BRTE, LAGL, ELEL, ARTR) ---
standardize_counts_wide <- function(df, barrel_col = c("Barrel ID","BARREL","Barrel"), year = NA) {
  # Try to find the barrel column name
  names_lower <- tolower(names(df))
  barrel_name <- names(df)[which(names_lower %in% tolower(barrel_col))[1]]
  if (is.na(barrel_name)) barrel_name <- names(df)[1]
  df2 <- df %>% rename(BARREL = !!sym(barrel_name))
  # rename known species columns if present
  df2 <- df2 %>% rename_with(~"BRTE", matches("BRTE", ignore.case = TRUE), only = FALSE)
  df2 <- df2 %>% rename_with(~"LAGL", matches("LAGL", ignore.case = TRUE), only = FALSE)
  df2 <- df2 %>% rename_with(~"ELEL", matches("ELEL", ignore.case = TRUE), only = FALSE)
  df2 <- df2 %>% rename_with(~"ARTR", matches("ARTR", ignore.case = TRUE), only = FALSE)
  # replace NA with 0 for species counts
  df2 <- df2 %>% mutate(across(c(BRTE, LAGL, ELEL, ARTR), ~replace_na(.x, 0)))
  df2$BARREL <- as.numeric(df2$BARREL)
  df2 <- df2 %>% dplyr::select(BARREL, any_of(c("BRTE","LAGL","ELEL","ARTR")))
  # pivot long
  df_long <- df2 %>% pivot_longer(cols = c(BRTE,LAGL,ELEL,ARTR), names_to = "SPECIES", values_to = "count") %>% 
    group_by(BARREL, SPECIES) %>% summarize(total = sum(count, na.rm = TRUE), .groups = "drop")
  if (!is.na(year)) df_long <- df_long %>% mutate(Year = year)
  return(df_long)
}

C2023_counts <- C2023_counts %>% # Remove all Unknown plants
  subset(., SPECIES != "U") %>% 
  subset(., SPECIES !="UG") %>% 
  subset(., SPECIES != "UD")

C2023_std <- C2023_counts %>% rename(total = total) %>% mutate(Year = 2023)


C2024_std <- standardize_counts_wide(C2024, year = 2024)
C2025_std <- standardize_counts_wide(C2025, year = 2025)

C_all <- bind_rows(C2023_std, C2024_std, C2025_std) %>%
  filter(!is.na(BARREL)) %>%   # <-- drop rows where BARREL is NA
  arrange(Year, BARREL, SPECIES)


# --- Helper: standardize plant (demographic) data ---
clean_plant_data <- function(df, year) {
  # Standardize column names to uppercase
  names(df) <- toupper(names(df))
  
  # Find the column that matches "BARREL"
  barrel_col <- names(df)[grepl("^BARREL", names(df), ignore.case = TRUE)][1]
  
  if (is.na(barrel_col)) {
    stop("No column found matching 'BARREL' in dataset for year ", year)
  }
  
  df %>%
    rename(BARREL = !!sym(barrel_col)) %>%
    # Convert all HT_* and FLWR_* columns to numeric
    mutate(across(matches("^HT_"), ~ suppressWarnings(as.numeric(.)))) %>%
    mutate(across(matches("^FLWR_"), ~ suppressWarnings(as.numeric(.)))) %>%
    mutate(YEAR = year)
}


P2023_clean <- clean_plant_data(P2023, year = 2023)
P2024_clean <- clean_plant_data(P2024, year = 2024)
P2025_clean <- clean_plant_data(P2025, year = 2025)

P_all <- bind_rows(P2023_clean, P2024_clean, P2025_clean)

# --- Summaries / Outputs to keep (from previous scripts) ---

# 1) Mean seed production per species (with sd / se)
P_all <- P_all %>% 
  dplyr::select(., BARREL, SPECIES, `PLANT ID`, TOOTHPICK, QUAD, HT_1, FLWR_1, FLWR_2, HEIGHT_8, YEAR, YEAR, FLWR_9)


seed_stats <- P_all %>%
  filter(!is.na(SEEDTOTAL)) %>%
  group_by(YEAR, SPECIES) %>%
  summarise(
    n = n(),
    mean_seeds = mean(SEEDTOTAL, na.rm = TRUE),
    sd = sd(SEEDTOTAL, na.rm = TRUE),
    se = sd / sqrt(n),
    .groups = "drop"
  )


print(seed_stats)

# 2) Total seeds produced per species per year (sum)
seed_totals <- P_all %>% group_by(Year, SPECIES) %>% summarise(total_seeds = sum(SEEDTotal, na.rm = TRUE), .groups = "drop")
print(seed_totals)

# 3) Per-capita production per species per year (total seeds / initial seeded)
# initial seeding numbers (adjust if your experimental design differs)
init_seeds_per_individual <- tibble(SPECIES = c("ARTR","ELEL","BRTE","LAGL"),
                                    seeds_per_ind = c(231,105,130,130),
                                    barrels = c(64,64,96,96)) # barrels seeded in original design
init_seeds_per_individual <- init_seeds_per_individual %>% mutate(init_total = seeds_per_ind * barrels)
# compute per-capita as seeds produced / init_total (note: this is coarse — adjust to per-capita per barrel if needed)
percap <- seed_totals %>% left_join(init_seeds_per_individual, by = "SPECIES") %>% 
  mutate(percap = total_seeds / init_total)
print(percap)

# 4) Germination proportions (rough): how many individuals reproduced (FLWRTotal>0) relative to initial seeding
germ_counts <- P_all %>% group_by(Year, SPECIES) %>% 
  summarise(repro_ind = sum(FLWRTotal > 0, na.rm = TRUE), n = n(), .groups = "drop") %>% 
  left_join(init_seeds_per_individual, by = "SPECIES") %>% 
  mutate(prop_germ = repro_ind / init_total)
print(germ_counts)

# 5) Plots: mean seed production and per-capita (simple ggplots)
p1 <- seed_stats %>% filter(Year == 2023 | Year == 2024 | Year == 2025) %>% 
  ggplot(aes(x = SPECIES, y = mean_seeds, fill = SPECIES)) + geom_col() + facet_wrap(~Year) + 
  theme_minimal() + ylab("Mean Seeds per Individual") + ggtitle("Mean seed production by species and year")
ggsave("mean_seed_production_by_species_and_year.png", p1, width = 8, height = 4)

p2 <- percap %>% ggplot(aes(x = SPECIES, y = percap, fill = SPECIES)) + geom_col() + facet_wrap(~Year) + 
  theme_minimal() + ylab("Per-capita (total seeds / initial seeds)") + ggtitle("Per-capita seed output per species and year")
ggsave("percap_seed_output_by_species_and_year.png", p2, width = 8, height = 4)

# 6) Treatment comparisons & ANOVA for BRTE competition (if barrel key / treatment info present)
if (file.exists("barrel_key.csv")) {
  barrelkey <- read_csv("barrel_key.csv")
  # ensure barrelkey has BARREL and Trt or Species combo
  if (all(c("BARREL","Trt") %in% names(barrelkey))) {
    # join counts for 2024 or 2025
    counts_latest <- C_all %>% filter(Year %in% c(2024,2025)) %>% pivot_wider(names_from = SPECIES, values_from = total, values_fill = 0)
    counts_latest <- counts_latest %>% left_join(barrelkey, by = "BARREL")
    # Example ANOVA: BRTE ~ Sp.combo + Trt
    if (all(c("BRTE","Sp.combo","Trt") %in% names(counts_latest))) {
      counts_latest$Trt <- as.factor(counts_latest$Trt)
      counts_latest$Sp.combo <- as.factor(counts_latest$Sp.combo)
      anmod <- aov(BRTE ~ Sp.combo + Trt, data = counts_latest)
      print(summary(anmod))
      tuk <- HSD.test(anmod, c("Sp.combo","Trt"), group = TRUE)
      print(tuk)
    }
  }
}

# Save consolidated cleaned plant table for 2025 work
write_csv(P_all, "Pdata_all_cleaned_2023_2024_2025.csv")
write_csv(C_all, "Counts_all_2023_2024_2025.csv")

message("R script generation complete. Edit multipliers, barrel counts, and file names as needed for final analyses.")
