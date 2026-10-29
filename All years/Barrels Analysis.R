########################
### ALL Barrel Data  ###
### Manuscript Script  ###
########################

library(tidyverse)
library(readxl)
library(rstan)
library(rstanarm)
library(brms)
library(bayesplot)

#### What this script aims to do
# 1. Organize data from all years
# 2. clean data and organize it into an easy to work with form
# 3. Analyse data
# 3a. GLMMs and MANOVA and ANOVA
# 4. Visualize model outputs

#######################
# 1. Organize data  ###
#######################
# Set WD for Desktop
# setwd("~/GitHub/Barrels")
# Set WD for laptop
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

################
### ALL DATA ###
################
# read in all data
########
# 2023 #

Pdata23 <- read_xlsx("Barrel_Data_2023_FINAL.xlsx")
Cdata23 <- Pdata23 %>% dplyr::select(BARREL, SPECIES) %>% 
  count(BARREL, SPECIES, name = "count")
Cdata23 <- Cdata23 %>% # Remove all Unknown plants
  subset(., SPECIES != "U") %>% 
  subset(., SPECIES !="UG") %>% 
  subset(., SPECIES != "UD")
########


##### 2024 #####

Cdata24 <- read_xlsx("2024 Data_Updated_Clean.xlsx", sheet = 1)
Cdata24 <- Cdata24 %>% rename(BRTE = `BRTE count`, LAGL = `LAGL count`, ELEL = `ELEL count`, ARTR = `ARTR count`) %>% 
  rename(BARREL = `Barrel ID`) %>% mutate(BARREL = as.numeric(BARREL))
Cdata24 <- Cdata24[-161,]
CO_24 <- read_xlsx("2024 Data_Updated_Clean.xlsx", sheet = 3) # carry over perennial from 2024
Pdata24 <- read_xlsx("2024 Data_Clean.xlsx", sheet = 2)

CO_24 <- read_xlsx("2024 Data_Updated_Clean.xlsx", sheet = 3)

CO_24 <- CO_24 %>% filter(!is.na(`BARREL #`)) %>%        # drop empty rows
  rename(BARREL = `BARREL #`) 

CO_24_counts <- CO_24 %>% # count all the species in each barrel (LAGL & BRTE should be limited to 5)
  count(BARREL, SPECIES)

CO_24_wide <- CO_24_counts %>% pivot_wider(
  names_from = SPECIES,
  values_from = n,
  values_fill = NA) 
CO_24_wide <- CO_24_wide %>% select(BARREL, ELEL, ARTR)

Cdata24_updated <- Cdata24 %>%
  left_join(CO_24_wide, by = "BARREL", suffix = c("", "_CO")) %>%
  mutate(
    ELEL = ELEL + replace_na(ELEL_CO, 0),
    ARTR = ARTR + replace_na(ARTR_CO, 0)) %>%
  select(-ELEL_CO, -ARTR_CO)

Cdata24_updated %>% select(BARREL, ELEL, ARTR)
Cdata24 %>% select(BARREL, ELEL, ARTR)
Cdata24 <- Cdata24_updated



###### 2025 #####
Cdata25 <- read_xlsx("2025 Data.xlsx", sheet = 2) # counts of species in each barrel
Cdata25 <- Cdata25 %>% rename(BRTE = `BRTE count`, LAGL = `LAGL count`, ELEL = `ELEL count`, ARTR = `ARTR count`) %>% 
  mutate(Barrel = as.numeric(Barrel))
Cdata25 <- Cdata25[-161,] # remove "total" row at bottom

Pdata25 <- read_xlsx("2025 Data.xlsx", sheet = 1) # demographic data
# clean it up
Pdata25 


CO_25 <-  read_xlsx("2025 Data.xlsx", sheet = 6) # carry over perennial from 2024
# get rid of all the empty rows
CO_25 <- CO_25 %>% filter(!is.na(`BARREL #`)) %>% mutate(HT_FINAL = HT_1) %>% rename(Barrel = `BARREL #`)
head(CO_25)
C_rows <- colnames(Cdata25)

CO_25_counts <- CO_25 %>% #count the species in each barrel
  count(Barrel, SPECIES)


CO_25_wide <- CO_25_counts %>%
  pivot_wider(names_from = SPECIES, values_from = n, values_fill = NA)

Cdata25_updated <- Cdata25 %>%
  left_join(CO_25_wide, by = "Barrel", suffix = c("", "_CO")) %>%
  mutate(
    ELEL = ELEL + replace_na(ELEL_CO, 0),
    ARTR = ARTR + replace_na(ARTR_CO, 0)
  ) %>%
  select(-ELEL_CO, -ARTR_CO)

### Sanity checks
(Cdata25_updated %>% select(Barrel ,ELEL, ARTR))
(Cdata25 %>% select(Barrel ,ELEL, ARTR))
# looks good !


Cdata25 <- Cdata25_updated 


################
# Tidying time #
################

#### Counts data ###
# 2023 #
Cdata23 <- Cdata23 %>%
  rename_with(~ "total", .cols = any_of("count")) %>%  # rename count -> total if present
  mutate(Year = 2023) 

Cdata23 <- Cdata23 %>%
  pivot_wider(names_from = SPECIES,
              values_from = total,
              values_fill = NA) %>%      # non-seeded species counts = 0
  relocate(BARREL, BRTE, LAGL, ELEL, .before = Year) %>% 
  mutate( TrT = NA_character_)

# 2024 #
Cdata24 <- Cdata24 %>% 
  #mutate(across(c(BRTE, LAGL, ELEL, ARTR), ~replace_na(., 0))) %>% 
  mutate(Year = 2024) 
view(Cdata24)

# 2025 #
Cdata25 <- Cdata25 %>% rename(BARREL = Barrel) %>% 
  mutate(Year = 2025)


view(Cdata25)


# apply to each dataset
Cdata_all <- bind_rows(Cdata23, Cdata24, Cdata25) %>%
  mutate(BARREL = as.numeric(BARREL), Year = as.integer(Year)) %>%
  arrange(BARREL, Year)

head(Cdata_all, 20)

# more tidy
#unique(trt_lookup$Trt)

Cdata_all <- Cdata_all %>% 
  relocate(., ARTR, .before = "Year") %>% # relocate ARTR
  mutate(ARTR = ifelse(Year == 2023 & Trt %in% c("AL_1", "AL_A", "BA_A", "BA_1"), 0, ARTR)) %>%  # replace all 2023 no ARTR with 0 
  select(., -TrT) 

head(Cdata_all,20)

trt_lookup <- Cdata_all %>%
  filter(!is.na(Trt)) %>% 
  select(BARREL, Trt) %>%
  distinct()          

Cdata_all <- Cdata_all %>%
  left_join(trt_lookup, by = "BARREL", suffix = c("", "_correct")) %>%
  mutate(Trt = if_else(is.na(Trt), Trt_correct, Trt)) %>%
  select(-Trt_correct)

head(Cdata_all,20)


###################################
### Now for the demography data ###
###################################

#  function to standardize column names
clean_pdata <- function(df, year) {
  df %>%
    # rename to standard names
    rename(
      BARREL = matches("BARREL"),
      SPECIES = SPECIES,
      PLANT_ID = matches("PLANT ?ID"),
      TOOTHPICK = TOOTHPICK,
      QUAD = QUAD,
      HT_1 = matches("HT_1"),
      AorD_1 = matches("AorD_1"),
      DATE_1 = matches("DATE_1"),
      FLWR_1 = matches("FLWR_1"),
      NOTES_1 = matches("NOTES_1"),
      HT_FINAL = matches(c("HT_2", "Height_8")),
      AorD_2 = matches("AorD_2"),
      DATE_2 = matches("DATE_2"),
      FLWR_2 = matches("FLWR_2"),
      NOTES_2 = matches("NOTES_2"),
      DESIGNATION = matches("DESIGNATION|DESIGN")
    ) %>%
    # add missing cols if not present in this year
    mutate(
      PLANT_ID = coalesce(PLANT_ID, as.numeric(NA)),
      DESIGNATION = if (!"DESIGNATION" %in% names(.)) NA_character_ else DESIGNATION,
      HT_FINAL = as.numeric(HT_FINAL),
      FLWR_FINAL = as.numeric(FLWR_2)
    ) %>%
    # assign unique ID
    mutate(
      UID = str_c("B", BARREL, "_", SPECIES, "_", TOOTHPICK, "_Q", QUAD, "_", row_number()),
      Year = year
    )
}

# apply to each year
Pdata23_clean <- clean_pdata(Pdata23, 2023)


Pdata23_clean <- Pdata23_clean %>% dplyr::select(., BARREL, SPECIES, PLANT_ID, TOOTHPICK, QUAD, DESIGNATION, HT_FINAL, FLWR_9, UID, Year) %>% 
  mutate(., DESIGNATION = ifelse(SPECIES == "BRTE", "I-F",
                                 ifelse(SPECIES == "LAGL", "N-F",
                                        ifelse(SPECIES == "ELEL", "N-M",
                                               ifelse(SPECIES == "ARTR", "N-S", NA))))) %>% 
  rename(.,FLWR_FINAL = FLWR_9)


Pdata24_clean <- clean_pdata(Pdata24, 2024)
Pdata24_clean <- Pdata24_clean %>% 
  dplyr::select(., BARREL, SPECIES, PLANT_ID, TOOTHPICK, QUAD, DESIGNATION, HT_FINAL, FLWR_2, UID, Year)


Pdata25_clean <- clean_pdata(Pdata25, 2025)

# bind all together
Pdata_all <- bind_rows(Pdata23_clean, Pdata24_clean, Pdata25_clean)

Pdata_all <- Pdata_all %>% dplyr::select(., BARREL, SPECIES, PLANT_ID, TOOTHPICK, QUAD, DESIGNATION, HT_FINAL, FLWR_FINAL, UID, Year)
Pdata_all <- Pdata_all %>% # Remove all Unknown plants
  subset(., SPECIES != "U") %>% 
  subset(., SPECIES !="UG") %>% 
  subset(., SPECIES != "UD")


glimpse(Pdata_all)


#################
### Seed data ###
#################

####### Barrel key and life-history combinations #######

barrelkey <- read.csv("barrel_key.csv") |> 
  mutate(
    Trt = gsub("_[^_]+$", "", Trt),
    LH_combo = case_when(
      Trt %in% c("LE", "BE") ~ "FM",
      Trt == "BL" ~ "FF",
      Trt %in% c("AL", "BA") ~ "FS",
      TRUE ~ NA_character_))

lh_map <- list(
  LE = c("LAGL", "ELEL"),
  BE = c("BRTE", "ELEL"),
  BL = c("BRTE", "LAGL"),
  AL = c("ARTR", "LAGL"),
  BA = c("BRTE", "ARTR"))

barrelkey_expanded <- barrelkey |> 
  rowwise() |> 
  mutate(SPECIES = list(lh_map[[Trt]])) |> 
  ungroup() |> 
  tidyr::unnest(SPECIES) |> 
  mutate(
    BARREL = as.integer(BARREL),
    SPECIES = as.factor(SPECIES),
    LH_combo = as.factor(LH_combo))


####### 2022 #######

SDdata_22 <- barrelkey_expanded |> 
  mutate(Total_seeds = case_when(
      SPECIES == "ELEL" ~ 105,
      SPECIES == "LAGL" ~ 130,
      SPECIES == "ARTR" ~ 230,
      SPECIES == "BRTE" ~ 130,
      TRUE ~ NA_real_))


####### 2023 #######

SDdata_23_calc <- Pdata23_clean |> 
  select(BARREL, SPECIES, FLWR_FINAL) |> 
  mutate(
    Seed_tot = case_when(
      SPECIES == "LAGL" ~ FLWR_FINAL * 39,
      SPECIES == "ELEL" ~ FLWR_FINAL * 93,
      SPECIES == "BRTE" ~ FLWR_FINAL * 88,
      TRUE ~ 0)) |> 
  group_by(BARREL, SPECIES) |> 
  summarise(
    Total_seeds = sum(Seed_tot, na.rm = TRUE),
    .groups = "drop")

SDdata_23 <- barrelkey_expanded |> 
  left_join(SDdata_23_calc, by = c("BARREL", "SPECIES")) |> 
  mutate(Total_seeds = tidyr::replace_na(Total_seeds, 0))


####### 2024 #######

SDdata_24 <- readxl::read_xlsx(
  "2024 Data_Clean.xlsx",
  sheet = "seed count total") |> 
  select(
    `Barrel ID`, `BRTE Seeds`, `LAGL Seeds`,
    `ELEL Seeds`, `ARTR Seeds`) |> 
  rename(
    BARREL = `Barrel ID`,
    BRTE = `BRTE Seeds`,
    LAGL = `LAGL Seeds`,
    ELEL = `ELEL Seeds`,
    ARTR = `ARTR Seeds`
  ) |> 
  mutate(BARREL = as.integer(BARREL)) |> 
  filter(!is.na(BARREL)) |> 
  pivot_longer(
    cols = c(BRTE, LAGL, ELEL, ARTR),
    names_to = "SPECIES",
    values_to = "Total_seeds"
  ) |> 
  left_join(barrelkey_expanded, by = c("BARREL", "SPECIES")) |> 
  filter(!is.na(LH_combo))


####### 2025 #######

SDdata_25 <- readxl::read_xlsx(
  "2025 Data.xlsx",
  sheet = "Total Seeds Barrel") |> 
  select(`Barrel ID`, BRTE, LAGL, ELEL, ARTR) |> 
  rename(BARREL = `Barrel ID`) |> 
  pivot_longer(
    cols = c(BRTE, LAGL, ELEL, ARTR),
    names_to = "SPECIES",
    values_to = "Total_seeds") |> 
  left_join(barrelkey_expanded, by = c("BARREL", "SPECIES")) |> 
  filter(!is.na(LH_combo))


####### Combine seed data across years #######

SD_all <- bind_rows(
  SDdata_22 |> mutate(Year = 2022),
  SDdata_23 |> mutate(Year = 2023),
  SDdata_24 |> mutate(Year = 2024),
  SDdata_25 |> mutate(Year = 2025)) |> 
  arrange(BARREL, SPECIES, Year)


####### Check seed data #######

# Number of rows and unique barrels by year
SD_all |> 
  group_by(Year) |> 
  summarise(
    n_rows = n(),
    n_barrels = n_distinct(BARREL),
    n_missing_seeds = sum(is.na(Total_seeds)),
    .groups = "drop")

# Check species and treatment combinations
SD_all |> 
  count(Year, SPECIES, LH_combo)

# Check for duplicate barrel-species-year combinations
SD_all |> 
  count(BARREL, SPECIES, Year) |> 
  filter(n > 1)



#############
### GLMM  ###
#############
# Effect of life history and repeat seeding on plant abundance
count_new <- Cdata_all %>%
  # Pivot species columns into long format
  pivot_longer(
    cols = c(BRTE, LAGL, ELEL, ARTR),
    names_to = "Species",
    values_to = "Count")

trts <- unique(as.factor(count_new$Trt))

count_new_trt <- count_new %>%
  mutate(Treatment = recode(Trt,
                            LE_A = "Repeated" ,
                            BE_A = "Repeated" ,
                            BL_A= "Repeated" ,
                            AL_A = "Repeated" ,
                            BA_A = "Repeated" ,
                            BL_1 = "Single" , AL_1 = "Single" , BE_1 = "Single" , BA_1 = "Single" , LE_1 = "Single" ))
# need to be able to tell which barrels actually have 0 plants per species vs have 0 becuase no seeds were input


# Add in columns for LH, treatment and treatment
count_df <- count_new_trt %>%
  mutate(
    # get part before "_" (e.g., "BA", "LA", "E", "A")
    CodePre = sub("_.*", "", Trt),
    
    # logicals: which species letters are present in the treatment?
    Has_B = grepl("B", CodePre),
    Has_L = grepl("L", CodePre),
    Has_E = grepl("E", CodePre),
    Has_A = grepl("A", CodePre),
    
    # species seeded in that barrel
    SeededSpecies = case_when(
      Species == "BRTE" & Has_B ~ "BRTE",
      Species == "LAGL" & Has_L ~ "LAGL",
      Species == "ELEL" & Has_E ~ "ELEL",
      Species == "ARTR" & Has_A ~ "ARTR",
      TRUE ~ NA_character_), 
    Seeded = as.factor(!is.na(SeededSpecies)), # creates a seeded species column. 
    LH = as.factor(case_when(Species == "BRTE" ~ "FAST",
                   Species == "LAGL" ~ "FAST", 
                   Species == "ELEL" ~ "MEDIUM",
                   Species == "ARTR" ~ "SLOW")),
    Year = as.factor(Year))

# Make factors 
count_df <- count_df %>% mutate(Species = factor(Species, levels = c("LAGL", "ELEL", "ARTR", "BRTE")),
                                          Treatment = factor(Treatment, levels = c("Single", "Repeated")))


count_df <- count_df %>% 
  mutate(Count = case_when(Species == "ARTR" & Has_A == T & is.na(Count) ~ 0, T ~ Count)) 



# prepare a new df for GLMM
count_df_mod <- count_df %>% 
  select(., BARREL, Year, Species, LH, , Count, Treatment, Seeded) %>% 
  filter(., Seeded == "TRUE") %>% 
  mutate(Count = replace_na(Count, 0))

#### have to specify Fast - native vs fast - invasive
count_df_mod<- count_df_mod %>%
  mutate(LH_2 = LH,
        LH_2 = case_when(Species == "BRTE" ~"FAST_I", 
                         Species == "LAGL" ~ "FAST_N", 
                         T ~ as.factor(LH)))


#########################
###    Do some data checking
count_df_mod
barrel <- count_df_mod$BARREL
counts <- count_df_mod$Count
plot(barrel, counts, type = "h")
hist(counts)


###############
### Make the response variable a proportion of plants that germinated and survived to reproduce ### 
###############
SD_all
count_df_mod_1 <- count_df_mod 
count_df_mod_1 <- count_df_mod |> mutate(Init_seeds = case_when(Year == 2023 &
  Species == "ELEL" ~ 105,
  Year == 2023 & Species == "LAGL" ~ 130,
  Year == 2023 & Species == "ARTR" ~ 230,
  Year == 2023 & Species == "BRTE" ~ 130,
  TRUE ~ NA_real_))


SD_all <- SD_all |> mutate(Year = as.factor(Year))
glimpse(SD_all)
count_df_mod_1 <- count_df_mod 
count_df_mod_1 <- count_df_mod_1 |> 
  left_join(
    SD_all |> 
      select(BARREL, Year, SPECIES, Total_seeds),
    by = c("BARREL", "Year", "Species" = "SPECIES")
  ) |> 
  rename(Seeds = Total_seeds)

glimpse(count_df_mod_1)

####### Add seed production to count data #######

count_df_mod_1 <- SD_all |> 
  select(BARREL, Trt, LH_combo, SPECIES, Year, Seeds = Total_seeds) |> 
  left_join(count_df_mod |> 
      select(BARREL, Year, Species, Count, Treatment, Seeded, LH, LH_2),
    by = c(
      "BARREL",
      "Year",
      "SPECIES" = "Species")) |> 
  mutate(Count = replace_na(Count, 0)) |> 
  select(
    BARREL, Year, SPECIES, LH, Count,
    Treatment, Seeded, LH_2, Seeds, Trt, LH_combo) |> 
  arrange(BARREL, SPECIES, Year)

count_df_mod_2 <- count_df_mod_1 |> 
  mutate(Year = as.integer(as.character(Year))) |> 
  group_by(BARREL, SPECIES) |> 
  arrange(Year, .by_group = TRUE) |> 
  mutate(
    Previous_seeds = lag(Seeds),
    Seed_growth = Seeds  / Previous_seeds,
    Seed_growth_log = log(Seeds / lag(Seeds))
  ) |> 
  ungroup()

glimpse(count_df_mod_2)
#########################
##### New model time  ###
#########################
###############
### Make the response variable seed growth (log change in seeds from year to year) ### 
###############

SD_all <- SD_all |> mutate(Year = as.factor(Year))
glimpse(SD_all)

####### Add seed production to count data #######

count_df_mod_1 <- SD_all |> 
  select(BARREL, Trt, LH_combo, SPECIES, Year, Seeds = Total_seeds) |> 
  left_join(count_df_mod |> 
              select(BARREL, Year, Species, Count, Treatment, Seeded, LH, LH_2),
            by = c(
              "BARREL",
              "Year",
              "SPECIES" = "Species")) |> 
  mutate(Count = replace_na(Count, 0)) |> 
  select(
    BARREL, Year, SPECIES, LH, Count,
    Treatment, Seeded, LH_2, Seeds, Trt, LH_combo) |> 
  arrange(BARREL, SPECIES, Year)

### check that every row in count_df_mod found a match in SD_all (should be 0 rows)
count_df_mod |> anti_join(SD_all, by = c("BARREL", "Year", "Species" = "SPECIES"))

count_df_mod_2 <- count_df_mod_1 |> 
  mutate(Year = as.integer(as.character(Year))) |> 
  group_by(BARREL, SPECIES) |> 
  arrange(Year, .by_group = TRUE) |> 
  mutate(Previous_seeds = lag(Seeds)) |> 
  ungroup()

glimpse(count_df_mod_2)

#########################
##### New model time  ###
#########################

### fill BEFORE dropping 2022, because 2022 is needed for the lag
count_df_growth <- count_df_mod_2 |>
  group_by(SPECIES) |>
  fill(LH, LH_2, .direction = "downup") |>
  ungroup() |>
  group_by(BARREL) |>
  fill(Treatment, Seeded, Trt, LH_combo, .direction = "downup") |>
  ungroup()

### add the reseeded seeds for the Repeated treatment (BRTE is never reseeded)
### 2022 is the initial seeding and is already in Seeds, so nothing is added in 2022
### Added_seeds in year t is sown after year t's seed production, so it joins the pool going into t + 1
count_df_growth <- count_df_growth |> 
  mutate(
    Added_seeds = case_when(
      Year > 2022 & Treatment == "Repeated" & SPECIES == "ELEL" ~ 105,
      Year > 2022 & Treatment == "Repeated" & SPECIES == "LAGL" ~ 130,
      Year > 2022 & Treatment == "Repeated" & SPECIES == "ARTR" ~ 230,
      TRUE ~ 0),
    Available_seeds = Seeds + Added_seeds) |> 
  group_by(BARREL, SPECIES) |> 
  arrange(Year, .by_group = TRUE) |> 
  mutate(
    Previous_available = lag(Available_seeds),
    ### numerator is ONLY this year's production, denominator is last year's available pool
    Seed_growth_log = log((Seeds + 1) / (Previous_available + 1))) |> 
  ungroup() |> 
  filter(Year > 2022) |> 
  mutate(Year = as.factor(Year))

### check for NAs
count_df_growth |> summarise(
  across(c(Seed_growth_log, Added_seeds, LH, Treatment, Year, BARREL),
         ~ sum(is.na(.x))))


### sanity checks
count_df_growth |> count(Treatment, Seeded, SPECIES, Added_seeds)
count_df_growth |> filter(Previous_available == 0) |> count(Treatment, SPECIES)

####### Seed population growth model #######

# take a look at the data first

hist(count_df_growth$Seed_growth, breaks = 20)
hist(count_df_growth$Seed_growth_log, breaks = 20)


##############
### Once models are finished  
# plot posteriors 
# show effect sizes
#####################################
###
count_df_growth
glimpse(count_df_growth) # species needs to be factor, LH_2 also needs to be a factor

count_df_growth <- count_df_growth %>% 
  mutate(SPECIES = as.factor(SPECIES), 
         LH_2 = as.factor(LH_2))

# Model 1
mod_seed_1 <- brms::brm(
  Seed_growth_log ~ LH + Treatment + (1 | BARREL),
  data = count_df_growth,
  family = gaussian(),
  iter = 4000,
  warmup = 1500,
  chains = 3)


mod_seed_2 <- brms::brm(
  Seed_growth_log ~ LH + Treatment + Year + (1 | BARREL),
  data = count_df_growth,
  family = gaussian(),
  iter = 4000,
  warmup = 1500,
  chains = 3)

mod_seed_3 <- brms::brm( 
  Seed_growth_log ~ LH * Treatment + Year + (1 | BARREL),
  data = count_df_growth,
  family = gaussian(),
  iter = 4000,
  warmup = 1500,
  chains = 3, 
  control = list(max_treedepth = 12))

fixef(mod_seed_3)[grep("LH.*Treatment|Treatment.*LH", 
                         rownames(fixef(mod_seed_3))), ]
plot(mod_seed_4,
     pars = "^b_",
     prob = 0.95)

# mod_seed_4 <- brms::brm( ### this one has RHAt of 2.66... BAD
#   Seed_growth_log ~ LH * Treatment + Year + SPECIES + Treatment * SPECIES + (1 | BARREL),
#   data = count_df_growth,
#   family = gaussian(),
#   iter = 4000,
#   warmup = 1500,
#   chains = 3)


###   Well if adding Species is causing problems... Let's code LH_2
mod_seed_4 <- brms::brm( 
  Seed_growth_log ~ LH_2 * Treatment + Year + (1 | BARREL),
  data = count_df_growth,
  family = gaussian(),
  iter = 4000,
  warmup = 1500,
  chains = 3)

mod_seed_5 <- brms::brm( 
  Seed_growth_log ~ LH_2 * Treatment + LH_2 * Year + (1 | BARREL),
  data = count_df_growth,
  family = gaussian(),
  iter = 4000,
  warmup = 1500,
  chains = 3)


#mod_seed_4 <- add_criterion(mod_seed_4, criterion = "")
summary(mod_seed_1)
summary(mod_seed_2)
summary(mod_seed_3)
summary(mod_seed_4)
pp_check(mod_seed_3, ndraws = 100)
pp_check(mod_seed_3, type = "stat_grouped", stat = "median", group = "LH")


loo_compare(loo(mod_seed_1), loo(mod_seed_2), loo(mod_seed_3), loo(mod_seed_4), loo(mod_seed_5))
## Move forward with models 3 & 4


##########################################################
params_3 <- as_tibble(as.data.frame(mod_seed_3))
n_iter <- nrow(params_3)
names(params_3)[grepl("^b_", names(params_3))]   # check the coefficient names

# remember all are in reference to FAST_SINGLE_2023
name_map <- c(
  FAST_Single_2023        = "b_Intercept",
  MEDIUM          = "b_LHMEDIUM",
  SLOW            = "b_LHSLOW",
  Repeated_FAST = "b_TreatmentRepeated",
  MEDIUM_Repeated   = "b_LHMEDIUM:TreatmentRepeated",
  SLOW_Repeated     = "b_LHSLOW:TreatmentRepeated",
  Year2024        = "b_Year2024",
  Year2025        = "b_Year2025",
  sd_barrel               = "sd_BARREL__Intercept"
)

params_3 <- params_3 |> rename(all_of(name_map))

params_3 <- params_3 %>% 
  relocate(MEDIUM_Repeated, SLOW_Repeated, .after = Repeated_FAST,  )
glimpse(params_3)

# # Median and 95% interval of a set of draws
# summ <- function(x) {
#   c(median = median(x),
#     lower  = unname(quantile(x, 0.025)),
#     upper  = unname(quantile(x, 0.975)))
# }


params_3 %>% 
  select(1:3) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.5) + 
  geom_vline(xintercept = 0)
  facet_wrap(~ parameter, scales = "free")

params_3 %>% 
  select(4:6) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.5) + 
  facet_wrap(~ parameter, scales = "free")

# Repeated vs Single effect within each life history.
# Because of the interaction, Repeated_FAST is the effect for FAST only;
# MEDIUM and SLOW add their interaction terms to it.
repeated <- params_3 |>
  transmute(FAST   = Repeated_FAST,
            MEDIUM = Repeated_FAST + MEDIUM_Repeated,
            SLOW   = Repeated_FAST + SLOW_Repeated) |>
  pivot_longer(everything(), names_to = "LH", values_to = "effect") |>
  mutate(LH = factor(LH, levels = c("FAST", "MEDIUM", "SLOW")))

# Density plot (log scale on the bottom, growth ratio on the top axis)
ggplot(repeated, aes(x = effect, fill = LH)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(sec.axis = sec_axis(~ exp(.), name = "Growth ratio (Repeated / Single)")) +
  labs(x = "Repeated - Single (Seed_growth_log)", y = "Posterior density", fill = NULL) +
  theme_classic()+
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) 

# Numbers behind the plot
repeated |>
  group_by(LH) |>
  summarise(median    = median(effect),
            lower95   = quantile(effect, 0.025),
            upper95   = quantile(effect, 0.975),
            p_above_0 = mean(effect > 0),
            ratio     = exp(median(effect)),
            ratio_lower = exp(quantile(effect, 0.025)),
            ratio_upper = exp(quantile(effect, 0.975)))




### Model 4 output & visualization
params_4 <- as_tibble(as.data.frame(mod_seed_4))


names(params_4)[grepl("^b_", names(params_4))]   # check the coefficient names
# name_map_4 <- c(
#   FAST_BRTE_Single_2023        = "b_Intercept",
#   FAST_LAGL        = "b_LH_2FAST_N",
#   MEDIUM          = "b_LH_2MEDIUM",
#   SLOW            = "b_LH_2SLOW",
#   FAST_BRTE_REP = "b_TreatmentRepeated",
#   FAST_LAGL_Repeated = "b_LH_2FAST_N:TreatmentRepeated",
#   MEDIUM_Repeated   = "b_LH_2MEDIUM:TreatmentRepeated",
#   SLOW_Repeated     = "b_LH_2SLOW:TreatmentRepeated",
#   Year2024        = "b_Year2024",
#   Year2025        = "b_Year2025",
#   sd_barrel               = "sd_BARREL__Intercept"
# )

par_names_4 <- c(BRTE = "b_Intercept",
                 LAGL = "b_LH_2FAST_N",
                 ELEL = "b_LH_2MEDIUM",              
                 ARTR = "b_LH_2SLOW",
                 REP = "b_TreatmentRepeated",
                 YR_2024 = "b_Year2024",     
                 YR_2025 = "b_Year2025",
  LAGL_REP = "b_LH_2FAST_N:TreatmentRepeated",
  ELEL_REP ="b_LH_2MEDIUM:TreatmentRepeated", 
  ARTR_REP = "b_LH_2SLOW:TreatmentRepeated")


params_4 <- params_4 |> rename(all_of(par_names_4))

params_4 <- params_4 %>% 
  relocate(MEDIUM_Repeated, SLOW_Repeated, .after = Repeated_FAST  )
glimpse(params_4)


params_4 %>% 
  select(1:4) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.5) + facet_wrap(~ parameter, scales = "free")

params_4 %>% 
  select(1:4) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.5) + 
  facet_wrap(~ parameter, scales = "free")


params_4 %>% 
  select(REP) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.5) + 
  geom_vline(xintercept = 0)+
  facet_wrap(~ parameter, scales = "free")


params_4 %>% 
    select(YR_2024, YR_2025) %>% 
    pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
    ggplot(aes(x = value, fill = parameter)) +
    geom_density(alpha = 0.5) + 
    geom_vline(xintercept = 0)

  
###################################
###  MODEL 5 ### 
# Look at year * species effect

params_5 <- as_tibble(as.data.frame(mod_seed_5))


names(params_5)[grepl("^b_", names(params_5))]   # check the coefficient names
# name_map_4 <- c(
#   FAST_BRTE_Single_2023        = "b_Intercept",
#   FAST_LAGL        = "b_LH_2FAST_N",
#   MEDIUM          = "b_LH_2MEDIUM",
#   SLOW            = "b_LH_2SLOW",
#   FAST_BRTE_REP = "b_TreatmentRepeated",
#   FAST_LAGL_Repeated = "b_LH_2FAST_N:TreatmentRepeated",
#   MEDIUM_Repeated   = "b_LH_2MEDIUM:TreatmentRepeated",
#   SLOW_Repeated     = "b_LH_2SLOW:TreatmentRepeated",
#   Year2024        = "b_Year2024",
#   Year2025        = "b_Year2025",
#   sd_barrel               = "sd_BARREL__Intercept"
# )

par_names_5 <- c(BRTE_S_2023 = "b_Intercept",
                 LAGL_ = "b_LH_2FAST_N",
                 ELEL = "b_LH_2MEDIUM",              
                 ARTR = "b_LH_2SLOW",
                 REP = "b_TreatmentRepeated",
                 YR_2024 = "b_Year2024",     
                 YR_2025 = "b_Year2025",
                 LAGL_REP = "b_LH_2FAST_N:TreatmentRepeated",
                 ELEL_REP ="b_LH_2MEDIUM:TreatmentRepeated", 
                 ARTR_REP = "b_LH_2SLOW:TreatmentRepeated")


params_5 <- params_5 |> rename(all_of(par_names_5))

### nicer panel titles (mapping taken from your par_names_4: FAST_I = BRTE, FAST_N = LAGL, etc.)
lh_labels <- c(
  FAST_I = "BRTE (FAST_I)",
  FAST_N = "LAGL (FAST_N)",
  MEDIUM = "ELEL (MEDIUM)",
  SLOW   = "ARTR (SLOW)")

### only LH_2 x Treatment x Year combos that exist in the data
newdat <- count_df_growth |> 
  distinct(LH_2, Treatment, Year)

### posterior expected values; re_formula = NA is the "average barrel"
post_draws <- newdat |> 
  add_epred_draws(mod_seed_5, re_formula = NA)

dodge <- position_dodge(width = 0.6)

p_lh_year <- ggplot(post_draws, aes(x = Year, y = .epred, colour = Treatment, group = Treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  ### raw barrel-level data in the background
  geom_point(
    data = count_df_growth,
    aes(y = Seed_growth_log),
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.4),
    alpha = 0.2, size = 1) +
  ### posterior mean connected across years
  stat_summary(fun = mean, geom = "line", position = dodge) +
  ### 66% and 95% credible intervals
  stat_pointinterval(.width = c(0.66, 0.95), position = dodge) +
  facet_wrap(~ LH_2, nrow = 2, labeller = as_labeller(lh_labels)) +
  labs(
    x = "Year",
    y = "Seed growth (log change in seeds)",
    colour = "Treatment",
    title = "Seed growth by life history, treatment and year",
    subtitle = "Posterior mean with 66% and 95% credible intervals. Faint dots: raw barrels") +
  theme_bw()

p_lh_year

p_dens <- post_draws |> 
  ungroup() |> 
  ggplot(aes(x = .epred, fill = Year, colour = Year, linetype = Treatment)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_density(alpha = 0.25, linewidth = 0.6) +
  facet_wrap(~ LH_2, nrow = 2, labeller = as_labeller(lh_labels), scales = "free") +
  labs(
    x = "",
    y = "") +
  theme_bw()

p_dens



########################################
### Models of ABUNDANCE ###
########################################
count_df_growth

count_mod_1 <- brms::brm( 
  Count ~ LH * Treatment + Year + (1 | BARREL),
  data = count_df_growth,
  family = negbinomial(),
  iter = 5000,
  warmup = 3000,
  chains = 3, 
  control = list(max_treedepth = 12))

count_mod_2 <- brms::brm( 
  Count ~ LH_2 * Treatment + Year + (1 | BARREL),
  data = count_df_growth,
  family = negbinomial(),
  iter = 5000,
  warmup = 300,
  chains = 3, 
  control = list(max_treedepth = 12))

count_mod_3 <- brms::brm( 
  Count ~ LH_2 * Treatment + LH_2 * Year + (1 | BARREL),
  data = count_df_growth,
  family = negbinomial(),
  iter = 5000,
  warmup = 3000,
  chains = 3, 
  control = list(max_treedepth = 12))


