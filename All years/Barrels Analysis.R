########################
### ALL Barrel Data  ###
### Manuscript Script  ###
########################

library(tidyverse)
library(readxl)
library(rstan)
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
# so negative binomial or poisson




###### Do this in brms first, because we are going to test several models


mod_simp <- brms::brm(Count ~ LH + (1 | BARREL), 
                   data = count_df_mod, 
                   family = poisson(), # I think negative binomial will be better bc it allows the variance to be greater then the mean
                   iter = 4000, warmup = 1500, chains = 3)


summary(mod_simp)
plot(mod_simp)
pp_check(mod_simp, ndraws = 100)


mod_1 <- brms::brm(Count ~ LH + (1 + LH | BARREL), 
          data = count_df_mod, 
          family = negbinomial(),
          iter = 4000, warmup = 1500, chains = 3)

summary(mod_1)
plot(mod_1)
pp_check(mod_1, ndraws = 100) # a little high on the posterior but the shape matches. 


### Try a Negative Binomial
mod_2 <- brms::brm(Count ~ LH + (1 + LH | BARREL), 
                   data = count_df_mod, 
                   family = negbinomial(),
                   iter = 4000, warmup = 1500, chains = 3)

summary(mod_2)
plot(mod_2)
pp_check(mod_2, ndraws = 10)


### model with year RE
mod_3 <- brms::brm(Count ~ LH_2 * Treatment + (1 | Year), 
                   data = count_df_mod, 
                   family = negbinomial(),
                   iter = 4000, warmup = 1000, chains = 3, cores = 4)

summary(mod_3)
plot(mod_3)
pp_check(mod_3, ndraws = 10)

summary(mod_3)$fixed[, 5:7]

# quick and dirty plot on the original scale
plot(conditional_effects(mod_3), ask = FALSE)

posterior_3 <- as.matrix(mod_3)

mcmc_areas(posterior_3,
           pars = c("b_TreatmentRepeated", "b_LH_2FAST_N:TreatmentRepeated", "b_LH_2MEDIUM:TreatmentRepeated"),
           # arbitrary threshold for shading probability mass
           prob = 0.9) 

########
# Want to graph the posteriors of the predictors 
library(tidybayes)
post_data <- count_df_mod %>% 
  expand_grid(LH_2, Treatment) %>%
  add_fitted_draws(mod_3, n = 9000, re_formula = NA)






data_grid
### most complex model
# how does abundance vary with LH and treatment 
#   allowing for abundance to vary among barrels and change differently over years
mod_4 <- brms::brm(Count ~ LH * Treatment + (1 + Year | BARREL), 
                   data = count_df_mod, 
                   family = negbinomial(),
                   iter = 4000, warmup = 1500, chains = 3)

summary(mod_4)
plot(mod_4)
pp_check(mod_4, ndraws = 10)

# make_stancode(Count ~ LH * Treatment + (1 + Year | BARREL), 
#               data = count_df_mod, 
#               family = negbinomial(),
#               iter = 4000, warmup = 1500, chains = 3)
summary(mod_4)$fixed

fixef(mod_4)
fixef(mod_4)[, "Estimate"]
mod_4_post <-  as.matrix(mod_4)

ranef(mod_4)

head(mod_4_post)

draws <- as_draws_df(mod_4)
names(draws)
draws$b_LHSLOW


pars_4 <- as_draws_df(mod_4)

# Create prediction dataset
newdat <- expand.grid(BARREL = unique(count_df_mod$BARREL),
                      LH = unique(count_df_mod$LH),
  Treatment = c("Single", "Repeated"), Year = unique(count_df_mod$Year))

# Posterior predictions
pred_4 <- posterior_epred(mod_4, newdata = newdat, re_formula = NULL)

# Calculate posterior mean and 95% credible interval
pred_df <- newdat %>%
  mutate(Estimate = colMeans(pred_4),
    Lower = apply(pred_4, 2, quantile, probs = 0.025),
    Upper = apply(pred_4, 2, quantile, probs = 0.975))



ggplot(pred_df, aes(x = BARREL, y = Estimate, 
                    group = LH, color = LH)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Barrel",
    y = "Predicted abundance",
    color = "Life history") +
  theme_classic()


# mcmc_areas(mod_4_post,
#            pars = c("b_Intercept", 
#                     "b_LHMEDIUM", "b_LHSLOW", 
#                     "b_TreatmentRepeated", "b_LHMEDIUM:TreatmentRepeated", "b_LHSLOW:TreatmentRepeated"),
#            # arbitrary threshold for shading probability mass
#            prob = 0.83) 
# 
# mcmc_areas(mod_4_post,
#            pars = c("b_TreatmentRepeated", "b_LHMEDIUM:TreatmentRepeated"),
#            # arbitrary threshold for shading probability mass
#            prob = 0.83) 

# we are curious about the effect of LH and seeding treatment on plant counts
# do this in Stan?

#start easy, then work up in complexity

# modeldata <- list(C = count_df_mod$Count,
#                   Yr = count_df_mod$Count,
#                   )
# 
# # Run the model
# BRTE_fitness_mod <- stan(file = "Barrel_glmm.stan",
#                          data = modeldata, 
#                          chains = 3,iter = 2000, warmup = 1000)
#   




##############
### Once models are finshed  
# plot posteriors 
# show effect sizes











