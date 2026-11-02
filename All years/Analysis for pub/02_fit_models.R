########################
### ALL Barrel Data  ###
### 02 - FIT MODELS  ###
########################

library(tidyverse)
library(brms)
library(parallel)

#### What this script does
# 1. Loads the data saved by 01_data_prep.R
# 2. Fits the seed growth, abundance and fitness models
# 3. Tests them (convergence, posterior predictive checks, LOO)
# Every fit is cached in models/. Re-running the script loads the saved fit
# instead of refitting, unless the data, formula, family or priors changed.
# To force a refit of one model, delete its .rds file in models/.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir.create("models", showWarnings = FALSE)

options(mc.cores = parallel::detectCores())
options(brms.file_refit = "on_change")   # refit only if data/formula/family/priors change
n_cores <- parallel::detectCores()

count_df_growth  <- readRDS("data/count_df_growth.rds")
count_df_fitness <- readRDS("data/count_df_fitness.rds")


#################################
### 1. SEED GROWTH MODELS     ###
#################################

mod_seed_1 <- brms::brm(
  Seed_growth_log ~ LH + Treatment + (1 | BARREL),
  data = count_df_growth,
  family = gaussian(),
  iter = 4000,
  warmup = 1500,
  chains = 3,
  file = "models/mod_seed_1")

mod_seed_2 <- brms::brm(
  Seed_growth_log ~ LH + Treatment + Year + (1 | BARREL),
  data = count_df_growth,
  family = gaussian(),
  iter = 4000,
  warmup = 1500,
  chains = 3,
  file = "models/mod_seed_2")

mod_seed_3 <- brms::brm( 
  Seed_growth_log ~ LH * Treatment + Year + (1 | BARREL),
  data = count_df_growth,
  family = gaussian(),
  iter = 4000,
  warmup = 1500,
  chains = 3, 
  control = list(max_treedepth = 12),
  file = "models/mod_seed_3")

# mod_seed_4 with SPECIES added had an Rhat of 2.66... BAD
# Species causes problems, so use LH_2 (separates FAST native vs FAST invasive)
mod_seed_4 <- brms::brm( 
  Seed_growth_log ~ LH_2 * Treatment + Year + (1 | BARREL),
  data = count_df_growth,
  family = gaussian(),
  iter = 4000,
  warmup = 1500,
  chains = 3,
  file = "models/mod_seed_4")

mod_seed_5 <- brms::brm( 
  Seed_growth_log ~ LH_2 * Treatment + LH_2 * Year + (1 | BARREL),
  data = count_df_growth,
  family = gaussian(),
  iter = 4000,
  warmup = 1500,
  chains = 3,
  file = "models/mod_seed_5")

### Convergence table for every model fit so far (want Rhat < 1.01, no divergences)
seed_models <- list(mod_seed_1 = mod_seed_1, mod_seed_2 = mod_seed_2, mod_seed_3 = mod_seed_3,
                    mod_seed_4 = mod_seed_4, mod_seed_5 = mod_seed_5)

check_fits <- function(models) {
  purrr::map_dfr(models, function(m) {
    tibble(
      max_rhat       = max(rhat(m), na.rm = TRUE),
      min_neff_ratio = min(neff_ratio(m), na.rm = TRUE),
      n_divergent    = sum(subset(nuts_params(m), Parameter == "divergent__")$Value))
  }, .id = "model")
}

check_fits(seed_models)

### Summaries and checks
summary(mod_seed_1)
summary(mod_seed_2)
summary(mod_seed_3)
summary(mod_seed_4)
summary(mod_seed_5)

fixef(mod_seed_3)[grep("LH.*Treatment|Treatment.*LH", rownames(fixef(mod_seed_3))), ]

plot(mod_seed_4, variable = "^b_", regex = TRUE)   # trace plots

pp_check(mod_seed_3, ndraws = 100)
pp_check(mod_seed_3, type = "stat_grouped", stat = "median", group = "LH")
pp_check(mod_seed_5, ndraws = 100)

### Model comparison (LOO is stored with each model so it is only computed once)
mod_seed_1 <- add_criterion(mod_seed_1, "loo", file = "models/mod_seed_1")
mod_seed_2 <- add_criterion(mod_seed_2, "loo", file = "models/mod_seed_2")
mod_seed_3 <- add_criterion(mod_seed_3, "loo", file = "models/mod_seed_3")
mod_seed_4 <- add_criterion(mod_seed_4, "loo", file = "models/mod_seed_4")
mod_seed_5 <- add_criterion(mod_seed_5, "loo", file = "models/mod_seed_5")

loo_compare(mod_seed_1, mod_seed_2, mod_seed_3, mod_seed_4, mod_seed_5)
## Move forward with models 3 & 4 (and look at 5 for year * species)


#################################
### 2. ABUNDANCE MODELS       ###
#################################

count_mod_1 <- brms::brm( 
  Count ~ LH * Treatment + Year + (1 | BARREL),
  data = count_df_growth,
  family = negbinomial(),
  iter = 5000,
  warmup = 3000,
  chains = 3, 
  control = list(max_treedepth = 12),
  file = "models/count_mod_1")

count_mod_2 <- brms::brm( 
  Count ~ LH_2 * Treatment + Year + (1 | BARREL),
  data = count_df_growth,
  family = negbinomial(),
  iter = 5000,
  warmup = 3000,   # was 300 in the old script (typo?)
  chains = 3, 
  control = list(max_treedepth = 12),
  file = "models/count_mod_2")

count_mod_3 <- brms::brm( 
  Count ~ LH_2 * Treatment + LH_2 * Year + (1 | BARREL),
  data = count_df_growth,
  family = negbinomial(),
  iter = 5000,
  warmup = 3000,
  chains = 3, 
  control = list(max_treedepth = 12),
  file = "models/count_mod_3")

check_fits(list(count_mod_1 = count_mod_1, count_mod_2 = count_mod_2, count_mod_3 = count_mod_3))

count_mod_1 <- add_criterion(count_mod_1, "loo", file = "models/count_mod_1")
count_mod_2 <- add_criterion(count_mod_2, "loo", file = "models/count_mod_2")
count_mod_3 <- add_criterion(count_mod_3, "loo", file = "models/count_mod_3")

loo_compare(count_mod_1, count_mod_2, count_mod_3)


#################################
### 3. FITNESS MODELS         ###
#################################
## Fitness = Count / seeds available going into that year.
## Hobbs and Hooten suggest Beta... zero_inflated_beta handles the exact zeros.

### zero_inflated_beta() needs every Fitness in [0, 1). This stops the script if not.
count_df_fitness |> summarise(
  n_rows = n(),
  n_zero = sum(Fitness == 0),
  n_ge_1 = sum(Fitness >= 1))

stopifnot("Fitness has values >= 1: zero_inflated_beta() will fail. Fix in 01_data_prep.R or use beta_binomial()" =
            all(count_df_fitness$Fitness < 1))

fit_mod_1 <- brms::brm(
  Fitness ~ LH_2 * Treatment + LH_2 * Year + (1 | BARREL),
  data = count_df_fitness,
  family = zero_inflated_beta(),
  iter = 5000,
  warmup = 3000,
  chains = 3, 
  control = list(max_treedepth = 12),
  file = "models/fit_mod_1")

#### zero-inflation part also depends on LH_2 and Treatment
fit_mod_2 <- brms::brm(
  bf(Fitness ~ LH_2 * Treatment + LH_2 * Year + (1 | BARREL),
     zi ~ LH_2 + Treatment),
  data = count_df_fitness,
  family = zero_inflated_beta(),
  iter = 5000,
  warmup = 3000,
  chains = 4,                 # one chain per core is the usual setup
  cores = min(4, n_cores),    # cores can't usefully exceed chains
  control = list(max_treedepth = 12),
  file = "models/fit_mod_2")

check_fits(list(fit_mod_1 = fit_mod_1, fit_mod_2 = fit_mod_2))

summary(fit_mod_1)
summary(fit_mod_2)
pp_check(fit_mod_2, ndraws = 100)

fit_mod_1 <- add_criterion(fit_mod_1, "loo", file = "models/fit_mod_1")
fit_mod_2 <- add_criterion(fit_mod_2, "loo", file = "models/fit_mod_2")

loo_compare(fit_mod_1, fit_mod_2)
