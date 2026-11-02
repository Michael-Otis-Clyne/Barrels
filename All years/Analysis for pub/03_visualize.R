##############################
### ALL Barrel Data        ###
### 03 - MODEL VISUALIZATION ###
##############################

library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)

#### What this script does
# Loads the data (01_data_prep.R) and the saved model fits (02_fit_models.R)
# and makes the figures. No models are fit here.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir.create("figures", showWarnings = FALSE)

count_df_growth  <- readRDS("data/count_df_growth.rds")
count_df_fitness <- readRDS("data/count_df_fitness.rds")

mod_seed_3 <- readRDS("models/mod_seed_3.rds")
mod_seed_4 <- readRDS("models/mod_seed_4.rds")
mod_seed_5 <- readRDS("models/mod_seed_5.rds")


#############################################
### MODEL 3: posteriors and effect sizes  ###
#############################################
params_3 <- as_tibble(as.data.frame(mod_seed_3))
n_iter <- nrow(params_3)
names(params_3)[grepl("^b_", names(params_3))]   # check the coefficient names

# remember all are in reference to FAST_SINGLE_2023
name_map <- c(
  FAST_Single_2023  = "b_Intercept",
  MEDIUM            = "b_LHMEDIUM",
  SLOW              = "b_LHSLOW",
  Repeated_FAST     = "b_TreatmentRepeated",
  MEDIUM_Repeated   = "b_LHMEDIUM:TreatmentRepeated",
  SLOW_Repeated     = "b_LHSLOW:TreatmentRepeated",
  Year2024          = "b_Year2024",
  Year2025          = "b_Year2025",
  sd_barrel         = "sd_BARREL__Intercept"
)

params_3 <- params_3 |> rename(all_of(name_map))

params_3 <- params_3 %>% 
  relocate(MEDIUM_Repeated, SLOW_Repeated, .after = Repeated_FAST)
glimpse(params_3)

params_3 %>% 
  select(1:3) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.5) + 
  geom_vline(xintercept = 0) +
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
  theme_classic() +
  geom_vline(xintercept = 0, colour = "black", linewidth = 1) 

# Numbers behind the plot
repeated |>
  group_by(LH) |>
  summarise(median      = median(effect),
            lower95     = quantile(effect, 0.025),
            upper95     = quantile(effect, 0.975),
            p_above_0   = mean(effect > 0),
            ratio       = exp(median(effect)),
            ratio_lower = exp(quantile(effect, 0.025)),
            ratio_upper = exp(quantile(effect, 0.975)))


#############################################
### MODEL 4: posteriors and effect sizes  ###
#############################################
params_4 <- as_tibble(as.data.frame(mod_seed_4))

names(params_4)[grepl("^b_", names(params_4))]   # check the coefficient names

# remember all are in reference to BRTE, Single, 2023
par_names_4 <- c(BRTE     = "b_Intercept",
                 LAGL     = "b_LH_2FAST_N",
                 ELEL     = "b_LH_2MEDIUM",              
                 ARTR     = "b_LH_2SLOW",
                 REP      = "b_TreatmentRepeated",
                 YR_2024  = "b_Year2024",     
                 YR_2025  = "b_Year2025",
                 LAGL_REP = "b_LH_2FAST_N:TreatmentRepeated",
                 ELEL_REP = "b_LH_2MEDIUM:TreatmentRepeated", 
                 ARTR_REP = "b_LH_2SLOW:TreatmentRepeated")

params_4 <- params_4 |> rename(all_of(par_names_4))
glimpse(params_4)

### life history and year effects (selected by name, not by column position)
plot_pars <- c("BRTE", "LAGL", "ELEL", "ARTR", "YR_2024", "YR_2025")

### dashed line at 0 for every panel except the intercept (BRTE is a baseline, not a contrast)
zero_lines <- tibble(
  parameter = factor(setdiff(plot_pars, "BRTE"), levels = plot_pars),
  x = 0)

params_4 |> 
  select(all_of(plot_pars)) |> 
  pivot_longer(everything(), names_to = "parameter", values_to = "value") |> 
  mutate(
    parameter = factor(parameter, levels = plot_pars),
    Type = case_when(
      parameter == "BRTE" ~ "Intercept (BRTE, Single, 2023)",
      str_starts(parameter, "YR_") ~ "Year effect (vs. 2023)",
      TRUE ~ "Life history (vs. BRTE)")) |> 
  ggplot(aes(x = value, fill = Type)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = zero_lines, aes(xintercept = x), linetype = "dashed") +
  facet_wrap(~ parameter, scales = "free", ncol = 3) +
  labs(x = "Coefficient (log seed growth)", y = "Posterior density") +
  theme_bw()

### repeated seeding effect (BRTE baseline)
params_4 %>% 
  select(REP) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.5) + 
  geom_vline(xintercept = 0) +
  facet_wrap(~ parameter, scales = "free")


###################################################
### MODEL 5: life history x treatment x year    ###
###################################################
params_5 <- as_tibble(as.data.frame(mod_seed_5))

names(params_5)[grepl("^b_", names(params_5))]   # check the coefficient names

par_names_5 <- c(BRTE_S_2023 = "b_Intercept",
                 LAGL_       = "b_LH_2FAST_N",
                 ELEL        = "b_LH_2MEDIUM",              
                 ARTR        = "b_LH_2SLOW",
                 REP         = "b_TreatmentRepeated",
                 YR_2024     = "b_Year2024",     
                 YR_2025     = "b_Year2025",
                 LAGL_REP    = "b_LH_2FAST_N:TreatmentRepeated",
                 ELEL_REP    = "b_LH_2MEDIUM:TreatmentRepeated", 
                 ARTR_REP    = "b_LH_2SLOW:TreatmentRepeated")

params_5 <- params_5 |> rename(all_of(par_names_5))

### nicer panel titles (FAST_I = BRTE, FAST_N = LAGL, etc.)
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

### posterior densities, log scale
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

### posterior densities, exponentiated (growth ratio; reference line is 1, not 0)
p_dens_ratio <- post_draws |> 
  ungroup() |> 
  mutate(Growth_ratio = exp(.epred)) |> 
  ggplot(aes(x = Growth_ratio, fill = Year, colour = Year, linetype = Treatment)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_density(alpha = 0.25, linewidth = 0.6) +
  facet_wrap(~ LH_2, nrow = 2, labeller = as_labeller(lh_labels), scales = "free") +
  labs(
    x = "Model-estimated seed growth ratio (seeds this year / seeds available last year)",
    y = "Posterior density") +
  theme_bw()

p_dens_ratio

### save figures (uncomment as needed)
# ggsave("figures/seed_growth_by_LH.png",      p_lh_year,     width = 9, height = 7, dpi = 300)
# ggsave("figures/seed_growth_density.png",    p_dens,        width = 9, height = 7, dpi = 300)
# ggsave("figures/seed_growth_ratio_dens.png", p_dens_ratio,  width = 9, height = 7, dpi = 300)


#############################################
### FITNESS: distribution of the response ###
#############################################
hist(count_df_fitness$Fitness, breaks = 40)

ggplot(data = count_df_fitness,
       mapping = aes(x = Fitness)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "grey80", colour = "white") +
  geom_density(fill = "steelblue", colour = "steelblue4",
               alpha = 0.25, linewidth = 0.8) +
  labs(x = NULL, y = NULL) +
  theme_classic()
## So it looks like maybe a Beta, Gamma, or Binomial distribution might work. 

### fitness model output goes here once fit_mod_2 is chosen:
# fit_mod_2 <- readRDS("models/fit_mod_2.rds")
