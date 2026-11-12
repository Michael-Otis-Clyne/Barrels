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

lh_cols <- c(FAST_I = "#B33F40", FAST_N = "#F6B505", MEDIUM = "#6F7C12", SLOW = "#AAC7B9")  # BRTE colour is a placeholder


#############################################
### ABUNDANCE: distribution of the response ###
#############################################
count_mod_1 <- readRDS("models/count_mod_1.rds")
summary(count_mod_1)

# Create prediction grid
pred_grid <- expand_grid(
  LH = levels(count_df_growth$LH),
  Treatment = levels(count_df_growth$Treatment),
  Year = levels(count_df_growth$Year))

# Ensure factors have the same levels as the original data
pred_grid <- pred_grid |> 
  mutate(
    LH = factor(LH, levels = levels(count_df_growth$LH)),
    Treatment = factor(Treatment, levels = levels(count_df_growth$Treatment)),
    Year = factor(Year, levels = levels(count_df_growth$Year)))

# Generate posterior expected abundance
pred_draws <- posterior_epred(
  count_mod_1,
  newdata = pred_grid,
  re_formula = NA)

summarize_predictions <- function(draws, grid, group_vars) {
  
  group_id <- grid |> 
    group_by(across(all_of(group_vars))) |> 
    group_indices()
  
  groups <- grid |> 
    group_by(across(all_of(group_vars))) |> 
    group_keys()
  
  pred_summary <- map_dfr(
    seq_len(nrow(groups)),
    function(i) {
      
      # Average predictions across combinations within each group
      group_draws <- rowMeans(
        draws[, group_id == i, drop = FALSE]
      )
      
      tibble(
        Estimate = mean(group_draws),
        Est.Error = sd(group_draws),
        Q2.5 = quantile(group_draws, 0.025),
        Q97.5 = quantile(group_draws, 0.975)
      )
    }
  )
  
  bind_cols(groups, pred_summary)
}


pred_LH <- summarize_predictions(
  draws = pred_draws,
  grid = pred_grid,
  group_vars = "LH"
)

ggplot(pred_LH, aes(x = LH, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = Q2.5, ymax = Q97.5),
    width = 0.15,
    linewidth = 0.8
  ) +
  labs(
    x = "Life-history strategy",
    y = "Predicted abundance",
    title = "Effect of life-history strategy on abundance"
  ) +
  theme_classic(base_size = 14)


pred_treatment <- summarize_predictions(
  draws = pred_draws,
  grid = pred_grid,
  group_vars = c("LH", "Treatment"))

ggplot(
  pred_treatment,
  aes(
    x = Treatment,
    y = Estimate,
    color = LH,
    group = LH
  )
) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(
    aes(ymin = Q2.5, ymax = Q97.5),
    width = 0.15,
    linewidth = 0.8
  ) +
  labs(
    x = "Treatment",
    y = "Predicted abundance",
    color = "Life history",
    title = "Treatment effects on abundance"
  ) +
  theme_classic(base_size = 14)





pred_interaction <- summarize_predictions(
  draws = pred_draws,
  grid = pred_grid,
  group_vars = c("LH", "Treatment", "Year"))

ggplot(
  pred_interaction,
  aes(
    x = Treatment,
    y = Estimate,
    color = LH,
    group = LH
  )
) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(
    aes(ymin = Q2.5, ymax = Q97.5),
    width = 0.15,
    linewidth = 0.8
  ) +
  facet_wrap(~ Year) +
  labs(
    y = "Predicted abundance",
    color = "Life history") 

######################
###  Count mod 2  ###
######################
count_mod_2 <- readRDS("models/count_mod_2.rds")
summary(count_mod_2)

# Create prediction grid
pred_grid_count_2 <- expand_grid(
  LH_2 = levels(count_df_growth$LH_2),
  Treatment = levels(count_df_growth$Treatment),
  Year = levels(count_df_growth$Year))

# Ensure factors have the same levels as the original data
pred_grid_count_2 <- pred_grid_count_2 |> 
  mutate(
    LH_2 = factor(LH_2, levels = levels(count_df_growth$LH_2)),
    Treatment = factor(Treatment, levels = levels(count_df_growth$Treatment)),
    Year = factor(Year, levels = levels(count_df_growth$Year)))

# Generate posterior expected abundance
pred_draws_count_2 <- posterior_epred(
  count_mod_2,
  newdata = pred_grid_count_2,
  re_formula = NA)

pred_LH_count_2 <- summarize_predictions(
  draws = pred_draws_count_2,
  grid = pred_grid_count_2,
  group_vars = "LH_2")

pred_treatment_count_2 <- summarize_predictions(
  draws = pred_draws_count_2,
  grid = pred_grid_count_2,
  group_vars = c("LH_2", "Treatment"))

#### Show LH and abundance: 
ggplot(pred_LH_count_2, aes(x = LH_2, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = Q2.5, ymax = Q97.5),
    width = 0.15,
    linewidth = 0.8) +
  labs(x = "Life history", y = "Predicted abundance")


### now show LH & treatment effect
ggplot(pred_treatment_count_2|> filter(LH_2  !="FAST_I"), 
       aes(x = Treatment, y = Estimate, 
                                   color = LH_2, 
                                   group = LH_2))+   
  geom_point(width = 0.1,size = 3) +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5),
    width = 0.15, linewidth = 0.8)

ggplot(pred_treatment_count_2 |> filter(LH_2  !="FAST_I"),
       aes(x = Treatment, y = Estimate,
    color = LH_2,
    group = LH_2)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(
    aes(ymin = Q2.5, ymax = Q97.5),
    width = 0.15,
    linewidth = 0.8
  ) +
  labs(
    x = "Treatment",
    y = "Predicted abundance",
    color = "Life history",
    title = "Treatment effects on abundance"
  )


pred_int_count_2 <- summarize_predictions(
  draws = pred_draws_count_2,
  grid = pred_grid_count_2,
  group_vars = c("LH_2", "Treatment", "Year"))

ggplot(pred_int_count_2|> filter(LH_2  !="FAST_I"), 
       aes(
    x = Treatment,
    y = Estimate,
    color = LH_2,
    group = LH_2)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(
    aes(ymin = Q2.5, ymax = Q97.5),
    width = 0.15,
    linewidth = 0.8) +
  scale_colour_manual(values = lh_cols, name = "Life history") +
  facet_wrap(~ Year) +
  labs(
    y = "Predicted abundance",
    color = "Life history") + theme_classic()


#####################################################
### Log response ratio figure ###
#####################################################

# lh_cols <- c(FAST_N = "#F6B505", MEDIUM = "#6F7C12", SLOW = "#AAC7B9")

# Log response ratio per posterior draw

lrr_draws <- pred_grid_count_2 |>
  add_epred_draws(count_mod_2, re_formula = NA) |>
  ungroup() |>
  select(LH_2, Treatment, Year, .draw, .epred) |>
  pivot_wider(names_from = Treatment, values_from = .epred) |>
  mutate(LRR = log((Repeated + 1) / (Single + 1)))

# Summarize across draws
lrr_summary <- lrr_draws |>
  group_by(LH_2, Year) |>
  summarise(Estimate = median(LRR),
            Q2.5     = quantile(LRR, 0.025),
            Q97.5    = quantile(LRR, 0.975),
            .groups = "drop")

# Figure
ggplot(lrr_summary |> filter(LH_2 != "FAST_I"),
       aes(x = LH_2, y = Estimate, colour = LH_2)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.15, linewidth = 0.8) +
 # scale_colour_manual(values = lh_cols, name = "Life history") +
  facet_wrap(~ Year, nrow = 1) +
  labs(x = NULL,
       y = "Log response ratio") +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())







###################################################
###  plots of posterior distributions
###################################################
## model form =   Count ~ LH_2 * Treatment + Year + (1 | BARREL),

count_2_post <- as_tibble(as.data.frame(count_mod_2))
names(count_2_post)

# plot the species effect on abundcnace 
count_2_post %>% 
  select(1:4) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.5) + 
  facet_wrap(~ parameter, scales = "free")

#### Take a look at the treatment effect by Species
count_2_post %>% 
  select(5,8:10) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.5) + 
  facet_wrap(~ parameter, scales = "free")


count_2_fig <- count_2_post 


names(count_2_post)[grepl("^b_", names(count_2_post))]   # get the coefficient names
levels(count_df_growth$LH_2); levels(count_df_growth$Treatment); levels(count_df_growth$Year)

par_names_c2 <- c(BRTE_S_2023 = "b_Intercept",
                 LAGL       = "b_LH_2FAST_N",
                 ELEL        = "b_LH_2MEDIUM",              
                 ARTR        = "b_LH_2SLOW",
                 REP         = "b_TreatmentRepeated",
                 YR_2024     = "b_Year2024",     
                 YR_2025     = "b_Year2025",
                 FAST_REP    = "b_LH_2FAST_N:TreatmentRepeated",
                 MED_REP    = "b_LH_2MEDIUM:TreatmentRepeated", 
                 SLOW_REP    = "b_LH_2SLOW:TreatmentRepeated")

count_2_fig <- count_2_fig |> rename(all_of(par_names_c2))


#count_2_fig <- count_2_fig %>% pivot_longer(everything(), names_to = "parameter", values_to = "value")

# Easier: estimated counts for every cell, no mental arithmetic
library(emmeans)
emmeans(count_mod_2, ~ Treatment | LH_2, type = "response")

# Treatment contrast within each LH_2 level (as ratios)
pairs(emmeans(count_mod_2, ~ Treatment | LH_2), type = "response")

levels(count_df_growth$LH_2)
levels(count_df_growth$Treatment)
levels(count_df_growth$Year)




### In order to show the actual effects (posteriors) of the predictors, I need to add the 
### rest of the parameters to b0 

ce_count <- conditional_effects(
  count_mod_2,
  effects = "LH_2:Treatment",
  conditions = data.frame(Year = levels(count_df_growth$Year)),
  re_formula = NA)

plot(ce_count, points = T, ask = FALSE)

plot(ce_count, plot = FALSE)













############################################################
###  Count mod 3 = 
###   Count ~ LH_2 + Treatment + LH_2 * Year + (1 | BARREL)
############################################################
count_mod_3 <- readRDS("models/count_mod_3.rds")
summary(count_mod_3)












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

##### COUNT/available seed models

fit_mod_1 <- readRDS("models/fit_mod_1.rds")
fit_mod_2 <- readRDS("models/fit_mod_2.rds")
fit_mod_3 <- readRDS("models/fit_mod_3.rds")
fit_mod_4 <- readRDS("models/fit_mod_4.rds")


####################
###  Fit mod 1  ####
####################
summary(fit_mod_1)



####################
###  Fit mod 2  ####
####################
summary(fit_mod_2)


####################
###  Fit mod 3  ####
####################
summary(fit_mod_3)







####################
###  Fit mod 4  ####
####################
summary(fit_mod_4)
post_4 <- as_draws_df(fit_mod_4) |>
  select(starts_with("b_"), sd_BARREL__Intercept, phi, zi) |>
  pivot_longer(everything(), names_to = "parameter", values_to = "value")

# 1. Identified parameters, one density panel each (SLOW terms excluded, see below)
post_4 |>
  filter(!str_detect(parameter, "SLOW")) |>
  ggplot(aes(value)) +
  geom_density(fill = "grey70", colour = NA) +
  geom_vline(data = ~ filter(.x, str_starts(parameter, "b_"), parameter != "b_Intercept"),
             aes(xintercept = 0), linetype = 2) +   # zero line on effect terms only
  facet_wrap(~ parameter, scales = "free", ncol = 3) +
  labs(x = NULL, y = "Posterior density") +
  theme_classic() +
  theme(strip.background = element_blank(), strip.text = element_text(face = "bold"))

# 2. Same coefficients overlaid on one axis (logit scale), with 66% / 95% intervals
fit_mod_4 |>
  gather_draws(`b_.*`, regex = TRUE) |>
  filter(!str_detect(.variable, "Intercept|SLOW")) |>
  ggplot(aes(.value, .variable)) +
  stat_halfeye(.width = c(0.66, 0.95)) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "Effect on logit(mean fitness)", y = NULL) +
  theme_classic()

# 3. The problem terms: the two SLOW coefficients vs. their sum
as_draws_df(fit_mod_4) |>
  transmute(`SLOW (main)`        = b_LH_2SLOW,
            `SLOW:Repeated`      = `b_LH_2SLOW:TreatmentRepeated`,
            `Sum (SLOW, Repeated)` = b_LH_2SLOW + `b_LH_2SLOW:TreatmentRepeated`) |>
  pivot_longer(everything()) |>
  ggplot(aes(value, fill = name)) +
  geom_density(alpha = 0.5, colour = NA) +
  labs(x = "logit scale", y = "Density", fill = NULL) +
  theme_classic()










############################################################################
### I've decided against these models, but here is the code just in case ###
############################################################################


mod_logfit_1 <- readRDS("models/mod_logfit_1.rds")
mod_logfit_2 <- readRDS("models/mod_logfit_2.rds")
mod_logfit_3 <- readRDS("models/mod_logfit_3.rds")
mod_logfit_4 <- readRDS("models/mod_logfit_4.rds")



#############################################
### logfit mod 1  ###
#############################################
summary(mod_logfit_1)

logfit_1 <- as_tibble(as.data.frame(mod_logfit_1))

log_exp_1 <- logfit_1 |> # exponentiate before plotting to put it back into the original response
  mutate(across(where(is.numeric), exp)) # (abundance +1 / available seeds +1)




logfit_1 %>% 
  select(2:4) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
  ggplot(aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.5) + 
  facet_wrap(~ parameter, scales = "free")


lh_cols <- c(FAST_I = "#B33F40", FAST_N = "#F6B505", MEDIUM = "#6F7C12", SLOW = "#AAC7B9")  # BRTE colour is a placeholder

# replace colour = Treatment with colour = LH_2, group = Treatment, linetype = Treatment,
# add scale_colour_manual(values = lh_cols), and drop the facet or facet by Treatment instead














pred <- nd |>
  add_epred_draws(mod_logfit_1, re_formula = NA) |>
  mutate(.epred_exp = exp(.epred))

# 3. Figure: raw data behind model predictions, by year, treatment and LH_2
ggplot(pred, aes(Year, .epred_exp, colour = Treatment, group = Treatment)) +
  geom_point(data = count_df_fitness,
             aes(Year, exp(log_fit), colour = Treatment),
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.5),
             alpha = 0.2, size = 1, inherit.aes = FALSE) +
  stat_summary(fun = median, geom = "line",
               position = position_dodge(width = 0.5)) +
  stat_pointinterval(.width = c(0.66, 0.95), point_size = 3,
                     position = position_dodge(width = 0.5)) +
  scale_y_log10() +
  facet_wrap(~ LH_2, nrow = 1) +
  labs(x = NULL,
       y = "(Count + 1) / (Available seeds + 1)",
       colour = "Seeding treatment") +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))









#############################################
### logfit mod 2  ###
#############################################
summary(mod_logfit_2)














############################################################################
### I've decided against these models, but here is the code just in case ###
############################################################################


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



# Prediction grid
pred_grid_seed_4 <- expand_grid(
  LH_2 = levels(count_df_growth$LH_2),
  Treatment = levels(count_df_growth$Treatment),
  Year = levels(count_df_growth$Year))

# Ensure factors match the model data
pred_grid_seed_4 <- pred_grid_seed_4 |>
  mutate(
    LH_2 = factor(LH_2, levels = levels(count_df_growth$LH_2)),
    Treatment = factor(
      Treatment,
      levels = levels(count_df_growth$Treatment)
    ),
    Year = factor(
      Year,
      levels = levels(count_df_growth$Year)))

# Posterior expected seed growth
pred_draws_seed_4 <- posterior_epred(
  mod_seed_4,
  newdata = pred_grid_seed_4,
  re_formula = NA)


summarize_predictions <- function(draws, grid, group_vars) {
  
  group_id <- grid |>
    group_by(across(all_of(group_vars))) |>
    group_indices()
  
  groups <- grid |>
    group_by(across(all_of(group_vars))) |>
    group_keys()
  
  pred_summary <- map_dfr(
    seq_len(nrow(groups)),
    function(i) {
      
      group_draws <- rowMeans(
        draws[, group_id == i, drop = FALSE]
      )
      
      tibble(
        Estimate = mean(group_draws),
        Est.Error = sd(group_draws),
        Q2.5 = quantile(group_draws, 0.025),
        Q97.5 = quantile(group_draws, 0.975)
      )
    }
  )
  
  bind_cols(groups, pred_summary)
}


pred_LH_seed_4 <- summarize_predictions(
  draws = pred_draws_seed_4,
  grid = pred_grid_seed_4,
  group_vars = "LH_2"
)

ggplot(
  pred_LH_seed_4,
  aes(x = LH_2, y = Estimate)
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = Q2.5, ymax = Q97.5),
    width = 0.15,
    linewidth = 0.8
  ) +
  labs(
    x = "Life-history strategy",
    y = "Predicted seed growth (log scale)",
    title = "Effect of life-history strategy on seed growth"
  ) +
  theme_classic(base_size = 14)


pred_treatment_seed_4 <- summarize_predictions(
  draws = pred_draws_seed_4,
  grid = pred_grid_seed_4,
  group_vars = c("LH_2", "Treatment"))

ggplot(
  pred_treatment_seed_4,
  aes(
    x = Treatment,
    y = Estimate,
    color = LH_2,
    group = LH_2)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(
    aes(ymin = Q2.5, ymax = Q97.5),
    width = 0.15,
    linewidth = 0.8
  ) +
  labs(
    x = "Treatment",
    y = "Predicted seed growth (log scale)",
    color = "Life-history strategy",
    title = "Treatment effects on seed growth"
  ) +
  theme_classic(base_size = 14)




pred_interaction_seed <- summarize_predictions(
  draws = pred_draws_seed_4,
  grid = pred_grid_seed_4,
  group_vars = c("LH_2", "Treatment", "Year"))

ggplot(
  pred_interaction_seed,
  aes(
    x = Treatment,
    y = Estimate,
    color = LH_2,
    group = LH_2)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(
    aes(ymin = Q2.5, ymax = Q97.5),
    width = 0.15,
    linewidth = 0.8) +
  facet_wrap(~ Year) +
  labs(
    x = "Treatment",
    y = "Predicted seed growth (log scale)",
    color = "Life-history strategy") 







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

