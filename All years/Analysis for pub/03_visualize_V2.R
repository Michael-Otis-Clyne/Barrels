##############################
### ALL Barrel Data        ###
### 03 - MODEL VISUALIZATION ###
##############################

library(tidyverse)
library(brms)
library(tidybayes)

#### What this script does
# Loads the data (01_data_prep.R) and the saved model fits (02_fit_models.R)
# and makes the figures. No models are fit here.
#
# 0. Setup: data, models, helpers, palette
# 1. ABUNDANCE MODELS         (count_mod_2)
# 2. BRTE COMPETITOR MODELS   (BRTE by native partner: count_mod_2, brte_mod_1, brte_mod_2)
# 3. SEED GROWTH MODELS       (mod_seed_3, mod_seed_4, mod_seed_5)
# 4. FITNESS MODELS           (fit_mod_2; figures still to come)


##########################################
### 0. SETUP                           ###
##########################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir.create("figures", showWarnings = FALSE)

### Set to TRUE to write every figure to figures/ when the script is run
save_figs <- FALSE

### ---- data ----
count_df_growth  <- readRDS("data/count_df_growth.rds")
count_df_fitness <- readRDS("data/count_df_fitness.rds")
brte_df          <- readRDS("data/brte_df.rds")   # BRTE rows + Partner (LAGL / ELEL / ARTR); made in 02

### ---- models (all loaded here, none fit) ----
load_model <- function(name) readRDS(file.path("models", paste0(name, ".rds")))

count_mod_2 <- load_model("count_mod_2")   # swap in count_mod_1 / count_mod_3 here if needed

brte_mod_1  <- load_model("brte_mod_1")    # Partner * Treatment + Year
brte_mod_2  <- load_model("brte_mod_2")    # Partner * Treatment * Year

mod_seed_3  <- load_model("mod_seed_3")
mod_seed_4  <- load_model("mod_seed_4")
mod_seed_5  <- load_model("mod_seed_5")

# fit_mod_2 <- load_model("fit_mod_2")     # fitness model, used in section 4

### the Partner levels in the data must match the models, or predictions are mislabelled
stopifnot(identical(levels(brte_df$Partner), levels(brte_mod_1$data$Partner)),
          identical(levels(brte_df$Partner), levels(brte_mod_2$data$Partner)))

### ---- palette, labels, theme ----
partner_colors <- c(LAGL = "#F6B505", ELEL = "#6F7C12", ARTR = "#AAC7B9")
partner_labels <- c(LAGL = "LAGL (fast)", ELEL = "ELEL (medium)", ARTR = "ARTR (slow)")

# FAST_I = BRTE (fast, invasive); FAST_N = LAGL (fast, native)
lh_labels <- c(FAST_I = "BRTE (FAST_I)",
               FAST_N = "LAGL (FAST_N)",
               MEDIUM = "ELEL (MEDIUM)",
               SLOW   = "ARTR (SLOW)")

theme_paper <- theme_classic() +
  theme(strip.background = element_blank(),
        strip.text       = element_text(face = "bold"))

### ---- helpers ----
save_fig <- function(plot, name, width = 7, height = 5) {
  if (save_figs) {
    ggsave(file.path("figures", paste0(name, ".png")), plot,
           width = width, height = height, dpi = 300)
  }
  invisible(plot)
}

get_post <- function(model) as_tibble(as.data.frame(model))

### Numbers behind an effect plot: median, 95% interval, P(effect > 0) and the
### same on the ratio scale (exp of a log-scale effect)
summarise_effects <- function(d) {
  d |>
    pivot_longer(everything(), names_to = "parameter", values_to = "value") |>
    mutate(parameter = factor(parameter, levels = unique(parameter))) |>
    group_by(parameter) |>
    summarise(median      = median(value),
              lower95     = quantile(value, 0.025),
              upper95     = quantile(value, 0.975),
              p_above_0   = mean(value > 0),
              ratio       = exp(median),
              ratio_lower = exp(lower95),
              ratio_upper = exp(upper95),
              .groups = "drop")
}

### Posterior densities of log-scale effects shown as ratios (x axis on log scale).
### Dashed line at 1 = no effect.
plot_effects_ratio <- function(d, xlab) {
  d |>
    pivot_longer(everything(), names_to = "parameter", values_to = "value") |>
    mutate(parameter = factor(parameter, levels = unique(parameter))) |>
    ggplot(aes(x = exp(value), fill = parameter)) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_x_log10() +
    facet_wrap(~ parameter, scales = "free") +
    labs(x = xlab, y = "Posterior density") +
    theme_paper +
    theme(legend.position = "none")
}

### Posterior densities of raw coefficients, one panel each.
### baseline = intercept-type parameter(s) that get no dashed zero line
### types    = optional named vector (parameter -> group label) used for the fill
plot_coef_density <- function(post, pars, baseline = NULL, types = NULL,
                              ncol = 3, xlab = "Coefficient") {
  long <- post |>
    select(all_of(pars)) |>
    pivot_longer(everything(), names_to = "parameter", values_to = "value") |>
    mutate(parameter = factor(parameter, levels = pars))

  long$Type <- if (is.null(types)) {
    "Coefficient"
  } else {
    factor(unname(types[as.character(long$parameter)]), levels = unique(types))
  }

  zero_lines <- tibble(parameter = factor(setdiff(pars, baseline), levels = pars),
                       x = 0)

  p <- ggplot(long, aes(x = value, fill = Type)) +
    geom_density(alpha = 0.5) +
    geom_vline(data = zero_lines, aes(xintercept = x), linetype = "dashed") +
    facet_wrap(~ parameter, scales = "free", ncol = ncol) +
    labs(x = xlab, y = "Posterior density", fill = NULL) +
    theme_paper

  if (is.null(types)) p + theme(legend.position = "none") else p
}

### Population-level predictions (re_formula = NA = the "average barrel") for every
### LH_2 x Treatment x Year combination that exists in the data
get_lh_draws <- function(model, data) {
  data |>
    distinct(LH_2, Treatment, Year) |>
    add_epred_draws(model, re_formula = NA)
}

### Predictions (median, 66% / 95% intervals) with the raw barrel-level data behind them,
### one panel per life history. y = name of the response column in `data`.
plot_lh_treatment_year <- function(draws, data, y, ylab, title = NULL, subtitle = NULL,
                                   scales = "fixed", zero_line = FALSE) {
  dodge <- position_dodge(width = 0.6)

  p <- ggplot(draws, aes(x = Year, y = .epred, colour = Treatment, group = Treatment))
  if (zero_line) p <- p + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50")

  p +
    geom_point(data = data, aes(y = .data[[y]]),
               position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.6),
               alpha = 0.2, size = 1) +
    stat_summary(fun = mean, geom = "line", position = dodge) +
    stat_pointinterval(.width = c(0.66, 0.95), position = dodge) +
    facet_wrap(~ LH_2, nrow = 2, scales = scales, labeller = as_labeller(lh_labels)) +
    labs(x = "Year", y = ylab, colour = "Treatment", title = title, subtitle = subtitle) +
    theme_paper +
    theme(legend.position = "bottom")
}


##########################################
### 1. ABUNDANCE MODELS (count_mod_2)  ###
##########################################
# Count ~ LH_2 * Treatment + Year + (1 | BARREL), negative binomial.
# Reference levels: FAST_I (BRTE), Single, 2023.

count_2_post <- get_post(count_mod_2)

### ---- 1.1 Life history effect (vs BRTE / FAST_I), within each treatment ----
# Single = the coefficient alone; Repeated = coefficient + its interaction.
lh_effect <- count_2_post |>
  transmute(
    FAST_N_Single   = b_LH_2FAST_N,
    MEDIUM_Single   = b_LH_2MEDIUM,
    SLOW_Single     = b_LH_2SLOW,
    FAST_N_Repeated = b_LH_2FAST_N + `b_LH_2FAST_N:TreatmentRepeated`,
    MEDIUM_Repeated = b_LH_2MEDIUM + `b_LH_2MEDIUM:TreatmentRepeated`,
    SLOW_Repeated   = b_LH_2SLOW   + `b_LH_2SLOW:TreatmentRepeated`)

p_abund_lh <- plot_effects_ratio(lh_effect, xlab = "Ratio of expected count vs BRTE (FAST_I)")
p_abund_lh
summarise_effects(lh_effect)

### ---- 1.2 Treatment effect (Repeated vs Single), within each life history ----
# Effect for each life history = TreatmentRepeated + that life history's interaction.
trt_effect <- count_2_post |>
  transmute(
    FAST_I = b_TreatmentRepeated,
    FAST_N = b_TreatmentRepeated + `b_LH_2FAST_N:TreatmentRepeated`,
    MEDIUM = b_TreatmentRepeated + `b_LH_2MEDIUM:TreatmentRepeated`,
    SLOW   = b_TreatmentRepeated + `b_LH_2SLOW:TreatmentRepeated`)

p_abund_trt <- plot_effects_ratio(trt_effect, xlab = "Ratio of expected count, Repeated / Single")
p_abund_trt
summarise_effects(trt_effect)

### ---- 1.3 Year effect (same for every group; the model has no Year interaction) ----
year_effect <- count_2_post |>
  transmute(`2024 vs 2023` = b_Year2024,
            `2025 vs 2023` = b_Year2025)

p_abund_year <- plot_effects_ratio(year_effect, xlab = "Ratio of expected count vs 2023")
p_abund_year
summarise_effects(year_effect)

### ---- 1.4 Predicted abundance for every life history, treatment and year ----
abund_draws <- get_lh_draws(count_mod_2, count_df_growth)

p_abund_pred <- plot_lh_treatment_year(
  abund_draws, count_df_growth, y = "Count", ylab = "Count",
  title    = "Abundance by life history, treatment and year",
  subtitle = "Posterior median with 66% and 95% credible intervals. Faint dots: raw barrels",
  scales   = "free_y")
p_abund_pred

### ---- 1.5 BRTE abundance by treatment and year ----
brte_pred <- get_lh_draws(count_mod_2, filter(count_df_growth, LH_2 == "FAST_I"))

p_brte_trt_year <- ggplot(brte_pred, aes(Year, .epred, colour = Treatment, shape = Treatment)) +
  geom_point(data = brte_df, aes(Year, Count, colour = Treatment),
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6),
             alpha = 0.3, size = 1.5, inherit.aes = FALSE) +
  stat_pointinterval(aes(group = Treatment), .width = c(0.66, 0.95), point_size = 3.5,
                     position = position_dodge(width = 0.6), colour = "black") +
  labs(x = NULL, y = "BRTE count",
       colour = "Seeding treatment", shape = "Seeding treatment") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"))
p_brte_trt_year

### ---- save ----
save_fig(p_abund_lh,       "abund_lh_effect")
save_fig(p_abund_trt,      "abund_treatment_effect")
save_fig(p_abund_year,     "abund_year_effect")
save_fig(p_abund_pred,     "abund_predictions",   width = 9, height = 7)
save_fig(p_brte_trt_year,  "brte_treatment_year")


#############################################
### 2. BRTE COMPETITOR MODELS             ###
#############################################
# Partner = the native species sharing the barrel with BRTE (LAGL, ELEL or ARTR).

### ---- 2.1 From the abundance model: BRTE by partner, treatment and year ----
# Predictions include the barrel intercepts (re_formula = NULL), then are averaged
# across barrels within each partner x treatment x year.
brte_partner_draws <- brte_df |>
  add_epred_draws(count_mod_2, re_formula = NULL) |>
  group_by(.draw, Partner, Year, Treatment) |>
  summarise(.epred = mean(.epred), .groups = "drop")

p_brte_partner <- ggplot(brte_partner_draws, aes(Partner, .epred, colour = Partner)) +
  geom_point(data = brte_df,
             aes(Partner, Count, shape = Treatment, group = Treatment),
             colour = "black", alpha = 0.4, size = 1,
             position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.6),
             inherit.aes = FALSE) +
  stat_pointinterval(aes(shape = Treatment, group = Treatment),
                     .width = c(0.66, 0.95), point_size = 3,
                     position = position_dodge(width = 0.6)) +
  scale_colour_manual(name = "Native species", values = partner_colors,
                      breaks = names(partner_labels), labels = partner_labels) +
  guides(colour = guide_legend(order = 1, override.aes = list(shape = 16, size = 5)),
         shape  = guide_legend(order = 2)) +
  facet_wrap(~ Year) +
  labs(x = NULL, y = "BRTE count") +
  theme_paper +
  theme(legend.key.size  = unit(1.2, "cm"),
        legend.position  = "bottom",
        legend.title     = element_text(face = "bold", size = 14),
        axis.text.x      = element_blank(),   # the legend replaces the x labels
        axis.ticks.x     = element_blank())
p_brte_partner

### ---- 2.2 Dedicated BRTE competitor models ----
# brte_mod_1: Count ~ Partner * Treatment + Year + (1 | BARREL)
# brte_mod_2: Count ~ Partner * Treatment * Year + (1 | BARREL)
# Model comparison (LOO) is in 02_fit_models.R.

get_brte_pred <- function(model) {
  model$data |>
    distinct(Partner, Treatment, Year) |>
    add_epred_draws(model, re_formula = NA)
}

plot_brte <- function(pred, raw = brte_df) {
  ggplot(pred, aes(Treatment, .epred, colour = Partner, group = Partner)) +
    geom_point(data = raw, aes(Treatment, Count, colour = Partner),
               position = position_jitterdodge(jitter.width = 0.08, dodge.width = 0.4),
               alpha = 0.25, size = 1, inherit.aes = FALSE) +
    stat_summary(fun = median, geom = "line", linewidth = 0.8,
                 position = position_dodge(width = 0.4)) +
    stat_pointinterval(.width = c(0.66, 0.95), point_size = 3,
                       position = position_dodge(width = 0.4)) +
    scale_colour_manual(values = partner_colors, name = "Native partner") +
    facet_wrap(~ Year, scales = "free_y") +
    labs(x = "Seeding treatment", y = "BRTE count") +
    theme_paper +
    theme(legend.position = "bottom")
}

p_brte_mod_1 <- plot_brte(get_brte_pred(brte_mod_1))
p_brte_mod_2 <- plot_brte(get_brte_pred(brte_mod_2))
p_brte_mod_1
p_brte_mod_2

### ---- save ----
save_fig(p_brte_partner, "brte_partner_count_mod_2", width = 9, height = 5)
save_fig(p_brte_mod_1,   "brte_mod_1",               width = 9, height = 5)
save_fig(p_brte_mod_2,   "brte_mod_2",               width = 9, height = 5)


#############################################
### 3. SEED GROWTH MODELS                 ###
#############################################
# Response: Seed_growth_log = log((seeds this year + 1) / (seeds available last year + 1))

### ---- 3.1 mod_seed_3: LH * Treatment + Year ----
# All coefficients are relative to FAST, Single, 2023.
params_3 <- get_post(mod_seed_3) |>
  rename(all_of(c(
    FAST_Single_2023 = "b_Intercept",
    MEDIUM           = "b_LHMEDIUM",
    SLOW             = "b_LHSLOW",
    Repeated_FAST    = "b_TreatmentRepeated",
    MEDIUM_Repeated  = "b_LHMEDIUM:TreatmentRepeated",
    SLOW_Repeated    = "b_LHSLOW:TreatmentRepeated",
    Year2024         = "b_Year2024",
    Year2025         = "b_Year2025",
    sd_barrel        = "sd_BARREL__Intercept")))

p_seed3_coefs <- plot_coef_density(
  params_3,
  pars = c("FAST_Single_2023", "MEDIUM", "SLOW",
           "Repeated_FAST", "MEDIUM_Repeated", "SLOW_Repeated",
           "Year2024", "Year2025"),
  baseline = "FAST_Single_2023",
  xlab = "Coefficient (log seed growth)")
p_seed3_coefs

# Repeated vs Single effect within each life history.
# Because of the interaction, Repeated_FAST is the effect for FAST only;
# MEDIUM and SLOW add their interaction terms to it.
repeated_3 <- params_3 |>
  transmute(FAST   = Repeated_FAST,
            MEDIUM = Repeated_FAST + MEDIUM_Repeated,
            SLOW   = Repeated_FAST + SLOW_Repeated)

p_seed3_repeated <- repeated_3 |>
  pivot_longer(everything(), names_to = "LH", values_to = "effect") |>
  mutate(LH = factor(LH, levels = c("FAST", "MEDIUM", "SLOW"))) |>
  ggplot(aes(x = effect, fill = LH)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(sec.axis = sec_axis(~ exp(.), name = "Growth ratio (Repeated / Single)")) +
  labs(x = "Repeated - Single (Seed_growth_log)", y = "Posterior density", fill = NULL) +
  theme_classic()
p_seed3_repeated

summarise_effects(repeated_3)   # numbers behind the plot

### ---- 3.2 mod_seed_4: LH_2 * Treatment + Year ----
# LH_2 separates the fast invasive (BRTE) from the fast native (LAGL).
# All coefficients are relative to BRTE, Single, 2023.
params_4 <- get_post(mod_seed_4) |>
  rename(all_of(c(
    BRTE     = "b_Intercept",
    LAGL     = "b_LH_2FAST_N",
    ELEL     = "b_LH_2MEDIUM",
    ARTR     = "b_LH_2SLOW",
    REP      = "b_TreatmentRepeated",
    YR_2024  = "b_Year2024",
    YR_2025  = "b_Year2025",
    LAGL_REP = "b_LH_2FAST_N:TreatmentRepeated",
    ELEL_REP = "b_LH_2MEDIUM:TreatmentRepeated",
    ARTR_REP = "b_LH_2SLOW:TreatmentRepeated")))

seed4_types <- c(BRTE     = "Intercept (BRTE, Single, 2023)",
                 LAGL     = "Life history (vs. BRTE)",
                 ELEL     = "Life history (vs. BRTE)",
                 ARTR     = "Life history (vs. BRTE)",
                 YR_2024  = "Year effect (vs. 2023)",
                 YR_2025  = "Year effect (vs. 2023)",
                 REP      = "Repeated seeding (BRTE)",
                 LAGL_REP = "Repeated x life history",
                 ELEL_REP = "Repeated x life history",
                 ARTR_REP = "Repeated x life history")

p_seed4_coefs <- plot_coef_density(
  params_4,
  pars = names(seed4_types),
  baseline = "BRTE",
  types = seed4_types,
  xlab = "Coefficient (log seed growth)")
p_seed4_coefs

### ---- 3.3 mod_seed_5: LH_2 * Treatment + LH_2 * Year ----
seed5_draws <- get_lh_draws(mod_seed_5, count_df_growth)

p_seed5_pred <- plot_lh_treatment_year(
  seed5_draws, count_df_growth, y = "Seed_growth_log",
  ylab     = "Seed growth (log change in seeds)",
  title    = "Seed growth by life history, treatment and year",
  subtitle = "Posterior mean with 66% and 95% credible intervals. Faint dots: raw barrels",
  zero_line = TRUE)
p_seed5_pred

# posterior densities, log scale
p_seed5_dens <- seed5_draws |>
  ungroup() |>
  ggplot(aes(x = .epred, fill = Year, colour = Year, linetype = Treatment)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_density(alpha = 0.25, linewidth = 0.6) +
  facet_wrap(~ LH_2, nrow = 2, labeller = as_labeller(lh_labels), scales = "free") +
  labs(x = "Model-estimated seed growth (log scale)", y = "Posterior density") +
  theme_bw()
p_seed5_dens

# posterior densities, exponentiated (growth ratio; reference line is 1, not 0)
p_seed5_ratio <- seed5_draws |>
  ungroup() |>
  mutate(Growth_ratio = exp(.epred)) |>
  ggplot(aes(x = Growth_ratio, fill = Year, colour = Year, linetype = Treatment)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_density(alpha = 0.25, linewidth = 0.6) +
  facet_wrap(~ LH_2, nrow = 2, labeller = as_labeller(lh_labels), scales = "free") +
  labs(x = "Model-estimated seed growth ratio (seeds this year / seeds available last year)",
       y = "Posterior density") +
  theme_bw()
p_seed5_ratio

### ---- save ----
save_fig(p_seed3_coefs,    "seed3_coefficients",     width = 9, height = 6)
save_fig(p_seed3_repeated, "seed3_repeated_effect")
save_fig(p_seed4_coefs,    "seed4_coefficients",     width = 9, height = 7)
save_fig(p_seed5_pred,     "seed5_by_LH",            width = 9, height = 7)
save_fig(p_seed5_dens,     "seed5_density",          width = 9, height = 7)
save_fig(p_seed5_ratio,    "seed5_ratio_density",    width = 9, height = 7)


#############################################
### 4. FITNESS MODELS                     ###
#############################################
# Fitness = Count / seeds available going into that year (zero-inflated beta).

### ---- 4.1 Distribution of the response ----
p_fitness_dist <- ggplot(count_df_fitness, aes(x = Fitness)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "grey80", colour = "white") +
  geom_density(fill = "steelblue", colour = "steelblue4",
               alpha = 0.25, linewidth = 0.8) +
  labs(x = "Fitness", y = "Density") +
  theme_classic()
p_fitness_dist

save_fig(p_fitness_dist, "fitness_distribution")

### ---- 4.2 Fitness model output goes here once fit_mod_2 is chosen ----
# fit_mod_2 <- load_model("fit_mod_2")   # (or load it in section 0)
