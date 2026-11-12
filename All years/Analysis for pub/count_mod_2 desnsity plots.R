library(dplyr)
library(tidyr)
library(ggplot2)

count_2_post <- as_tibble(as.data.frame(count_mod_2))

# Your density-by-facet style, on the ratio scale (exp of the log-scale effect).
# Dashed line at 1 = no effect.
plot_effects <- function(d, xlab) {
  d %>%
    pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
    mutate(parameter = factor(parameter, levels = unique(parameter))) %>%
    ggplot(aes(x = exp(value), fill = parameter)) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = 1, linetype = 2) +
    scale_x_log10() +
    facet_wrap(~ parameter, scales = "free") +
    labs(x = xlab, y = "Density")
}

#### Life history effect on abundance (vs FAST_I), within each Treatment
# Single = the coefficient alone; Repeated = coefficient + its interaction.

life_history <- count_2_post %>%
  transmute(
    `FAST_N_Single`   = b_LH_2FAST_N,
    `MEDIUM_Single`   = b_LH_2MEDIUM,
    SLOW_Single   = b_LH_2SLOW,
    `FAST_N_Repeated` = b_LH_2FAST_N + `b_LH_2FAST_N:TreatmentRepeated`,
    `MEDIUM_Repeated` = b_LH_2MEDIUM + `b_LH_2MEDIUM:TreatmentRepeated`,
    `SLOW_Repeated`   = b_LH_2SLOW   + `b_LH_2SLOW:TreatmentRepeated`
  )

plot_effects(life_history, xlab = "") # Ratio of expected count vs FAST_I

#### Treatment effect (Repeated vs Single) by life history
# Effect for each life history = TreatmentRepeated + that life history's interaction.

treatment <- count_2_post %>%
  transmute(
    `FAST_I` = b_TreatmentRepeated,
    `REP_FAST_N` = b_TreatmentRepeated + `b_LH_2FAST_N:TreatmentRepeated`,
    `REP_MEDIUM` = b_TreatmentRepeated + `b_LH_2MEDIUM:TreatmentRepeated`,
    REP_SLOW = b_TreatmentRepeated + `b_LH_2SLOW:TreatmentRepeated`
  )

plot_effects(treatment, xlab = "") # "Ratio of expected count, Repeated / Single

#### Year effect (same for every group; no Year interaction)
year <- count_2_post %>%
  transmute(
    `2024 vs 2023` = b_Year2024,
    `2025 vs 2023` = b_Year2025
  )

plot_effects(year, "Ratio of expected count vs 2023")


# Quick version: posterior means and 95% CIs of BRTE abundance by native life history
# ce <- conditional_effects(count_mod_2, effects = "LH_2")
# plot(ce, points = TRUE, point_args = list(alpha = 0.15, width = 0.1))[[1]] +
#   labs(x = "", y = "BRTE abundance")
#############################################
###  BRTE competitor #####
#############################################

brte_df <- count_df_growth |>
  filter(SPECIES == "BRTE") |>
  mutate(Partner = case_match(str_remove(Trt, "B"), 
                              "L" ~ "FAST",
                              "E" ~ "MEDIUM",
                              "A" ~ "SLOW"),
         Partner = factor(Partner, levels = c("FAST", "MEDIUM", "SLOW")))

ggplot(brte_df, aes(Partner, Count, colour = Treatment)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.15), alpha = 0.3) +
  stat_summary(fun.data = mean_cl_boot, position = position_dodge(0.75), size = 0.6) +
  #scale_y_log10() +   # use scale_y_continuous(trans = "pseudo_log") if you have zeros
  facet_wrap(~ Year, scales = "free_y") +
  labs(x = "", y = "BRTE") # +
  theme_classic()

# 1. Identify each BRTE barrel's native partner
brte_barrels <- count_df_growth |> filter(SPECIES == "BRTE") |> distinct(BARREL)

partner_df <- count_df_growth |>
  semi_join(brte_barrels, by = "BARREL") |>
  filter(SPECIES != "BRTE") |>
  distinct(BARREL, Partner = SPECIES)     

newdata <- count_df_growth |>
  filter(SPECIES == "BRTE") |>
  left_join(partner_df, by = "BARREL") |>
  mutate(Partner = factor(Partner, levels = c("LAGL", "ELEL", "ARTR")))

# 2. Posterior predictions for BRTE rows, INCLUDING barrel intercepts
draws <- newdata |>
  add_epred_draws(count_mod_2, re_formula = NULL) |>   # NULL keeps (1 | BARREL)
  group_by(.draw, Partner, Year) |>
  summarise(.epred = mean(.epred), .groups = "drop")   # average barrels within partner

# 3. Figure: BRTE abundance by partner, faceted by year
ggplot(draws, aes(Partner, .epred, fill = Partner)) +
  stat_halfeye(.width = c(0.66, 0.95), alpha = 0.8) +
  facet_wrap(~ Year, scales = "free_y") +
  labs(x = "", y = "Predicted BRTE count") +
  theme_classic() +
  theme(legend.position = "none")


#### Make a figure that shows model predictions and the raw datas overlaid. 
colors = c(LAGL = "#F6B505", ELEL = "#6F7C12", ARTR = "#AAC7B9")

ggplot(draws, aes(Partner, .epred, fill = Partner)) +
  geom_violin(alpha = 0.75, colour = NA, scale = "width", trim = FALSE) +
  geom_jitter(data = newdata,
              aes(Partner, Count, colour = Treatment),
              width = 0.15, alpha = 0.3, size = 1,
              inherit.aes = FALSE) +
  stat_pointinterval(.width = c(0.5, 0.95), point_interval = "median_qi") +
  scale_fill_manual(values = c(LAGL = "#F6B505", ELEL = "#6F7C12", ARTR = "#AAC7B9")) +
  facet_wrap(~ Year) +
  guides(fill = "none") +
  labs(x = "Native species sharing the barrel",
       y = "BRTE count (points = observed, violins = model-predicted mean)") +
  theme_classic()


ggplot(draws, aes(Partner, .epred, colour = Partner)) +
  geom_jitter(data = newdata, aes(Partner, Count),
              colour = "grey60", width = 0.12, alpha = 0.4, size = 1,
              inherit.aes = FALSE) +
  stat_pointinterval(.width = c(0.66, 0.95), point_size = 3,
                     position = position_nudge(x = 0.25)) +
  scale_colour_manual(values = colors) +
  facet_wrap(~ Year, scales = "free_y") +
  guides(colour = "none") +
  labs(x = "Native Species", y = "BRTE count") +
  theme_classic()+   theme(
    legend.position = "bottom",
    legend.title    = element_text(face = "bold"),
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold"),
    axis.text.x  = element_blank(),   # legend replaces the x labels
    axis.ticks.x = element_blank())



ggplot(draws, aes(Partner, .epred, colour = Partner)) +
  geom_jitter(data = newdata, aes(Partner, Count),
              colour = "black", width = 0.12, alpha = 0.4, size = 1,
              inherit.aes = FALSE) +
  stat_pointinterval(.width = c(0.66, 0.95), point_size = 3,
                     position = position_nudge(x = 0.25))  +
  scale_colour_manual(
    name   = "Native species",
    values = colors,
    breaks = c("LAGL", "ELEL", "ARTR"),
    labels = c("LAGL (fast)", "ELEL (medium)", "ARTR (slow)")
  ) +
  facet_wrap(~ Year) +
  labs(x = NULL, y = "BRTE count") +
  theme_classic() +
  theme(legend.key.size = unit(1.2, "cm"),
    legend.position = "bottom",
    legend.title    = element_text(face = "bold", size = 14),
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold"),
    axis.text.x  = element_blank(),   # legend replaces the x labels
    axis.ticks.x = element_blank())




colors <- c(LAGL = "#F6B505", ELEL = "#6F7C12", ARTR = "#AAC7B9")

draws <- newdata |>
  add_epred_draws(count_mod_2, re_formula = NULL) |>
  group_by(.draw, Partner, Year, Treatment) |>
  summarise(.epred = mean(.epred), .groups = "drop")

ggplot(draws, aes(Partner, .epred, colour = Partner)) +
  geom_point(data = newdata,
             aes(Partner, Count, shape = Treatment, group = Treatment),
             colour = "black", alpha = 0.4, size = 1,
             position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.6),
             inherit.aes = FALSE) +
  stat_pointinterval(aes(shape = Treatment, group = Treatment),
                     .width = c(0.66, 0.95), point_size = 3,
                     position = position_dodge(width = 0.6)) +
  scale_colour_manual(
    name   = "Native species",
    values = colors,
    breaks = c("LAGL", "ELEL", "ARTR"),
    labels = c("LAGL (fast)", "ELEL (medium)", "ARTR (slow)")
  ) +
  guides(colour = guide_legend(order = 1, override.aes = list(shape = 16, size = 5)),
         shape  = guide_legend(order = 2)) +
  facet_wrap(~ Year, scales = "free_y") +
  labs(x = NULL, y = "BRTE count") +
  theme_classic() +
  theme(legend.key.size   = unit(1.2, "cm"),
        legend.position   = "bottom",
        legend.title      = element_text(face = "bold", size = 14),
        strip.background  = element_blank(),
        strip.text        = element_text(face = "bold"),
        axis.text.x       = element_blank(),
        axis.ticks.x      = element_blank())

ggplot(draws, aes(Partner, .epred, colour = Partner)) +
  geom_point(data = newdata,
             aes(Partner, Count, shape = Treatment, group = Treatment),
             colour = "black", alpha = 0.4, size = 1,
             position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.6),
             inherit.aes = FALSE) +
  stat_pointinterval(aes(shape = Treatment, group = Treatment),
                     .width = c(0.66, 0.95), point_size = 3,
                     position = position_dodge(width = 0.6)) +
  scale_colour_manual(
    name   = "Native species",
    values = colors,
    breaks = c("LAGL", "ELEL", "ARTR"),
    labels = c("LAGL (fast)", "ELEL (medium)", "ARTR (slow)")
  ) +
  guides(colour = guide_legend(order = 1, override.aes = list(shape = 16, size = 5)),
         shape  = guide_legend(order = 2)) +
  facet_wrap(~ Year) +
  labs(x = NULL, y = "BRTE count") +
  theme_classic() +
  theme(legend.key.size   = unit(1.2, "cm"),
        legend.position   = "bottom",
        legend.title      = element_text(face = "bold", size = 14),
        strip.background  = element_blank(),
        strip.text        = element_text(face = "bold"),
        axis.text.x       = element_blank(),
        axis.ticks.x      = element_blank())






#########################
### Would it be more powerful to show BRTE by treatment and year?
brte_raw <- count_df_growth |> filter(SPECIES == "BRTE")

ggplot(brte_raw, aes(Year, Count, colour = Treatment)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6),
             alpha = 0.35, size = 1.5) +
  stat_summary(fun.data = mean_cl_boot, position = position_dodge(width = 0.6),
               size = 0.8) +
  labs(x = NULL, y = "BRTE count", colour = "Seeding treatment") +
  theme_classic() +
  theme(legend.position = "bottom")


pred_brte <- count_df_growth |>
  filter(LH_2 == "FAST_I") |>
  distinct(LH_2, Treatment, Year) |>
  add_epred_draws(count_mod_2, re_formula = NA)   # population-level, ignores barrel

ggplot(pred_brte, aes(Year, .epred, colour = Treatment)) +
  geom_point(data = brte_raw, aes(Year, Count, colour = Treatment),
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6),
             alpha = 0.3, size = 1.5, inherit.aes = FALSE) +
  stat_pointinterval(.width = c(0.66, 0.95), point_size = 3,
                     position = position_dodge(width = 0.6)) +
  labs(x = NULL, y = "BRTE count", colour = "Seeding treatment") +
  theme_classic() +
  theme(legend.position = "bottom")


brte_raw <- count_df_growth |> filter(SPECIES == "BRTE")

pred_brte <- count_df_growth |>
  filter(LH_2 == "FAST_I") |>
  distinct(LH_2, Treatment, Year) |>
  add_epred_draws(count_mod_2, re_formula = NA)   # population-level, ignores barrel

ggplot(pred_brte, aes(Year, .epred, colour = Treatment, shape = Treatment)) +
  # raw data
  geom_point(data = brte_raw,
             aes(Year, Count, colour = Treatment),
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6),
             alpha = 0.3, size = 1.5, inherit.aes = FALSE) +
  # model predictions (median, 66% and 95% credible intervals)
  stat_pointinterval(.width = c(0.66, 0.95), point_size = 3.5,
                     position = position_dodge(width = 0.6),
                     colour = "black", aes(group = Treatment)) +
  labs(x = NULL, y = "BRTE count",
       colour = "Seeding treatment", shape = "Seeding treatment") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"))






##########################################
###   From the BRTE competitor models ###
##########################################
brte_mod_1 <- readRDS("models/brte_mod_1.rds")
brte_mod_2 <- readRDS("models/brte_mod_2.rds")


brte_df <- newdata   # BRTE rows with Partner = LAGL / ELEL / ARTR

levels(brte_df$Partner)                # should be "LAGL" "ELEL" "ARTR"
levels(brte_mod_1$data$Partner)        # should be identical

colors <- c(LAGL = "#F6B505", ELEL = "#6F7C12", ARTR = "#AAC7B9")

get_pred <- function(mod) {
  mod$data |>
    distinct(Partner, Treatment, Year) |>
    add_epred_draws(mod, re_formula = NA)
}

plot_brte <- function(pred) {
  ggplot(pred, aes(Treatment, .epred, colour = Partner, group = Partner)) +
    geom_point(data = brte_df, aes(Treatment, Count, colour = Partner),
               position = position_jitterdodge(jitter.width = 0.08, dodge.width = 0.4),
               alpha = 0.25, size = 1, inherit.aes = FALSE) +
    stat_summary(fun = median, geom = "line", linewidth = 0.8,
                 position = position_dodge(width = 0.4)) +
    stat_pointinterval(.width = c(0.66, 0.95), point_size = 3,
                       position = position_dodge(width = 0.4)) +
    scale_colour_manual(values = colors, name = "Native partner") +
    facet_wrap(~ Year, scales = "free_y") +
    labs(x = "Seeding treatment", y = "BRTE count") +
    theme_classic() +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(face = "bold"))
}

pred_1 <- get_pred(brte_mod_1); plot_brte(pred_1)
pred_2 <- get_pred(brte_mod_2); plot_brte(pred_2)




