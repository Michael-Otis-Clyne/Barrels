########################
### Barrel Data 2025 ###
########################

library(tidyverse)
library(readxl)
library(dplyr)
library(conflicted)
conflict_prefer("select", "dplyr", "filter")

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


#############################
### Now for the demo data ###
#############################

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





#########################################
# To do # 
#########################################
# 1) Figures of abundance changes year to year
# 2) effect of treatments on BRTE
#   2a) repeat seeding effect
#   2b) species treatment on BRTE
#   2c) repeat seeding on Slow LH species abundance over time


count_new <- Cdata_all %>%
  # Pivot species columns into long format
  pivot_longer(
    cols = c(BRTE, LAGL, ELEL, ARTR),
    names_to = "Species",
    values_to = "Count")


###################################
#############
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

#
count_plot_df <- count_new_trt %>%
  mutate(
    # get part before "_" (e.g., "BA", "LA", "E", "A")
    CodePre = sub("_.*", "", Trt),
    
    # logicals: which species letters are present in the treatment?
    Has_B = grepl("B", CodePre),
    Has_L = grepl("L", CodePre),
    Has_E = grepl("E", CodePre),
    Has_A = grepl("A", CodePre),
    
    # species seeded in that barrel (may be multiple!)
    SeededSpecies = case_when(
      Species == "BRTE" & Has_B ~ "BRTE",
      Species == "LAGL" & Has_L ~ "LAGL",
      Species == "ELEL" & Has_E ~ "ELEL",
      Species == "ARTR" & Has_A ~ "ARTR",
      TRUE ~ NA_character_
    ), Seeded = !is.na(SeededSpecies)) %>% # creates a seeded species column. 
  mutate(Year = as.factor(Year))

count_plot_df <- count_plot_df %>% mutate(Species = factor(Species, levels = c("LAGL", "ELEL", "ARTR", "BRTE")),
                                          Treatment = factor(Treatment, levels = c("Single", "Repeated")))


count_plot_df <- count_plot_df %>% 
  mutate(Count = case_when(Species == "ARTR" & Has_A == T & is.na(Count) ~ 0, T ~ Count)) 

#count_plot_df %>% filter(Species == "ARTR" & Treatment == "Single" & Count >0)

#### Show each treatment effects across species and all years

### first we gotta do an ANOVA and tukey then label the plots ###
# ANOVA 
library("report") 
library(multcomp)
library(emmeans)
# gotta fix the data for ANOVA
aov_count_data <- count_plot_df %>% filter(Seeded == T) %>% select(BARREL, Year, Species, Count, Treatment, Seeded)
aov_count_data <- aov_count_data %>%
  mutate(Count = case_when(
      Species == "ARTR" & Year == 2023 & is.na(Count) ~ 0,
      Species == "LAGL" & Year == 2023 & is.na(Count) ~ 0,
      Species == "ELEL" & Year == 2023 & is.na(Count) ~ 0,
      Species == "LAGL" & Year == 2024 & is.na(Count) ~ 0,
      Species == "ARTR" & Year == 2024 & is.na(Count) ~ 0,
      Species == "ARTR" & Year == 2025 & is.na(Count) ~ 0,
      TRUE ~ Count))

which(is.na(aov_count_data_t$Count))

aov_trt_all <- aov(Count ~ Treatment,
               data = aov_count_data)

summary(aov_trt_all)

aov_NO_brte <- aov_count_data %>% filter(!Species %in% "BRTE") %>% 
  mutate(Treatment = as.factor(Treatment), Year = as.factor(Year))
head(aov_NO_brte)

aov_trt_Nbrte <- aov(Count ~ Treatment,
                   data = aov_NO_brte)
summary(aov_trt_Nbrte)
report(aov_trt_Nbrte) # p=0.006, effect size is 0.01. 
# effects size of 0.01 explains only 1% of plant counts from treatments

# Tukey time
# Tukey HSD test:
post_test <- glht(aov_trt_Nbrte,
                  linfct = mcp(Treatment = "Tukey"))

summary(post_test)
plot(post_test)


emm_trt <- emmeans(aov_trt_Nbrte, ~ Treatment)
pair_trt <- pairs(emm_trt)

pair_trt
pval <- summary(pair_trt)$p.value


### show all native species together
# ggplot(
#   data = aov_NO_brte,
#   aes(x = Treatment, y = Count, fill = Treatment)) +
#   geom_boxplot(alpha = 0.9, outliers = F) +
#   annotate("text", x = 1, y = 25, label = "a", size = 6) + 
#   annotate("text", x = 2, y = 50, label = "b", size = 6) + 
#   scale_fill_brewer(palette = "Set2") +
#   ggtitle("Native Species Abundance by Treatment") +
#   labs(fill = "Treatment") + 
#   theme_light()
  


#################################
#### ARTR counts by treatment ###

ggplot(data = count_plot_df %>% filter(Species == "ARTR" & Seeded == T), 
       aes(x = Treatment, y = Count, fill = Treatment)) + 
  geom_boxplot(alpha = 0.9, outliers = F)  +
  annotate("text", x = 1, y = 2.5, label = "a", size = 6) +
  annotate("text", x = 2, y = 10, label = "b", size = 6) + 
  #theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("ARTR Abundance by Treatment") +
  xlab("Treatment") + ylab("Plant Count")+
  labs(fill = "") + 
  theme_light()+
  scale_fill_brewer(palette = "Set2") + 
  theme(legend.position = "bottom")



##########################
### ARTR counts by year & treatment ###

# # do stats
# aov_ARTR_trt <- aov(Count ~ Treatment*Year,
#                        data = aov_NO_brte)
# summary(aov_ARTR_trt)
# report(aov_ARTR_trt) # p=0.006, effect size is 0.01. 
# # effects size of 0.01 explains only 1% of plant counts from treatments
# 
# # Tukey time
# # Tukey HSD test:
# post_test_ARTR <- glht(aov_ARTR_trt,
#                   linfct = mcp(Treatment = "Tukey", Year = "Tukey"))
# 
# summary(post_test_ARTR)
# plot(post_test_ARTR)
# emm_trt_ARTR <- emmeans(aov_ARTR_trt, ~ Treatment)
# pair_trt_ARTR <- pairs(emm_trt_ARTR)
# 
# pair_trt_ARTR
# pval_ARTR <- summary(pair_trt_ARTR)$p.value
# 
# 
# ggplot(data = count_plot_df %>% filter(Species == "ARTR" & Has_A == T), 
#        aes(x = Year, y = Count, fill = Treatment)) + 
#   geom_boxplot(alpha = 0.9, outliers = F)  +
#   annotate("text", x = 0.8, y = 10, label = "a", size = 6) + 
#   annotate("text", x = 1.2, y = 10, label = "a", size = 6) + 
#   annotate("text", x = 1.8, y = 10, label = "a", size = 6) +
#   annotate("text", x = 2.19, y = 30, label = "b", size = 6) + 
#   annotate("text", x = 3.19, y = 10, label = "b", size = 6) +
#   #theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#   ggtitle("ARTR Abundance by Treatment") +
#   xlab("Treatment") + ylab("Plant Count")+
#   labs(fill = "Treatment") + 
#   theme_light()+
#   scale_fill_brewer(palette = "Set2") + 
#   theme(legend.position = "bottom")
# 



# show all species and all years

ggplot(count_plot_df %>% 
         filter(Seeded == TRUE & Species != "BRTE"), 
       aes(x = factor(Year), y = Count, fill = factor(Treatment))) +
  geom_boxplot(alpha = 0.9, outliers = F,
               position = position_dodge(width = 0.75)) +
  geom_point(aes(group = Treatment),
             position = position_jitterdodge(
               jitter.width = 0.15,
               dodge.width = 0.75),
             alpha = 0.5, size = 1) +
  facet_wrap(~Species, scales = "free_y") +
  ggtitle("Plant Counts by Species and Treatment") +
  xlab("Year") + ylab("Plant Count") +
  labs(fill = "Treatment") + 
  theme_light() +
  scale_fill_brewer(palette = "Set2") + 
  theme(legend.position = "bottom")




ggplot(count_plot_df %>% 
         filter(Seeded == TRUE & !Species == "BRTE" & !Year == 2023), 
       aes(x = factor(Year), y = Count, fill = factor(Treatment))) +
  geom_boxplot(alpha = 0.9, outliers = F) +
  facet_wrap(~Species, scales = "free_y") +
  #theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Plant Counts by Species and Treatment") +
  xlab("Year") + ylab("Plant Count")+
  labs(fill = "Treatment") + 
  theme_light()+
  scale_fill_brewer(palette = "Set2") + 
  theme(legend.position = "bottom")


########################
# Linear Models #
library(lme4)
### start with counts
# TO DO
# 1. Treatment ~ count
# 1. Treatment ~ count | Barrel

# 2. native Species ~ BRTE count

count_model <- aov_count_data 
count_model <- count_model %>% 
  mutate(Treatment = droplevels(Treatment), trt = ifelse(Treatment == "Repeated", 1, 0)) 

###### LAGL LMs
LAGL_model_df <- count_model %>% filter(Species == "LAGL") 

lm_LAGL <- lm(Count ~ trt, data = LAGL_model_df)
summary(lm_LAGL)

lm_LAGL_2 <- lmer(Count ~ trt + (1 | BARREL), data = LAGL_model_df)
summary(lm_LAGL_2)

#######
# Random effects:
#   Groups   Name        Variance Std.Dev.
# BARREL   (Intercept)  299.3   17.30   
# Residual             4795.6   69.25   
# Number of obs: 272, groups:  BARREL, 96
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)   26.629      6.428   4.142
# trt           21.906      9.121   2.402


###### ELEL LMs
ELEL_model_df <- count_model %>% filter(Species == "ELEL") 

lm_ELEL <- lmer(Count ~ trt + (1 | BARREL), data = ELEL_model_df)
summary(lm_ELEL)

# Random effects:
#   Groups   Name        Variance Std.Dev.
# BARREL   (Intercept)  2.037   1.427   
# Residual             79.359   8.908   
# Number of obs: 192, groups:  BARREL, 64
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)   7.8021     0.9436   8.269
# trt           1.7292     1.3344   1.296

#####
##### ARTR 
#####
ARTR_model_df <- count_model %>% filter(Species == "ARTR") 

lm_ARTR <- lmer(Count ~ trt + (1 | BARREL), data = ARTR_model_df)
summary(lm_ARTR)

# Random effects:
#   Groups   Name        Variance Std.Dev.
# BARREL   (Intercept)  0.00    0.000   
# Residual             24.66    4.966   
# Number of obs: 176, groups:  BARREL, 64
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept) -5.356e-16  5.264e-01   0.000
# trt          3.667e+00  7.487e-01   4.897



################################################################################
################################################################################

### Native abundance on BRTE abundance ###
################################################################################

brte_comp_df <- count_plot_df %>%
  mutate(Count = case_when(
    Species == "ARTR" & Year == 2023 & is.na(Count) ~ 0,
    Species == "LAGL" & Year == 2023 & is.na(Count) ~ 0,
    Species == "ELEL" & Year == 2023 & is.na(Count) ~ 0,
    Species == "LAGL" & Year == 2024 & is.na(Count) ~ 0,
    Species == "ARTR" & Year == 2024 & is.na(Count) ~ 0,
    Species == "ARTR" & Year == 2025 & is.na(Count) ~ 0,
    TRUE ~ Count))


brte_comp_df_test <- brte_comp_df %>% select(-Notes, -Date) %>% 
  filter(., Has_B == T, complete.cases(.))

brte_comp_df <- brte_comp_df_test %>%
  select(BARREL, Year, Species, Count, Treatment, Trt, Has_B) %>%
  pivot_wider(names_from  = Species,
    values_from = Count,
    values_fill = NA) %>%
  mutate(BARREL = factor(BARREL))


brte_long <- brte_comp_df %>%
  pivot_longer(
    cols = c(LAGL, ELEL, ARTR),
    names_to  = "Competitor",
    values_to = "Comp_Count") %>%
  filter(!is.na(Comp_Count)) %>%      # keep the actual competitor
  mutate(Competitor = factor(Competitor, levels = c("LAGL", "ELEL", "ARTR")), 
         BARREL     = factor(BARREL),
         Year       = factor(Year))

library(glmmTMB)

m_brte_comp <- glmmTMB(
  BRTE ~ Comp_Count * Competitor + (1 | BARREL),
  family = nbinom2,
  data   = brte_long
)

summary(m_brte_comp)

library(ggeffects)

pred <- ggpredict(
  m_brte_comp,
  terms = c("Comp_Count [0:30]", "Competitor"))

plot(pred) +
  theme_light() +
  labs(x = "Competitor abundance",
    y = "Predicted BRTE count")


brte_long_1 <- brte_long %>% mutate(Competitor = case_when(Competitor == "LAGL" ~ "FAST", 
                                                           Competitor == "ELEL" ~ "MEDIUM",
                                                           Competitor == "ARTR" ~ "SLOW"), 
                                    Competitor = as.factor(Competitor))

ggplot(brte_long_1,
       aes(x = Comp_Count, y = BRTE,
           color = Treatment)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Competitor, scales = "free_x") +
  theme_light() +
  labs(
    x = "Competitor Abundance",
    y = "Observed BRTE Abundance") + scale_color_brewer(palette = "Set2") + 
  theme(legend.position = "bottom")









ggplot(brte_long,
  aes(x = Native_count,
    y = BRTE,
    color = Treatment,
    shape = Year)) +
  geom_point(alpha = 0.7, size = 2, position = position_jitter(width = 0.3, height = 0.1)) +
  facet_wrap(~ Native_species, scales = "free_x") + 
  labs(x = "Native species count", y = "Observed BRTE count", 
       color = "Treatment", shape = "Year") +
  geom_smooth(
    method = "loess",
    se = FALSE,
    linewidth = 0.8)+
  theme_bw()









# ggplot(
#   count_plot_df %>% 
#     filter(!Species %in% c("BRTE")),
#   aes(x = Treatment, y = Count, fill = Treatment)
# ) +
#   geom_boxplot(outlier.alpha = 0.4) +
#   
#   ## trend line (mean) between treatments
#   stat_summary(
#     aes(group = interaction(Year, Species)),
#     fun = mean,
#     geom = "line",
#     linewidth = 0.8,
#     color = "black"
#   ) +
#   stat_summary(
#     aes(group = interaction(Year, Species)),
#     fun = mean,
#     geom = "point",
#     size = 2,
#     color = "black"
#   ) +
#   
#   facet_grid(Species ~ Year, scales = "free_y") +
#   scale_fill_brewer(palette = "Set2") +
#   theme_bw()







### go species by species
# ARTR
# ggplot(count_plot_df %>%
#          filter(Seeded == TRUE, Species == "ARTR"),
#        aes(x = factor(Year), y = Count, fill = factor(Treatment))) +
#   geom_boxplot(alpha = 0.9, outliers = F) +
#   #geom_jitter(width = 0.15, alpha = 0.5, size = 1) +
#   ggtitle("ARTR Counts by Year and Treatment") +
#   xlab("Year") + ylab("ARTR Count")+
#   labs(fill = "Treatment") +
#   theme_light()+
#   scale_fill_brewer(palette = "Set2") +
#   theme(legend.position = "bottom")
# 

# 
# 
# 
# # ELEL
# ggplot(count_plot_df %>% 
#          filter(Seeded == TRUE, Species == "ELEL"), 
#        aes(x = factor(Year), y = Count, fill = factor(Treatment))) +
#   geom_boxplot(alpha = 0.9) +
#   #theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#   ggtitle("Squirreltail (ELEL) Counts by Year and Treatment") +
#   xlab("Year") + ylab("New Plants Count")+
#   labs(fill = "Treatment") + 
#   theme_light()+
#   scale_fill_brewer(palette = "Set2") + 
#   theme(legend.position = "bottom")
# 
# 
# # LAGL
# ggplot(count_plot_df %>% 
#          filter(Seeded == TRUE, Species == "LAGL"), 
#        aes(x = factor(Year), y = Count, fill = factor(Treatment))) +
#   geom_boxplot(alpha = 0.9) +
#   #theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#   ggtitle("Tidytips (LAGL) Counts by Year and Treatment") +
#   xlab("Year") + ylab("New Plants Count")+
#   labs(fill = "Treatment") + 
#   theme_light()+
#   scale_fill_brewer(palette = "Set2") + 
#   theme(legend.position = "bottom")
# 
# 
# # BRTE
# ggplot(count_plot_df %>% 
#          filter(Seeded == TRUE, Species == "BRTE"), 
#        aes(x = factor(Year), y = Count, fill = factor(Treatment))) +
#   geom_boxplot(alpha = 0.9) +
#   #theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#   ggtitle("Cheatgrass (BRTE) Counts by Year and Treatment") +
#   xlab("Year") + ylab("New Plants Count")+
#   labs(fill = "Treatment") + 
#   theme_light()+
#   scale_fill_brewer(palette = "Set2") + 
#   theme(legend.position = "bottom")








########
# break down the BRTE abundance by competing species
# start with 3 panels of BRTE with each native species
count_BRTE_df <- count_plot_df %>% filter(Has_B == T) 
codePRE <- unique(count_BRTE_df$CodePre)
print(codePRE)
count_BRTE_df <- count_BRTE_df %>% 
  select( BARREL, Year, Species, Count, Treatment, CodePre) %>% 
  filter(!is.na(Count)) %>% mutate(Treatment = as.factor(Treatment))
count_BRTE_df

count_BRTE_df <- count_BRTE_df %>% 
  pivot_wider(names_from = Species, values_from = Count) %>%
  mutate(Competitor = as.factor(case_when(
      CodePre == "BE" ~ "ELEL",
      CodePre == "BL" ~ "LAGL",
      CodePre == "BA" ~ "ARTR",
      TRUE ~ NA_character_)))

count_BRTE_df <- count_BRTE_df %>%
  mutate(Competitor = factor(Competitor, levels = c("LAGL", "ELEL", "ARTR")))


### BRTE counts by competitor 
ggplot(count_BRTE_df, aes(x = Competitor, y = BRTE)) +
  geom_boxplot(alpha = 0.9, outliers = F) +
  scale_fill_brewer(palette = "Set2") +
  labs( title = "",
    x = "Species",
    y = "BRTE Count") +
  theme(legend.position = "bottom",
    strip.text = element_text(face = "bold")) +   theme_light() 


# BRTE by competitor and treatment
ggplot(count_BRTE_df %>% filter(Year == 2025), aes(x = Treatment, y = BRTE, fill = Treatment)) +
  geom_boxplot(alpha = 0.85, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1) +
  facet_wrap(~ Competitor) + scale_fill_brewer(palette = "Set2") +
  theme_light() +
  labs(title = "Effect of Competing Species on BRTE Counts",
    x = "Treatment",
    y = "BRTE Count",
    fill = "Treatment") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
  
# BRTE by competitor and year
ggplot(count_BRTE_df, aes(x = Year, y = BRTE, fill = Treatment)) +
  geom_boxplot(alpha = 0.85, outlier.shape = NA) +
  geom_point(aes(group = Treatment),
             position = position_jitterdodge(
               jitter.width = 0.15,
               dodge.width = 0.75),
             alpha = 0.5, size = 1) +
  facet_wrap(~ Competitor) + scale_fill_brewer(palette = "Set2") +
  theme_light() +
  labs(title = " ", # no title
       x = "Treatment",
       y = "BRTE Count",
       fill = "Treatment") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))




# 2024 & 2025 
ggplot(count_BRTE_df %>% filter(Year == c("2024", "2025")), aes(x = Year, y = BRTE, fill = Treatment)) +
  geom_boxplot(alpha = 0.85, outliers = F) +
  geom_point(aes(group = Treatment),
             position = position_jitterdodge(
               jitter.width = 0.15,
               dodge.width = 0.75),
             alpha = 0.5, size = 1) +
  facet_wrap(~ Competitor) + scale_fill_brewer(palette = "Set2") +
  theme_light() +
  labs(title = "", # BRTE abundance by Treatment and competing native species
       x = "Year",
       y = "BRTE Count",
       fill = "Treatment") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

### Only 2025
ggplot(count_BRTE_df %>% filter(Year == 2025), aes(x = Year, y = BRTE, fill = Treatment)) +
  geom_boxplot(alpha = 0.85, outliers = F) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1) +
  facet_wrap(~ Competitor) + scale_fill_brewer(palette = "Set2") +
  theme_light() +
  labs(title = "", # BRTE abundance by Treatment and competing native species
       x = "Year",
       y = "BRTE Count",
       fill = "Treatment") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
  

### then pick the best competing species (hopefully LAGL and break it down by year) ###



















##################
### Model time ###
##################

############ first model will be JUST COUNTS of plants in each barrel
count_model
count_model_df <- count_model %>%
  mutate(species_id = as.integer(Species),
    barrel_id  = as.integer(factor(BARREL)),
    trt = ifelse(Treatment == "Repeated", 1, 0))

lm_count <- lm(Count ~ trt, data = brte_df)
summary(lm_count)















# simple linear model of seeds in to seeds out each year

# start by setting up the seed data
##### 2022 
SDdata_22 <- barrelkey_expanded
glimpse(test_22)

SDdata_22 <- SDdata_22 %>% 
  mutate(Total_seeds = case_when(
    SPECIES == "ELEL" ~ 105,
    SPECIES == "LAGL" ~ 130,
    SPECIES == "ARTR" ~ 230,
    SPECIES == "BRTE" ~ 130,
    TRUE ~ NA_real_
  ))

glimpse(SDdata_22)

###### 2023 
SDdata_23_calc <- Pdata23_clean %>% 
  select(BARREL, SPECIES, FLWR_FINAL) %>% 
  mutate(
    Seed_tot = case_when(
      SPECIES == "LAGL" ~ FLWR_FINAL * 39,
      SPECIES == "ELEL" ~ FLWR_FINAL * 93,
      SPECIES == "BRTE" ~ FLWR_FINAL * 88,
      TRUE ~ 0
    )
  ) %>%
  group_by(BARREL, SPECIES) %>% 
  summarise(Total_seeds = sum(Seed_tot, na.rm = TRUE),
            .groups = "drop")

SDdata_23_test <- species_lookup_23 %>%
  left_join(SDdata_23_calc, by = c("BARREL", "SPECIES")) %>%
  mutate(Total_seeds = replace_na(Total_seeds, 0))



SDdata_23 <- Pdata23_clean %>% 
  select(BARREL, SPECIES, FLWR_FINAL, ) %>% 
  group_by(BARREL, SPECIES) %>% 
  mutate(
    Seed_tot = case_when(
      SPECIES == "LAGL" ~ FLWR_FINAL * 39,
      SPECIES == "ELEL" ~ FLWR_FINAL * 93,
      SPECIES == "BRTE" ~ FLWR_FINAL * 88,
      TRUE ~ NA_real_)) %>% 
  group_by(BARREL, SPECIES) %>% 
  summarise(
    Total_seeds = sum(Seed_tot, na.rm = TRUE),
    .groups = "drop")

SDdata_23_calc <- Pdata23_clean %>% 
  select(BARREL, SPECIES, FLWR_FINAL) %>% 
  mutate(
    Seed_tot = case_when(
      SPECIES == "LAGL" ~ FLWR_FINAL * 39,
      SPECIES == "ELEL" ~ FLWR_FINAL * 93,
      SPECIES == "BRTE" ~ FLWR_FINAL * 88,
      TRUE ~ 0
    )
  ) %>%
  group_by(BARREL, SPECIES) %>% 
  summarise(Total_seeds = sum(Seed_tot, na.rm = TRUE),
            .groups = "drop")

SDdata_23 <- full_join(barrelkey_expanded, SDdata_23_calc)
glimpse(SDdata_23)
SDdata_23 <- SDdata_23_test %>% 
  mutate(Total_seeds = case_match(
    Total_seeds,
    NA ~ 0,
    .default = Total_seeds))

glimpse(SDdata_23)




#### 2024
SDdata_24 <- read_xlsx("2024 Data_Clean.xlsx", sheet = "seed count total")
SDdata_24 <- SDdata_24 %>% select(`Barrel ID`, `BRTE Seeds`, `LAGL Seeds`, `ELEL Seeds`, `ARTR Seeds`) %>% 
  rename(BARREL = matches("Barrel"),
         BRTE = matches("BRTE"), LAGL = matches("LAGL"), ELEL = matches("ELEL"), ARTR = matches("ARTR")) %>% 
  mutate(BARREL = as.integer(BARREL))
SDdata_24 <- SDdata_24[-160,] # remove "total" row sum

SDdata_24 <- SDdata_24 %>% 
  pivot_longer(cols = c(BRTE, LAGL, ELEL, ARTR),values_to = "Total_seeds",
               names_to =  "SPECIES") 

SDdata_24_test <- SDdata_24 %>% 
  left_join(barrelkey_expanded)

SDdata_24_test <- SDdata_24_test %>%
  filter(!is.na(LH_combo))

SDdata_24 <- SDdata_24_test


#### 2025 
SDdata_25 <- read_xlsx("2025 Data.xlsx", sheet = "Total Seeds Barrel")
SDdata_25 <- SDdata_25 %>% select(`Barrel ID`, BRTE, LAGL, ELEL, ARTR) %>% 
  rename(BARREL = matches("BARREL"))


SDdata_25 <- SDdata_25 %>% 
  pivot_longer(cols = c(BRTE, LAGL, ELEL, ARTR),values_to = "Total_seeds",
               names_to =  "SPECIES")


SDdata_25_test <- SDdata_25 %>% 
  left_join(barrelkey_expanded)

SDdata_25_test <- SDdata_25_test %>%
  filter(!is.na(LH_combo))


SDdata_25 <- SDdata_25_test
# join to barrel-key to match 2022, 2023, 2024

SDdata_22
SDdata_23
SDdata_24
SDdata_25


### combine all SDdata years

SD_all <- bind_rows(
  SDdata_22 %>% mutate(Year = 2022),
  SDdata_23 %>% mutate(Year = 2023),
  SDdata_24 %>% mutate(Year = 2024),
  SDdata_25 %>% mutate(Year = 2025)
)

SD_all <- SD_all %>%
  arrange(BARREL, SPECIES, Year)

SD_all_mod <- SD_all %>%
  group_by(BARREL, SPECIES) %>%
  mutate(Total_seeds_prev = lag(Total_seeds),
    seed_ratio = Total_seeds / Total_seeds_prev) %>% ungroup()

SD_all_mod <- SD_all_mod %>%
  group_by(BARREL, SPECIES) %>%
  mutate(
    seed_ratio = if_else(
      !is.na(lag(Total_seeds)) & lag(Total_seeds) > 0,
      Total_seeds / lag(Total_seeds),
      NA_real_)) %>% ungroup()

# 
# SD_all_mod_test %>%
#   filter(Year > 2022) %>%
#   count(is.na(seed_ratio), is.infinite(seed_ratio))

# # set up response variable (BRTE)
# Brte_lag <- SD_all_mod %>% select(BARREL, SPECIES, Year, seed_ratio) %>% 
#   filter(SPECIES == "BRTE", !is.na(seed_ratio))
# 
# # Set up predictor variables (other species)
# LAGL_lag <- SD_all_mod %>% select(BARREL, SPECIES, Year, Total_seeds, seed_ratio) %>% 
#   filter(SPECIES == "LAGL", !is.na(seed_ratio))

test_mod <- lm(data = SD_all_mod, formula = Total_seeds ~ Trt + SPECIES)



# model time

# to do
# write stan code
library(StanHeaders)
library(rstan)
library(brms)

# do brms first
library(dplyr)

brte_dat <- SD_all_mod %>%
  filter(SPECIES == "BRTE",
    Year > 2022,
    !is.na(seed_ratio)) %>%
  mutate(log_seed_ratio = log(seed_ratio)) %>% # create response category
  left_join(SD_all_mod %>%
      filter(Year > 2022) %>%
      select(BARREL, Year, SPECIES, Total_seeds) %>%
      tidyr::pivot_wider(
        names_from = SPECIES,
        values_from = Total_seeds,
        values_fill = 0), by = c("BARREL", "Year"))


brms_fit <- brm(
  formula = log_seed_ratio ~ 
    LAGL + ELEL + ARTR + (1 | Year),
  data = brte_dat,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept"),
    prior(exponential(1), class = "sd"),
    prior(exponential(1), class = "sigma")),
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.95))

summary(brms_fit)
posterior_summary(brms_fit, variable = c(
  "b_Intercept",
  "b_LAGL", 
  "b_ELEL",
  "b_ARTR"
))





modeldata <- list(years = 3, # 2023, 2024, 2025
                  B = length(unique(SD_all_mod$BARREL)),
                  N = length(SD_all_mod_test$BARREL), 
                  BRTE = Brte_lag,
                  LAGL = ,
                  ELEL = ,
                  ARTR = SD_all_mod$)

# Run the model
BRTE_fitness_mod <- stan(file = "fitness_lm.stan",
            data = modeldata, 
            chains = 3,iter = 2000, warmup = 1000)


#################################################################
### Make a population growth figure for thr SRM presentation #### 
#################################################################

sp_A_vect <- rep(NA_real_, 10)
sp_B_vect <- rep(NA_real_, 10)

t <- 1:10

# Species A (fast LH)
r_A <- 0.2
sp_A_vect[1] <- 20

for(i in 1:9){
  sp_A_vect[i+1] <- sp_A_vect[i] * exp(r_A)
}

# Species B (slow LH with delay)
sp_B_vect[1] <- 20

r_B_decline <- -0.42  # decline phase
r_B_growth  <-  0.4  # recovery phase

for(i in 1:9){
  if(i < 5){
    sp_B_vect[i+1] <- sp_B_vect[i] * exp(r_B_decline)
  } else {
    sp_B_vect[i+1] <- sp_B_vect[i] * exp(r_B_growth)
  }
}

#################################################################
# Plot
plot(t, sp_A_vect,
     type = "l",
     lwd = 4,
     col = "#E7A864",
     ylim = c(0, max(sp_A_vect, sp_B_vect)),
     xlab = "Time",
     ylab = "Population size")

lines(t, sp_B_vect,
      lwd = 4,
      col = "#AAC7B9")

legend("topleft",
       legend = c("Fast LH", "Slow LH"),
       col = c("#E7A864", "#AAC7B9"),
       lwd = 4,
       bty = "n")
#######################  
# Species B with ADDITIONAL seedings
sp_B_SUPP <- rep(NA_integer_, 10)

sp_B_vect[1] <- 20

r_B_decline <- -0.42  # decline phase
r_B_growth  <-  0.4  # recovery phase

for(i in 1:9){
  if(i < 5){
    sp_B_SUPP[i+1] <- sp_B_SUPP[i] * exp(r_B_decline)
  } else {
    sp_B_SUPP[i+1] <- sp_B_SUPP[i] * exp(r_B_growth)
  }
}


sp_B_SUPP[1:4] <- c(20,20,20, 20) #initial population size (same for 1st 3 time steps)
r_B <- 0.15
t <- 1:10
for(i in 4:9){ # Species A

  sp_B_SUPP[i+1] <- sp_B_SUPP[i]*exp(r_B)

}

#plot(sp_A_vect, type = "l") # BRTE
plot(sp_B_SUPP, type = "l") # ARTR

plot(t, sp_A_vect,
     type = "l",
     lwd = 4,
     col = "#E7A864",
     ylim = c(0, max(sp_A_vect, sp_B_vect)),
     xlab = "Time",
     ylab = "Population size")

lines(t, sp_B_vect,
      lwd = 4,
      col = "#AAC7B9")

lines(t, sp_B_SUPP,
      lwd = 4,
      col = "#6F7C12")

legend("topleft",
       legend = c("Fast LH", "Slow LH", "Repeated Slow LH"),
       col = c("#E7A864", "#AAC7B9", "#6F7C12"),
       lwd = 4,
       bty = "n")


############
# Species A (fast LH)
r_A <- 0.15
sp_A_vect[1] <- 20

for(i in 1:9){
  sp_A_vect[i+1] <- sp_A_vect[i] * exp(r_A)
}

sp_C_vect <- rep(NA_integer_, 10)
r_C <- 0.14
sp_C_vect[1] <- 20

for(i in 1:9){
  sp_C_vect[i+1] <- sp_C_vect[i] * exp(r_C)
}


plot(t, sp_B_vect,
     type = "l",
     lwd = 4,
     col = "#AAC7B9",
     ylim = c(0, max(sp_B_SUPP, sp_B_vect)),
     xlab = "Time",
     ylab = "Population size")

lines(t, sp_A_vect,
      lwd = 4,
      col = "#E7A864")

# legend("topleft",
#        legend = c("Fast LH", "Slow LH", "Slow LH Repeated", "Native Fast LH"),
#        col = c("#E7A864", "#AAC7B9", "#6F7C12", "#A63A50"),
#        lwd = 4,
#        bty = "n")

lines(t, sp_B_SUPP,
      lwd = 4,
      col = "#6F7C12")


lines(t, sp_C_vect,
      lwd = 4,
      col = "#A63A50")

legend("topleft",
       legend = c("Fast LH", "Slow LH", "Slow LH Repeated", "Native Fast LH"),
       col = c("#E7A864", "#AAC7B9", "#6F7C12", "#A63A50"),
       lwd = 4,
       bty = "n")


# #######
# ### Conceptual boxplots for repeat seeding ####
# 
# concept_df <- count_plot_df %>% 
#   select(Species, Treatment, Count) %>%
#   mutate(Count = case_when(
#       Species == "LAGL" & Treatment == "Control"  ~ 10,
#       Species == "LAGL" & Treatment == "Repeated" ~ 20,
#       Species == "ELEL" & Treatment == "Control"  ~ 10,
#       Species == "ELEL" & Treatment == "Repeated" ~ 30,
#       Species == "ARTR" & Treatment == "Control"  ~ 10,
#       Species == "ARTR" & Treatment == "Repeated" ~ 40,
#       TRUE ~ Count))
# 
# concept_df <- concept_df %>% mutate(Species = factor(Species, levels = c("LAGL", "BRTE"), "ELEL", "ARTR")))
# 
# 
# ggplot(concept_df %>% filter(!Species == "BRTE"), 
#        aes(x = Treatment, y = Count, fill = Treatment)) +
#   geom_boxplot(outliers = F)+
#   facet_wrap(~Species) +
#   scale_fill_brewer(palette = "Set2") + theme_light() +
#   labs(title = "BRTE abundance by Treatment and competing native species", # no title
#        x = "Treatment",
#        y = "BRTE Count",
#        fill = "Treatment") 
    
  

ggplot(concept_df %>% filter(!Species == "BRTE"), 
       aes(x = Treatment, y = Count, fill = Treatment)) +
  geom_blank() +
  facet_wrap(~Species, ncol = 3) +
  theme_light() +
  labs(title = NULL,
    x = "Treatment",
    y = "BRTE Count",
    fill = "Treatment")


######################################
### Make all changes proportional ###
###       Log response Ratio      ###
######################################

percent_DF <- count_plot_df
percent_DF <- count_plot_df %>% filter(Seeded == T) %>% 
  mutate(Count = Count + 1, 
          Count = replace_na(Count, 1)) ## change all NA's to 1 

## log response ratio
#log (mean( repeated )/ mean(single))

LRR_mean_func <- function(repeated, single){
  log(mean(repeated)/ mean(single))
}

LRR_count_df <- percent_DF %>%
  group_by(Species, Year, Treatment) %>%
  summarise(mean_count = mean(Count), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = Treatment,
    values_from = mean_count) %>%
  mutate(LRR = log(Repeated / Single))


colors_vect <- c("LAGL" = "#F9D42E",
                 "ELEL" = "#132A13",
                 "ARTR" = "#AAC7B9")

colors_vect2 <- c("FAST" = "#F9D42E",
                 "MEDIUM" = "#3D6E41",
                 "SLOW" = "#AAC7B9")


LRR_count_df <- LRR_count_df %>% mutate(Species = case_when(Species == "LAGL" ~ "FAST", 
                                            Species == "ELEL" ~ "MEDIUM", 
                                            Species == "ARTR" ~ "SLOW",
                                            Species == "BRTE" ~ "BRTE",
                                            TRUE ~ Species), 
                        Species = factor(Species, levels = c("FAST", "MEDIUM", "SLOW", "BRTE")))


ggplot(LRR_count_df %>% filter(!Species == "BRTE"), 
       aes(x = Year, y = LRR, fill = Species)) +
  geom_col(position = "dodge") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_light() +
  labs(title = "",
       y = "Log Response Ratio",
       x = "Year") +
  scale_fill_manual(values = colors_vect2) + 
  theme(
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(1.5, "cm")
  )


ggplot(LRR_count_df %>% filter(Species != "BRTE"), 
       aes(x = Year, y = LRR, fill = Species)) +
  geom_col(alpha = 0, position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = colors_vect2) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  theme_light() +
  labs(title = "",
       y = "Log Response Ratio",
       x = "Year") + 
  theme(
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(1.5, "cm")
  )


ggplot(LRR_count_df %>% filter(Species != "BRTE"), 
       aes(x = Year, y = LRR, fill = Species)) +
  geom_col(alpha = 0, position = "dodge") +  # invisible bars → legend appears
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_light() +
  labs(title = "",
       y = "Log Response Ratio",
       x = "Year") +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))



# # Try Hedges' d
# hedges_d <- function(x1, x2) {
#   x1 <- x1[!is.na(x1)]
#   x2 <- x2[!is.na(x2)]
#   
#   n1 <- length(x1)
#   n2 <- length(x2)
#   
#   s1 <- var(x1)
#   s2 <- var(x2)
#   
#   sp <- sqrt(((n1 - 1)*s1 + (n2 - 1)*s2) / (n1 + n2 - 2))
#   
#   d  <- (mean(x1) - mean(x2)) / sp
#   
#   # small sample correction
#   J <- 1 - (3 / (4*(n1 + n2) - 9))
#   
#   J * d
# }
# 
# hedges_df <- percent_DF %>%
#   filter(Species != "BRTE") %>%
#   group_by(Species, Year) %>%
#   summarise(d = hedges_d(
#       Count[Treatment == "Repeated"],
#       Count[Treatment == "Single"]), .groups = "drop")
# 
# 
# ggplot(hedges_df,
#        aes(x = Year, y = d, fill = Species)) +
#   geom_col(position = "dodge") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   theme_light() +
#   labs(title = "Effect Size (Hedges' d)",
#        y = "Hedges' d (Repeated - Single)",
#        x = "Year")






#### now do BRTE response 

LRR_BRTE_DF <- brte_long %>% 
  mutate(Comp_Count = Comp_Count + 1, 
         Comp_Count = replace_na(Comp_Count, 1))  ## change all NA's to 1 


LRR_BRTE_DF <- LRR_BRTE_DF %>%
  group_by(Competitor, Year, Treatment) %>%
  summarise(mean_count = mean(Comp_Count), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = Treatment,
    values_from = mean_count) %>%
  mutate(LRR = log(Repeated / Single))


LRR_BRTE_DF <- LRR_BRTE_DF %>% mutate(Competitor = case_when(Competitor == "LAGL" ~ "FAST", 
                                              Competitor == "ELEL" ~ "MEDIUM", 
                                              Competitor == "ARTR" ~ "SLOW",
                                              Competitor == "BRTE" ~ "BRTE",
                               TRUE ~ Competitor), 
                      Competitor = factor(Competitor, levels = c("FAST", "MEDIUM", "SLOW", "BRTE")))

ggplot(LRR_BRTE_DF, 
       aes(x = Year, y = LRR, fill = Competitor)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_light() +
  labs(title = "BRTE",
       y = "Log Response Ratio",
       x = "Year") +
  scale_fill_manual(values = colors_vect2) + 
  theme(legend.title = element_text(size = 16),
        legend.text  = element_text(size = 12),
        legend.key.size = unit(1.5, "cm") )










