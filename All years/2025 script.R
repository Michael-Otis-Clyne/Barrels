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
Pdata23 <- read_xlsx("Barrel_Data_2023_FINAL.xlsx")
Cdata23 <- Pdata23 %>% dplyr::select(BARREL, SPECIES) %>% 
  count(BARREL, SPECIES, name = "count")
Cdata23 <- Cdata23 %>% # Remove all Unknown plants
  subset(., SPECIES != "U") %>% 
  subset(., SPECIES !="UG") %>% 
  subset(., SPECIES != "UD")

Cdata24 <- read_xlsx("2024 Data_Clean.xlsx", sheet = 1)
Pdata24 <- read_xlsx("2024 Data_Clean.xlsx", sheet = 2)

Cdata25 <- read_xlsx("2025 Data.xlsx", sheet = 1) # counts of species in each barrel
Pdata25 <- read_xlsx("2025 Data.xlsx", sheet = 2)
# clean it up
Pdata25 <- Pdata25[, -3] # extra row



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
    values_fill = 0) %>%      # non-seeded species counts = 0
  relocate(BARREL, BRTE, LAGL, ELEL, .before = Year) %>% 
  mutate( TrT = NA_character_)




# 2024 #
Cdata24 <- Cdata24 %>% 
  rename(., BARREL = `Barrel ID`, BRTE = `BRTE count`, LAGL = `LAGL count`, ELEL = `ELEL count`, ARTR = `ARTR count`) %>% 
  mutate(across(c(BRTE, LAGL, ELEL, ARTR), ~replace_na(., 0)))%>% 
  mutate(BARREL = as.numeric(BARREL), Year = 2024) 
Cdata24 <- Cdata24[-161,]

# 2025 #
Cdata25 <- Cdata25 %>% 
  rename(., BARREL = Barrel, BRTE = `BRTE count`, LAGL = `LAGL count`, ELEL = `ELEL count`, ARTR = `ARTR count`) %>% 
  mutate(across(c(BRTE, LAGL, ELEL, ARTR), ~replace_na(., 0))) %>% 
  mutate(BARREL = as.numeric(BARREL), Year = 2025)
Cdata25 <- Cdata25[-161,]

view(Cdata25)


# apply to each dataset
Cdata_all <- bind_rows(Cdata23, Cdata24, Cdata25) %>%
  mutate(BARREL = as.numeric(BARREL), Year = as.integer(Year)) %>%
  arrange(BARREL, Year)

head(Cdata_all, 20)

# more tidy
Cdata_all <- Cdata_all %>% 
  relocate(., ARTR, .before = "Year") %>% # relocate ARTR
  mutate(., across(c(BRTE, LAGL, ELEL, ARTR), ~replace_na(., 0))) %>%  # replace al 2023 no ARTR with 0 %>% 
  select(., -TrT) 

trt_lookup <- Cdata_all %>%
  filter(!is.na(Trt)) %>% 
  select(BARREL, Trt) %>%
  distinct()          

Cdata_all <- Cdata_all %>%
  left_join(trt_lookup, by = "BARREL", suffix = c("", "_correct")) %>%
  mutate(Trt = if_else(is.na(Trt), Trt_correct, Trt)) %>%
  select(-Trt_correct)

glimpse(Cdata_all)


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

Pdata25 <- Pdata25 %>% 
  mutate(PLANT_ID = NA)

Pdata25_clean <- clean_pdata(Pdata25, 2025)

# bind all together
Pdata_all <- bind_rows(Pdata23_test, Pdata24_clean, Pdata25_clean)

Pdata_all <- Pdata_all %>% dplyr::select(., BARREL, SPECIES, PLANT_ID, TOOTHPICK, QUAD, DESIGNATION, HT_FINAL, FLWR_FINAL, UID, Year)
Pdata_all <- Pdata_all %>% # Remove all Unknown plants
  subset(., SPECIES != "U") %>% 
  subset(., SPECIES !="UG") %>% 
  subset(., SPECIES != "UD")


glimpse(Pdata_all)





#########################################
# To do # 
#########
# 1) Figures of abundance changes year to year
# 2) effect of treatments on BRTE
#   2a) repeat seeding effect
#   2b) species treatment on BRTE
#   2c) repeat seeding on Slow LH species abundance over time

# # Assign each barrel the treatments and LH combos
# barrelkey <- read.csv("barrel_key.csv")
# 
# barrelkey$Species <- gsub("_[^_]+$","", barrelkey$Trt)
# #remove trailing part about if control or not; irrelevant bc this is 1 year only
# #barrelkey$LH_combo <- gsub("_[^_]+$","", barrelkey$LH_combo)
# # remove the LH combo in the trt column
# barrelkey$Trt <- gsub(".*_", "", barrelkey$Trt)
# head(barrelkey)
# #looks like it worked well; now to add life history key (manually)
# 
# unique(barrelkey$Trt)
# barrelkey <- as_tibble(barrelkey %>% mutate(., Trt=as.factor(Trt)))
# head(barrelkey)
# #B = cheatgrass = F
# #A = sagebrush = S
# #L = whitedaisy = F
# #E = bottlebrush = M
# 
# 
# barrelkey <- barrelkey %>% 
#   mutate(
#     LH_combo = ifelse(Species == "LE" | Species == "BE", "FM", 
#                       ifelse(Species == "BL", "FF",
#                              ifelse(Species == "AL" | Species == "BA", "FS", NA)))
#   )
# 
# head(barrelkey)
# #now add treatment and LH combo info to the main dataframe
# #make new columns:
# Pdata_all$Trt = NA
# Pdata_all$LH_combo = NA
# Pdata_all$Spec.TRT=NA
# #fill in based on info in barrel key:
# for (i in 1:nrow(Pdata_all)) {
#   Pdata_all$Trt[i] = barrelkey$Trt[barrelkey$BARREL == Pdata_all$BARREL[i]]
#   Pdata_all$LH_combo[i] = barrelkey$LH_combo[barrelkey$BARREL == Pdata_all$BARREL[i]]
#   Pdata_all$Spec.TRT[i] = barrelkey$Species[barrelkey$BARREL==Pdata_all$BARREL[i]]
#   
# }
# 
# # Cdata_all$Trt = NA
# # Cdata_all$LH_combo = NA
# # Cdata_all$Spec.TRT=NA
# # #fill in based on info in barrel key:
# # for (i in 1:nrow(Cdata_all)) {
# #   Cdata_all$Trt[i] = barrelkey$Trt[barrelkey$BARREL == Cdata_all$BARREL[i]]
# #   Cdata_all$LH_combo[i] = barrelkey$LH_combo[barrelkey$BARREL == Cdata_all$BARREL[i]]
# #   Cdata_all$Spec.TRT[i] = barrelkey$Species[barrelkey$BARREL==Cdata_all$BARREL[i]]
# #   
# # }

# count_new <- Cdata_all %>%
#   group_by(BARREL, Trt, , Year) %>%
#   summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
#   spread(key = SPECIES, value = total, fill = 0)

count_new <- Cdata_all %>%
  # Pivot species columns into long format
  pivot_longer(
    cols = c(BRTE, LAGL, ELEL, ARTR),
    names_to = "Species",
    values_to = "Count")

print(range(count_new %>% filter(Species == "ARTR") %>% select(Count)))

###################################
#############

### show counts of species by year
ggplot(data = count_new, aes(x = factor(Year), y = Count, fill = Species)) +
  geom_boxplot(outliers = F) +
  geom_jitter(alpha = 0.3, width = 0.15)+
  theme_bw() +
  labs(x = "Year",
    y = "Count") + 
  facet_wrap(~Species, scales = "free_y") 

ggplot(data = count_new, aes(x = factor(Year), y = Count, fill = Species)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Year",
       y = "Count") + 
  facet_wrap(~Species, scales = "free_y") 

trts <- unique(as.factor(count_new$Trt))

count_new_trt <- count_new %>%
  mutate(Treatment = recode(Trt,
                      LE_A = "Repeated" ,
                      BE_A = "Repeated" ,
                      BL_A= "Repeated" ,
                      AL_A = "Repeated" ,
                      BA_A = "Repeated" ,
                      BL_1 = "Control" , AL_1 = "Control" , BE_1 = "Control" , BA_1 = "Control" , LE_1 = "Control" ))

# 
# count_new <- count_new %>%
#   rename(`I-F` = BRTE,
#          `N-F` = LAGL,
#          `N-M` = ELEL,
#          `N-S` = ARTR)


count_long <- count_new_trt 


diffplot_25 <- ggplot(count_new_trt %>% filter(Year == 2025), 
                   aes(x = Treatment, y = Count, fill = Treatment)) +
  geom_boxplot(alpha = 0.9) +
  facet_wrap(~Species, scales = "free_y") +
  #theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Plant Counts by Species and Treatment", subtitle = "2025") +
  xlab("Treatment") + ylab("Plant Count")+
  labs(fill = "Treatment") + 
  theme_light()+
  scale_fill_brewer(palette = "Set2")

diffplot_25


diffplot_all <- ggplot(count_new_trt, 
                       aes(x = factor(Year), y = Count, fill = Treatment)) +
  geom_boxplot(outliers = F, alpha = 0.9) +
  facet_wrap(~Species, scales = "free_y") +
  #theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Plant Counts by Species and Treatment", subtitle = "All years") +
  xlab("Treatment") + ylab("Plant Count")+
  labs(fill = "Treatment") + 
  theme_light()+
  scale_fill_brewer(palette = "Set2")

diffplot_all


####### focus on one plot per species
### LAGL
ggplot(count_new_trt %>% filter(Species == "LAGL"), 
       aes(x = factor(Year), y = Count, fill = Treatment)) +
  geom_boxplot(outliers = F, alpha = 0.9) +
#  facet_wrap(~Species, scales = "free_y") +
  #theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("LAGL abundance by Seeding Treatment", subtitle = "All years") +
  xlab("Year") + ylab("Count")+
  labs(fill = "Treatment") + 
  theme_light()+
  scale_fill_brewer(palette = "Set2")



### ELEL
ggplot(count_new_trt %>% filter(Species == "ELEL"), 
       aes(x = factor(Year), y = Count, fill = Treatment)) +
  geom_boxplot(outliers = F, alpha = 0.9) +
  #  facet_wrap(~Species, scales = "free_y") +
  #theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("ELEL abundance by Seeding Treatment", subtitle = "All years") +
  xlab("Year") + ylab("Count")+
  labs(fill = "Treatment") + 
  theme_light()+
  scale_fill_brewer(palette = "Set2")


### ARTR
ggplot(count_new_trt %>% filter(Species == "ARTR"), 
       aes(x = factor(Year), y = Count, fill = Treatment)) +
  geom_boxplot( alpha = 0.9) +
  #  facet_wrap(~Species, scales = "free_y") +
  #theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("ARTR abundance by Seeding Treatment", subtitle = "All years") +
  xlab("Year") + ylab("Count")+
  labs(fill = "Treatment") + 
  theme_light()+
  scale_fill_brewer(palette = "Set2")


####### quick stats
library(tidyverse)
library(emmeans)
library(multcomp)
library(multcompView)

df <- count_new  # or count_new_trt (either works)

# ---- 1. Run ANOVA by species ----
library(tidyverse)
library(emmeans)
library(multcomp)
library(multcompView)

anova_results <- count_new_trt %>%
  group_by(Species) %>%
  group_modify(~{
    
    df_sub <- .x   # .x is the data for each species
    
    # Run ANOVA
    mod <- aov(Count ~ Treatment, data = df_sub)
    
    # Tukey
    tk  <- emmeans(mod, pairwise ~ Treatment)
    cld <- multcomp::cld(tk$emmeans)
    
    # Extract letters
    cld_tbl <- cld %>%
      as.data.frame() %>%
      select(Treatment, .group)
    
    # Summary stats for annotation placement
    sum_stats <- df_sub %>%
      group_by(Treatment) %>%
      summarise(
        mean_count = mean(Count, na.rm = TRUE),
        sd_count   = sd(Count, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Return one row per treatment per species
    left_join(sum_stats, cld_tbl, by = "Treatment") %>%
      mutate(Species = unique(df_sub$Species))
  }) %>%
  ungroup()


treatment_plot <- ggplot(count_new_trt, aes(x = Treatment, y = Count, fill = Treatment)) +
  geom_boxplot(alpha = 0.9) +
  facet_wrap(~Species, scales = "free_y") +
  theme_light() +
  ggtitle("Treatment Effects on Species Counts") +
  ylab("Count") +
  xlab("Treatment") +
  scale_fill_brewer(palette = "Set2") +
  
  geom_text(
    data = anova_results,
    aes(
      x = Treatment,
      y = mean_count + sd_count * 2.2,   # scalable offset above box
      label = .group
    ),
    size = 6,
    fontface = "bold",
    color = "black",
    stroke = 0.35,       # adds white outline
    linewidth = 0.35
  )

treatment_plot

################
# next up is LAGL and repeat seeding on BRTE
count_new_trt


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
    ),
    
    Seeded = !is.na(SeededSpecies))


count_plot_df %>% count(Species, Seeded)

ggplot(
  count_plot_df %>% 
    filter(Seeded == TRUE, Species %in% c("BRTE", "LAGL")),
  aes(x = Species, y = Count, fill = Treatment)
) +
  geom_boxplot() +
  facet_wrap(~Year, scales = "free_y") +
  scale_fill_brewer(palette = "Set2") +
  theme_light()





##### Plots counts of BRTE and LAGL by year and treatmnt
ggplot(data = count_plot_df %>% filter(Species == c("BRTE", "LAGL")), ##### Plots counts of BRTE and LAGL by year and treatmnt
       aes(x = Species, y = Count, fill = Treatment)) + 
  geom_boxplot() + facet_wrap(~Year, scales = "free_y") + 
  scale_fill_brewer(palette = "Set2")

count_wide <- count_plot_df %>%
  select(BARREL, Year, Treatment,Trt, Species, Count) %>%
  pivot_wider(
    names_from = Species,
    values_from = Count,
    values_fill = 0)



plot2 <- ggplot(count_plot_df %>%  filter(Seeded == TRUE, Species %in% c("BRTE", "LAGL"),
                aes(x = Treatment, y = BRTE, fill = Treatment))) +
  geom_boxplot(alpha = 0.9) +
  theme_bw() +
  labs(
    x = "",
    y = "BRTE Count",
    title = "Effect of Repeated LAGL Seeding on BRTE Counts"
  ) +
  + scale_fill_brewer(palette = "Set2")

plot2


int_plot <- ggplot(count_wide, 
                aes(x = LAGL, y = BRTE, color = Treatment)) +
  geom_point(alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  theme_bw() +
  labs(
    x = "LAGL Count",
    y = "BRTE Count",
    title = "LAGL × Repeated Seeding on BRTE Abundance"
  )+ scale_color_brewer(palette = "Set2")

int_plot




# 1) Figures of abundance changes year to year
count_long %>% filter(., Year == 2023) %>% 
ggplot(count_long, aes(x = SPECIES, y = Count, fill = Trt)) +
  geom_boxplot()
 #  geom_boxplot(alpha = 0.9, outliers = F) +
 # # facet_wrap(~SPECIES, scales = "free_y") +
 #  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
 #  ggtitle("Plant Counts by Species and Treatment") +
 #  xlab("Treatment") + ylab("Plant Count")+
 #  labs(fill = "Species") + 
 #  theme_light()+
 #  scale_fill_brewer(palette = "Set2")
# Map LH codes to actual species
lh_map <- list(
  LE = c("LAGL", "ELEL"),
  BE = c("BRTE", "ELEL"),
  BL = c("BRTE", "LAGL"),
  AL = c("ARTR", "LAGL")
)

# Expand barrelkey so each barrel has one row per species
barrelkey_expanded <- barrelkey %>%
  rowwise() %>%
  mutate(SPECIES = list(lh_map[[Species]])) %>%
  unnest(SPECIES)

# Now filter count_long to only the seeded species
count_long <- count_long %>%
  inner_join(barrelkey_expanded %>% select(BARREL, SPECIES),
             by = c("BARREL", "SPECIES"))


##################
### Model time ###
##################

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
