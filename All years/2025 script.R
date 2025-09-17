########################
### Barrel Data 2025 ###
########################

library(tidyverse)
library(readxl)
library(dplyr)
library(conflicted)
conflict_prefer("select", "dplyr")

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

Cdata25 <- read_xlsx("Copy of 2025 Data.xlsx", sheet = 1) # counts of species in each barrel
Pdata25 <- read_xlsx("Copy of 2025 Data.xlsx", sheet = 2)
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

# 2024 #
Cdata24 <- Cdata24 %>% 
  rename(., BARREL = `Barrel ID`, BRTE = `BRTE count`, LAGL = `LAGL count`, ELEL = `ELEL count`, ARTR = `ARTR count`) %>% 
  mutate(across(c(BRTE, LAGL, ELEL, ARTR), ~replace_na(., 0)))%>% 
  mutate(BARREL = as.numeric(BARREL))
Cdata24 <- Cdata24[-161,]

# 2025 #
Cdata25 <- Cdata25 %>% 
  rename(., BARREL = Barrel, BRTE = `BRTE count`, LAGL = `LAGL count`, ELEL = `ELEL count`, ARTR = `ARTR count`) %>% 
  mutate(across(c(BRTE, LAGL, ELEL, ARTR), ~replace_na(., 0))) %>% 
  mutate(BARREL = as.numeric(BARREL))
Cdata25 <- Cdata25[-161,]

view(Cdata25)

# helper function to reshape and summarise a single year
summarise_year <- function(df, year) {
  df %>%
    mutate(across(c(BRTE, LAGL, ELEL, ARTR), ~replace_na(., 0))) %>%
    pivot_longer(
      cols = c(BRTE, LAGL, ELEL, ARTR),
      names_to = "SPECIES",
      values_to = "count"
    ) %>%
    group_by(BARREL, SPECIES) %>%
    summarise(total = sum(count), .groups = "drop") %>%
    mutate(Year = year)
}

# apply to each dataset
Cdata_all <- bind_rows(
  Cdata23,
  summarise_year(Cdata24, 2024),
  summarise_year(Cdata25, 2025)
) %>%
  mutate(
    BARREL = as.numeric(BARREL),
    total  = as.numeric(total),
    Year   = as.integer(Year)
  ) %>%
  arrange(Year, BARREL, SPECIES)

head(Cdata_all, 20)


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

Pdata23_test <- Pdata23_clean %>% dplyr::select(., BARREL, SPECIES, PLANT_ID, TOOTHPICK, QUAD, DESIGNATION, HT_FINAL, FLWR_9, UID, Year) %>% 
  mutate(., DESIGNATION = ifelse(SPECIES == "BRTE", "I-F",
                              ifelse(SPECIES == "LAGL", "N-F",
                                     ifelse(SPECIES == "ELEL", "N-M",
                                            ifelse(SPECIES == "ARTR", "N-S", NA))))) %>% rename(.,FLWR_FINAL = FLWR_9)


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





#########################################
# To do # 
#########
# 1) Figures of abundance changes year to year
# 2) effect of treatments on BRTE
#   2a) repeat seeding effect
#   2b) species treatment on BRTE
#   2c) repeat seeding on Slow LH species abundance over time

# Assign each barrel the treatments and LH combos
barrelkey <- read.csv("barrel_key.csv")

barrelkey$Species <- gsub("_[^_]+$","", barrelkey$Trt)
#remove trailing part about if control or not; irrelevant bc this is 1 year only
#barrelkey$LH_combo <- gsub("_[^_]+$","", barrelkey$LH_combo)
# remove the LH combo in the trt column
barrelkey$Trt <- gsub(".*_", "", barrelkey$Trt)
head(barrelkey)
#looks like it worked well; now to add life history key (manually)

unique(barrelkey$Trt)
barrelkey <- as_tibble(barrelkey %>% mutate(., Trt=as.factor(Trt)))
head(barrelkey)
#B = cheatgrass = F
#A = sagebrush = S
#L = whitedaisy = F
#E = bottlebrush = M


barrelkey <- barrelkey %>% 
  mutate(
    LH_combo = ifelse(Species == "LE" | Species == "BE", "FM", 
                      ifelse(Species == "BL", "FF",
                             ifelse(Species == "AL" | Species == "BA", "FS", NA)))
  )

head(barrelkey)
#now add treatment and LH combo info to the main dataframe
#make new columns:
Pdata_all$Trt = NA
Pdata_all$LH_combo = NA
Pdata_all$Spec.TRT=NA
#fill in based on info in barrel key:
for (i in 1:nrow(Pdata_all)) {
  Pdata_all$Trt[i] = barrelkey$Trt[barrelkey$BARREL == Pdata_all$BARREL[i]]
  Pdata_all$LH_combo[i] = barrelkey$LH_combo[barrelkey$BARREL == Pdata_all$BARREL[i]]
  Pdata_all$Spec.TRT[i] = barrelkey$Species[barrelkey$BARREL==Pdata_all$BARREL[i]]
  
}

Cdata_all$Trt = NA
Cdata_all$LH_combo = NA
Cdata_all$Spec.TRT=NA
#fill in based on info in barrel key:
for (i in 1:nrow(Cdata_all)) {
  Cdata_all$Trt[i] = barrelkey$Trt[barrelkey$BARREL == Cdata_all$BARREL[i]]
  Cdata_all$LH_combo[i] = barrelkey$LH_combo[barrelkey$BARREL == Cdata_all$BARREL[i]]
  Cdata_all$Spec.TRT[i] = barrelkey$Species[barrelkey$BARREL==Cdata_all$BARREL[i]]
  
}

count_new <- Cdata_all %>%
  group_by(BARREL, Trt, SPECIES, Year) %>%
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
  spread(key = SPECIES, value = total, fill = 0)


count_new <- count_new %>%
  mutate(Trt = recode(Trt,
                      `1` = "Control",
                      `2` = "Repeated"))

# 
# count_new <- count_new %>%
#   rename(`I-F` = BRTE,
#          `N-F` = LAGL,
#          `N-M` = ELEL,
#          `N-S` = ARTR)


count_long <- count_new %>%
  pivot_longer(cols = c(BRTE, LAGL, ELEL, ARTR),
               names_to = "SPECIES",
               values_to = "Count")


diffplot <- ggplot(count_long, aes(x = Trt, y = Count, fill = Trt)) +
  geom_boxplot(alpha = 0.9) +
  facet_wrap(~SPECIES, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Plant Counts by Species and Treatment") +
  xlab("Treatment") + ylab("Plant Count")+
  labs(fill = "Species") + 
  theme_light()+
  scale_fill_brewer(palette = "Set2")

diffplot



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



##########
# 1) Figure of abundance changes year to year
ggplot(count_long, aes(x = SPECIES, y = Count, fill = Trt)) +
  geom_boxplot(outliers = F) +
  facet_wrap(~Year, scales = "free_y") + labs(
    title = "Abundance per barrel by Species Across Years",
    x = "Species",
    y = "Count",
    fill = "Treatment"
  ) 

##########
# 2) Species Trt w/ BRTE
count_long %>% filter(., SPECIES == "BRTE") %>% 
ggplot( aes(x = Trt, y = Count, fill = Trt)) +
  geom_boxplot(outliers = F) 



