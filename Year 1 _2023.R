########################
### Barrel Data 2023 ###
########################

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)

# Load in data
data <- read_xlsx("Barrel_Data_2023_FINAL.xlsx")
soil_data <- read_xlsx("Barrel_Data_2023_FINAL.xlsx", sheet = 2)

# Move the last VWC Column ot match format
soil_data <- soil_data %>% 
  relocate(DATE...16, .after = VWC...17)

#RENAME COLUMNS 
data <- data %>% 
  rename( BARREL = `BARREL #`)

# Change inflorescence NAs to 0's
data <- data %>%
  mutate(FLWR_5 = ifelse(is.na(FLWR_5), 0, FLWR_5),
                FLWR_6 = ifelse(is.na(FLWR_6), 0, FLWR_6),
                FLWR_7 = ifelse(is.na(FLWR_7), 0, FLWR_7),
                FLWR_8 = ifelse(is.na(FLWR_8), 0, FLWR_8),
                FLWR_9 = ifelse(is.na(FLWR_9), 0, FLWR_9))




##################################
####### HOW MANY INVADERS ??? ####
##################################
# invaders <- data %>% 
#   group_by(NOTES_2, NOTES_3, NOTES_4, NOTES_5, NOTES_6) %>% 
#   filter(== "INVADER", na.rm = T)


##########################
#######  Reproduction ####
##########################
###### Create final column for FLWR that gives final count of inflorescences 
FLWR <- data %>% 
  select(FLWR_5, FLWR_6, FLWR_7, FLWR_8, FLWR_9) 

max(FLWR$FLWR_7, na.rm = T)


FLWR_MAX <- apply(FLWR, 1, max, na.rm = T)
###### Find 

FLWR %>% 
  mutate(FLWR_MAX = cummax(FLWR[,]))


### Create a new DF for each time step???
TS_1 <- data %>% 
  select(BARREL, SPECIES, AorD_1, HT_1, FLWR_1)

TS_2 <- data %>% 
  select(BARREL, SPECIES,AorD_2, FLWR_2)

TS_3 <- data %>% 
  select(BARREL, SPECIES, AorD_3, FLWR_3)

TS_4 <- data %>% 
  select(BARREL, SPECIES, AorD_4, FLWR_4)

TS_5 <- data %>% 
  select(BARREL,  SPECIES,AorD_5, FLWR_5)

TS_6 <- data %>% 
  select(BARREL, SPECIES,AorD_6, FLWR_6)

TS_7 <- data %>% 
  select(BARREL, SPECIES, AorD_7, FLWR_7)

TS_8 <- data %>% 
  select(BARREL, SPECIES, AorD_8, FLWR_8)

TS_9 <- data %>% 
  select(BARREL, SPECIES, AorD_9, FLWR_9)

#####################################
### Create DFs for visualization ####
#####################################

num_ind_barrel <- data %>% count(BARREL)

ggplot(num_ind_barrel, aes(x = n)) +
  geom_histogram()



####### Try w/o making new df #######

df <- data %>% # Remove all Unknown splants
  subset(., SPECIES != "U") %>% 
  subset(., SPECIES !="UG") %>% 
  subset(., SPECIES != "UD")

### Number of inflorescence per plant
ggplot(df, aes(x=SPECIES, color = BARREL)) +
  geom_jitter(aes(y=FLWR_9)) + 
  scale_colour_gradient(low ="red", high = "blue")
  
### number of plants per species total
ggplot(df, aes(x=SPECIES, fill = SPECIES)) +
  geom_bar()

#### make AoD comparison to beginning and end

AOD_bar <- ggplot(df, aes(AorD_1))+
  geom_bar()

#######
TS_9

ggplot(df, aes(x = DATE_1, y = SPECIES)) +
  geom_jitter()

####
# Date of Emergence #
###



