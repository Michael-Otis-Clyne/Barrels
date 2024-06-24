########################
### Barrel Data 2024 ###
########################

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
# Set WD for Desktop
setwd("E:/.shortcut-targets-by-id/1FKZ0f1hvfDvznQq24o_UB6n2gzrWLCTt/LH_PriorEffects/PE experiment/Data/2024")

# Set WD for laptop

# Load in data
countdata <- read_xlsx("2024 Data.xlsx", sheet = 1)
plantdata <- read_xlsx("2024 Data.xlsx", sheet = 2)
VWCdata <- read_xlsx("2024 Data.xlsx", sheet = 3)


countdata <- countdata %>% 
  mutate(., BRTE = as.numeric(ifelse(`BRTE count` == "X", NA, `BRTE count`)),
         LAGL = as.numeric(ifelse(`LAGL count` == "X", NA, `LAGL count`)),
         ELEL = as.numeric(ifelse(`ELEL count` == "X", NA, `ELEL count`)),
         ARTR = as.numeric(ifelse(`ARTR count` == "X", NA, `ARTR count`)))


#### COME UP WITH A WAY TO CALCULATE NUMBER OF NBEW ELEL BASED ON SURVEY DATA, NT FROM 1ST SWEEP DATA ABOVE. 


