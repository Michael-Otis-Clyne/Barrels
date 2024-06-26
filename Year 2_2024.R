########################
### Barrel Data 2024 ###
########################

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
# Set WD for Desktop
setwd("~/GitHub/Barrels")
# Set WD for laptop

# Load in data
#countdata <- read_xlsx("2024 Data.xlsx", sheet = 1)
plantdata <- read_xlsx("2024 Data.xlsx", sheet = 2)
VWCdata <- read_xlsx("2024 Data.xlsx", sheet = 4)


# countdata <- countdata %>% 
#   mutate(., BRTE = as.numeric(ifelse(`BRTE count` == "X", NA, `BRTE count`)),
#          LAGL = as.numeric(ifelse(`LAGL count` == "X", NA, `LAGL count`)),
#          ELEL = as.numeric(ifelse(`ELEL count` == "X", NA, `ELEL count`)),
#          ARTR = as.numeric(ifelse(`ARTR count` == "X", NA, `ARTR count`)))


#### COME UP WITH A WAY TO CALCULATE NUMBER OF NBEW ELEL BASED ON SURVEY DATA, NT FROM 1ST SWEEP DATA ABOVE. 




###### Create count data from toothpick sheet
plantdata <- plantdata %>%
  mutate(FLWR_1 = ifelse(is.na(FLWR_1), 0, FLWR_1),
         FLWR_2 = ifelse(is.na(FLWR_2), 0, FLWR_2)) 

speciescounts <- plantdata %>% 
  mutate(., ELEL = (SPECIES=="ELEL"),
         ARTR = (SPECIES=="ARTR"),
         BRTE = (SPECIES=="BRTE"),
         LAGL = (SPECIES=="LAGL")) %>% 
  select(., `BARREL #`, QUAD, HT_1, HT_2, FLWR_1, ELEL, BRTE, ARTR, LAGL, FLWR_2, SPECIES) %>% 
  FILTER(., SPECIES == ELEL)


mutate(., LH = SPECIES) %>% # Create dummy column
  mutate(., LH = ifelse(LH=="ELEL", "MEDIUM", "FAST")) %>% # create fast and medium 
  relocate(LH, .before = `PLANT ID`) %>% # relocate LH col to beginning 
  mutate(data, NvI = SPECIES) %>% # another dummy col to set up mutate
  mutate(., NvI = ifelse(NvI == "BRTE", "INVASIVE", "NATIVE")) %>% # Create Native vs Invasive 
  relocate(NvI, .before = `PLANT ID` )
