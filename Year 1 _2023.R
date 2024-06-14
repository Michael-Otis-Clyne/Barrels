########################
### Barrel Data 2023 ###
########################

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
setwd("~/Desktop/Github/Barrels")

# Load in data
data <- read_xlsx("Barrel_Data_2023_FINAL.xlsx")
soil <- read_xlsx("Barrel_Data_2023_FINAL.xlsx", sheet = 2)
brte.indv <- read_xlsx("Barrel_Data_2023_FINAL.xlsx", sheet = 3)

# data <- read_xlsx("Barrel_Data_2023_CLYNE_RD.xlsx", sheet = 1)
# soil <- read_xlsx("Barrel_Data_2023_CLYNE_RD.xlsx", sheet = 2)

# Change inflorescence NAs to 0's
data <- data %>%
  mutate(FLWR_1 = ifelse(is.na(FLWR_1), 0, FLWR_1),
         FLWR_2 = ifelse(is.na(FLWR_2), 0, FLWR_2),
         FLWR_3 = ifelse(is.na(FLWR_3), 0, FLWR_3),
         FLWR_4 = ifelse(is.na(FLWR_4), 0, FLWR_4),
         FLWR_5 = ifelse(is.na(FLWR_5), 0, FLWR_5),
                FLWR_6 = ifelse(is.na(FLWR_6), 0, FLWR_6),
                FLWR_7 = ifelse(is.na(FLWR_7), 0, FLWR_7),
                FLWR_8 = ifelse(is.na(FLWR_8), 0, FLWR_8),
                FLWR_9 = ifelse(is.na(FLWR_9), 0, FLWR_9))


data <- data %>% # Remove all Unknown plants
  subset(., SPECIES != "U") %>% 
  subset(., SPECIES !="UG") %>% 
  subset(., SPECIES != "UD")


########
# create Life History  and Native vs invasive columns

data <- data %>% 
  mutate(., LH = SPECIES) %>% # Create dummy column
  mutate(., LH = ifelse(LH=="ELEL", "MEDIUM", "FAST")) %>% # create fast and medium 
  relocate(LH, .before = `PLANT ID`) %>% # relocate LH col to beginning 
  mutate(data, NvI = SPECIES) %>% # another dummy col to set up mutate
  mutate(., NvI = ifelse(NvI == "BRTE", "INVASIVE", "NATIVE")) %>% # Create Native vs Invasive 
  relocate(NvI, .before = `PLANT ID` )
  

##############################################
### Merge the soil moisture data by Barrel ###
##############################################
# create a named vector
lookup <- c(BARREL = "BARREL", VWC_2 =  "VWC...2", VWC_3 = "VWC...4", VWC_4 = "VWC...6",  VWC_5 = "VWC...8", 
            VWC_6 ="VWC...10",  VWC_7 = "VWC...12", VWC_8 = "VWC...14", VWC_9 = "VWC...16")
soil <- soil %>% 
  select(., BARREL, contains("VWC")) %>% #select only VWC columns
  rename(., all_of(lookup)) # rename by the named vector to match the time period of plant data collection

data <- data %>% 
  full_join(soil, by = "BARREL")

##################################################
############ Clean up the BRTE data ##############
##################################################

col_FLWR <- c("FLWR_1", "FLWR_2", "FLWR_3", "FLWR_4","FLWR_5", "FLWR_6","FLWR_7","FLWR_8", "FLWR_9")
data <- data %>% 
  mutate(., FLWRTotal =  apply(data[c(col_FLWR)], 1, max ))

#brte.indv <- brte.indv[complete.cases(brte.indv),] # for when the weighing of BRTE seeds wasn't done

# 1. BRTE is currently per barrel, not per individual. Need to back-calculate weight total to # of seeds/ individual. 

# barr.spec.sum <- data %>% # Creates DF for # of inflorescence total per barrel.
#   select(., BARREL, SPECIES, FLWRTotal) %>% 
#   mutate(., FLWRTotal =  as.numeric(FLWRTotal)) %>% 
#   group_by(BARREL, SPECIES) %>% 
#   dplyr::summarize(., FLWRSUM = sum(FLWRTotal))
# 
# 
# # Take barr.spec.sum to back-calculate number of seeds per inflorescense of BRTE
# # Need to match up 
# check <- barr.spec.sum %>% 
#   filter(., SPECIES == "BRTE")
# 
# brte.inf.seeds <- cbind(check, brte.indv$SEEDS) # Uh OH, lengths don't match
# 
# mismatch <- dplyr::anti_join(check, brte.indv) # find which barrel had BRTE encountered that wasn't seeded with BRTE???
# mismatch # barrel 24 is not seeded with BRTE and therefore BRTE is an INVADER
# brte.inf.seeds <- check[-13,] # remove row 13 where in which barrel 24 has BRTE is an INVADER
# 
# BRTE.DF <- brte.inf.seeds %>% # Gives average seeds per inflorescence per barrel for BTRE
#   add_column(., SEEDS = brte.indv$SEEDS) %>% 
#   mutate(., SEEDS.PER.INF = SEEDS/FLWRSUM)
# 
# BRTE.SEEDS.PER.INF <- BRTE.DF$SEEDS.PER.INF
# mean(BRTE.SEEDS.PER.INF)
# 88.29695 on average per inflorescence 



######
# Initial seeds per barrel #
#init.seed <- tibble(ARTR = 230.6306526, ELEL = 105.168092, BRTE = 130, LAGL = 130.21, .rows = 2)

##########################
#######  Reproduction ####
##########################
###### Create final column for FLWR that gives final count of inflorescence 
# FLWR <- data %>% 
#   select(FLWR_1, FLWR_2, FLWR_3, FLWR_4, FLWR_5, FLWR_6, FLWR_7, FLWR_8, FLWR_9) 
# 
# FLWR_MAX <- apply(FLWR, 1, max, na.rm = T) # GIVES MAX INFLORESECENCE VALUE PER PLANT
# df_FLWR <- cbind(df_FLWR, FLWR_MAX) # create a new column for the max value
# # Now Rename new column 'FLWR Total'

### try this in dplyr
# create a new column for the max value and rename new column 'FLWR Total'

col_FLWR <- c("FLWR_1", "FLWR_2", "FLWR_3", "FLWR_4","FLWR_5", "FLWR_6","FLWR_7","FLWR_8", "FLWR_9")
data <- data %>% 
  mutate(., FLWRTotal =  apply(data[c(col_FLWR)], 1, max ))
### Create new column for inflorescence total per individual
data <- data %>% 
  mutate(SEEDTotal = as.numeric(FLWRTotal))

#Edit this to have exact seed output per plant

for(i in 1:length(data$BARREL)){
if(data$SPECIES[i] == "LAGL"){
  data$SEEDTotal[i] <-  (data$FLWRTotal[i]*39)
  }
  if(data$SPECIES[i] == "ELEL"){
    data$SEEDTotal[i] <-  (data$FLWRTotal[i]*93)
  }
  if(data$SPECIES[i] == "BRTE"){
    data$SEEDTotal[i] <-  (data$FLWRTotal[i]*88)
  }
}




#write.csv(data, file = "CLYNE_DATA_RD2024.csv", row.names = F)
write.csv(data, file = "barrel_data_all.csv", row.names = F) # create .csv for Research Design class


# Create column with TOTAL reproductive output per plant (i.e. LAGL = x inflor * 39 seeds/ inflor)
### LAGL ###
# LAGL.data <- data %>% 
#   filter(., SPECIES == "LAGL") %>% 
#   mutate(., SEEDTotal = as.numeric(FLWRTotal)*39)
# 
# ### ELEL ###
# ELEL.data <- data %>% 
#   filter(., SPECIES == "ELEL") %>% 
#   mutate(., SEEDTotal = as.numeric(FLWRTotal)*93)


### ARTR ###
# hahahahaha none ;(



# Plot of the number of BRTE seeds per barrel 
B.S.PLot <- ggplot(brte.indv, aes(x = BARREL, y = SEEDS))+ 
  geom_bar(stat = "identity") 

B.S.PLot # need to make this more pretty 

#####################
### Plots for Bob ###
#####################
# Mean output of each species maybe both in absolute numbers of seeds and per capita (i.e. total seeds produced/seeds seeded). 
#With confidence intervals that summarize variability across barrels
# mean seed production per species with error bars
#Get means for each species for the individual 

#### stats anyone? 

BRTEmean <- data %>% 
  filter(SPECIES == "BRTE") %>% 
  summarise(., BRTEavg = mean(seeds),
            BRTEsd = sd(seeds),
            se = sd(seeds) / sqrt(length(seeds)))
BRTEmean

ELELmean <- data %>% 
  filter(SPECIES == "ELEL") %>% 
  summarise(., ELELavg = mean(seeds),
            ELELsd = sd(seeds),
            se = sd(seeds) / sqrt(length(seeds)))
ELELmean

LAGLmean <- data %>% 
  filter(SPECIES == "LAGL") %>% 
  summarise(., LAGLavg = mean(seeds), 
            LAGLsd = sd(seeds),
            se = sd(seeds) / sqrt(length(seeds)))
LAGLmean

Bobdata <- tibble(Species = c("LAGL", "ELEL", "BRTE"),
                  Average = c(LAGLmean$LAGLavg, ELELmean$ELELavg, BRTEmean$BRTEavg) ,
               sd = c(LAGLmean$LAGLsd, ELELmean$ELELsd, BRTEmean$BRTEsd),
               se = c(LAGLmean$se, ELELmean$se, BRTEmean$se))

##### first plot is Mean output of each species maybe both in absolute numbers of seeds
forBob <- ggplot(data = Bobdata) +
  geom_bar(aes(x = Species, y = Average), stat = "identity", fill = "darkgreen") + 
  geom_errorbar(aes(x = Species, ymin = Average-sd, ymax = Average+sd), 
                width=0.4, colour="black", alpha=1, linewidth=1) + theme_light() +
  ylab("Seeds") +
  ggtitle("Mean Seed Production")

forBob
########
#### Second plot is per capita (i.e. total seeds produced/seeds seeded). 
########

bobdata2 <- data %>% 
  group_by(BARREL) %>% 
  mutate(., LAGLtotal = sum(seeds), 
            LAGLpercap.perbarrel = sum(FLWRTotal)/(96*130),
            LAGLsd = sd(seeds/(130)),
            se = sd(seeds) / sqrt(length(seeds)))
  
BRTEpercap <- data %>% 
  filter(SPECIES == "BRTE") %>% 
  summarise(., BRTEtotal = sum(seeds), 
            percap = sum(seeds)/ (96*130),
            sd = sd(seeds/(130)),
            se = sd(seeds) / sqrt(length(seeds)))

ELELpercap <- data %>% 
  filter(SPECIES == "ELEL") %>% 
  summarise(., LAGLtotal = sum(seeds), 
            percap = sum(seeds)/ (64*105),
            sd = sd(seeds/(105)),
            se = sd(seeds) / sqrt(length(seeds)))


LAGLpercap <- data %>% 
  filter(SPECIES == "LAGL") %>% 
  summarise(., LAGLtotal = sum(seeds), 
            percap = sum(seeds)/ (96*130),
            sd = sd(seeds/(130)),
            se = sd(seeds) / sqrt(length(seeds)))

Bobdata.percap <- tibble(Species = c("LAGL", "ELEL", "BRTE"),
                  Average = c(LAGLpercap$percap, ELELpercap$percap, BRTEpercap$percap) ,
                  sd = c(LAGLpercap$sd, ELELpercap$sd, BRTEpercap$sd))



forBob.percap <- ggplot(data = Bobdata.percap) +
  geom_bar(aes(x = Species, y = Average), stat = "identity", fill = "darkgreen") + 
  geom_errorbar(aes(x = Species, ymin = Average-sd, ymax = Average+sd), 
                width=0.4, colour="black", alpha=1, size=1)+
  theme_light() +
  ylab("Seeds") + 
  ggtitle("Seed Production Per Capita")

forBob.percap

ggplot(fecLAGL, aes(x = BARREL, y = SEEDTotal))+
  geom_bar(stat = "identity") 

ggplot( fecELEL, aes(x = BARREL, y = SEEDTotal))+
  geom_bar(stat = "identity") 

# Plot of the number of BRTE seeds per barrel 
B.S.PLot <- ggplot(brte.indv, aes(x = BARREL, y = SEEDS))+ 
  geom_bar(stat = "identity") 




#######################
### Fitness Metrics ###
#######################

#How many seeds went in to the barrels in year 1 vs how many plants emerged? 


##########
### Do a series of If statements for pulling the plant date about birth, death date and flwr output 
#Birth date, death date , FLWR and date observed

if(data$NOTES_1 == "NEW") 


#######################################################
# How many of each LH strategy survived to reproduce? #
#######################################################

dim(filter(data, SPECIES == "ELEL") %>% # how many ELEL survived to the last survey?
  filter(., AorD_9 == "A"))[1]
# 348
dim(filter(data, SPECIES == "ELEL") %>% # how many ELEL survived to last survey and reproduced?
  filter(., FLWRTotal > 0))[1]
#23

dim(filter(data, SPECIES == "BRTE") %>% # how many BRTE reproduced at least 1 inflorescence?
  filter(., FLWRTotal > 0))[1]
# 1212

dim(filter(data, SPECIES == "LAGL") %>% # how many LAGL reproduced at least 1 inflorescence?
  filter(., FLWRTotal > 0))[1]
# 80

totalplants = 348+1212+80 #1640 total
native.proportion = (348+80)/1640 # proportion of native plants
FAST.total = 1212+80 # 1292 FAST LH
FAST.proportion = 1292/1640 # proportion of fast LH




