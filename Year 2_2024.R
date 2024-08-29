########################
### Barrel Data 2024 ###
########################

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(reshape2)
library(viridis)

# Set WD for Desktop
# setwd("~/GitHub/Barrels")
# Set WD for laptop
setwd("/Users/michaelotisclyne/Desktop/Github/Barrels")


# Load in data
#countdata <- read_xlsx("2024 Data.xlsx", sheet = 1)
plantdata <- read_xlsx("2024 Data.xlsx", sheet = 3)
VWCdata <- read_xlsx("2024 Data.xlsx", sheet = 4)


# countdata <- countdata %>% 
#   mutate(., BRTE = as.numeric(ifelse(`BRTE count` == "X", NA, `BRTE count`)),
#          LAGL = as.numeric(ifelse(`LAGL count` == "X", NA, `LAGL count`)),
#          ELEL = as.numeric(ifelse(`ELEL count` == "X", NA, `ELEL count`)),
#          ARTR = as.numeric(ifelse(`ARTR count` == "X", NA, `ARTR count`)))


#### COME UP WITH A WAY TO CALCULATE NUMBER OF NEW ELEL BASED ON SURVEY DATA, NT FROM 1ST SWEEP DATA ABOVE. 
dim(plantdata %>% filter(SPECIES == "ELEL"))[1]



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
  filter(., SPECIES == "ELEL")


# mutate(., LH = SPECIES) %>% # Create dummy column
#   mutate(., LH = ifelse(LH=="ELEL", "MEDIUM", "FAST")) %>% # create fast and medium 
#   relocate(LH, .before = `PLANT ID`) %>% # relocate LH col to beginning 
#   mutate(data, NvI = SPECIES) %>% # another dummy col to set up mutate
#   mutate(., NvI = ifelse(NvI == "BRTE", "INVASIVE", "NATIVE")) %>% # Create Native vs Invasive 
#   relocate(NvI, .before = `PLANT ID` )




###########################
### FOR ESA 2024 POSTER ###
###########################

# make a figure of number of plants survived to last survey in each year 1 and year 2
data23 <- read.csv("barrel_data_all.csv")

data24 <- read_xlsx("2024 Data.xlsx", sheet = 3)
count24 <- read_xlsx("2024 Data.xlsx", sheet = 1)
data24 <- data24 %>% rename(., BARREL = `BARREL #`, PLANT.ID = `PLANT ID`) 

data24 <- data24[-380,] # remove the ELEL onvader in barel 44


count.df <- tribble(
  ~SPECIES, ~Year1, ~Year2,
  "ARTR", 0, count24 %>% select(`ARTR count`) %>% colSums(),
  "ELEL", dim(filter(data23, SPECIES == "ELEL") %>% # how many ELEL survived to the last survey?
                filter(., AorD_9 == "A"))[1], 
  count24 %>% select(`ELEL count`) %>% colSums(),
  "BRTE", dim(filter(data23, SPECIES == "BRTE") %>% # how many BRTE reproduced at least 1 inflorescence?
                filter(., FLWRTotal > 0))[1], 
  dim(data24 %>% filter(SPECIES == "ELEL"))[1],
  "LAGL", dim(filter(data23, SPECIES == "LAGL") %>% # how many BRTE reproduced at least 1 inflorescence?
                filter(., FLWRTotal > 0))[1], 
  count24 %>% select(`LAGL count`) %>% colSums()
)

data.comb <- tribble(
  ~SPECIES, ~YEAR, ~COUNT,
  "ARTR", 2023, 0,
  "ARTR", 2024, count24 %>% select(`ARTR count`) %>% colSums(),
  "ELEL", 2023, dim(filter(data23, SPECIES == "ELEL") %>% # how many ELEL survived to the last survey?
                      filter(., AorD_9 == "A"))[1],
  "ELEL", 2024, dim(data24 %>% filter(SPECIES == "ELEL"))[1],
  "BRTE", 2023, dim(filter(data23, SPECIES == "BRTE") %>% # how many BRTE reproduced at least 1 inflorescence?
                      filter(., FLWRTotal > 0))[1],
  "BRTE", 2024, count24 %>% select(`BRTE count`) %>% colSums(),
  "LAGL", 2023, dim(filter(data23, SPECIES == "LAGL") %>% # how many BRTE reproduced at least 1 inflorescence?
                      filter(., FLWRTotal > 0))[1],
  "LAGL", 2024, count24 %>% select(`LAGL count`) %>% colSums()
) %>% mutate(., YEAR = as.factor(YEAR), SPECIES =as.factor(SPECIES), COUNT = as.numeric(COUNT))


data.comb %>% 
  filter(., SPECIES == "ELEL") %>% 
ggplot(., aes(fill=YEAR, x=SPECIES, y =COUNT)) + 
  geom_bar(position="stack", stat="identity") 

data.comb %>% 
  filter(., SPECIES == "ARTR") %>% 
  ggplot(., aes(fill=YEAR, x=SPECIES, y =COUNT)) + 
  geom_bar(position="dodge", stat="identity") 







#######
# Figure of year 1 and 2 germination rates #
#######
# Initial seeds per barrel #
#init.seed <- tibble(ARTR = 231, ELEL = 105, BRTE = 130, LAGL = 130)
init.seed <- tibble(ARTRtotal = 231*64, ELELtotal = 105*64, BRTEtotal = 130*96, LAGLtotal = 130*96)


init.seed <- init.seed %>% 
  mutate(., ARTRgerm = 0, 
         ELELgerm = dim(filter(data23, SPECIES == "ELEL") %>% # how many ELEL survived to the last survey?
                                                   filter(., AorD_9 == "A"))[1],
         BRTEgerm = dim(filter(data23, SPECIES == "BRTE") %>% # how many BRTE reproduced at least 1 inflorescence?
                                                   filter(., FLWRTotal > 0))[1],
         LAGLgerm = dim(filter(data23, SPECIES == "LAGL") %>% # how many LAGL reproduced at least 1 inflorescence?
                                                   filter(., FLWRTotal > 0))[1])
germ.plot2 <- tribble(
  ~SPECIES, ~YEAR ,~INIT.Seed, ~GERM.tot, ~prop.germ,
  #--|--|----
  "Native-Slow", 2023, init.seed$ARTRtotal, init.seed$ARTRgerm, (init.seed$ARTRgerm/init.seed$ARTRtotal), # initial seeding, germ tot and prop year 1
  "Native-Slow", 2024, #year 2
  data23 %>% filter(., SPECIES == "ARTR") %>% select(., SEEDTotal) %>% colSums() + (init.seed$ARTRtotal/2)*231, #total self seeded + repeat seed
  data.comb %>% filter(SPECIES=="ARTR") %>% select(COUNT) %>% colSums(), # total germ in year 2 
  (data.comb %>% filter(SPECIES=="ARTR") %>% select(COUNT) %>% colSums())/(data23 %>% filter(., SPECIES == "ARTR") %>% select(., SEEDTotal) %>% colSums() + (init.seed$ARTRtotal/2)*231),  # germinated this year / total seeds in from last year + repeat seed
  
  "Native-Medium", 2023, init.seed$ELELtotal, init.seed$ELELgerm, (init.seed$ELELgerm/init.seed$ELELtotal),
  "Native-Medium", 2024, #year 2
  data23 %>% filter(., SPECIES == "ELEL") %>% select(., SEEDTotal) %>% colSums() + (init.seed$ELELtotal/2)*105, #total self seeded + repeat seed
  data.comb %>% filter(SPECIES=="ELEL") %>% select(COUNT) %>% colSums(), # total germ in year 2 
  (data.comb %>% filter(SPECIES=="ELEL") %>% select(COUNT) %>% colSums())/(data23 %>% filter(., SPECIES == "ELEL") %>% select(., SEEDTotal) %>% colSums() + (init.seed$ELELtotal/2)*105),  # germinated this year / total seeds in from last year + repeat seed
 
  "Invasive-Fast", 2023, init.seed$BRTEtotal, init.seed$BRTEgerm, (init.seed$BRTEgerm/init.seed$BRTEtotal),
  "Invasive-Fast", 2024,
  data23 %>% filter(., SPECIES == "BRTE") %>% select(., SEEDTotal) %>% colSums(), #total self seeded // no repeat seed for BRTE
  data.comb %>% filter(SPECIES=="BRTE") %>% select(COUNT) %>% colSums(), # total germ in year 2 
  (data.comb %>% filter(SPECIES=="BRTE") %>% select(COUNT) %>% colSums())/(data23 %>% filter(., SPECIES == "BRTE") %>% select(., SEEDTotal) %>% colSums()),  # germinated this year / total seeds in from last year
  
  "Native-Fast", 2023, init.seed$LAGLtotal, init.seed$LAGLgerm, (init.seed$LAGLgerm/init.seed$LAGLtotal),
  "Native-Fast", 2024,
  data23 %>% filter(., SPECIES == "LAGL") %>% select(., SEEDTotal) %>% colSums() + (init.seed$LAGLtotal/2)*130, #total self seeded + repeat seed
  data.comb %>% filter(SPECIES=="LAGL") %>% select(COUNT) %>% colSums(), # total germ in year 2 
  (data.comb %>% filter(SPECIES=="LAGL") %>% select(COUNT) %>% colSums())/(data23 %>% filter(., SPECIES == "LAGL") %>% select(., SEEDTotal) %>% colSums() + (init.seed$LAGLtotal/2)*130),  # germinated this year / total seeds in from last year + repeat seed
  
) %>% mutate(.,YEAR = as.factor(YEAR))

data23 %>% filter(., SPECIES == "ELEL") %>% select(., SEEDTotal) %>% colSums()  #this get all seeds produced by plants
# add re-seeded seed total 
(init.seed$ELELtotal/2)*105


### change this to not inlcude species names, but ise Native and LH denominations instead

library(wesanderson)
names(wes_palettes)

ggplot(data = germ.plot2) + 
  geom_bar(aes(x = SPECIES, y = prop.germ, fill = YEAR), position = "dodge", stat = "identity")+
  theme_light() + 
  ylab("Germination Proportion") + 
  ggtitle("Species Germination Rates In Each Year") + 
  scale_fill_manual(values = wes_palette("FrenchDispatch", n = 2))+
  theme(legend.position = c(0.8,0.7), legend.background = element_rect(color = "black" , size=0.5, linetype="solid")) 
  


##############################
### Make a plot that depicts if you can see a difference in treatments (initial seeding vs re-seed)


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
data24$Trt = NA
data24$LH_combo = NA
data24$Spec.TRT=NA
#fill in based on info in barrel key:
for (i in 1:nrow(data24)) {
  data24$Trt[i] = barrelkey$Trt[barrelkey$BARREL == data24$BARREL[i]]
  data24$LH_combo[i] = barrelkey$LH_combo[barrelkey$BARREL == data24$BARREL[i]]
  data24$Spec.TRT[i] = barrelkey$Species[barrelkey$BARREL==data24$BARREL[i]]
  
}
head(data24[15:2])

#assign year column to each year's dataframe
data23$YEAR = 2023
data24$YEAR = 2024

# create DESIGNATION column for data23
data23 <- as.tibble(data23) 
  


data23<- data23 %>% 
  mutate(., LH = SPECIES) %>% # Create dummy column
  mutate(., LH = ifelse(LH=="BRTE" | LH == "LAGL", "F", 
                       ifelse(LH == "ELEL", "M",
                              ifelse(LH == "ARTR", "S", NA)))) %>% # create fast and medium 
  relocate(LH, .before = PLANT.ID) %>% # relocate LH col to beginning 
  mutate(., NvI = SPECIES) %>% # another dummy col to set up mutate
  mutate(., NvI = ifelse(NvI == "BRTE", "I", "N")) %>% # Create Native vs Invasive 
  mutate(., DESIGNATION = paste(NvI, LH, sep = "-")) %>% 
  relocate(NvI, .before =  PLANT.ID) %>% 
  relocate(DESIGNATION, .before =  PLANT.ID)
    
#######
 

#get ELEL sums from each Barrel in data24
elel.b.sum <- data24 %>% 
  filter(SPECIES == "ELEL") %>%   # select just ELEL
  select(., BARREL, SPECIES, Trt) %>% 
  mutate(., Count = 1)

elel.agg <- aggregate(Count~BARREL, elel.b.sum, FUN = sum) # sum all ELEL in each barrel

write_csv(elel.agg, file="ELEL_SUM.csv")





#set-up data for plot
count24$Trt <- gsub(".*_", "", count24$Trt)
count24 <- count24 %>% mutate(., Trt = as.factor(Trt))

diff.plot <- count24 %>% 
  mutate(., Trt = ifelse(Trt == "1", "Control", "Repeated")) %>% 
  mutate(., BARREL = `Barrel ID`,
         "I-F" = as.numeric(`BRTE count`), 
         "N-F" = as.numeric(`LAGL count`),
         "N-M" = as.numeric(`ELEL count`),
         "N-S" = as.numeric(`ARTR count`)) %>% 
  select(., BARREL, Trt, "I-F", "N-F", "N-M", "N-S") 

diff.plot <- diff.plot[!grepl("TOTAL", diff.plot$BARREL),]


write_csv(diff.plot, file="diff.csv")

diffplot <- read_csv("diff.csv")

# head(elel.agg)
# head(TESTY)
# test.merge <- TESTY %>% 
#   mutate(., `N-M`= ifelse(BARREL==elel.agg$BARREL, elel.agg$Count, 0))
# 
# match(elel.agg$BARREL, TESTY$BARREL)

diffplot <- melt(diffplot, id.vars = c("BARREL", "Trt")) 
  
# test$Trt <- gsub(".*_", "", test$Trt)
diffplot <- diffplot %>% rename(., Treatment = Trt) %>% mutate(., Treatment = as.factor(Treatment))

diffplot <- diffplot %>% mutate(., variable = ifelse(variable == "I-F", "Invasive-Fast",
                                ifelse(variable == "N-F", "Native-Fast",
                                  ifelse(variable == "N-M", "Native-Medium",
                                      ifelse(variable == "N-S", "Native-Slow", NA)))))

library(ggpubr)
alldiffplot <- ggplot(data = diffplot)+
  geom_boxplot(aes(x = Treatment, y = value, fill = variable), outliers = F)+
  ggtitle("Plant Counts by Species and Treatment") +
  xlab("Treatment") + ylab("Plant Count")+
  labs(fill = "Species") + 
  theme_light()+
  theme(legend.position = c(0.85,0.7), legend.background = element_rect(color = "black" , size=0.5, linetype="solid")) +
  scale_fill_brewer(palette = "Set2")

# + coord_cartesian(ylim=c(0,500))
  
alldiffplot

names(wes_palettes)

IFplot <- diffplot %>% 
  filter(., variable == "Invasive-Fast") %>% 
  ggplot(data = .)+
  geom_boxplot(aes(x = Treatment, y = value, fill = Treatment), outliers = T)+
  ggtitle("") +
  xlab("Invasive-Fast") + ylab("") +
  theme_minimal()+
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 2)) +coord_flip()
#+ theme(plot.title = element_text(hjust=0.5))
  

IFplot # plot for just cheatgrass


Nativeplot <- diffplot %>%  # plot for all natives
  filter(., variable %in% c("N-F", "N-M", "N-S")) %>% 
  ggplot(data = .)+
  geom_boxplot(aes(x = Treatment, y = value, fill = variable), outliers = T)+
  ggtitle("Plant Counts by Species and Treatment") +
  xlab("Treatment") + ylab("Plant Count")+
  labs(fill = "Species")+
  scale_fill_brewer()

Nativeplot

NFplot <- diffplot %>% 
  filter(., variable == "Native-Fast") %>% 
  ggplot(data = .)+
  geom_boxplot(aes(x = Treatment, y = value, fill = Treatment), outliers = T)+  
  ggtitle("") +
  xlab("Native-Fast") + ylab("") +  
  theme_minimal()+
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 2)) +coord_flip()

NFplot


NMplot <- diffplot %>% 
  filter(., variable == "Native-Medium") %>% 
  ggplot(data = .)+
  geom_boxplot(aes(x = Treatment, y = value, fill = Treatment), outliers = T)+  
  ggtitle("") +
  xlab("Native-Medium") + ylab("") +  
  theme_minimal()+
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 2)) +coord_flip()

NMplot


Nsplot <- diffplot %>% 
  filter(., variable == "Native-Slow") %>% 
  ggplot(data = .)+
  geom_boxplot(aes(x = Treatment, y = value, fill = Treatment), outliers = T)+  
  ggtitle("") +
  xlab("Native-Slow") + ylab("") +  
  theme_minimal()+
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 2))+coord_flip()

Nsplot



figure <- ggarrange(IFplot, NFplot, NMplot, Nsplot, 
          common.legend = T, legend = "top",
          labels = c("(A)", "(B)", "(C)", "(D)"),
          ncol = 2, nrow = 2)

annotate_figure(figure,
                top = text_grob("Counts of Species by Treatment", color = "black", face = "bold", size = 14),
                left = "Species", bottom = "Plant Count"
)



###
