#### Analysis of Otis Clyne's data #####
#### Author: Victoria Peechatt
#### Date: 04/21/2024

#### Questions for Otis: ####
#### Can increasing seed introduction frequency of slow LH plants 
#### aid in overcoming reproductive time-lags and 
#### resulting establishment disadvantages of slow LH species?

##### Objectives for myself: #####
##### - MANOVA
##### - SEM

##### Package Test ######

#checks to see if you have package
#if not, installs it 

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkgTest("data.table")
pkgTest("inspectdf")
pkgTest("tidyverse")
pkgTest("gridExtra")
pkgTest("ggpubr")
pkgTest("lme4")
pkgTest("sjPlot")
pkgTest("forcats")
pkgTest("vegan")
pkgTest("asbio")
pkgTest("lavaan")
pkgTest("lavaanPlot")

##### Loading + Visualizing Data #####

clyne.data = data.table::fread(
  "barrel_data_all.csv",
)
head(clyne.data)

str(clyne.data)

barrel = clyne.data$BARREL
species = clyne.data$SPECIES
clyne.data$Height_8 = as.numeric(clyne.data$Height_8)
clyne.data = clyne.data %>% drop_na(AorD_9)

######################################################
##### To make treatment column, and summarized dataset           
######################################################

all.barrels = clyne.data[1980]
all.barrels.summary = data.frame()

for (i in 1:160){
  
  # making a treatment column in original dataset
  
  barrel = clyne.data %>% dplyr::filter(BARREL == i)
  barreli = barrel %>% group_by(SPECIES) %>% count()
  counts = barreli  %>% summarise(n =sum(n))
  treatment = paste(counts[1,2],counts[1,1],
                    ifelse(is.na(counts[2,1]),"",counts[2,1]),
                    ifelse(is.na(counts[2,2]),"",counts[2,2]))
  species_combo = paste(barreli[1,1],
                        ifelse(is.na(barreli[2,1]),"",paste("+",barreli[2,1])))
  species_ratio = paste(counts[1,2],
                        ifelse(is.na(counts[2,2]),":0",paste(":",counts[2,2])))
  top = as.numeric(ifelse(is.na(counts[2,2]),0,counts[1,2]))
  bottom = as.numeric(ifelse(is.na(counts[2,2]),1,counts[2,2]))
  species_ratio_calc = top/bottom
  barrel$TREATMENT = toString(treatment)
  barrel$SPECIES_COMBO = toString(species_combo)
  barrel$SPECIES_RATIO = species_ratio
  barrel$SPECIES_RATIO_CALC = species_ratio_calc
  
  all.barrels  = rbind(all.barrels, barrel, fill =TRUE)
  
  # making a summary dataset
  
  barrel.height = barrel %>% 
    group_by(SPECIES) %>%
    summarise(n=mean(Height_8, na.rm = TRUE)) 
  barrel.seeds = barrel %>%
    group_by(SPECIES) %>%
    summarise(n=sum(SEEDTotal, na.rm = TRUE))
  barrel.flowers = barrel %>%
    group_by(SPECIES) %>%
    summarise(n=sum(FLWRTotal, na.rm = TRUE))
  
  #Survivorship (only took into consideration status at Date 9)
  
  barrel.survival = barrel %>% 
    group_by(SPECIES,AorD_9) %>%
    count() %>%
    pivot_wider(names_from = AorD_9, values_from = n)
  
  if ("A" %in% colnames(barrel.survival)) {
    barrel.survival = barrel.survival %>%
      mutate(A = replace_na(A, 0))
  } else {
    barrel.survival = barrel.survival %>%
      mutate(A = 0)
  }
  
  if ("D" %in% colnames(barrel.survival)) {
    barrel.survival = barrel.survival %>%
      mutate(D = replace_na(D, 0))
  } else {
    barrel.survival = barrel.survival %>%
      mutate(D = 0)
  }
  
  barrel.summary = left_join(barrel.height, barrel.seeds, by = "SPECIES")
  barrel.summary = left_join(barrel.summary, barrel.flowers, by = "SPECIES")
  barrel.summary = left_join(barrel.summary, barrel.survival, by = "SPECIES")
  
  colnames(barrel.summary) = 
    c("Species","Final_Height", "Seed_Total", "Flower_Total", "DEAD","ALIVE")
  
  barrel.summary$Treatment = toString(treatment)
  barrel.summary$Species_Combo = toString(species_combo)
  barrel.summary$Species_Ratio = species_ratio
  barrel.summary$Species_Ratio_Calc = species_ratio_calc
  
  all.barrels.summary = rbind(all.barrels.summary, barrel.summary)
}

all.barrels.summary = all.barrels.summary %>% drop_na(Species)
all.barrels = all.barrels %>% drop_na(SPECIES)

h1 = ggplot(data=all.barrels.summary, aes(Species, Final_Height, fill = Species_Combo))+
  geom_boxplot(outlier.shape = NA)

s2 = ggplot(data=all.barrels.summary, aes(Species, Seed_Total, fill = Species_Combo))+
  geom_boxplot(outlier.shape = NA)

f3 = ggplot(data=all.barrels.summary, aes(Species, Flower_Total, fill = Species_Combo))+
  geom_boxplot(outlier.shape = NA)

ggarrange(h1,s2,f3,common.legend = TRUE, legend = "bottom", ncol=3) 


all.barrels.summary %>% 
  mutate(Species_Ratio = fct_reorder(Species_Ratio, Seed_Total, .fun = 'median'))  %>%
  ggplot(aes(Species_Ratio, Seed_Total, fill = Species))+
  geom_boxplot() +
  scale_x_discrete(guide = guide_axis(angle = 90))

all.barrels.summary %>% 
  mutate(Species_Ratio = fct_reorder(Species_Ratio, Seed_Total, .fun = 'median'))  %>%
  ggplot(aes(Species_Ratio, Seed_Total, fill = Species_Combo))+
  geom_boxplot(alpha = 0.80) +
  geom_point(aes(fill = Species), size = 5, shape = 21, position = position_jitterdodge()) +
  scale_x_discrete(guide = guide_axis(angle = 90))


##### MANOVA ##### 

vars = as.matrix(all.barrels.summary[2:6])

barrel.manova = summary(manova(vars ~ all.barrels.summary$Treatment))
barrel.manova

# Voila, in view there's a visible difference between various treatments

summary(aov(vars ~ all.barrels.summary$Treatment))

# Looks like the difference in final height is
# driving that difference between treatments 
# Let's try to look at differences based on just species combos alone 

barrel.manova = summary(manova(vars ~ all.barrels.summary$Species_Combo))
barrel.manova

summary(aov(vars ~ all.barrels.summary$Species_Combo))

# Looking at species combos as the "treatment" 
# we see all the response variables are 
# pretty pretty different across treatments 

##### General Linear Regression Models ##### 

all.barrels = mutate(all.barrels, ones = 1) |> 
  spread(key = SPECIES_COMBO, value = ones, fill = 0) |>
  mutate(all.barrels, ones=1) |>
  spread(key = AorD_9, value = ones, fill = 0) |> 
  mutate(all.barrels, ones = 1) |>
  spread(key = LH, value = ones, fill = 0) |> 
  mutate(all.barrels, ones =1) |>
  spread(key = NvI, value = ones, fill = 0)

survival.model1 = glm(data = all.barrels, A ~ `BRTE `+ `ELEL ` + `LAGL `+ 
                      `BRTE + ELEL` + `BRTE + LAGL` + `ELEL + LAGL` + 
                        MEDIUM + NATIVE + 
                      VWC_2 + VWC_3 + VWC_4 + VWC_5 + VWC_6 + VWC_7 + 
                      VWC_8 + VWC_9,
                      family = binomial)

summary(survival.model1)

# If you are a medium LH & native plant, the log odds of survival 
# increases by 23.17 &  decreases by -0.033 respectively 
# compared to a fast LH & invasive plant 

survival.model1.2 = glm(data = all.barrels, A ~ 
                        `BRTE + ELEL` + `BRTE + LAGL` + `ELEL + LAGL` + 
                        VWC_2 + VWC_3 + VWC_4 + VWC_5 + VWC_6 + VWC_7 + 
                        VWC_8 + VWC_9,
                        family = binomial)

summary(survival.model1.2)

# If you are a plant growing within the BRTE+LAGL combo
# your log odds of survival decrease by 16.26
# If you are a plant growing within the BRTE+ELEL combo
# your log odds of survival increase by 0.969
# If you are a plant growing within the ELEL+LAGL combo
# your log odds of survival increases by 2.400
# 


# Taking the log of flower totals to look at it 
all.barrels$FLWRTotal_log = log(all.barrels$FLWRTotal)

# This figure shows how ELEL only did well by itself, 
# and after, number of flowers declined with increasing ratio w/ other sp
# LAGL and BRTE were consistent with flower number 

# For some reason i couldn't get the color of the point 
# to correspond to the species 
# and im spending way too much time trying too figure it out, sorry, peace 

ggplot(all.barrels) +
  geom_point(aes(x = SPECIES_RATIO_CALC, y = FLWRTotal_log, fill = SPECIES))+
  stat_smooth(aes(x = SPECIES_RATIO_CALC, y = FLWRTotal_log, fill = SPECIES))


flowers.model2 = glm(data = all.barrels, FLWRTotal ~  
                        `BRTE + ELEL` + `BRTE + LAGL` + `ELEL + LAGL` + 
                       MEDIUM + NATIVE + 
                        VWC_2 + VWC_3 + VWC_4 + VWC_5 + VWC_6 + VWC_7 + 
                        VWC_8 + VWC_9,
                      family = gaussian)

summary(flowers.model2)


# This figure shows something similar to Flowers plot 
# how ELEL only did well by itself, after that the number of seeds declined 
# LAGL and BRTE were consistent with seed number despite ratio with other sp.

ggplot(all.barrels) +
  geom_point(aes(x = SPECIES_RATIO_CALC, y = log(SEEDTotal), fill = SPECIES))+
  stat_smooth(aes(x = SPECIES_RATIO_CALC, y = log(SEEDTotal), fill = SPECIES))


seeds.model3 = glm(data = all.barrels, SEEDTotal ~  
                       `BRTE + ELEL` + `BRTE + LAGL` + `ELEL + LAGL` + 
                       VWC_2 + VWC_3 + VWC_4 + VWC_5 + VWC_6 + VWC_7 + 
                       VWC_8 + VWC_9,
                     family = gaussian)

summary(seeds.model3)

##### SEM's woooooooooo ##### 
all.barrels = mutate(all.barrels, ones = 1) |> 
  spread(key = SPECIES, value = ones, fill = 0)
  
data_scaled = apply(all.barrels[,c(36,50,51)], MARGIN = 2, scale)
data_scaled = cbind(data_scaled, all.barrels[,c(62,65,66,71,72,73,55:60)])
colnames(data_scaled) = c("Height_8", "FLWRTotal",  "SEEDTotal", "survival", 
                          "FAST", "MEDIUM", "BRTE", "ELEL", "LAGL", 
                          "B", "B_E", "B_L", "E", "E_L", "L")

##### Interpretation of SEM: #####
# Model5 is the one that makes the most sense in my head
# All the treatment groups (species combos) are negatively influencing final 
# height, and two (ELEL+LAGL) + (BRTE+LAGL) are positively influencing total 
# number of flowers. (BRTE + ELEL) has a -0.4 effect on flower total 
# Flower total is clearly a predictor of seed total, and height less so 
# These are standardized and comparable to the solo species treatment groups 
###################################

model5 = '
  SEEDTotal ~ FLWRTotal + Height_8
  FLWRTotal + Height_8 ~ B_E + E_L + B_L
  FLWRTotal ~ Height_8
  '

model5.fit = sem(model5, data = data_scaled)
lavResiduals(model5.fit)
summary(model5.fit, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

lavaanPlot(name = "model5", model = model5.fit, coefs = TRUE)


## Model 6

model6 = '
  SEEDTotal ~ FLWRTotal + Height_8 + ELEL
  FLWRTotal + Height_8 ~ ELEL
  FLWRTotal ~ Height_8
  '

model6.fit = sem(model6, data = data_scaled)
lavResiduals(model6.fit)
summary(model6.fit, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

lavaanPlot(name = "model6", model = model6.fit, coefs = TRUE)

## Model 7

model7 = '
  SEEDTotal ~ FLWRTotal + Height_8 + BRTE
  FLWRTotal + Height_8 ~ BRTE
  FLWRTotal ~ Height_8
  '

model7.fit = sem(model7, data = data_scaled)
lavResiduals(model7.fit)
summary(model7.fit, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

lavaanPlot(name = "model7", model = model7.fit, coefs = TRUE)

## Model 8 

model7 = '
  
  SEEDTotal ~ FLWRTotal + Height_8
  FLWRTotal + Height_8 ~  BRTE
  FLWRTotal ~ Height_8
  '

model7.fit = sem(model7, data = data_scaled)
lavResiduals(model7.fit)
summary(model7.fit, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

lavaanPlot(name = "model7", model = model7.fit, coefs = TRUE)
