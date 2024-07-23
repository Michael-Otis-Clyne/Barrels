#################################
#  Sage Babish analyses for Otis # -----
#           04/xx/24              #
#################################

###################
#  Libraries      # -----
##################
library(tidyverse)
library(ggplot2)
library(asbio)
library(vegan)
library(lme4)
library(DHARMa)

#########################
#  Background Info      # -----
#########################
# Objective of My Analyses: Examine the responses of plants to different 
# treatments and examine how these responses differ across life histories and 
# natives vs. exotics; responses measured by growth, survivorship, seed production,
# and flower production. Incl. data exploration, ANOVA and similar analyses, 
# and survivorship GLMs

# Relevant Assumptions Made: ANOVA assumptions (normality, etc.) despite knowing
# the data isn't normally distributed. Also just the assumption that seeing 
# someone else's attempts at GLMs hat didn't work will be at least a little helpful


#############################
#  Code/Analysis Script     # -----
#############################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#set working directory

df <- read.csv("barrel_data_all.csv")

#1: Data Set-Up ----
#need to change some of the cols to factors, others to dummy vars, and remove extra
data <- df %>% 
  select(c(BARREL, SPECIES, LH, NvI, PLANT.ID, HT_1, Height_8, AorD_8, AorD_9, 
           FLWRTotal, SEEDTotal)) %>% 
  mutate( 
    SURV = ifelse(AorD_9 == "A", 1, 0),
    SURV2 = ifelse(AorD_8 == "A", 1, 0), #a second surv measure bc so few made it to 9
    BARREL = as.factor(BARREL),
    SPECIES = as.factor(SPECIES),
    LH = as.factor(LH),
    NvI = as.factor(NvI),
    HT_1 = as.numeric(HT_1),
    Height_8 = as.numeric(Height_8),
    deltaH = Height_8 - HT_1) %>% 
  mutate( #independent dummy vars for each LH type
    LH_F = ifelse(LH == "FAST", 1, 0),
    LH_M = ifelse(LH == "MEDIUM", 1, 0),
    LH_S = ifelse(LH == "SLOW", 1, 0)
  ) %>% 
  filter(!is.na(SPECIES)) #drop empty rows in there somehow
#there are errors from the NAs in the columns we're making numeric but otherwise
# it runs okay and the output looks right:
summary(data)

#read in df for coding treatment levels in barrels:
barrelkey <- read.csv("barrel_key.csv")
#remove trailing part about if control or not; irrelevant bc this is 1 year only
barrelkey$Trt <- gsub("_[^_]+$","", barrelkey$Trt)
head(barrelkey)
#looks like it worked well; now to add life history key (manually)

unique(barrelkey$Trt)
#B = cheatgrass = F
#A = sagebrush = S
#L = whitedaisy = F
#E = bottlebrush = M

barrelkey <- barrelkey %>% 
  mutate(
    LH_combo = ifelse(Trt == "LE" | Trt == "BE", "FM", 
                      ifelse(Trt == "BL", "FF",
                             ifelse(Trt == "AL" | Trt == "BA", "FS", NA)))
  )

#now add treatment and LH combo info to the main dataframe
#make new columns:
data$Trt = NA
data$LH_combo = NA
#fill in based on info in barrel key:
for (i in 1:nrow(data)) {
  data$Trt[i] = barrelkey$Trt[barrelkey$BARREL == data$BARREL[i]]
  data$LH_combo[i] = barrelkey$LH_combo[barrelkey$BARREL == data$BARREL[i]]
}
head(data)
#looks like it filled in properly

#2: Data Exploration -----
#Before the MANOVA and GLMs, I wanted to do some data exploration comparing
# plants from different LHs, treatments, and LH combos to see which of those 
# may be relevant for future analyses
#(largely because I'm not sure if each species should be treated separately for
# the ANOVA given that ones w different LHs and NvI might react differently, so
# we might not want to analyze growth based solely on treatment)

data %>% 
  group_by(SPECIES, LH_combo) %>% 
  summarize(
    n = length(SPECIES),
    nsurv = sum(SURV == "1", na.rm = T),
    nsurv2 = sum(SURV2 == "1", na.rm = T),
    meanFLWR = mean(FLWRTotal, na.rm = T),
    sdFLWR = sd(FLWRTotal, na.rm = T),
    meanSEED = mean(SEEDTotal, na.rm = T),
    sdSEED = sd(SEEDTotal, na.rm = T),
    meandH = mean(deltaH, na.rm = T),
    sddH = sd(deltaH, na.rm = T)
  )
#no sagebrush at all (which i now remember being said during the presentation)
#didn't realize until now how few individuals survived to date 9 (or that they
# were all the same species and almost all in the same treatment)
#that will probably end up making the survivorship models strange
#(to counteract this i'm also making models for time 8 survivorship)

#some plots to visualize these differences a little more intuitively:
#make long version of data:
data_long <- data %>% 
  select(SPECIES, LH_combo, Trt, HT_1, Height_8, FLWRTotal:deltaH) %>% 
  pivot_longer(cols = HT_1:deltaH, 
               names_to = "variable", values_to = "value")
head(data_long)
#coded by species:
sp_boxes <- ggplot(data_long, aes(SPECIES, value, fill = SPECIES)) +
  geom_boxplot(outlier.shape = NA, na.rm = T) +
  facet_wrap (. ~ variable, scales = 'free', shrink = T) +
  xlab('') +
  ylab('')
sp_boxes
#obviously survivorship very different; seems like they all grew about the same
# before largely dying off; LAGL definitely produced more flowers and seeds by 
# a good margin but I'd bet that's a product of the fast LH

#coded by LH treatment:
LH_boxes <- ggplot(data_long, aes(LH_combo, value, fill = LH_combo)) +
  geom_boxplot(outlier.shape = NA, na.rm = T) +
  facet_wrap (. ~ variable, scales = 'free', shrink = T) +
  xlab('') +
  ylab('')
LH_boxes
#pretty much everything appears equivalent across combinations except survival;
# all survival in fast-medium combos (and we know from above it's all ELEL)

#coded by specific species combo:
trt_boxes <- ggplot(data_long, aes(Trt, value, fill = Trt)) +
  geom_boxplot(outlier.shape = NA, na.rm = T) +
  facet_wrap (. ~ variable, scales = 'free', shrink = T) +
  xlab('') +
  ylab('')
trt_boxes
#more ELEL seems to have survived when paired with LAGL compared to BRTE
#most seeds and flowers from AL pairing, which makes sense because LAGL wasn't 
# competing with any other plants in that treatment (though height also smaller
# in that treatment)

#3: ANOVA/Kruskal-Wallis/Pairwise Wilcox -----
#before doing any analyses, want to check for normality and colinearity:
#pull out response variables for analysis (based on which have least NAs):
compdat <- data[complete.cases(data[,c(7,10,11)]),]
#dropping obs that are all 0 (will cause Bray Curtis errors)
compdat <- compdat[rowSums(compdat[,c(7,10,11)]) !=0, ]
#pull out ht8, flwrtotal, seed total
vars <- (as.matrix(compdat[,c(7,10,11)])) 

#test for multinormality:
DH.test(compdat[,c(7,10,11)])
#don't meet assumptions of multinormality
hist(compdat[,7]) #this one looks okay
hist(compdat[,10]) #zero-inflated
hist(compdat[,11]) #also zero-inflated

#I attempted a PerMANOVA as the non-parametric MANOVA alternative but I can't
# get it to run; left in to show it may not be possible w this data (or at least
# the distance metric I chose)
# permanova <- adonis2(vars ~ compdat$SPECIES*compdat$Trt, 
#                      method = "bray", perm = 999)

#We already know the data violates the normality assumptions of an ANOVA, so 
# I'll do Kruskal-Wallis Tests as the non-parametric alternative
#only annoying part is it doesn't have a two-way option
#can follow each test will a pairwise wilcox test to tell us which species differ

#first, look at all three vars based on species:
kruskal.test(Height_8 ~ SPECIES, data = compdat)
pairwise.wilcox.test(compdat$Height_8, compdat$SPECIES, p.adjust.method = "BH")
#BRTE different from both ELEL and LAGL; ELEL and LAGL somewhat similar
kruskal.test(FLWRTotal ~ SPECIES, data = compdat)
pairwise.wilcox.test(compdat$FLWRTotal, compdat$SPECIES, p.adjust.method = "BH")
#all three species have very big differences in flower total
kruskal.test(SEEDTotal ~ SPECIES, data = compdat)
pairwise.wilcox.test(compdat$SEEDTotal, compdat$SPECIES, p.adjust.method = "BH")
#Again, all three have very big differences

#none of this is surprising based on the box plots but it's good to confirm

#also looking by treatment group:
kruskal.test(Height_8 ~ LH_combo, data = compdat)
pairwise.wilcox.test(compdat$Height_8, compdat$LH_combo, p.adjust.method = "BH")
#All three pretty different
kruskal.test(FLWRTotal ~ LH_combo, data = compdat)
pairwise.wilcox.test(compdat$FLWRTotal, compdat$LH_combo, p.adjust.method = "BH")
#visually they all looked the same so interesting that FM is so different
kruskal.test(SEEDTotal ~ LH_combo, data = compdat)
pairwise.wilcox.test(compdat$SEEDTotal, compdat$LH_combo, p.adjust.method = "BH")
#again, FM very different from the other two


#4: Survivorship GL(M)Ms -----
#now to make some GLMs for survivorship
#first just want to dummy variable code the LH combinations, species, NvI:
glmdat <- data %>% 
  mutate(ones = 1, 
         ones2 = 1,
         Species = SPECIES, #don't want to lost original species col
         Trt = as.factor(Trt),
         Native = ifelse(NvI == "NATIVE", 1, 0)) %>% #want dummy var for this
  pivot_wider(names_from = LH_combo, values_from = ones, values_fill = 0) %>% 
  pivot_wider(names_from = Species, values_from = ones2, values_fill = 0)
summary(glmdat)
#just noticed that according to the deltaH column some of the plants shrunk?

#will do each model for both survivorship metrics (SURV to d9 and SURV2 to d8)
#model one (just species and LH combo):
glm1 <- glm(SURV ~ ELEL + BRTE + LAGL + FF + FM + FS, data = glmdat,
            family = "binomial"(link = "logit"))
summary(glm1)
#error may just be because some species never survived; will compare to surv2
glm1.5 <- glm(SURV2 ~ ELEL + BRTE + LAGL + FF + FM + FS, data = glmdat,
            family = "binomial"(link = "logit"))
summary(glm1.5)
#didn't get same warnings but this time had issues defining 2 of the coefs; should
# inspect correlation between variables (excluding SURV and LH_S):
cor(glmdat[,c(12,15,16,19:25)])
#lots of the cor between species and LH combos, which makes sense
#will see if species as factor vs dummy variable helps:
glm1.6 <- glm(SURV2 ~ SPECIES + FF + FM + FS, data = glmdat,
            family = "binomial"(link = "logit"))
summary(glm1.6)
#still the same singularity issue; suppose all the LH combos being mutually
# exclusive might be causing issues

#will persist and compare different models anyway because I'm not sure what the 
# best way to handle this is

#model two (LH combo and NvI):
#was going to also include species in this but realized it may be too correlated
glm2 <- glm(SURV ~ FF + FM + FS + Native, data = glmdat,
            family = "binomial"(link = "logit"))
summary(glm2)
#same fitting error and failure to resolve FS coefficient
#will try the same model with treatment (out of curiosity)
glm2.1 <- glm(SURV ~ Trt + Native, data = glmdat,
              family = "binomial"(link = "logit"))
summary(glm2.1)
#that helps! going to quickly re-run model one with that adaptation
glm1.1 <- glm(SURV ~ ELEL + BRTE + LAGL + Trt, data = glmdat,
              family = "binomial"(link = "logit"))
summary(glm1.1)
#this did not work/converge/look good at all

#also going to model with surv2 if only because it's more interesting
glm2.5 <- glm(SURV2 ~ Trt + Native, data = glmdat,
              family = "binomial"(link = "logit"))
summary(glm2.5)
exp(coef(glm2.5)) #pull out effect sizes
#natives survived much better, which is interesting
#and natives inc odds of survival by 3.838

#model three (species, Trt, and species*Trt interaction):
glm3 <- glm(SURV2 ~ Trt*SPECIES, data = glmdat,
            family = "binomial"(link = "logit"))
summary(glm3)
#so many problems with the interaction term; not a good model

#model four (species, LH combo/Trt, NvI):
glm4 <- glm(SURV2 ~ ELEL + BRTE + LAGL + FF + FM + FS + Native, data = glmdat,
              family = "binomial"(link = "logit"))
summary(glm4)
#also lots of problems; see if the Trt-LH combo swap helps again:
glm4.1 <- glm(SURV2 ~ ELEL + BRTE + LAGL + Trt + Native, data = glmdat,
            family = "binomial"(link = "logit"))
summary(glm4.1)
#Native term probably not a good predictor when you have species in there as well

#model five (species, LH combo/Trt, and barrel random effect):
#will also try a GLMM with a random effect for different barrels:
#first random intercept:
glmm_int <- glmer(SURV2 ~ ELEL + BRTE + LAGL + FF + FM + FS + (1 | BARREL),
                  data = glmdat, family = "binomial")
summary(glmm_int)
#model chose to drop LAGL and FS because of rank deficiency

#see what happens using Trt instead of LH combo:
glmm_int_2 <- glmer(SURV2 ~ ELEL + BRTE + LAGL + Trt + (1 | BARREL),
                  data = glmdat, family = "binomial")
summary(glmm_int_2)
#this one worked even worse than the LH combo model

#might also try coding individual lh strategy (don't know why I didn't sooner):
glmm_int_3 <- glmer(SURV2 ~ LH_F + LH_M + LH_S + Native + (1 | BARREL),
                    data = glmdat, family = "binomial")
summary(glmm_int_3)
#model dropped both LH_M and LH_S
#natives continue to have higher survival estimates under different models 
# (which is what you'd want to see)

#will also try random slope:
#not sure which of the above models to build off of bc they all have issues; will
# look at them both with DHARMa diagnostics:
testResiduals(simulateResiduals(glmm_int))
testResiduals(simulateResiduals(glmm_int_3))
#both look very good from a diagnostics stance, which makes sense because what
# other distribution would you really want for 0/1 survival??

#will try out random slopes with both models
glmm_slope <- glmer(SURV2 ~ ELEL + BRTE + LAGL + FF + FM + FS + (ELEL | BARREL),
                data = glmdat, family = "binomial")
summary(glmm_slope)
#we get the same rank deficiency issues as before
glmm_slope_2 <- glmer(SURV2 ~ LH_F + LH_M + LH_S + Native + (Native | BARREL),
                    data = glmdat, family = "binomial")
summary(glmm_slope_2)
#rank deficiency again

#think I've exhausted my (limited) knowledge/ideas of how to deal with data that's
# this inherently correlated, and I lack the knowledge of the system to know which
# vars are most useful to focus on, so I'll leave it at that as a record of what
# doesn't really work (sorry I couldn't be more helpful)

######################
#  Interpretation    # ----
#####################
#See Word doc in folder for interpretation
