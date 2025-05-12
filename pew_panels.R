#########################
# Pew / BLM and climate analysis
#
# Last updated: Seth, 8-20-24
#########################

# libraries
library(haven)
library(scales)
library(stargazer)
library(dplyr)

# working directory
setwd("C:/Users/sbw10001/OneDrive/Research")

# Load Data
pew_w53 <- read_sav('data/Pew ATP/ATP W53.sav')
pew_w68 <- read_sav('data/Pew ATP/ATP W68.sav')
pew_w89 <- read_sav('data/Pew ATP/ATP W89.sav')


###
# 1. Create key variables

# DV - protest
pew_w68$protest_race <- ifelse(pew_w68$RACEACTIVISM_c_W68 == 1, 1, 0)
pew_w89$protest_climate <- ifelse(pew_w89$ENGACT_a_W89 == 1, 1, 0)
pew_w89$ENGACT_a_W89

# IV1 - partisanship
pew_w53$party <- ifelse(pew_w53$F_PARTYSUM_FINAL == 1, 'Republican',
                        ifelse(pew_w53$F_PARTYSUM_FINAL == 2, 'Democrat', 'aaIndependent'))
pew_w53$democrat <- ifelse(pew_w53$F_PARTY_FINAL == 2, 1, 0)

# IV2 - partisan animosity
pew_w53$THERMDEM_W53[pew_w53$THERMDEM_W53 > 100] <- NA
pew_w53$THERMREP_W53[pew_w53$THERMREP_W53 > 100] <- NA

pew_w53$animosity <- ifelse(pew_w53$party == 'Republican', 100 - pew_w53$THERMDEM_W53,
                            ifelse(pew_w53$party == 'Democrat', 100 - pew_w53$THERMREP_W53, NA))

# IV3 - strength of party ID
pew_w53$DESCREP_W53[pew_w53$DESCREP_W53 > 4] <- NA
pew_w53$DESCDEM_W53[pew_w53$DESCDEM_W53 > 4] <- NA

pew_w53$identifier <- ifelse(pew_w53$F_PARTY_FINAL < 3, 1, 0)
pew_w53$describesme <- ifelse(complete.cases(pew_w53$DESCREP_W53),
                              (pew_w53$DESCREP_W53 - 1) / 3, (pew_w53$DESCDEM_W53 - 1) / 3)

pew_w53$strength_partisanship <- (pew_w53$identifier + pew_w53$describesme) / 2


# IV4 - views on Black inequality
pew_w53$RACESURV12_W53[pew_w53$RACESURV12_W53 > 3] <- NA
pew_w53$not_enough <- ifelse(pew_w53$RACESURV12_W53 == 2, 2,
                             ifelse(pew_w53$RACESURV12_W53 == 3, 1, 0)) / 2

pew_w53$WHADVANT_W53[pew_w53$WHADVANT_W53 > 4] <- NA
pew_w53$white_bias <- -1*(pew_w53$WHADVANT_W53 - 4) / 3

pew_w53$racial_inequality <- (pew_w53$not_enough + pew_w53$white_bias) / 2


# IV5 - views on climate change
pew_w53$climate_believer <- ifelse(pew_w53$CLIM1A_W53 == 1, 1, 0)

pew_w53$NATPROBS_k_W53[pew_w53$NATPROBS_k_W53 > 4] <- NA
pew_w53$climate_serious <- -1*(pew_w53$NATPROBS_k_W53-4) / 3
pew_w53$climate_avg <- (pew_w53$climate_believer + pew_w53$climate_serious) / 2

###

# 2. Create control variables

# Ideology
pew_w53$liberal <- ifelse(pew_w53$F_IDEO > 3 & pew_w53$F_IDEO < 6, 1, 0)
pew_w53$extreme <- ifelse(pew_w53$F_IDEO == 1 | pew_w53$F_IDEO == 5, 1, 0)
pew_w53$extreme_ord <- ifelse(pew_w53$F_IDEO != 99, abs(pew_w53$F_IDEO - 3), NA)

# Covid masking
pew_w68$COVIDMASK1_W68[pew_w68$COVIDMASK1_W68 > 5] <- NA
pew_w68$mask_behavior <- ifelse(pew_w68$COVIDMASK1_W68 == 5, 1,
                                -1*(pew_w68$COVIDMASK1_W68 - 4)/3)

# Personal background
pew_w53$race <- factor(pew_w53$F_RACETHN)
pew_w53$male <- ifelse(pew_w53$F_SEX == 1, 1, 0)
pew_w53$bachelors <- ifelse(pew_w53$F_EDUCCAT == 1, 1, 0)
pew_w53$F_AGECAT[pew_w53$F_AGECAT > 4] <- NA
pew_w53$age_cat <- factor(pew_w53$F_AGECAT)
pew_w53$married <- ifelse(pew_w53$F_MARITAL == 1, 1, 0)


###

# 3. Merge and model attrition

# a. race dataset

analysis_race <- merge(pew_w53, pew_w68, by = 'QKEY', all.x = T, all.y = F)

analysis_race$reinterview <- ifelse(complete.cases(analysis_race$protest_race), 1, 0)
reinterview.fit <- glm(reinterview ~ race + male + bachelors + age_cat + married +
                         WEIGHT_W53, data = analysis_race, family = 'binomial')

analysis_race$attrition_wt <- 1 / predict(reinterview.fit, analysis_race, type = 'response')
analysis_race$attrition_wt <- analysis_race$attrition_wt / mean(analysis_race$attrition_wt, na.rm = T)

analysis_race$weight <- analysis_race$WEIGHT_W68 * analysis_race$attrition_wt
analysis_race$weight <- analysis_race$weight / mean(analysis_race$weight, na.rm = T)


# b. Climate dataset

analysis_climate <- merge(pew_w53, pew_w89, by = 'QKEY', all.x = T, all.y = F)
analysis_climate <- merge(analysis_climate, pew_w68[,c('QKEY','mask_behavior')], all.x = T, all.y = F)

analysis_climate$reinterview <- ifelse(complete.cases(analysis_climate$protest_climate) & complete.cases(analysis_climate$mask_behavior), 1, 0)
reinterview.fit <- glm(reinterview ~ race + male + bachelors + age_cat + married +
                         WEIGHT_W53, data = analysis_climate, family = 'binomial')

analysis_climate$attrition_wt <- 1 / predict(reinterview.fit, analysis_climate, type = 'response')
analysis_climate$attrition_wt <- analysis_climate$attrition_wt / mean(analysis_climate$attrition_wt, na.rm = T)

analysis_climate$weight <- analysis_climate$WEIGHT_W89 * analysis_climate$attrition_wt
analysis_climate$weight <- analysis_climate$weight / mean(analysis_climate$weight, na.rm = T)


###

# 4. Model

# a. climate

race_fit <- glm(protest_race ~ scale(-1 * THERMREP_W53) + democrat +
                  scale(racial_inequality) + liberal + extreme + mask_behavior + (FOLGOV_W53 == 1) +
                  race + male + bachelors + age_cat + married, data = analysis_race,
                weights = weight, family = 'binomial')

summary(race_fit)


# b. Climate

climate_fit <- glm(protest_climate ~ scale(-1 * THERMREP_W53) + democrat +
                     scale(climate_avg) + liberal + extreme + (FOLGOV_W53 == 1) + mask_behavior +
                     race + male + bachelors + age_cat + married, data = analysis_climate,
                   weights = weight, family = 'binomial')
summary(climate_fit) 

