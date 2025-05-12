#########################
# Pew / Tea Party analysis
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

# Load data
survey <- read_por('data/pew2014.por')


###
# 1. Create key variables

# DV - protest
survey$rally <- ifelse(complete.cases(survey$Q150) & survey$Q150 == 1, 1, 
                       ifelse(survey$PHASE == 1, 0, NA))


# IV1 - partisanship
survey$republican <- ifelse(survey$PARTY == 1, 1, 0)
survey$party <- ifelse(survey$PARTYSUM == 1, 'Republican',
                       ifelse(survey$PARTYSUM == 2, 'Democrat', 'aaIndependent'))

# IV2 - partisan animosity
survey$Q11A[survey$Q11A > 4] <- NA #Reps
survey$Q11B[survey$Q11B > 4] <- NA #Dems

survey$rep_animosity <- (survey$Q11A - 1) / 3
survey$dem_animosity <- (survey$Q11B - 1) / 3

survey$animosity <- ifelse(survey$party == 'Republican', survey$dem_animosity,
                           ifelse(survey$party == 'Democrat', survey$rep_animosity, NA))


# IV3 - strength of party ID
survey$strength_partisanship <- ifelse(complete.cases(survey$PARTYSTR) & survey$PARTYSTR < 3, -1*(survey$PARTYSTR - 4),
                                       ifelse(survey$PARTYLN < 3, 1, 0))

table(survey$strength_partisanship, survey$party)

survey$IDEO

# IV4 - views on role of government
survey$Q25A # 1 = govt wasteful and inefficient
survey$Q25C # 1 = poor people have it easy bc of govt benefits
survey$Q51KK # 1 = govt aid more harm than good
survey$Q121 # 2 = not govt responsibility for healthcare
survey$Q125 # 1 = reduce social security

survey$wasteful <- ifelse(survey$Q25A == 1, 1, 0)
survey$poor_easy <- ifelse(survey$Q25C == 1, 1, 0)
survey$aid_harm <- ifelse(survey$Q51KK == 1, 1, 0)
survey$no_health_responsibility <- ifelse(survey$Q121 == 2, 1, 0)
survey$reduce_social_security <- ifelse(survey$Q125 == 1, 1, 0)

# Create a new variable that takes the average of the recoded variables
survey$anti_govt <- rowMeans(survey[, c("wasteful", "poor_easy", "aid_harm", "no_health_responsibility", "reduce_social_security")], na.rm = TRUE)


###

# 2. Create control variables

# Ideology
survey$IDEO
survey$liberal <- ifelse(survey$IDEO == 4 | survey$IDEO == 5, 1, 0)
survey$extreme <- ifelse(survey$IDEO == 1 | survey$IDEO == 5, 1, 0)
survey$extreme_ord <- ifelse(survey$IDEO == 9, NA, abs(survey$IDEO - 3))
survey$bachelors <- ifelse(survey$EDUC >= 6 & survey$EDUC <= 8, 1, 0)
survey$age <- ifelse(survey$AGE < 98, survey$AGE, NA)
survey$age_cat <- cut(survey$age,
                      breaks = c(-Inf, 29, 49, 64, Inf),
                      labels = c(1, 2, 3, 4),
                      right = TRUE)
survey$age_cat <- as.character(survey$age_cat)
survey$married <- ifelse(survey$MARITAL == 1, 1, 0)
survey$race <- factor(survey$RACETHN)
survey$male <- ifelse(survey$SEX == 1, 1, 0)
survey$WEIGHT


###

# 3. Model

teaparty_fit <- glm(rally ~ scale(dem_animosity) + republican +
                      scale(anti_govt) + liberal + extreme + 
                      race + male + bachelors + age_cat + married, data = survey,
                    weights = WEIGHT, family = 'binomial')
summary(teaparty_fit) 

