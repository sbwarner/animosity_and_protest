#######################
# ANES panel models
#
# Last updated: 3-6-25
########################

# libraries
library(haven)
library(scales)
library(stargazer)
library(mirt)

# working directory
setwd("C:/Users/sbw10001/OneDrive/Research")

# data
anes16 <- read_dta('data/anes/anes16.dta')
anes20 <- read_dta('data/anes/anes20.dta')

###

# 0. Cleaning

# remove all non-responses
anes16[anes16 < 0] <- NA
anes20[anes20 < 0] <- NA

###

# 1. Key variables

# a. protest

anes16$protest <- ifelse(anes16$V162018a == 1, 1, 0)
anes20$protest <- ifelse(anes20$V202025 == 1, 1, 0)

# b. Party ID and strength of partisanship

anes16$partyid <- ifelse(anes16$V161158x < 4, 'Democrat',
                         ifelse(anes16$V161158x > 4, 'Republican', 'Independent'))

anes20$partyid <- ifelse(anes20$V201231x < 4, 'Democrat',
                         ifelse(anes20$V201231x > 4, 'Republican', 'Independent'))

anes16$pid_strength <- abs(anes16$V161158x - 4) / 3
anes20$pid_strong <- abs(anes20$V201231x - 4) / 3
anes20$pid_important <- abs(anes20$V201232 - 5) / 4

anes20$pid_strength <- ifelse(anes20$partyid == 'Democrat' | anes20$partyid == 'Republican',
                              (anes20$pid_strong + anes20$pid_important) / 2, 0)

# c. Feeling thermometers / affect

# ANES 16 feeling thermometers
anes16$dem_feel <- anes16$V161095
anes16$gop_feel <- anes16$V161096

# ANES 16 partisan affect
anes16$inparty_feel <- ifelse(anes16$partyid == 'Democrat', anes16$dem_feel,
                              ifelse(anes16$partyid == 'Republican', anes16$gop_feel,
                                     ifelse(anes16$dem_feel > anes16$gop_feel, anes16$dem_feel,
                                            anes16$gop_feel)))

anes16$outparty_feel <- ifelse(anes16$partyid == 'Democrat', anes16$gop_feel,
                               ifelse(anes16$partyid == 'Republican', anes16$dem_feel,
                                      ifelse(anes16$dem_feel > anes16$gop_feel, anes16$gop_feel,
                                             anes16$dem_feel)))

anes16$aff_pol <- anes16$inparty_feel - anes16$outparty_feel

# ANES 20 feeling thermometers
anes20$dem_feel <- anes20$V201156
anes20$dem_feel[anes20$dem_feel > 100] <- NA
anes20$gop_feel <- anes20$V201157
anes20$gop_feel[anes20$gop_feel > 100] <- NA

# ANES 20 partisan affect
anes20$inparty_feel <- ifelse(anes20$partyid == 'Democrat', anes20$dem_feel,
                              ifelse(anes20$partyid == 'Republican', anes20$gop_feel,
                                     ifelse(anes20$dem_feel > anes20$gop_feel, anes20$dem_feel,
                                            anes20$gop_feel)))

anes20$outparty_feel <- ifelse(anes20$partyid == 'Democrat', anes20$gop_feel,
                               ifelse(anes20$partyid == 'Republican', anes20$dem_feel,
                                      ifelse(anes20$dem_feel > anes20$gop_feel, anes20$gop_feel,
                                             anes20$dem_feel)))

anes20$aff_pol <- anes20$inparty_feel - anes20$outparty_feel

anes20$outparty_hate <- ((-1 * anes20$outparty_feel) + 100)
anes16$outparty_hate <- ((-1 * anes16$outparty_feel) + 100)


###

# 2. Control variables

# a. Ideology
anes16$V161126
anes16$ideo <- ifelse(anes16$V161126 == 4 | anes16$V161126 == 99, 
                      ifelse(anes16$V161127 == 1, 3,
                             ifelse(anes16$V161127 == 2, 5, 4)),
                      anes16$V161126)
anes16$liberal <- ifelse(anes16$ideo < 4, 1, 0)
anes16$extreme <- ifelse(anes16$ideo == 1 | anes16$ideo == 7, 1, 0)
anes16$extreme_ord <- abs(anes16$ideo - 4)


anes20$ideo <- ifelse(anes20$V201200 == 4 | anes20$V201200 == 99, 
                      ifelse(anes20$V201201 == 1, 3,
                             ifelse(anes20$V201201 == 2, 5, 4)),
                      anes20$V201200)
anes20$liberal <- ifelse(anes20$ideo < 4, 1, 0)
anes20$extreme <- ifelse(anes20$ideo == 1 | anes20$ideo == 7, 1, 0)
anes20$extreme_ord <- abs(anes20$ideo - 4)


# b. Political trust
anes16$trustdc <- -1 * (anes16$V161215 - 5)
anes16$notcorrupt <- anes16$V161218 - 1

anes20$trustdc <- -1 * (anes20$V201233 - 5)
anes20$notcorrupt <- anes20$V201236 - 1


# c. Political discussion
anes16$pol_discussion <- ifelse(anes16$V162174 == 2, 0, anes16$V162174a)
anes20$pol_discussion <- ifelse(anes20$V202022 == 2, 0, anes20$V202023)


# d. Age group
anes16$age <- ifelse(anes16$V161267x <= 4, 'Ages 18 to 34',
                     ifelse(anes16$V161267x <= 8, 'Ages 35 to 54',
                            ifelse(anes16$V161267x <= 12, 'Ages 55 to 74',
                                   'Ages 75+')))

anes20$age <- ifelse(anes20$V201507x < 35, 'Ages 18 to 34',
                     ifelse(anes20$V201507x < 55, 'Ages 35 to 54',
                            ifelse(anes20$V201507x < 75, 'Ages 55 to 74', 'Ages 75+')))


# e. Education
anes16$bach <- ifelse(anes16$V161270 >= 13 & anes16$V161270 < 20, 1, 0)
anes20$bach <- ifelse(anes20$V201510 >= 6 & anes20$V201510 < 10, 1, 0)

# f. Gender
anes16$male <- ifelse(anes16$V161342 == 1, 1, 0)
anes20$male <- ifelse(anes20$V201600 == 1, 1, 0)

# g. Race
anes16$race <- ifelse(anes16$V161310x == 1, 'aaWhite',
                      ifelse(anes16$V161310x == 2, 'Black',
                             ifelse(anes16$V161310x == 3, 'Asian',
                                    ifelse(anes16$V161310x == 6, 'Hispanic', 'Other'))))

anes20$race <- ifelse(anes20$V201549x == 1, 'aaWhite',
                      ifelse(anes20$V201549x == 2, 'Black',
                             ifelse(anes20$V201549x == 3, 'Hispanic',
                                    ifelse(anes20$V201549x == 4, 'Asian', 'Other'))))

# h. Marital status
anes16$married <- ifelse(anes16$V161268 == 1, 1, 0)
anes20$married <- ifelse(anes20$V201508 == 1, 1, 0)


# i. Employment status
anes16$employed <- ifelse(anes16$V161275x == 10, 1, 0)
anes20$employed <- ifelse(anes20$V201533x == 10, 1, 0)


# j. Parenthood
anes16$children <- ifelse(anes16$V161324 > 0, 1, 0)
anes20$children <- ifelse(anes20$V201567 > 0, 1, 0)


# k. COVID not strict enough
anes20$covid_stricter <- ifelse(anes20$V201393 > 3, 1, 0)
anes20$covid_too_strict <- ifelse(anes20$V201393 < 3, 1, 0)

# l. News attention
anes16$news_att <- -1 * (anes16$V161003 - 5)
anes20$news_att <- -1 * (anes20$V201005 - 5)

# m. Volunteer
anes20$volunteer <- ifelse(anes20$V202033 == 1, 1, 0)
anes20$union_hh <- ifelse(anes20$V201544 == 1, 1, 0)
anes20$church_weekly <- ifelse(complete.cases(anes20$V201454), 1, 0)

# n. Racial attitudes
anes16$discrimination <- (-1*(anes16$V162357-5))/4
anes16$not_enough <- (-1*(anes16$V162213 - 5)) / 4

anes16$racial_inequality <- (anes16$discrimination + anes16$not_enough) / 2

# o. Ideology variables
ideology_vars_16 <- c(
  "V161178", "V161179", "V161180", "V161181", "V161182", "V161183", 
  "V161184", "V161185", "V161186", "V161187", "V161188", "V161189", 
  "V161190", "V161191", "V161192", "V161193", "V161194x", 
  "V161195x", "V161196", "V161196x", "V161197", "V161198", "V161199", 
  "V161200", "V161201", "V161202", "V161203", "V161204x", 
  "V161205", "V161206", "V161207", "V161208", "V161209", "V161210", 
  "V161211", "V161212", "V161213x",  "V161214x", 
  "V161215", "V161216", "V161217", "V161218", "V161219", "V161220", 
  "V161221", "V161222", "V161223", "V161224", "V161225x",  
  "V161226x",  "V161227x",  "V161228x",  
  "V161229x", "V161230", "V161231", "V161232", "V161233x")

# Replace values > 10 with NA
anes16[, ideology_vars_16] <- lapply(anes16[, ideology_vars_16], function(x) ifelse(x > 10, NA, x))

# Function to calculate percentage of min or max responses per row for each column
calculate_min_max_pct <- function(df, cols) {
  apply(df[, cols], 1, function(row) {
    # Find the min and max for each row dynamically
    min_values <- apply(df[, cols], 2, min, na.rm = TRUE)
    max_values <- apply(df[, cols], 2, max, na.rm = TRUE)
    
    # Count the number of min or max values per row
    min_max_count <- sum(row == min_values | row == max_values, na.rm = TRUE)
    total_responses <- sum(!is.na(row))
    
    if (total_responses > 0) {
      return(min_max_count / total_responses * 100)
    } else {
      return(NA)
    }
  })
}

anes16$min_max_pct <- calculate_min_max_pct(anes16, ideology_vars_16)
anes16$pct_extreme_views <- anes16$min_max_pct

# IRT scores
library(mirt)
set.seed(1234)

anes_irt_matrix <- as.matrix(anes16[,ideology_vars_16])
irt_model <- mirt(anes_irt_matrix, model = 1, itemtype = 'graded', na.action = na.exclude)
theta_scores <- fscores(irt_model)

anes16$irt_ideology <- theta_scores
anes16$irt_extremism <- abs(anes16$irt_ideology)


###

# 4. Run panel models

panel <- merge(anes16, anes20, by = 'V160001_orig')

fit.panel1 <- glm(protest.y ~ protest.x + scale(outparty_hate.x) + scale(pid_strength.x) + scale(extreme_ord.x) +
                    covid_too_strict + scale(irt_ideology) + 
                    pol_discussion.x + news_att.x + bach.x + male.x + 
                    married.x + employed.x + children.x + race.x + age.x,
                  data = panel, weights = V200011b, family = 'binomial')

fit.panel2 <- glm(protest.y ~ protest.x + scale(outparty_hate.x) + scale(pid_strength.x) + pct_extreme_views +
                    covid_too_strict + scale(irt_ideology) + 
                    pol_discussion.x + news_att.x + bach.x + male.x + 
                    married.x + employed.x + children.x + race.x + age.x,
                  data = panel, weights = V200011b, family = 'binomial')

fit.panel3 <- glm(protest.y ~ protest.x + scale(outparty_hate.x) + scale(pid_strength.x) + scale(irt_extremism) +
                    covid_too_strict + scale(irt_ideology) + 
                    pol_discussion.x + news_att.x + bach.x + male.x + 
                    married.x + employed.x + children.x + race.x + age.x,
                  data = panel, weights = V200011b, family = 'binomial')

summary(fit.panel1)
summary(fit.panel3) # Model 2 in paper
summary(fit.panel2)

# Note that high values of irt_ideology indicate conservatism; 
# I reverse the coefficient for presentation of liberalism in the paper