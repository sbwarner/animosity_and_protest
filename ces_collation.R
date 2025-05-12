library(haven)
library(dplyr)

setwd('C:/Users/sethb/OneDrive/Research/data')
ces18 <- read_dta('ces/ces18.dta')
ces20 <- read_dta('ces/ces20.dta')
ces22 <- read_dta('ces/ces22.dta')


###

# 1. Collate variables

# DV. Protest
ces18$protest <- ifelse(ces18$CC18_417a_4 == 1, 1, 0)
ces20$protest <- ifelse(ces20$CC20_430a_4 == 1, 1, 0)
ces22$protest <- ifelse(ces22$CC22_430a_4 == 1, 1, 0)

# a. Strength of party ID
ces18$pid7[ces18$pid7 > 7] <- 4
ces18$partypref <- ifelse(ces18$pid7 < 4, 'Democrat',
                          ifelse(ces18$pid7 > 4, 'Republican','Independent'))
ces18$pidstrength <- abs(ces18$pid7 - 4)

ces20$pid7[ces20$pid7 > 7] <- 4
ces20$partypref <- ifelse(ces20$pid7 < 4, 'Democrat',
                        ifelse(ces20$pid7 > 4, 'Republican','Independent'))
ces20$pidstrength <- abs(ces20$pid7 - 4)

ces22$pid7[ces22$pid7 > 7] <- 4
ces22$partypref <- ifelse(ces22$pid7 < 4, 'Democrat',
                          ifelse(ces22$pid7 > 4, 'Republican','Independent'))
ces22$pidstrength <- abs(ces22$pid7 - 4)


# b. Age
ces18$age <- 2018 - ces18$birthyr
ces18$age_cat <- ifelse(ces18$age < 35, 'Ages 18 to 34',
                        ifelse(ces18$age < 55, 'Ages 35 to 54',
                               ifelse(ces18$age < 75, 'Ages 55 to 74', 'Ages 75+')))

ces20$age <- 2020 - ces20$birthyr
ces20$age_cat <- ifelse(ces20$age < 35, 'Ages 18 to 34',
                      ifelse(ces20$age < 55, 'Ages 35 to 54',
                             ifelse(ces20$age < 75, 'Ages 55 to 74', 'Ages 75+')))

ces22$age <- 2022 - ces22$birthyr
ces22$age_cat <- ifelse(ces22$age < 35, 'Ages 18 to 34',
                        ifelse(ces22$age < 55, 'Ages 35 to 54',
                               ifelse(ces22$age < 75, 'Ages 55 to 74', 'Ages 75+')))


# c. Children under 18
ces18$children <- ifelse(ces18$child18 == 1, 1, 0)
ces20$children <- ifelse(ces20$child18 == 1, 1, 0)
ces22$children <- ifelse(ces22$child18 == 1, 1, 0)


# d. Full-time employment
ces18$ft_emp <- ifelse(ces18$employ == 1, 1, 0)
ces20$ft_emp <- ifelse(ces20$employ == 1, 1, 0)
ces22$ft_emp <- ifelse(ces22$employ == 1, 1, 0)

# e. Married
ces18$married <- ifelse(ces18$marstat == 1, 1, 0)
ces20$married <- ifelse(ces20$marstat == 1, 1, 0)
ces22$married <- ifelse(ces22$marstat == 1, 1, 0)

# f. 4-year degree
ces18$bach <- ifelse(ces18$educ >= 5, 1, 0)
ces20$bach <- ifelse(ces20$educ >= 5, 1, 0)
ces22$bach <- ifelse(ces22$educ >= 5, 1, 0)

# g. Very interested in politics
ces18$interested <- ifelse(ces18$newsint > 5, NA, -1*(ces18$newsint - 4))
ces20$interested <- ifelse(ces20$newsint > 5, NA, -1*(ces20$newsint - 4))
ces22$interested <- ifelse(ces22$newsint > 5, NA, -1*(ces22$newsint - 4))

# i. gender
ces18$male <- ifelse(ces18$gender == 1, 1, 0)
ces20$male <- ifelse(ces20$gender == 1, 1, 0)
ces22$male <- ifelse(ces22$gender4 == 1, 1, 0)

# j. race
ces18$race_collapse <- ifelse(ces18$race == 1, 'aaWhite',
                              ifelse(ces18$race == 2, 'Black',
                                     ifelse(ces18$race == 3, 'Hispanic',
                                            ifelse(ces18$race == 4, 'Asian', 'Other'))))

ces20$race_collapse <- ifelse(ces20$race == 1, 'aaWhite',
                            ifelse(ces20$race == 2, 'Black',
                                   ifelse(ces20$race == 3, 'Hispanic',
                                          ifelse(ces20$race == 4, 'Asian', 'Other'))))

ces22$race_collapse <- ifelse(ces22$race == 1, 'aaWhite',
                              ifelse(ces22$race == 2, 'Black',
                                     ifelse(ces22$race == 3, 'Hispanic',
                                            ifelse(ces22$race == 4, 'Asian', 'Other'))))


# k. ideology
ces18$liberal <- ifelse(ces18$ideo5 < 3, 1, 0)
ces18$extreme <- ifelse(ces18$ideo5 == 1 | ces18$ideo5 == 5, 1, 0)
ces18$extreme_ord <- ifelse(ces18$ideo5 < 6, abs(ces18$ideo5 - 3), NA)
ces20$liberal <- ifelse(ces20$ideo5 < 3, 1, 0)
ces20$extreme <- ifelse(ces20$ideo5 == 1 | ces20$ideo5 == 5, 1, 0)
ces20$extreme_ord <- ifelse(ces20$ideo5 < 6, abs(ces20$ideo5 - 3), NA)
ces22$liberal <- ifelse(ces22$ideo5 < 3, 1, 0)
ces22$extreme <- ifelse(ces22$ideo5 == 1 | ces22$ideo5 == 5, 1, 0)
ces22$extreme_ord <- ifelse(ces22$ideo5 < 6, abs(ces22$ideo5 - 3), NA)

# l. urbanity
ces18$urbanity <- ifelse(ces18$urbancity == 5, NA,
                         (-1 * (ces18$urbancity - 4))/3)
ces20$urbanity <- ifelse(ces20$urbancity == 5, NA,
                       (-1 * (ces20$urbancity - 4))/3)
ces22$urbanity <- ifelse(ces22$urbancity == 5, NA,
                         (-1 * (ces22$urbancity - 4))/3)


###

# 2. Combined datasets, find geo_info

# Add year variable to each dataset
ces18$year <- 2018
ces20$year <- 2020
ces22$year <- 2022

# Combine datasets
ces <- rbind(ces18[, c("caseid", "lookupzip", "countyfips", "commonweight", "protest", "partypref", "pidstrength", "age", "age_cat", "children", "ft_emp", "married", "bach", "interested", "male", "race_collapse", "liberal", "extreme", "extreme_ord", "urbanity", "year")],
                       ces20[, c("caseid", "lookupzip", "countyfips", "commonweight", "protest", "partypref", "pidstrength", "age", "age_cat", "children", "ft_emp", "married", "bach", "interested", "male", "race_collapse", "liberal", "extreme", "extreme_ord", "urbanity", "year")],
                       ces22[, c("caseid", "lookupzip", "countyfips", "commonweight", "protest", "partypref", "pidstrength", "age", "age_cat", "children", "ft_emp", "married", "bach", "interested", "male", "race_collapse", "liberal", "extreme", "extreme_ord", "urbanity", "year")])
rm(ces18, ces20, ces22)

# Identify town from ZIP
zips <- read.csv('zip2county.csv')

zips <- zips %>%
  arrange(desc(afact)) %>%
  distinct(zcta5, .keep_all = TRUE)
zips$zcta5 <- sprintf("%05d", zips$zcta5)

ces <- merge(ces, zips[,c('zcta5','zipname')], by.x = 'lookupzip', by.y = 'zcta5', all.x = T)
rm(temp, zips)

# Save collated version
save.image('ces_collated.RData')
