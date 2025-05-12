##################
# CES analysis
#
####################

setwd("C:/Users/sbw10001/OneDrive/Research/Partisan Animosity and Protest/Analysis/")
load('data/ces_collated.RData')
load('data/contextual_vars.RData')

library(dplyr)
library(lmtest)
library(sandwich)
library(stargazer)

# County level analysis

# a. Merge with county
county_ests$year <- county_ests$year + 1
county_ests$countyfips <- sprintf("%05d", county_ests$county14)
ces_county <- merge(ces, county_ests, by = c('countyfips', 'year'), 
                    all.x = T, all.y = F)

# b. Create indiv-specific vars
ces_county$inparty_size <- ifelse(ces_county$partypref == 'Democrat', ces_county$pct_dem,
                                  ifelse(ces_county$partypref == 'Republican', ces_county$pct_rep, NA))

ces_county$anim_of_inparty <- ifelse(ces_county$partypref == 'Democrat', ces_county$dem_animosity,
                                     ifelse(ces_county$partypref == 'Republican', ces_county$rep_animosity, NA))

ces_county$anim_from_outparty <- ifelse(ces_county$partypref == 'Democrat', ces_county$rep_animosity,
                                        ifelse(ces_county$partypref == 'Republican', ces_county$dem_animosity, NA))

# c. Create models and cluster SEs
county_fit <- glm(protest ~ ovr_animosity + pidstrength + liberal + extreme + interested +
                    married + ft_emp + age_cat + bach + race_collapse + inparty_size + urbanity +
                    orgs_per_cap + internet + pct_bach + pct_white + pct_black + pct_hispanic +
                    factor(year)*always_mask,
                  data = ces_county, weights = commonweight, family = 'binomial')
clustered_se <- vcovHC(county_fit, type = "HC0", cluster = ~ countyfips, data = ces_county)
coeftest_result <- coeftest(county_fit, vcov. = clustered_se)
coeftest_result


county_fit_types <- glm(protest ~ anim_of_inparty + anim_from_outparty + pidstrength + liberal + extreme + interested +
                          married + ft_emp + age_cat + bach + race_collapse + inparty_size + urbanity +
                          orgs_per_cap + internet + pct_bach + pct_white + pct_black + pct_hispanic +
                          factor(year)*always_mask,
                        data = ces_county, weights = commonweight, family = 'binomial')
clustered_se_types <- vcovHC(county_fit_types, type = "HC0", cluster = ~ countyfips, data = ces_county)
coeftest_result_types <- coeftest(county_fit_types, vcov. = clustered_se_types)
coeftest_result_types

