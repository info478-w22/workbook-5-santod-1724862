# Workbook 6: analyze NHANES data

# Set up
library(survey)
library(Hmisc)
library(tidyverse)

alco <- sasxport.get('Data/ALQ_I.XPT')

demo <- sasxport.get('Data/DEMO_I.XPT')

nhanes <- merge(x = demo, y = alco, by = 'seqn', all = TRUE)

# Represents overall US population (sum of sample weights = Represents total population of sample)
wt_sum <- sum(nhanes$wtint2yr, na.rm = TRUE)

# Analysis

# Greater survey weights mean that the survey is more likely to be representative of given population

# in ALQ151, we want 2 to be 0 and to ignore any 7 or 9

nhanes$alq151[nhanes$alq151 == 2] <- 0
nhanes$alq151[nhanes$alq151 == 7] <- NA
nhanes$alq151[nhanes$alq151 == 9] <- NA

# Create survey design 
nhanes_survey <- svydesign(
  id = ~sdmvpsu,
  nest = TRUE,
  strata = ~sdmvstra,
  weights = ~wtint2yr,
  data = nhanes
)

# About 17% of the population drink 4/5 more drinks per day
nhanes_mean <- svymean(~alq151, nhanes_survey, na.rm = TRUE)

# Accounts for Gender(1 = Male, 2 = Female) and shows Standard Error (se)
# 24% of men have 4/5 drinks per day vs 10% of women
nhanes_mean_by_gen <- svyby(~alq151, ~riagendr, nhanes_survey, svymean, na.rm = TRUE)
