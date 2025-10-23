### Data Analysis
### Lesson 3
library(dplyr)
library(ggplot2)

setwd("/Users/jakob/Documents/docs/teaching_data")
load("loans.Rdata")

## RQ: do black people get discriminated against when applying for loans?
# Hypothesis: Black people are more likely to get their loan denied.

## Transform data
loans = loans %>% mutate(
  deny = s7 == 3, # did the application for a loan get denied?
  pi_ratio = s46/100, # control variable: payment to income ratio
  black = s13 == 3, # equals TRUE if applicant is black.
  .keep = 'none'# Remove all other variables
)
head(loans)

## Look at the data graphically
# Pie chart
loans %>% group_by(black) %>% 
  summarise(count = n(),
            denied = sum(deny)) %>%
  ggplot(aes(x = "", y = count, fill = black)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar("y", start = 0) +
  theme_void() 

# Pie charts are confusing, better to use bars!
loans %>% group_by(black) %>% 
  summarise(count = n(),
            denied = sum(deny)) %>%
  ggplot(aes(x = "", y = count, fill = black)) +
  geom_bar(stat = 'identity', position = 'dodge')

# Percentage who got denied
loans %>% group_by(black) %>% 
  summarise(count = n(),
            denied = sum(deny)) %>% 
  mutate(percent_denied = denied/count * 100) %>%
  ggplot(aes(x = black, y = percent_denied, fill = black)) +
  geom_bar(aes(y = 100), stat = 'identity', fill = 'grey') +
  geom_bar(stat = 'identity') +
  ylim(0, 100) 


## Estimate logit model
logit1 = glm(
  deny ~ black,
  data = loans,
  family = 'binomial'
)
summary(logit1)

# install.packages("margins")
library(margins)
margins(logit1) # on average, black people are 19 %-points more likely to be denied


# Look at payment to income ratio
loans %>% group_by(black) %>%
  summarise(mean_pi = mean(pi_ratio))
# Black people are asking for higher loans relative to their income. 
# Let's control for this.

logit2 = glm(
  deny ~ black + pi_ratio,
  data = loans,
  family = 'binomial'
)
summary(logit2)
margins(logit2)
