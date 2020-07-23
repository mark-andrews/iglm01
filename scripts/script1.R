library(tidyverse)
library(here)

weight_df <- read_csv(here('data/weight.csv'))

weight_male_df <- weight_df %>% filter(gender == 'male')

M <- lm(weight ~ height, data = weight_male_df)

summary(M)
sigma(M)
coef(M)

coefs <- coef(M)

M_2 <- lm(weight ~ height + age, data = weight_male_df)

logit <- function(p) log(p/(1-p))

tibble(x = seq(0, 1, length.out = 1000),
       y = logit(x)) %>% 
  ggplot(aes(x = x, y = y)) + geom_line() +
  xlab('Probability') + 
  ylab('Log odds')


# Logistic regression -----------------------------------------------------
affairs_df <- read_csv("data/affairs.csv") %>% 
  mutate(cheater = affairs > 0)

Mb <- glm(cheater ~ yearsmarried,
          data = affairs_df,
          family = binomial(link = "logit"))

coefs <- coef(Mb)

# log odds of having an affairs is 
# a linear function of `yearsmarried`
coefs[1] + coefs[2] * 10

# Predictions in logistic regression --------------------------------------

affairs_df_new <- tibble(yearsmarried = c(1, 5, 10, 20, 25))

predict(Mb, newdata = affairs_df_new)

library(modelr)

affairs_df_new %>% 
  add_predictions(Mb)

add_predictions(affairs_df_new, Mb, type='response')


# Model comparison --------------------------------------------------------

M_0 <- glm(cheater ~ yearsmarried,
           data = affairs_df,
           family = binomial(link = "logit"))

M_1 <- glm(cheater ~ yearsmarried + age + gender,
           data = affairs_df,
           family = binomial(link = 'logit'))

# null hypothesis test of difference of deviances
anova(M_0, M_1, test = 'Chisq')


# Ordinal logistic regression ---------------------------------------------

library(pscl)
library(MASS)
library(modelr)

M <- polr(score ~ gre.quant, data = admit)


admit_df_new <- tibble(gre.quant = seq(300, 800, by = 100))


admit_df_new %>% 
  add_predictions(M, type = 'prob')

# mean of the logistic distribution for 
# people with gre_quant scores of 600
mu <- coef(M) * 600

# What's the probability of being below first cutpoint
# in a logistic distribution with mean mu
# We use the cumulative distribution function of the logistic distribution.
plogis(M$zeta[1], location = mu)

# What's the probability of being below second cutpoint
plogis(M$zeta[2], location = mu)

# What the probability of being *between* first and second cutpoints
plogis(M$zeta[2], location = mu) - plogis(M$zeta[1], location = mu)

# What's the probability of being below third cutpoint
plogis(M$zeta[3], location = mu) - plogis(M$zeta[2], location = mu)


# Categorical logistic ----------------------------------------------------

library(nnet)

Mc <- multinom(score ~ gre.quant, data = admit)

admit_df_new %>% 
  add_predictions(Mc, type = 'prob')
