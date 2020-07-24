lambda <- 7.75

# What is the probability that x (the Poisson random variable) takes the values of 
# e.g. k = 5
k <- 5

exp(-lambda) * lambda ^ k / factorial(k)

# using the built in Poisson mass function in R
dpois(k, lambda)
ppois(k, lambda)
qpois(0.95, lambda)
rpois(10, lambda)


# Doctor visits -----------------------------------------------------------

library(tidyverse)
doc_df <- read_csv('data/DoctorAUS.csv') %>% 
  mutate(gender = ifelse(sex == 1, 'female', 'male'))

# Poisson regression where we model number of visits to
# doctor as a function of gender
M <- glm(doctorco ~ gender,
         data = doc_df,
         family = poisson)

summary(M)

coefs <- coefficients(M)

# log of the rate for females
coefs[1] + coefs[2] * 0

# rate or average for females
exp(coefs[1] + coefs[2] * 0)

# log of the rate for males 
coefs[1] + coefs[2] * 1

# rate or average for males
exp(coefs[1] + coefs[2] * 1)

# using predict/add_predictions
library(modelr)

doc_df_new <- tibble(gender = c('female', 'male'))

doc_df_new %>% 
  add_predictions(M, type = 'response')


# Model comparison --------------------------------------------------------

M_1 <- glm(doctorco ~ gender + insurance,
           data = doc_df,
           family = poisson)

deviance(M) - deviance(M_1)

anova(M, M_1, test = 'Chisq')


# Exposure terms ----------------------------------------------------------

insur_df <- read_csv('data/Insurance.csv') %>%
  mutate(District = factor(District))

M_ins <- glm(Claims ~ District + Group + Age + offset(log(Holders)),
             data = insur_df,
             family = poisson)

insur_df <- read_csv('data/Insurance.csv') %>%
  mutate(District = factor(District))

M_ins <- glm(Claims ~ Age + offset(log(Holders)),
             data = insur_df,
             family = poisson)

tibble(Age = insur_df$Age %>% unique(), Holders = 1) %>% 
  add_predictions(M_ins, type = 'response')



# Binomial distribution ---------------------------------------------------

# Simulate n flips of a coin with "bias" theta, where bias is probability
# of coming up Heads.

theta <- 0.6
n <- 50

replicate(1000,
          rbernoulli(n, p = theta) %>% sum()
) %>% hist()


# Neg binomial distribution -----------------------------------------------

biochemist_df <- read_csv('data/biochemist.csv')

biochemist_df %>% group_by(publications) %>% tally() %>% 
  ggplot(aes(x = publications, y = n)) + geom_col()

biochemist_df %>% 
  ggplot(aes(x = publications)) + geom_bar()

# model probability distribution of `publications`
# first, using a Poisson model
Mbc_p <- glm(publications ~ 1, data = biochemist_df,
             family = poisson(link = 'log'))

exp(coefficients(Mbc_p))

# use the quasipoisson
Mbc_qp <- glm(publications ~ 1, data = biochemist_df,
              family = quasipoisson(link = 'log'))

# using a neg binom
library(MASS)
Mbc_nb <- glm.nb(publications ~ 1, data = biochemist_df)

# Using neg bin regression and predictors

Mbc_nb_1 <- glm.nb(publications ~ gender, data = biochemist_df)

tibble(gender = c('Men', 'Women')) %>% 
  add_predictions(Mbc_nb_1, type = 'response')

# e to the power of the gender coefficient
# is the factor by which the average number of pubs 
# changes as we go from Men to Women
coefficients(Mbc_nb_1)[2] %>% exp()

# Look at role of `married`

Mbc_nb_2 <- glm.nb(publications ~ married, data = biochemist_df)

tibble(married = c('Married', 'Single')) %>% 
  add_predictions(Mbc_nb_2, type = 'response')

# e to the power of the married coefficient
# is the factor by which the average number of pubs 
# changes as we go from Married to Single
coefficients(Mbc_nb_2)[2] %>% exp()

# Look at role of `prestige`
Mbc_nb_3 <- glm.nb(publications ~ prestige, data = biochemist_df)

tibble(prestige = seq(0, 5, by = 0.01)) %>% 
  add_predictions(Mbc_nb_3, type = 'response') %>% 
  ggplot(aes(x = prestige, y = pred)) + geom_line()

# e to the power of the prestige coefficient
# is the factor by which the average number of pubs 
# changes for a unit increase in prestige
coefficients(Mbc_nb_3)[2] %>% exp()


# Using all predictors
Mbc_nb_4 <- glm.nb(publications ~ gender + married + I(children > 0) + prestige + mentor,
                   data = biochemist_df)

summary(Mbc_nb_4)

# What is the role of `mentor` holding all other variables constant
coefficients(Mbc_nb_4)['mentor'] %>% exp()

tibble(mentor = seq(0, 50),
       gender = "Women",
       married = "Single",
       children = 0,
       prestige = 2.5) %>% 
  add_predictions(Mbc_nb_4, type = 'response') %>% 
  ggplot(aes(x = mentor, y = pred)) + geom_line()


# Model comparison to Mbc_nb_4
Mbc_nb_5 <- glm.nb(publications ~ gender + I(children > 0) + mentor,
                   data = biochemist_df)

anova(Mbc_nb_5, Mbc_nb_4, test = 'Chisq')



# Binomial logistic -------------------------------------------------------

library(tidyverse)

golf_df <- read_csv('data/golf_putts.csv')
golf_df <- golf_df %>% mutate(failure = attempts - success)


Mb <- glm(cbind(success, failure) ~ distance,
          data = golf_df,
          family = binomial(link = "logit"))
          
# log odds of a success is a linear function of distance
tibble(distance = seq(0, 20)) %>% 
  add_predictions(Mb, type = 'response') %>% 
  ggplot(aes(x = distance, y = pred)) + geom_line()

# Zero inflated Poisson ---------------------------------------------------

smoking_df <- read_csv('data/smoking.csv')  

library(pscl)

Mpois <- glm(cigs ~ educ, data = smoking_df, family = poisson)
coefficients(Mpois)[2] %>% exp()

Mzip <- zeroinfl(cigs ~ educ, data = smoking_df)

tibble()
educ <-  seq(5, 20)

# coefficients for the logistic regression in the ZIP
coefs_logistic <- coefficients(Mzip)[c(3,4)]

ilogit <- function(x) 1/(1 + exp(-x))
# predicted log odds of being a non-smoker, as a function of education 
coefs_logistic[1] + coefs_logistic[2] * educ
# predicted probability of being a non-smoker, as a function of education
ilogit(coefs_logistic[1] + coefs_logistic[2] * educ)

tibble(educ = educ) %>% 
  add_predictions(Mzip, type = 'zero')

# predicted probability of being a smoker as a function of education
tibble(educ = educ) %>% 
  add_predictions(Mzip, type = 'zero') %>% 
  mutate(prob_smoker = 1 - pred)


# coefficients for the Poisson regression in the ZIP
coefs_pois <- coefficients(Mzip)[c(1, 2)]

# predicted log of the average number of cigs as a function of education
coefs_pois[1] + coefs_pois[2] * educ

# predicted average number of cigs as a function of education
exp(coefs_pois[1] + coefs_pois[2] * educ)

# using add_predictions
tibble(educ = educ) %>% 
  add_predictions(Mzip, type = 'count')


# Predicted avg number of cigs smoked as a function of educ
tibble(educ = educ) %>% 
  add_predictions(Mzip, type = 'response')

tibble(educ = educ) %>% 
  add_predictions(Mpois, type = 'response')


# using the affairs
affairs_df <- read_csv('data/affairs.csv')

Mzip_affairs <- zeroinfl(affairs ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating,
                         data = affairs_df)

summary(Mzip_affairs)
