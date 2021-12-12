pacman::p_load(
  "ggplot2",
  "knitr",
  "arm",
  "rstanarm",
  "foreign",
  "bayesplot",
  "glmx",
  "reshape2",
  "VGAM",
  "dplyr",
  "magrittr",
  "tidyverse",
  "AER",
  "nnet",
  "lme4"
)

## Data Cleaning
heart<- read.csv("heart.csv")
# set binary outcomes
heart$Sex <- ifelse(heart$Sex=="M", 1, 0)
heart$ExerciseAngina<- ifelse(heart$ExerciseAngina=="Y", 1, 0)

heart$Age <- as.factor(heart$Age)
heart$Sex<- as.factor(heart$Sex)
heart$ChestPainType <- as.factor(heart$ChestPainType)
heart$RestingBP <- as.factor(heart$RestingBP)
heart$Cholesterol <- as.factor(heart$Cholesterol)
heart$FastingBS <- as.factor(heart$FastingBS)
heart$RestingECG <- as.factor(heart$RestingECG)
heart$MaxHR <- as.factor(heart$MaxHR)
heart$ExerciseAngina <-as.factor(heart$ExerciseAngina)
heart$Oldpeak<- as.factor(heart$Oldpeak)
heart$ST_Slope <- as.factor(heart$ST_Slope)
heart$HeartDisease <- as.factor(heart$HeartDisease)

fit1 <- stan_glm(HeartDisease ~ Sex+ RestingBP + Cholesterol + FastingBS+ MaxHR + Oldpeak + ExerciseAngina,data=heart, refresh=0)
print(fit1)
plot(fit1)


fit2 <- lm(HeartDisease ~ HeartDisease ~ Age + ChestPainType + Sex+ RestingBP + 
             Cholesterol + FastingBS+ MaxHR + Oldpeak +ST_Slope + ExerciseAngina,data=heart)
print(fit2, 3)
summary(fit2, 3)
plot(fit2)

# poisson regression
fit3 <- stan_glm(HeartDisease ~ Age + ChestPainType + Sex+ RestingBP + 
                   Cholesterol + FastingBS+ MaxHR +ST_Slope + ExerciseAngina,data=heart, refresh=0, family=poisson(link="log"))
print(fit3)

post.fit3 = posterior_predict(fit3)
pp_check(fit3)
#saw that number of zeros are same as predicted, so there is no zero inflation.
plot(fitted(fit3,resid(fit3),pch=40)) 
curve(sqrt(x),add=T)
ppc_error_scatter_avg(y=heart$HeartDisease,yrep=post.fit3)
dispersiontest(fit3)
# result shows the model is not overdispersion but underdispersion

# binomial 
fit6 <- glm(HeartDisease ~ as.numeric(Age) + ChestPainType + Sex+ as.numeric(RestingBP) + 
                   as.numeric(Cholesterol) + FastingBS+ as.numeric(MaxHR) +ST_Slope + ExerciseAngina,data=heart, family="binomial")
print(fit6)
par(mfrow=c(2,2))
plot(fit6)


# multinomial
fit4 <- multinom(data =heart, HeartDisease ~ as.numeric(Age) + ChestPainType + Sex+ as.numeric(RestingBP) + 
                   as.numeric(Cholesterol) + FastingBS+ as.numeric(MaxHR) +ST_Slope + ExerciseAngina)
summary(fit4)
plot(fitted(fit4), resid(fit4), pch=20)


# multinomial logit 
fit5 <- polr(data =heart, factor(HeartDisease) ~ Sex+ ExerciseAngina+ 
               ChestPainType + factor(ST_Slope) + RestingECG)



#install.packages("MatchIt")
library(MatchIt)

match.it = matchit(data=num_heart, group ~ Age + ChestPainType+ Sex+ RestingBP + 
                     Cholesterol + FastingBS+ MaxHR + Oldpeak +ST_Slope + ExerciseAngina, method="nearest")
summary(match.it)
plot(match.it, type = "qq", interactive = FALSE, which.xs = c('Age', 'Sex', 'ChestPainType', 'RestingBP', 'Cholesterol', 'FastingBS',  'MaxHR', 'Oldpeak', 'ST_Slope'))

# glmer

fit9 <- glmer(data=heart, HeartDisease ~ Age + (1|ChestPainType)+ ChestPainType + Sex + 
                Cholesterol + FastingBS+ MaxHR + Oldpeak +ST_Slope + ExerciseAngina,family = binomial(link="logit"))

binnedplot(fit9)
## references
# https://www.ncbi.nlm.nih.gov/books/NBK459364/
# https://peopleanalytics-regression-book.org/multinomial-logistic-regression-for-nominal-category-outcomes.html
# https://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/