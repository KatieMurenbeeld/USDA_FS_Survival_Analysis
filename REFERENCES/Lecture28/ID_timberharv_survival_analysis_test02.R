### Testing Survival Anlysis on Timber Harvests in Idaho National Forests

library(survminer)
library(survival)
library(lubridate)
library(dplyr)
library(tidyverse)


## Set working directory:
setwd("/Users/kathrynmurenbeeld//Desktop/Survival_Analysis/")

## load data:
th_dat <- read.csv("ID_TH_20200624_survana_v03.csv")
pals_dat <- read.csv("/Users/kathrynmurenbeeld//Desktop/Survival_Analysis/USFS_PALS_MYTR_adjusted_3-2019.csv")
pals_reg01 <- filter(pals_dat, REGION_ID==1)

th_dat$Completed <- as.numeric(th_dat$Completed)
th_dat$TIME_LAG <- as.numeric(th_dat$TIME_LAG)

head(th_dat)
#head(pals_reg01)

### TIME_LAG: Difference in days between SerDatesPlan (date activity was planned to be completed) and 
### SerDateComp (date activity was actually completed).

#### Conduct a survival analysis and plot survival curves:

## use the packages "survival" and "survminer" and the function survfit to create a survival curve

survcurveTH <- survfit(Surv(TIME_LAG, Completed) ~ 1, data=th_dat)
survcurveNEPA <- survfit(Surv(TIME_LAG, NEPA) ~ 1, data=th_dat)

survcurvREG01 <- survfit(Surv(ELAPSED.DAYS, APPEALED.OR.OBJECTED.) ~ 1, data = pals_reg01)

survcurvREG01_2 <- survfit(Surv(ELAPSED.DAYS, PROJECT.STATUS) ~ 1, data = pals_reg01)

survcurveTH ## data from the survival curve:

survcurvREG01
survcurvREG01_2

.line = c('hv')
## Be sure to make your plot panel a bit bigger to accomodate this plot:
ggsurvplot(survcurveTH, conf.int = TRUE, risk.table = TRUE, 
           ylab="Probability of not being completed", 
           surv.median.line = c('hv'))

ggsurvplot(survcurveNEPA, conf.int = TRUE, risk.table = TRUE, 
           ylab="Probability of not being completed", 
           surv.median.line = c('hv'))

ggsurvplot(survcurvREG01, conf.int = TRUE, risk.table = TRUE, 
           ylab="Probability of not being appealed or objected?",
           xlab="Time, elapsed days",
           surv.median.line = c('hv'))

ggsurvplot(survcurvREG01_2, conf.int = TRUE, risk.table = TRUE, 
           ylab="Probability of...",
           xlab="Time, elapsed days",
           surv.median.line = c('hv'))

### Use an exponential regression to assess differences in adoption between puppies/kittens and adults. Survreg is a function from the "survival" package.
modelREG01 <- survreg(Surv(ELAPSED.DAYS, APPEALED.OR.OBJECTED.) ~ TM.Forest.products...purpose,
                      dist = "gaussian", data=pals_reg01)

modelREG01_2 <- survreg(Surv(ELAPSED.DAYS, PROJECT.STATUS) ~ TM.Forest.products...purpose,
                      dist = "exponential", data=pals_reg01)

summary(modelREG01)
exp(coef(modelREG01))

## This will generate a survival curve from our data for the comparison we just conducted above, 
## between juvenile and adult pets.
ggsurvplot(survfit(Surv(ELAPSED.DAYS, APPEALED.OR.OBJECTED.) ~ TM.Forest.products...purpose, data=pals_reg01), conf.int = TRUE, 
           ylab="Probability of...",
           xlab="Time, elapsed days",
           surv.median.line = c('hv'))

## We can also plot our model's predictions (which will be curved, rather than stair-stepped, in contrast to the actual survival data):
plot(1, type="n", xlab="time", ylab="probability of...", xlim=c(0, 4000), ylim=c(0, 1))
lines(predict(modelREG01, newdata=list(TM.Forest.products...purpose=1),type="quantile",
              p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="blue")
lines(predict(modelREG01, newdata=list(TM.Forest.products...purpose=0),type="quantile",
              p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red")
legend(60, 1.0, legend=c("Non-Forest Product Projects", "Forest Product Projects"),
       col=c("red", "blue"), lty=1)

ggsurvplot(survfit(Surv(ELAPSED.DAYS, APPEALED.OR.OBJECTED.) ~ TM.Forest.products...purpose, data=pals_reg01), conf.int = TRUE, 
           ylab="Probability of...",
           xlab="Time, elapsed days",
           surv.median.line = c('hv'))


modelTH <-survreg(Surv(TIME_LAG, Completed) ~ NEPA, data=th_dat,
                dist='exponential')

summary(modelTH)

exp(coef(modelTH))
