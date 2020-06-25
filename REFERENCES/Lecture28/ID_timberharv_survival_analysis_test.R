### Testing Survival Anlysis on Timber Harvests in Idaho National Forests

library(survminer)
library(survival)
library(lubridate)
library(dplyr)

## load data:
th_dat <- read.csv("ID_TH_20200523_survana.csv")

head(th_dat)

### TIME_LAG: Difference in days between SerDatesPlan (date activity was planned to be completed) and 
### SerDateComp (date activity was actually completed).

#### Conduct a survival analysis and plot survival curves:

## use the packages "survival" and "survminer" and the function survfit to create a survival curve

survcurveTH <- survfit(Surv(TIME_LAG, Completed) ~ 1, data=th_dat)

survcurveTH ## data from the survival curve:

.line = c('hv')
## Be sure to make your plot panel a bit bigger to accomodate this plot:
ggsurvplot(survcurveTH, conf.int = TRUE, risk.table = TRUE, 
           ylab="Probability of not being completed", 
           surv.median.line = c('hv'))

### Use an exponential regression to assess differences in adoption between puppies/kittens and adults. Survreg is a function from the "survival" package.
modelTH <-survreg(Surv(TIME_LAG, Completed) ~ NEPA, data=th_dat,
                dist='exponential')

summary(modelTH)

exp(coef(modelTH))
