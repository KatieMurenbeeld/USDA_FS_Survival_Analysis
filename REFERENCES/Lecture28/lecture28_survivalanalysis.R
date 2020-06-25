#### Lecture 28: Survival Analysis

library(survminer)
library(survival)
library(lubridate)
library(dplyr)

## load data:
dat <- read.csv("adoptedpets.csv")

## This is a dataset from an animal shelter, containing information over a ~2 year period about:
## dayin: the day the animal arrived
## dayout: the day that the animal was adopted
## adopted: Whether or not the animal was adopted during the period when data was collected.
## adult: Whether the animal was a puppy or kitten less than 1 year old (0= juvenile, 1 = adult)
## dog: Whether the animal was a dog (1) or cat (0)


## Survival analysis requires data to contain: 
# 1) Information about how long the subject was observed (whether that be until the end of the study or until the event in question occurs)
## 2) whether the event did or did not occur.

# Often, however, survival data is not recorded in this way, so it requires some transformation.

head(dat)  

## Discuss: What is the current structure of the data, and what structure do we need to proceed?


## Data transformation: What are the steps below doing?
dat$dayin <- ymd(as.character(dat$dayin)) ## ymd is part of the lubridate package
dat$dayout <- ymd(as.character(dat$dayout))

str(dat)

dat$daysobserved <- dat$dayout - dat$dayin
head(dat)
str(dat$daysobserved)

#### Conduct a survival analysis and plot survival curves:

## use the packages "survival" and "survminer" and the function survfit to create a survival curve

survcurve <- survfit(Surv(daysobserved, adopted) ~ 1, data=dat)

survcurve ## data from the survival curve:

.line = c('hv')
## Be sure to make your plot panel a bit bigger to accomodate this plot:
ggsurvplot(survcurve, conf.int = TRUE, risk.table = TRUE, 
           ylab="Probability of not being adopted", 
           surv.median.line = c('hv'))

## Discuss: What is the approximate median time that it takes for an animal to be adopted?
## Why is the plot stair-stepped?
# Animals are not adopted continuously. There can be a few days between adoptions. There is a *lag between events.

### Use an exponential regression to assess differences in adoption between puppies/kittens and adults. Survreg is a function from the "survival" package.
model <-survreg(Surv(daysobserved, adopted) ~ adult, data=dat,
                 dist='exponential')

summary(model)
exp(coef(model))
## The Exponential regression above uses a log link (for the same reason a gamma regression does, to positively constrain values).

## Discuss: What does the intercept mean in this model output?
#The average time to event when the x is at 0 (baseline).


## This will generate a survival curve from our data for the comparison we just conducted above, between juvenile and adult pets.
ggsurvplot(survfit(Surv(daysobserved, adopted) ~ adult, data=dat), conf.int = TRUE, ylab="Probability of not being adopted", surv.median.line = c('hv'))


## We can also plot our model's predictions (which will be curved, rather than stair-stepped, in contrast to the actual survival data):
plot(1, type="n", xlab="time", ylab="probability of not being adopted", xlim=c(0, 100), ylim=c(0, 1))
lines(predict(model, newdata=list(adult=1),type="quantile",
              p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="blue")
lines(predict(model, newdata=list(adult=0),type="quantile",
              p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red")
legend(60, 1.0, legend=c("Juvenile", "Adult"),
       col=c("red", "blue"), lty=1)


### Discuss: Are juvenile animals significantly more likely to be adopted than adult animals?
## What is the difference in median time-to-adoption for puppies/kittens v. adult animals?
## How is the output of this analysis and the structure of the data different from other regression models we have used this semester?



## Next step:
## Are dogs more likely to be adopted than cats? Assess using an exponential regression.

modcat <-survreg(Surv(daysobserved, adopted) ~ dog, data=dat,
                dist='exponential')
exp(coef(modcat))

ggsurvplot(survfit(Surv(daysobserved, adopted) ~ dog, data=dat), conf.int = TRUE, ylab="Probability of not being adopted", surv.median.line = c('hv'))

plot(1, type="n", xlab="time", ylab="probability of not being adopted", xlim=c(0, 100), ylim=c(0, 1))
lines(predict(modcat, newdata=list(dog=1),type="quantile",
              p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="blue")
lines(predict(modcat, newdata=list(dog=0),type="quantile",
              p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red")
legend(60, 1.0, legend=c("Dog", "Cat"),
       col=c("red", "blue"), lty=1)
