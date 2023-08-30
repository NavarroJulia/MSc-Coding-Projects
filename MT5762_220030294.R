



                              ##      Final Project MT5762      ##


## Read in data below:

Bike_sharing <- readr::read_csv("~/Julias_stuff/University_stuff/St_Andrews/Semester_1/MT5762 Intro Data Analysis/Coursework/Bike_sharing.csv")


## Load in packages needed:


library(tidyverse) 
library(dplyr)
library(ggplot2)
library(pwr)



## Part 2: ##

########

# a) use filter

Bike_CountLess <- Bike_sharing %>% filter(count < 25000)

##number of observations with less than 25000 observations

N1 = nrow(Bike_CountLess)

N2 = nrow(Bike_sharing)

## proportion:

P= N1/N2 #(~ 43%)


# b) use CI for proportion (95% CI => z = 1.96)

# Confidence Interval = p  +/-  z*sqrt( p(1-p) / n)

CIupper= P + 1.96*sqrt(P*(1-P)/N2) 
CIlower= P - 1.96*sqrt(P*(1-P)/N2) 

CI = c(CIlower, CIupper)



# c)  the expected proportion of days with less than 25,000 rented bikes for each season;


Bike_Spring <- Bike_CountLess %>% filter(season == "spring")

Bike_SpringAll <- Bike_sharing %>% filter(season == "spring")

Pspring = nrow(Bike_Spring)/ nrow(Bike_SpringAll)

## ~67%


Bike_Summer <- Bike_CountLess %>% filter(season == "summer")

Bike_SummerAll <- Bike_sharing %>% filter(season == "summer")

Psummer = nrow(Bike_Summer)/ nrwo(Bike_SummerAll)

## ~2%


Bike_Fall   <- Bike_CountLess %>% filter(season == "fall")

Bike_FallAll <- Bike_sharing %>% filter(season == "fall")

Pfall = nrow(Bike_Fall)/ nrow(Bike_FallAll)

## ~19%


Bike_Winter <-Bike_CountLess  %>% filter(season == "winter")

Bike_WinterAll <- Bike_sharing %>% filter(season == "winter")

Pwinter = nrow(Bike_Winter)/ nrow(Bike_WinterAll)

## ~85%


# d) difference in prop between winter/spring?

## H0 : no difference in proportions  Pwinter == Pspring
## H1 : a difference in proportions   Pwinter =/= Pspring

## is there a sig difference between Pspring and pwinter? need probability (hypthesis testing)
## test statistic =difference-hypothesised value/standard error




## case c: (participants can be in either of the four seasons:)
p1=Pspring
p2=Pwinter
q1=1-p1
q2=1-p2

min = min(p1+p2, q1+q2)

stdError = sqrt( (min - (p1-p2)^2)/N2  )

Tstat= ((p2-p1)-0)/stdError
## ~ 1.345346 = Z

# look at probability associated with said Tstat:

2*pnorm(q=Tstat, lower.tail=FALSE)

# ~ 0.007790879 => sufficient evidence to reject H0: that the proportions differ significantly.

#########



##  Part 3   ##

########

## Test whether the expected number of rented bikes varies across seasons. Interpret and explain your results.


## Do ANOVA test:


ggplot(Bike_sharing) + 
  geom_boxplot(aes(x = season, y = count, col = season)) +
  ggtitle("Bike rent in each season")+
  xlab("Season")+
  ylab("Bikes rented per season")


# H0: proportions are the same for all seasons

# H1: at least one differs


# Fit ANOVA
bike.aov <- aov(count ~ season, data=Bike_sharing)
# Display ANOVA table
summary(bike.aov)

## Exact p-value
pvalue = pf(q=23.01, df1=3, df2=96, lower.tail=FALSE)

## there is difference between one or more seasons with each other.

## to find the differences between seasons, use the Tukey test:

## Tukeys HSD: gives differences between seasons:
TukeyHSD(bike.aov)

## explain the differences in seasons - clearly largest difference is between summer and winter.




#Furthermore, test whether there is a difference between the  ####  expected number  ##### of bikes rented on working
#days and weekends. Interpret and explain your results. 


## filter out data by weekend/weekday and find mean of bike rents per day:


# Data: Workdays:
Bike_Work <- Bike_sharing %>% filter (weekend == 0)

# Expected count of rents if day == working day
ExpworkCount <- mean(Bike_Work$count)


# Data: Weekends:
Bike_Weekend <- Bike_sharing %>% filter (weekend == 1)

# Expected count of rents if day == weekend
ExpweekendCount <- mean(Bike_Weekend$count)


## Our hypothesis:

## H0 : no difference in count between workday/weekend
## H1 : a difference in count between workday/weekend


## number of observations in each of the factor levels:

Owork = nrow(Bike_Work)
Oweekend = nrow(Bike_Weekend)

## sd of count in each factor level

Stdwork = sd(Bike_Work$count)
Stdweekend = sd(Bike_Weekend$count)

## calculate the standard error:

stdError2 = sqrt (Stdwork^2/Owork + Stdweekend^2/Oweekend )

###  Assumption is that observations are independent : population either rents 
### in the week or on weekends (Comment to self: same with Wilcoxon test?)

Tstat2= ((ExpworkCount - ExpweekendCount)-0)/stdError2

df= min(Owork-1, Oweekend-1)

## calculate the associated probability:

pt(q = Tstat2, df = df, lower.tail = FALSE)

## -> prob ~ 0.09603715

## no evidence to reject H0: that there is no difference 


########

## Power of the test:

# In addition, compute the power of the above test, assuming that the true difference is the one observed:
 
## Calculate Cohen's d:

d= (ExpweekendCount - ExpworkCount)/ sqrt( ((Stdwork)^2 + (Stdweekend)^2)/2 )

pwr.t.test(d= (ExpweekendCount - ExpworkCount)/sd(Bike_sharing$count), n= Owork + Oweekend ,power= NULL, sig.level=0.05, type="one.sample", alternative="two.sided")


## power is roughly 88%



#For the observed sample size, what effect size (i.e., difference between the expected values) would be 
#required to obtain a power of 90%? For the given effect size, what sample size would be required to 
#obtain a power of 90%? Explain the implications of your results for the target audience.



pwr.t.test(d= d, power= 0.9, sig.level=0.05, type="two.sample", alternative="two.sided")

## n ~ 234 in each sample (meaning in general trms that we require more data so that the chance of a type 2 error is 10%)


########



##   Part 4   ##

########

#Determine how the variables temperature, humidity, windspeed, season, weekend, and holiday affect the
#number of rented bikes. Interpret the estimated parameters of your model. 

lmBike <- lm(formula = count ~ temperature + humidity + windspeed + season + weekend + holiday, data = Bike_sharing)

summary(lmBike)

## comment how all variables besides temperature have a negative effect on bike count
## largest negative influencer appears to be holiday


#Estimate the expected number
#that the TfL can expect to be rented on any given day, together with 95% bounds, for:


#  a) a working day in spring with temperature 18oC, 6% humidity, and 10 km/h windspeed;


NewData1 <- data.frame(weekend = 0, season = "spring", temperature = 18, humidity = 6, windspeed = 10, holiday = 0  )


predict(lmBike, newdata = NewData1, interval = "predict", level = 0.95)

#        fit      lwr       upr
#    51670.74  38900.87   64440.6



#  b) a holiday on a summer weekend with temperature 28oC, 35% humidity, and 5 km/h windspeed;


NewData2 <- data.frame(weekend = 1, season = "summer", temperature = 28, humidity = 35, windspeed = 5, holiday = 1  )


predict(lmBike, newdata = NewData2, interval = "predict", level = 0.95)

#        fit      lwr        upr
#     40484.81   28315.75    52653.86


#  c) a working day in autumn with temperature 12oC, 90% humidity, and 35 km/h windspeed;


NewData3 <- data.frame(weekend = 0, season = "fall", temperature = 12, humidity = 90, windspeed = 35, holiday = 0  )


predict(lmBike, newdata = NewData3, interval = "predict", level = 0.95)

#       fit      lwr        upr
#      17850.35  6479.835  29220.86


#  d) a day on a winter weekend that is not a holiday with temperature -2oC, 75% humidity, and 15 km/h
#windspeed.

NewData4 <- data.frame(weekend = 1, season = "winter", temperature = -2, humidity = 75, windspeed = 15, holiday = 0  )


predict(lmBike, newdata = NewData4, interval = "confidence", level = 0.95)

#      fit      lwr      upr
#   10093.13  6220.159  13966.1

################


## Investigate Residuals:


par(mfrow=c(3,3))

plot(lmBike)

res <- resid(lmBike)

## density plot of residuals
plot(density(res))













































