

                 ##      Coursework 1 MT5763      ##


## Read in data below:


BikeSeoul <- readr::read_csv("Julias stuff/University stuff/St Andrews/Semester 1/MT5763 Software for Data Analysis/Coursework 1/BikeSeoul.csv")
BikeWashingtonDC <- readr::read_csv("~/Julias stuff/University stuff/St Andrews/Semester 1/MT5763 Software for Data Analysis/Coursework 1/BikeWashingtonDC.csv")


## Load in packages needed:

 
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)



                 ##      Data wrangling      ##


 
## Remove columns that are not needed and edit/ rename columns:

BikeS <- select(BikeSeoul, -'Visibility (10m)', -"Dew point temperature(C)",
               -"Solar Radiation (MJ/m2)", -"Rainfall(mm)", -"Snowfall (cm)", 
               -'Functioning Day')  %>% 
               filter(!is.na("Rented Bike Count")) %>% filter(`Rented Bike Count` >= 1) %>% 
               rename("Count" = "Rented Bike Count") %>% 
               rename("Temperature" = "Temperature(C)") %>% 
               rename("Humidity" = "Humidity(%)")  %>% 
               rename("WindSpeed" = "Wind speed (m/s)") %>%
               rename("Season" = Seasons)


BikeW <- select(BikeWashingtonDC, -instant, -yr, -mnth, -weekday, -workingday,
                -weathersit, -atemp, -casual, -registered) %>%
               rename("Date" = "dteday") %>%
               rename("Count" = "cnt") %>%
               rename("Hour" = "hr") %>%
                mutate(temp= temp*(39+8)-8) %>%
               rename("Temperature" = "temp") %>% 
                mutate(hum = hum * 100) %>% 
               rename("Humidity" = "hum") %>% 
                mutate(windspeed = windspeed *67*0.2777778) %>% 
               rename("WindSpeed" = "windspeed")  %>% 
               rename("Holiday" = "holiday") %>%
               rename("Season" = season)
               


## Set the following as factors and re-code:

as.factor(BikeW$Season)

BikeW$Season <- recode_factor(BikeW$Season, "1" = "Winter",
                              "2" = "Spring", "3" = "Summer", "4" = "Autumn")


## Change the order of the Season factor levels to Spring, Summer, Autumn and Winter
## use pipes (ie re-level): 

BikeW$Season <- factor(BikeW$Season, c("Spring", "Summer" ,"Autumn", "Winter"))

BikeS$Season <- factor(BikeS$Season, c("Spring", "Summer" ,"Autumn", "Winter"))


## Do the same for the Holiday/ No holiday column:


as.factor(BikeS$Holiday)

BikeS$Holiday <- recode_factor(BikeS$Holiday,  "Holiday" = "Yes",
                                             "No Holiday" = 'No' )


as.factor(BikeW$Holiday)

BikeW$Holiday <- recode_factor(BikeW$Holiday,  "1" = "Yes",
                                             "0" = 'No' )

##Re-level: Holiday to Yes / No

BikeW$Holiday <- factor(BikeW$Holiday, c("Yes", "No"))

BikeS$Holiday <- factor(BikeS$Holiday, c("Yes", "No"))





                       ##      Date - Time      ##


## Create a new column named FullDate (as a date_time object):

BikeW <-BikeW %>% 
  mutate(FullDate = make_datetime(
    year = year(Date),
    month = month(Date),
    day = day(Date),
    hour = `Hour`,
    min = 0,
    sec = 0
  ))  


## Convert Date to a date object:


BikeW <- BikeW %>% 
  mutate(Date = make_datetime(
    year = year(Date),
    month = month(Date),
    day = day(Date) )) 





## change to (Y/M/D):

BikeS <- BikeS %>% mutate(Date = ymd(BikeS$Date)) 



## Create a new column named FullDate (as a date_time object for the Seoul data set BikeS):


BikeS <-BikeS %>% 
  mutate(FullDate = make_datetime(
    year = year(Date),
    month = month(Date),
    day = day(Date),
    hour = Hour,
    min = 0,
    sec = 0
  ))  

## Convert Date to a date object (and again delete the Spare column)

BikeS <- BikeS %>% 
  mutate(Date = make_datetime(
    year = year(Date),
    month = month(Date),
    day = day(Date) )) 




                      ##      Data Visualization     ##


                            ##      Plots    ##
               

       ## How does air temperature vary over the course of a year?


## Plot for Washington DC:


ggplot(BikeW) +
  geom_line(aes(x = Date, y = Temperature, color = Temperature))+
  scale_color_gradient(low = "light blue", high = "red")+
  labs(colour = "Air temperature") +
  xlab("Date") +
  ylab("Air temperature in degree Celcius") +
  ggtitle(paste0("Air temperature in Washington for the years 2011 and 2012", paste0(rep("", 30), collapse = " ")))
  


## Plot for Seoul:


ggplot(BikeS) +
  geom_line(aes(x = Date, y = Temperature, color = Temperature))+
  scale_color_gradient(low = "light blue", high = "red")+
  labs(colour = "Air temperature") +
  xlab("Date") +
  ylab("Air temperature in degree Celcius") +
  ggtitle(paste0("Air temperature in Seoul for (mostly) 2018")) 





           ## Do seasons affect the average number of rented bikes?



ggplot(data=BikeW, mapping=aes(x = Season,y = Count, fill=Season) )+ 
     geom_boxplot()+
     xlab("Season") +
     ylab("Number of bike rents in each season in Washington DC") +
     ggtitle(paste0("Bike use per season in Washington DC", paste0(rep("", 30), collapse = " "))) 




ggplot(data=BikeS, mapping=aes(x = Season,y = Count, fill=Season) )+ 
  geom_boxplot()+
  xlab("Season") +
  ylab("Number of bike rents in each season in Seoul")+
  ggtitle(paste0("Bike use per season in Seoul", paste0(rep("", 30), collapse = " ")))




               ## Do holidays increase or decrease the demand for rented bikes?


options(scipen=999) ## avoids scientific notation on axis


ggplot(BikeW, aes(x = Holiday, y = Count, fill = Holiday)) + 
  geom_col( alpha = 0.6) +
  xlab("Holiday/ No holiday in Washington DC") +
  ylab("Count of bikes rented") +
  scale_fill_manual(values = c("#FFA000","#0097A7"))+
  ggtitle(paste0("Bike use during holiday/ non-holiday in Washington DC", paste0(rep("", 30), collapse = " "))) 



ggplot(BikeS, aes(x = Holiday, y = Count, fill = Holiday)) + 
  geom_col(alpha = 0.6) +
  xlab("Holiday/ No holiday in Seoul") +
  ylab("Count of bikes rented")+
  scale_fill_manual(values = c("#FFA000","#0097A7"))+
  ggtitle(paste0("Bike use during holiday/ non-holiday in Seoul", paste0(rep("", 30), collapse = " "))) 




## How does the time of day affect the demand for rented bikes?



ggplot(BikeW, aes(x = Hour, y = Count, color = red), alpha = 0.5) + 
  geom_smooth(aes(), colour= "dark green", se = FALSE, method = 'gam') +
  xlab("Hour of the day in Washington DC") +
  ggtitle(paste0("Bike use per hour in Washington DC")) 



ggplot(BikeS, aes(x = Hour, y = Count)) + 
  geom_smooth(aes(), colour= "dark green", se = FALSE,  method = "gam")+
  xlab("Hour of the day in Seoul")+
  ggtitle(paste0("Bike use per hour in Seoul")) 





## Next we investigate how meteorological conditions influence bike use:



#density plot:
ggplot(BikeW, aes(Temperature)) +
  geom_density(fill= "#FF8A65",alpha = 0.5)+
  xlab("Air temperature in degree Celcius") +
  ylab("Probability of bike rents in Washington DC")+
  ggtitle(paste0("Bike use by air temperature in Washington DC"))




#density plot:
ggplot(BikeW, aes(Humidity)) + geom_density(fill= "#BA68C8",alpha = 0.5)+
  xlab("Humidity in %") +
  ylab("Probability of bike rents in Washington DC")+
  ggtitle(paste0("Bike use by humidity in Washington DC")) 

 

#density plot:
ggplot(BikeW, aes(WindSpeed)) + geom_density(fill= "#4DB6AC",alpha = 0.5)+
  xlab("Wind Speed in m/s") +
  ylab("Probability of bike rents in Washington DC")+
  ggtitle(paste0("Bike use by wind speed in m/s in Washington DC")) 



# For the Seoul data set:


#density plot
ggplot(BikeS, aes(Temperature)) + geom_density(fill= "#FF8A65",alpha = 0.7)+
  xlab("Air temperature in degree Celcius") +
  ylab("Probability of bike rents in Seoul")+
  ggtitle(paste0("Bike use by air temperature in Seoul")) 


#density plot
ggplot(BikeS, aes(Humidity)) + geom_density(fill= "#BA68C8",alpha = 0.7)+
  xlab("Humidity in %") +
  ylab("Probability of bike rents in Seoul")+
  ggtitle(paste0("Bike use by humidity in Seoul")) 



#density plot
ggplot(BikeS, aes(WindSpeed)) + geom_density(fill= "#4DB6AC",alpha = 0.7)+
  xlab("Wind Speed in m/s") +
  ylab("Probability of bike rents in Seoul")+
  ggtitle(paste0("Bike use by wind speed in m/s in Seoul")) 





                     ##       Linear Model       ##


options(scipen=9)  ## Revert back to scientific notation

## Seoul Model:


LMSeoul <- lm(formula = log(Count) ~ Season + Temperature + Humidity + WindSpeed, data = BikeS)

summary(LMSeoul)

## Equation of the line:

## Nbr bikes rented = 6.73369 + 0.00360* Summer + 0.37332*Autumn + (-0.38303)*Winter +0.04927*temp 
##                   +(-0.02249)*Humidity + 0.02538*WindSpeed




## Washington DC Model:


LMWashingDC <- lm(log(Count) ~ Season +Temperature+ Humidity + WindSpeed, data = BikeW)

summary(LMWashingDC)

## Equation of the line:

## Nbr bikes rented = 4.62640 + (-0.36516)* Summer + 0.53618*Autumn + 0.10461*Winter + 0.07979*temp 
##                   +(-0.02334)*Humidity + 0.02450*WindSpeed




## Confidence Intervals for Model Parameters:

## 97% Confidence Interval:


confint(LMWashingDC, level = 0.97)


confint(LMSeoul, level = 0.97)



                               ## Predictions:

## want 90% CI:

## Create a new data.frame:

NewData <- data.frame(
  Season = "Winter", Temperature = 0 , WindSpeed = 0.5 , Humidity = 20
)



predict(LMWashingDC, newdata = NewData, interval = "confidence", level = 0.90)

predict(LMSeoul, newdata = NewData, interval = "confidence", level = 0.90)




