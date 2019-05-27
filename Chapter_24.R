#Based of Chapter 24 of R4DS
#https://r4ds.had.co.nz/model-building.html
#requires:
#Tidyverse
#ggplot
#modelr
#nycflights13
#diamonds


#import required packages and datasets
library(tidyverse)
library(modelr)
options(na.action = na.warn) #handle missing values with a warning

library(nycflights13) #may require installing
library(ggplot2)
library(lubridate) #makes dealing with dates easier



ggplot(diamonds, aes(cut,price))+geom_boxplot()
ggplot(diamonds, aes(color,price))+geom_boxplot()
ggplot(diamonds, aes(clarity, price))+geom_boxplot()

#may need to install hexbin for this next line
ggplot(diamonds, aes(carat,price))+geom_hex(bins = 50)

#weird relationships of various variables with price
#can delve into this with modelling
#first, we clena

#clean out data of price/carat a bit
diamonds2 <- diamonds%>%filter(carat <= 2.5) %>% mutate(lprice = log2(price), lcarat = log2(carat))

#from transformations can see there is a linear relationshipt between log(carat) and log(price)
ggplot(diamonds2, aes(lcarat, lprice))+geom_hex(bins = 50)

#Make pattern explicit by fitting model
mod_diamond = lm(lprice~lcarat, data = diamonds2) #fits simple linear model (yvar = xvar)


#add predictions back to data, need to remove the log effect
grid <- diamonds2 %>% data_grid(carat = seq_range(carat,n=20))%>%mutate (lcarat = log2(carat)) %>% add_predictions(mod_diamond, "lprice")%>% mutate(price = 2^lprice)

#plotting model line to data to check fit
ggplot(diamonds2, aes(carat,price))+geom_hex(bins = 50) + geom_line(data=grid,colour = "red",size=1)

#add residuals back to data
diamonds2 <- diamonds2 %>% add_residuals(mod_diamond, "lresid")

#plot residuals
ggplot(diamonds2, aes(lcarat,lresid))+geom_hex(bins=50)


#plot residuals vs cut/color/clarity to recheck relationships
ggplot(diamonds2, aes(cut,lresid))+geom_boxplot()
ggplot(diamonds2, aes(color,lresid))+geom_boxplot()
ggplot(diamonds2, aes(clarity,lresid))+geom_boxplot()

#interpreting plots
#residual of -1 indicates that lprice was 1 unit lower than a prediction that was done based solely on weight.

#build more complicated model, including all the extra effects we just plotted with the box plots
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

#add predictions
grid <- diamonds2 %>% data_grid(cut, .model = mod_diamond2) %>% add_predictions(mod_diamond2)

ggplot(grid, aes(cut,pred))+geom_point()

diamonds2 <- diamonds2%>% add_residuals(mod_diamond2,"lresid2")

ggplot(diamonds2, aes(lcarat,lresid2))+geom_hex(bins = 50)
#see large residuals of 2+ (this indicates that the price is 4x what we expected)

#investigate these instances
diamonds2 %>% filter(abs(lresid2) > 1) %>% add_predictions(mod_diamond2) %>% mutate(pred = round(2^pred))%>% select(price,pred,carat:table,x:z)%>% arrange(price)



