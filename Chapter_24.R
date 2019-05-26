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

