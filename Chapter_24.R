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


#need to fix some functions because reasons...
#first fixed function
data_grid2 <- function (data, ..., .model = NULL) {
  expanded <- tidyr::expand(data, ...)
  if (is.null(.model)) 
    return(expanded)
  needed <- setdiff(modelr:::predictor_vars(.model), names(expanded))
  typical <- tidyr::crossing(!!! lapply(data[needed], typical))
  tidyr::crossing(expanded, typical)
}


#second fixed function

seq_range <- function(x, n, by, trim = NULL, expand = NULL, pretty = FALSE) {
  if (!missing(n) && !missing(by)) {
    stop("May only specify one of `n` and `by`", call. = FALSE)
  }
  
  if (!is.null(trim)) {
    rng <- stats::quantile(x, c(trim / 2, 1 - trim / 2), na.rm = TRUE)
  } else {
    rng <- range(x, na.rm = TRUE)
  }
  
  if (!is.null(expand)) {
    rng <- rng + c(-expand / 2, expand / 2) * (rng[2] - rng[1])
  }
  
  if (missing(by)) {
    if (pretty) {
      pretty(rng, n)
    } else {
      seq(rng[1], rng[2], length.out = n)
    }
  } else {
    if (pretty) {
      rng[1] <- floor(rng[1] / by) * by
      rng[2] <- ceiling(rng[2] / by) * by
    }
    seq(rng[1], rng[2], by = by)
  }
}

#third fixed function

#add predictions back to data, need to remove the log effect
grid <- diamonds2 %>% data_grid2(carat = seq_range(carat,n=20))%>%mutate (lcarat = log2(carat)) %>% add_predictions(mod_diamond, "lprice")%>% mutate(price = 2^lprice)

