

#**************************************************
# ZERO-TRUNCATED POISSON REGRESSION FOR LOS on DAY OF WEEK, AGE, ETC.  
#**************************************************

library("tidyverse")
library("here") 
library("magrittr")
library("broom")
library("ggpubr")
library("VGAM")

# rm(list = ls())

# TODO: ---------

#****************


# 0) read in data: ---------------
if(!exists("df1.raw.data")){
    source(here("src",
                "2018-07-04_clean-data.R"))
}

str(df1.raw.data)
summary(df1.raw.data)


