

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


# reference: https://stats.idre.ucla.edu/r/dae/zero-truncated-poisson/ 


# 0) read in data: ---------------
if(!exists("df1.raw.data")){
    source(here("src",
                "2018-07-04_clean-data.R"))
}

str(df1.raw.data)
summary(df1.raw.data)




# 1) regression of LOSDays: -----------
# > 1st model: los ~ dow: --------

m3.los.vs.dow <- vglm(losdays ~ dow, 
                      family = pospoisson(), 
                      data = df1.raw.data)

summary(m3.los.vs.dow)

# note: broom can't deal with vglm objects 


# >> residual diagnostics: ----
m3.output <- data.frame(resid = resid(m3.los.vs.dow),
                        fitted = fitted(m3.los.vs.dow))

p9.resid.vs.fitted <- 
    ggplot(m3.output, 
           aes(fitted, resid)) +
    geom_jitter(position=position_jitter(width=.25), alpha=.5) +
    stat_smooth(method="loess"); p9.resid.vs.fitted

# mean is about 0 across all fitted values ==> that's good!




