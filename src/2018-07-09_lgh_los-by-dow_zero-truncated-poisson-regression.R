

#**************************************************
# ZERO-TRUNCATED POISSON REGRESSION FOR LOS on DAY OF WEEK, AGE, ETC.  
#**************************************************

library("tidyverse")
library("here") 
library("magrittr")
library("broom")
library("ggpubr")
library("VGAM")  # for function vglm( ) 
library("quantreg")  # for quantile regression 

# rm(list = ls())
source(here("src", 
            "bootstrap-coefficients_function.R"))

# TODO: ---------

#****************


# reference: https://stats.idre.ucla.edu/r/dae/zero-truncated-poisson/ 


# 0) read in data: ---------------------------------------------
if(!exists("df1.raw.data")){
    source(here("src",
                "2018-07-04_clean-data.R"))
}

str(df1.raw.data)
summary(df1.raw.data)




# 1) 1st model: los ~ dow: ------------------------------------
m3.los.vs.dow <- vglm(losdays ~ dow, 
                      family = pospoisson(), 
                      data = df1.raw.data)

summary(m3.los.vs.dow)

# note: broom can't deal with vglm objects 


# > residual diagnostics: ----
m3.output <- data.frame(resid = resid(m3.los.vs.dow),
                        fitted = fitted(m3.los.vs.dow))

p9.resid.vs.fitted <- 
    ggplot(m3.output, 
           aes(fitted, resid)) +
    geom_jitter(position=position_jitter(width=.25),
                alpha=.25) +
    stat_smooth(method="loess") + 
    theme_classic(base_size = 16); p9.resid.vs.fitted

# histogram of resids: 
# todo: does this look right? Should it be a zero-truncated Poisson distribution? 
m3.output$resid %>% hist

# mean is about 0 across all fitted values ==> that's good!
# todo: why does loess line not extend to ends of the x-axis? 


# > identifying influential points: quantile regression: ------
# there are some values that look rather extreme. To see if these have much
# influence, we can fit lines using quantile regression, these lines represent 
# the 75th, 50th, and 25th percentiles.

p10.add.quantiles <- 
    ggplot(m3.output, 
           aes(fitted, resid)) +
    geom_jitter(position=position_jitter(width=.25),
                alpha=.25) + 
    stat_quantile(method = "rq") + 
    theme_classic(base_size = 16); p10.add.quantiles

# the spread stays pretty much the same across the fitted values, so that's good 


# > model interpretation: ------
# Since resids look ok, we can proceed to interpret the model 
summary(m3.los.vs.dow)

# Coefficients: 
#     Estimate Std. Error z value Pr(>|z|)    
#     (Intercept)   1.472056   0.018248  80.670  < 2e-16 ***
#     dowTuesday   -0.029231   0.024707  -1.183 0.236757    
#     dowWednesday  0.002243   0.024023   0.093 0.925604    
#     dowThursday   0.212826   0.023319   9.127  < 2e-16 ***
#     dowFriday     0.189934   0.023720   8.007 1.17e-15 ***
#     dowSaturday  -0.138598   0.037128  -3.733 0.000189 ***
#     dowSunday     0.032899   0.034216   0.962 0.336293   


# > bootstrap confidence intervals for the model: -----------
set.seed(10)
bootresults <-
    boot(df1.raw.data,
         extract.coeffs.m3_function,
         R = 1000)
bootresults
    
bootresults.df <- bootresults %>% 
    tidy() %>%
    mutate(param = rep(c("coeff", "stderror"), 7)) %>%
    select(param, everything()); bootresults.df

# todo: what is bias here? 

# Estimating CIs: 
params <- 
    sapply(seq(1, 13, by=2), 
             function(i){
                 out <- boot.ci(bootresults, 
                                index = c(i, i+1), 
                                type = c("perc", "basic"), 
                                h = exp)  # back-transform parameter ==> coeff = 1 means no effect (since effects are multiplicative on scale of response var)
                 
                 # print(out)
                 # str(out)
                 
                 with(out, c(Est = t0,  # The observed value of the statistic of interest
                             pLL = percent[4],  # The intervals calculated using the bootstrap percentile method.
                             pUL = percent[5], 
                             basicLL = basic[4],  # The intervals calculated using the basic bootstrap method.
                             basicUL = basic[5]))
            }) %>% 
    t() 

# add labels: 
row.names(params) <- names(coef(m3.los.vs.dow))

params

# looks like only the intercept & Thursday are significant now!!
# All other CIs include 1 ==> no multiplicative effect. 












