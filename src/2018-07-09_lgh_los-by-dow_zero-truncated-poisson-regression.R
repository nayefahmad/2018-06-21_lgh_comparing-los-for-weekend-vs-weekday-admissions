

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
# remember: coefficients represent additive effects on log(mu)
# e.g. coeff of Thursday = 0.212826. 
# Therefore: 
# log(mu) for Thursday = 1.472056 + 0.212826 = 1.684882.
# mu for Thursday = exp(1.684882) = 5.391815

# also recall: exp(0) = 1. When coeff < 0, this means the effect of 
# that var is to reduce value of response from the level of the 
# intercept (by multiplying by a number less than 1.0.)


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





#***************************************************************
# 2nd model: los ~ dow + age + unit.code --------
#***************************************************************
m4.los.vs.dow.age.unit <- 
    vglm(losdays ~ dow + age + unit.code, 
         family = pospoisson(), 
         data = df1.raw.data)
summary(m4.los.vs.dow.age.unit)

# remember: coefficients represent additive effects on log(mu)
# e.g. coeff of Thursday = 0.212826. 
# Therefore: 
# log(mu) for Thursday = 1.472056 + 0.212826 = 1.684882.
# mu for Thursday = exp(1.684882) = 5.391815


# > residual diagnostics: ------------
m4.output <- data.frame(resid = resid(m4.los.vs.dow.age.unit),
                        fitted = fitted(m4.los.vs.dow.age.unit))

p11.resid.vs.fitted.m4 <- 
    ggplot(m4.output, 
           aes(fitted, resid)) +
    geom_jitter(position=position_jitter(width=.25),
                alpha=.25) +
    stat_smooth(method="loess") + 
    # scale_y_log10() + 
    theme_classic(base_size = 16); p11.resid.vs.fitted.m4

# some concern around resids for low fitted values: model seems to be overestimating LOS

# histogram of resids: 
# todo: does this look right? Should it be a zero-truncated Poisson distribution? 
m4.output$resid %>% hist

# > identifying influential points: quantile regression: ------
# there are some values that look rather extreme. To see if these have much
# influence, we can fit lines using quantile regression, these lines represent 
# the 75th, 50th, and 25th percentiles.

p12.add.quantiles <- 
    ggplot(m4.output, 
           aes(fitted, resid)) +
    geom_jitter(position=position_jitter(width=.25),
                alpha=.25) + 
    stat_quantile(method = "rq") + 
    theme_classic(base_size = 16); p12.add.quantiles

# the spread stays pretty much the same across the fitted values, so that's good 

# > model interpretation: ------
# Since resids look ok, we can proceed to interpret the model 
summary(m4.los.vs.dow.age.unit)

# Estimate Std. Error z value Pr(>|z|)    
#     (Intercept)                         0.9529996  0.0840731  11.335  < 2e-16 ***
#     dowTuesday                         -0.1137544  0.0258102  -4.407 1.05e-05 ***
#     dowWednesday                       -0.0895939  0.0251287  -3.565 0.000363 ***
#     dowThursday                         0.0528716  0.0244277   2.164 0.030433 *  
#     dowFriday                          -0.0067072  0.0249092  -0.269 0.787726    
#     dowSaturday                        -0.1633314  0.0394573  -4.139 3.48e-05 ***
#     dowSunday                          -0.0390195  0.0366033  -1.066 0.286419    
#     age                                 0.0146439  0.0005564  26.319  < 2e-16 ***
#     unit.code3E                        -0.3103045  0.0937379  -3.310 0.000932 ***
#     unit.code3W                        -0.5797255  0.1525287  -3.801 0.000144 ***
#     unit.code4E                         0.5389040  0.0851936   6.326 2.52e-10 ***
#     unit.code4W                         0.5932134  0.0753543   7.872 3.48e-15 ***
#     unit.code5E                         1.0774854  0.0739714  14.566  < 2e-16 ***
#     unit.code6E                        -0.0885913  0.0837451  -1.058 0.290115    
#     unit.code6W                         0.2151108  0.0807048   2.665 0.007690 ** 
#     unit.code7E                         1.0570433  0.0790719  13.368  < 2e-16 ***
#     unit.code7W                         0.1737497  0.0800304   2.171 0.029928 *  
#     unit.codeCarlile Youth CD Ctr - IP  1.9345319  0.0828873  23.339  < 2e-16 ***
#     unit.codeECC                       -0.4484701  0.5269421  -0.851 0.394725    
#     unit.codeICU                        0.5465786  0.0795467   6.871 6.37e-12 ***
#     unit.codeIPS                       -0.8882020  0.0730262 -12.163  < 2e-16 ***
#     unit.codeLD                        -0.6631873  0.0773502  -8.574  < 2e-16 ***
#     unit.codeMIU                        1.2766939  0.0764755  16.694  < 2e-16 ***
#     unit.codeNCU                        0.7252013  0.0936617   7.743 9.73e-15 ***
#     unit.codeOR                         0.4392114  0.1613633   2.722 0.006491 ** 
#     unit.codePAR                       -0.8473448  0.0850810  -9.959  < 2e-16 ***
#     unit.codeSCO                        0.8203850  0.1185220   6.922 4.46e-12 ***


# > bootstrap CIs for full model: ----------
# todo: don't know how to do this

