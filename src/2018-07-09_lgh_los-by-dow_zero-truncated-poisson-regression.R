

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
summary(df1.raw.data$service)




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
# set.seed(10)
# bootresults <-
#     boot(df1.raw.data,
#          extract.coeffs.m3_function,
#          R = 1000)
# bootresults
# 
# bootresults.df <- bootresults %>% 
#     tidy() %>%
#     mutate(param = rep(c("coeff", "stderror"), 7)) %>%
#     select(param, everything()); bootresults.df
# 
# # todo: what is bias here? 
# 
# # Estimating CIs: 
# params <- 
#     sapply(seq(1, 13, by=2), 
#            function(i){
#                out <- boot.ci(bootresults, 
#                               index = c(i, i+1), 
#                               type = c("perc", "basic"), 
#                               h = exp)  # back-transform parameter ==> coeff = 1 means no effect (since effects are multiplicative on scale of response var)
#                
#                # print(out)
#                # str(out)
#                
#                with(out, c(Est = t0,  # The observed value of the statistic of interest
#                            pLL = percent[4],  # The intervals calculated using the bootstrap percentile method.
#                            pUL = percent[5], 
#                            basicLL = basic[4],  # The intervals calculated using the basic bootstrap method.
#                            basicUL = basic[5]))
#            }) %>% 
#     t() 
# 
# # add labels: 
# row.names(params) <- names(coef(m3.los.vs.dow))
# 
# params

# looks like only the intercept & Thursday are significant now!!
# All other CIs include 1 ==> no multiplicative effect. 



#***************************************************************
# 2) 2nd model: los ~ dow + unit.code --------
#***************************************************************
m3.1.los.vs.dow.unit <- vglm(losdays ~ dow + unit.code,
                             family = pospoisson(), 
                             data = df1.raw.data)

summary(m3.1.los.vs.dow.unit)



#***************************************************************
# 3) 3rd model: los ~ dow + age + unit.code --------
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





#***************************************************************
# 4) 4th model: los ~ dow + age + unit.code + year --------
#***************************************************************
m5.los.vs.dow.age.unit.year <- 
    vglm(losdays ~ dow + age + unit.code + yearsfrom2016, 
         family = pospoisson(), 
         data = df1.raw.data)
summary(m5.los.vs.dow.age.unit.year)

# remember: coefficients represent additive effects on log(mu)
# e.g. coeff of Thursday = 0.212826. 
# Therefore: 
# log(mu) for Thursday = 1.472056 + 0.212826 = 1.684882.
# mu for Thursday = exp(1.684882) = 5.391815


# > residual diagnostics: ------------
m5.output <- data.frame(resid = resid(m5.los.vs.dow.age.unit.year),
                        fitted = fitted(m5.los.vs.dow.age.unit.year))

p18.resid.vs.fitted.m5 <- 
    ggplot(m5.output, 
           aes(fitted, resid)) +
    geom_jitter(position=position_jitter(width=.25),
                alpha=.25) +
    stat_smooth(method="loess") + 
    # scale_y_log10() + 
    theme_classic(base_size = 16); p18.resid.vs.fitted.m5

# some concern around resids for low fitted values: model seems to be overestimating LOS

# histogram of resids: 
# todo: does this look right? Should it be a zero-truncated Poisson distribution? 
m5.output$resid %>% hist

# > identifying influential points: quantile regression: ------
# there are some values that look rather extreme. To see if these have much
# influence, we can fit lines using quantile regression, these lines represent 
# the 75th, 50th, and 25th percentiles.

p19.add.quantiles <- 
    ggplot(m5.output, 
           aes(fitted, resid)) +
    geom_jitter(position=position_jitter(width=.25),
                alpha=.25) + 
    stat_quantile(method = "rq") + 
    theme_classic(base_size = 16); p19.add.quantiles

# the spread stays pretty much the same across the fitted values, so that's good 





#***************************************************************
# 5) 5th model: los ~ dow + age + unit.code + year + gender/service --------
#***************************************************************
m6.los.vs.dow.age.unit.year.gender <- 
    vglm(losdays ~ dow + age + unit.code + yearsfrom2016 + gender, 
         family = pospoisson(), 
         data = df1.raw.data)
summary(m6.los.vs.dow.age.unit.year.gender)
# gender not significant 

m6.1.add.service <- 
    vglm(losdays ~ dow + age + unit.code + yearsfrom2016 + service, 
         family = pospoisson(), 
         data = df1.raw.data)
summary(m6.1.add.service)    


# > residual diagnostics: ------------
m6.1.output <- data.frame(resid = resid(m6.1.add.service),
                        fitted = fitted(m6.1.add.service))

p22.resid.vs.fitted.m6.1 <- 
    ggplot(m6.1.output, 
           aes(fitted, resid)) +
    geom_jitter(position=position_jitter(width=.25),
                alpha=.25) +
    stat_smooth(method="loess") + 
    # scale_y_log10() + 
    theme_classic(base_size = 16); p22.resid.vs.fitted.m6.1

# some concern around resids for low fitted values: model seems to be overestimating LOS

# histogram of resids: 
# todo: does this look right? Should it be a zero-truncated Poisson distribution? 
m6.1.output$resid %>% hist

# > identifying influential points: quantile regression: ------
# there are some values that look rather extreme. To see if these have much
# influence, we can fit lines using quantile regression, these lines represent 
# the 75th, 50th, and 25th percentiles.

p23.add.quantiles <- 
    ggplot(m6.1.output, 
           aes(fitted, resid)) +
    geom_jitter(position=position_jitter(width=.25),
                alpha=.25) + 
    stat_quantile(method = "rq") + 
    theme_classic(base_size = 16); p23.add.quantiles

# the spread stays pretty much the same across the fitted values, so that's good 





#**********************************************************************
# 5) model comparison : ------
#**********************************************************************
# Since resids of m4 look ok, we can proceed to interpret & compare the models 

summary(m3.los.vs.dow)  # Log-likelihood: -47783 on 4879 degrees of freedom
logLik(m3.los.vs.dow)

summary(m3.1.los.vs.dow.unit)  # Log-likelihood: -32224 on 4860 degrees of freedom
logLik(m3.1.los.vs.dow.unit) 

summary(m4.los.vs.dow.age.unit)  # Log-likelihood: -31724 on 4859 degrees of freedom
logLik(m4.los.vs.dow.age.unit)

summary(m5.los.vs.dow.age.unit.year)  # Log-likelihood: -31609 on 4859 degrees of freedom
logLik(m5.los.vs.dow.age.unit.year)

summary(m6.1.add.service)  # Log-likelihood: -29844 on 4859 degrees of freedom
logLik(m6.1.add.service)



# model with lowest deviance is best. deviance = -2*logLike
# use lrtest to find whether difference in deviance is significant (diff in deviance has chi-sq. dist)
# reference: https://stats.stackexchange.com/questions/237702/comparing-models-using-the-deviance-and-log-likelihood-ratio-tests# 
?lrtest

# compare m3 and m3.1
lrtest(m3.los.vs.dow, m3.1.los.vs.dow.unit)  # significant: adding unit variable works! 
# since the prob that the chi-sq. test stat takes the value that's observed is <0.05, 
# we conclude that the diff in deviance is significant...?

# compare m3.1 and m4: 
lrtest(m3.1.los.vs.dow.unit, m4.los.vs.dow.age.unit)  # significant: adding age variable works!

# compare m4 and m5: 
lrtest(m4.los.vs.dow.age.unit, m5.los.vs.dow.age.unit.year)  # significant: adding year helps

# compare m5 and m6.1: 
lrtest(m5.los.vs.dow.age.unit.year, m6.1.add.service)  # significant!! adding service is good! 


# examine predictions: note that these are in log units. 
# predict(m4.los.vs.dow.age.unit) %>% head
# predict(m3.los.vs.dow) %>% head


# actual vs predicted values: 
df3.training.with.predicted <- 
    df1.raw.data %>% 
    filter(!is.na(losdays)) %>% 
    mutate(m3.pred = exp(predict(m3.los.vs.dow)), 
           m4.pred = exp(predict(m4.los.vs.dow.age.unit)), 
           m5.pred = exp(predict(m5.los.vs.dow.age.unit.year)), 
           m6.1.pred = exp(predict(m6.1.add.service)))


# model m3: 
p13.actual.vs.pred.m3 <- 
    df3.training.with.predicted %>% 
    ggplot(aes(x = m3.pred, 
               y = losdays)) +
    geom_point(aes(x = m3.pred, 
                   y = losdays), 
               shape = 1, 
               colour = "grey30") +
    stat_smooth(method = "loess"); p13.actual.vs.pred.m3


# model m4: 
p14.actual.vs.pred.m4 <- 
    df3.training.with.predicted %>% 
    ggplot(aes(x = m4.pred, 
               y = losdays)) +
    geom_point(aes(x = m4.pred, 
                   y = losdays), 
               shape = 1, 
               colour = "blue") +
    scale_y_continuous(breaks = seq(0,120, by = 10)) + 
    coord_cartesian(ylim = c(0,30), 
                    xlim = c(0,30)) + 
    stat_smooth(method = "loess") + 
    geom_abline(intercept = 0, 
                slope = 1,
                colour = "grey50") + 
    
    labs(title = "Modelling LOS at Lions Gate Hospital",
         subtitle = "Actual LOS vs fitted values using zero-truncated Poisson regression model \nSince loess line passes through origin with slope ~1.0, model is performing well \nModel uses Age, Day of Week and Nursing Unit as predictors", 
         x = "Predicted average LOS", 
         y = "Actual LOS") + 
    
    theme_classic(base_size = 14); p14.actual.vs.pred.m4

# write output: 
# ggsave(here("results", 
#             "output from src", 
#             "2018-07-10_actual-los-vs-fitted-from-full-model.pdf"))
# THIS IS GREAT!!! ALMOST A STRAIGHT LINE WITH SLOPE 1.0!!!


# > bootstrap CIs for full model: ----------
# todo: don't know how to do this


# model m5: 
p20.actual.vs.pred.m5 <- 
    df3.training.with.predicted %>% 
    ggplot(aes(x = m5.pred, 
               y = losdays)) +
    geom_point(aes(x = m5.pred, 
                   y = losdays), 
               shape = 1, 
               colour = "blue") +
    scale_y_continuous(breaks = seq(0,120, by = 10)) + 
    coord_cartesian(ylim = c(0,30), 
                    xlim = c(0,30)) + 
    stat_smooth(method = "loess") + 
    geom_abline(intercept = 0, 
                slope = 1,
                colour = "grey50") + 
    
    labs(title = "Modelling LOS at Lions Gate Hospital",
         subtitle = "Actual LOS vs fitted values using zero-truncated Poisson regression model \nSince loess line passes through origin with slope ~1.0, model is performing well \nModel uses Age, Day of Week, Unit & Year as predictors", 
         x = "Predicted average LOS", 
         y = "Actual LOS") + 
    
    theme_classic(base_size = 14); p20.actual.vs.pred.m5



# model m6.1: 
p24.actual.vs.pred.m6.1 <- 
    df3.training.with.predicted %>% 
    ggplot(aes(x = m6.1.pred, 
               y = losdays)) +
    geom_point(aes(x = m6.1.pred, 
                   y = losdays), 
               shape = 1, 
               colour = "blue") +
    scale_y_continuous(breaks = seq(0,120, by = 10)) + 
    coord_cartesian(ylim = c(0,30), 
                    xlim = c(0,30)) + 
    stat_smooth(method = "loess") + 
    geom_abline(intercept = 0, 
                slope = 1,
                colour = "grey50") + 
    
    labs(title = "Modelling LOS at Lions Gate Hospital",
         subtitle = "Actual LOS vs fitted values using zero-truncated Poisson regression model \nSince loess line passes through origin with slope ~1.0, model is performing well \nModel uses Age, Day of Week, Unit, Year & Service as predictors", 
         x = "Predicted average LOS", 
         y = "Actual LOS") + 
    
    theme_classic(base_size = 14); p24.actual.vs.pred.m6.1




# WRITE OUTPUTS: --------------
# saveRDS(m3.los.vs.dow, 
#         here("results", 
#              "output from src", 
#              "m3.los.vs.dow"))
# 
# saveRDS(m3.los.vs.dow, 
#         here("results", 
#              "output from src", 
#              "m3.los.vs.dow.Rds"))
# 
# saveRDS(m4.los.vs.dow.age.unit, 
#         here("results", 
#              "output from src", 
#              "m4.los.vs.dow.age.unit.Rds"))
