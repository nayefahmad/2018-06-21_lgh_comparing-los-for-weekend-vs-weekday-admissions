

#**************************************************
# POISSON REGRESSION FOR LOS on DAY OF WEEK, AGE, ETC.  
#**************************************************

library("tidyverse")
library("here") 
library("magrittr")
library("broom")

# rm(list = ls())

# TODO: ---------
# > Change filename
# > refit model without outliers 
# > Negative Binomial regression instead of Poisson (account for increasing variance as fitted values increase)
# > examine quadratic effect of age? 
#****************

# 0) read, prep data: ------------------
df1.raw.data <- read_csv(here("results", 
                              "output from src", 
                              "2018-06-21_lgh_comparing-los-for-weekend-vs-weekday-admissions.csv"), 
                         na = "NULL")
names(df1.raw.data) <- tolower(names(df1.raw.data))

df1.raw.data %<>% 
    rename(site = admissionfacilitylongname, 
         date = adjustedadmissiondate, 
         dow = day_of_week, 
         age = admissionage, 
         unit.code = admissionnursingunitcode) %>% 
    mutate(date = as.Date(date, '%m/%d/%Y'), 
         ishomeless = as.factor(ishomeless), 
         dow = fct_relevel(dow, 
                           "Monday", 
                           "Tuesday", 
                           "Wednesday", 
                           "Thursday",
                           "Friday", 
                           "Saturday", 
                           "Sunday")) %>% 
    mutate_if(is.character, factor) %>% 
    select(-admissionnursingunit)


str(df1.raw.data)
summary(df1.raw.data)
summary(df1.raw.data$unit.code)


# 1) basic analysis: --------------------------------------------
# admissions by dow: 
p1.dow <- 
    df1.raw.data %>% 
    ggplot(aes(x = dow)) +
    geom_bar(); p1.dow
  
p2.dow.by.unit <- 
    p1.dow + 
    facet_wrap(~unit.code) + 
    scale_y_log10(breaks = c(0, 1, 10, 50, 100, 1000) ,
                  limits = c(1, 2000)) +
    theme(axis.text.x = element_text(angle = 90, vjust = -.05)); p2.dow.by.unit

# hist of LOS (log x axis)
p3.hist <- 
    df1.raw.data %>%
    ggplot(aes(x=losdays)) +
    geom_histogram(binwidth = 5) + 
    scale_x_continuous(breaks = seq(0,150, 10)) +   
    scale_y_log10(breaks = c(0, 1, 5, 10, 50, 100, 500, 1000)); p3.hist
# todo: some values being dropped?? max losdays should be 130; remove log y-axis and set binwidth to 1 
# prob because scale of y starts at 1 

p4.hist.by.unit <- 
    p3.hist + 
    facet_wrap(~unit.code) +
    scale_x_continuous(breaks = c(10, 50, 100)); p4.hist.by.unit
# 4W, 5E and MIU are the units that seem to have more of the high LOS patients 

p5.los.by.day <- 
    df1.raw.data %>% 
    ggplot(aes(x = dow, 
               y = losdays)) + 
    geom_boxplot() + 
    stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) + 
    scale_y_continuous(breaks = seq(0, 20, 1)) + 
    coord_cartesian(ylim = c(0, 15)); p5.los.by.day

# Median LOS is 2 days regardless of day of admission 
# Thursday, Friday have highest mean LOS; todo: is this significant?? 

# exact values of means: 
# df1.raw.data %>% group_by(dow) %>% summarise(mean.los = mean(losdays, na.rm = TRUE))
# 90th percentile: 
# df1.raw.data %>% group_by(dow) %>% summarise(perc.90.los = quantile(losdays, probs = 0.90, na.rm = TRUE))

p6.los.by.day.by.unit <- 
    df1.raw.data %>% 
    ggplot(aes(x = dow, 
               y = losdays)) + 
    geom_boxplot() + 
    stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) + 
    scale_y_continuous(breaks = seq(0, 50, 5)) +
    coord_cartesian(ylim = c(0,50)) + 
    facet_wrap(~unit.code) + 
    theme(axis.text.x = element_text(angle = 90, vjust = -.05)); p6.los.by.day.by.unit
# hypothesis that LOS greater if weekend admission seems to hold for 4E, 2E, 5E 
# maybe not for other units??



# 2) regression of losdays: ----------------------
# since data is count data (counting days starting at 0), we use Poisson regression

# > 1st model: los vs dow ---------
m1.pois <- glm(losdays ~ dow,
               family = poisson(link=log), 
               data=df1.raw.data)

summary(m1.pois)

# create table to comare models: 
model.comparison <- 
    glance(m1.pois) %>% 
    as.data.frame() %>% 
    mutate(model = "los ~ dow") %>% 
    select(model, everything())




# > 2nd model: los vs day of week, age, unit: -----------
m2.pois <-  glm(losdays ~ dow + age + unit.code,
                       family = poisson(link=log), 
                       data=df1.raw.data)

summary(m2.pois)
predict(m2.pois) %>% as.vector %>% summary

# examine significant coeffs: 
m2.coeffs <- tidy(m2.pois) %>% 
    mutate(estimate.back.transformed = exp(estimate), 
           sig = ifelse(p.value < .05, TRUE, FALSE)) %>% 
    select(term, estimate, estimate.back.transformed, everything()) %>% 
    filter(sig == TRUE)

m2.coeffs


# other model summaries: 
augment(m2.pois) 
glance(m2.pois)

# compare m1 and m2: --------- 
model.comparison <- 
    rbind(model.comparison, 
      glance(m2.pois) %>% 
          as.data.frame() %>% 
          mutate(model = "los ~ dow + age + unit") %>% 
          select(model, everything()))
model.comparison

# AIC, BIC, deviance all lower for m2 ==> it's a better model! 



# m2 diagnostics: -----------
# reference for poisson regression for patient LOS: 
# https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/1472-6947-14-26 

par(mfrow = c(2,2))
plot(m2.pois)
par(mfrow = c(1,1))

# plot 1: loess line stays near 0 across the range of predicted values, so that's good 
# note that at low predicted values, residuals have a lower bound because actual LOS is never below 0. 

# todo: however, we expect no "fanning out" of resids ==> we're not accounting for all the variance! Maybe use a Negative Binomial regression instead of Poisson? 
# https://support.minitab.com/en-us/minitab/18/help-and-how-to/modeling-statistics/regression/how-to/fit-poisson-model/interpret-the-results/all-statistics-and-graphs/residual-plots/ 

# plot 2: inverted S-shape looks okay for Poisson resids? (see reference above)
# plot 3: looks ok?? (see reference above)
# plot 4: most points within bounds, that's good

resid(m2.pois) %>% hist  # looks like it could be a Poisson dist, so that's good 

# Pearson resids: supposed to be normal?: https://stats.stackexchange.com/questions/99052/residuals-in-poisson-regression 
plot(resid(m2.pois, "pearson") ~ predict(m2.pois))


# general: points 4024, 3499, 643 and 4495 might be outliers; todo: refit model without outliers 




# 3) interpreting the full model: ---------

# model: log(mu) = X_matrix.Beta ==> mu = exp(X.Beta)
# therefore, each coefficient shows the multiplicative increase in mu 
# by a factor of exp(coefficient) ==> 1 unit increase in X causes multiplicative increase in response by exp(beta) 

# intercept: mean los for Mondays for unit = 2E 

# check: 
# exp(1.0461063)  # 2.846546 days; this is coeff of the intercept
# check: 
df1.raw.data %>% 
    filter(unit.code == "2E", 
           dow == "Monday") %>% 
    select(losdays) %>% 
    summarize(mean.los = mean(losdays, na.rm = TRUE))
# 2.5 days  # todo: why doesn't this match with exp(1.0461...)? 


# look at the coefficients again: 
# reference: https://stats.stackexchange.com/questions/120030/interpretation-of-betas-when-there-are-multiple-categorical-variables 
m2.coeffs %>% select(term, estimate.back.transformed)

# reference group: unit=2E, dow=Monday


# > dow coefficients: ----------
# coeff of Mon (reference): 1.00

# coeff of Tue (backtransformed): 0.9060766
# this says that LOS of Tue is 0.91 of that of Monday on avg
# (after adjusting effect of age and unit)

# coeff of Thu is highest, 1.05 times Monday 
# coeff of Sat is lowest, 0.87 times Monday 


# > age coefficient: -----------
# coeff of age = 1.013
# this says every increase in age by 1 year increases LOS 
# by factor of 1.013 days (adj for dow and unit) compared to reference LOS of 2.8 days 
# increase age by 10 yrs ==> los increases by factor of 10.13?

# note: adding quadratic effect of age does not significantly improve the model (small decrease in deviance, AIC)




# actuals vs predicted values: ------
# todo: is this a useful plot?? 
p7.full.model <- 
    augment(m2.pois) %>% 
    select(losdays, .fitted) %>% 
    ggplot(aes(x=.fitted, 
               y=losdays)) + 
    geom_point() + 
    geom_smooth() + 
    theme_classic(base_size = 16); p7.full.model  
    # scale_y_log10()






