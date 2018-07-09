

#**************************************************
# POISSON REGRESSION FOR LOS on DAY OF WEEK, AGE, ETC.  
#**************************************************

library("tidyverse")
library("here") 
library("magrittr")
library("broom")

# rm(list = ls())

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



# regression of losdays: ----------------------
# since data is count data (counting days starting at 0), we use Poisson regression

m1.pois.linear <-  glm(losdays ~ dow + age,
                       family = poisson(link=log), 
                       data=df1.raw.data)

summary(m1.pois.linear)
predict(m1.pois.linear) %>% as.vector %>% summary

# model: log(mu) = exp(X.Beta) ==> mu = exp(X.Beta)
# therefore, each coefficient shows the multiplicative increase in mu 
# by a factor of exp(coefficient). 

# check: 
# exp(1.536703)  # 4.649236. this is coeff of the intercept
# mean(df1.raw.data$losdays, na.rm = T)  # 4.709783

m1.coeffs <- tidy(m1.pois.linear) %>% 
    mutate(estimate.back.transformed = exp(estimate), 
           sig = ifelse(p.value < .05, TRUE, FALSE)) %>% 
    select(term, estimate, estimate.back.transformed, everything())

m1.coeffs

augment(m1.pois.linear) 
glance(m1.pois.linear)

# after adjusting for age, los of patients admitted on Sundays are 
# significantly higher than Mondays by 1.518 days on average 

# holding dow equal, each year of age increases LOS by 1.06 days on average! 
# This is a huge effect. 



m2.unit <- glm(losdays ~ unit.code + age, 
               family = poisson(),  # todo: should we include link = identity? 
               data=df1.raw.data)

summary(m2.unit)