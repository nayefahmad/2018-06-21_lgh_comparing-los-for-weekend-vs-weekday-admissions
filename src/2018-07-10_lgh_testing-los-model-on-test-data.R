


#********************************************************
# TESTING MODEL M4 WITH TEST DATA FROM 2018
#********************************************************

library("tidyverse")
library("here") 
library("magrittr")
library("broom")
library("ggpubr")

# rm(list = ls())

if(!exists("df2.test.data")){
    source(here("src",
                "2018-07-04_clean-data.R"))
}

# remove NAs: 
df2.test.data %<>% 
    filter(!is.na(losdays)) 


# add predictions to test data: 
df4.testing.with.predicted <- 
    df2.test.data %>% 
    mutate(m0.pred = predict(m0.ols, 
                             newdata = df2.test.data),
           m3.pred = exp(predict(m3.los.vs.dow, 
                                 newdata = df2.test.data)), 
           m4.pred = exp(predict(m4.los.vs.dow.age.unit, 
                                 newdata = df2.test.data)))


# plot results: 
p15.test.data.actual.vs.pred.m4 <- 
    df4.testing.with.predicted %>% 
    ggplot(aes(x = m4.pred, 
               y = losdays)) +
    geom_point(aes(x = m4.pred, 
                   y = losdays), 
               shape = 1, 
               colour = "blue") +
    scale_y_continuous(breaks = seq(0,120, by = 10)) + 
    coord_cartesian(ylim = c(0,40)) + 
    stat_smooth(method = "loess") + 
    
    labs(title = "Predicting LOS at Lions Gate Hospital (test data from Jan to Feb 2018)",
         subtitle = "Actual LOS vs predicted values using zero-truncated Poisson regression model \nModel is performing well up to fitted values of 20 days\nModel uses Age, Day of Week and Nursing Unit as predictors") + 
    
    theme_classic(base_size = 14); p15.test.data.actual.vs.pred.m4

# save output: 
ggsave(here("results", 
            "output from src", 
            "2018-07-10_lgh_test-data-performance-los-predictions-from-full-model.pdf"), 
       p15.test.data.actual.vs.pred.m4)



# ols model for comparison: 
p16.test.data.actual.vs.pred.m0 <- 
    df4.testing.with.predicted %>% 
    ggplot(aes(x = m0.pred, 
               y = losdays)) +
    geom_point(aes(x = m0.pred, 
                   y = losdays), 
               shape = 1, 
               colour = "blue") +
    scale_y_continuous(breaks = seq(0,120, by = 2)) + 
    coord_cartesian(ylim = c(0,10)) + 
    stat_smooth(method = "loess") + 
    
    labs(title = "Null model for predicting LOS at Lions Gate Hospital using day of week", 
         subtitle = "This model simply calculates average by day of week, and uses that as prediction \nThe fact that the loess line is not monotonously increasing shows that the model performs poorly") + 
    
    theme_classic(base_size = 14); p16.test.data.actual.vs.pred.m0
    
# output: 
ggsave(here("results", 
            "output from src", 
            "2018-07-10_lgh_test-data-los-predictions-from-null-model-OLS-with-only-DOW.pdf"), 
       p16.test.data.actual.vs.pred.m0)
