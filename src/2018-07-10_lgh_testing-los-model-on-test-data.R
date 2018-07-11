


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
                                 newdata = df2.test.data)), 
           
           m0.err.sq = (losdays - m0.pred)^2, 
           m3.err.sq = (losdays - m3.pred)^2, 
           m4.err.sq = (losdays - m4.pred)^2)

summary(df4.testing.with.predicted)

# Find RMSE: ---------------- 
(m0.rmse <- sqrt(mean(df4.testing.with.predicted$m0.err.sq)))  # 8.720378
(m3.rmse <- sqrt(mean(df4.testing.with.predicted$m3.err.sq)))  # 8.72092
(m4.rmse <- sqrt(mean(df4.testing.with.predicted$m4.err.sq)))  # 7.547195


# RMSE for shorter/longer stays: -------
m0.rmse.filtered <- df4.testing.with.predicted %>% 
    filter(losdays <= quantile(df4.testing.with.predicted$losdays, .75), 
           losdays >= quantile(df4.testing.with.predicted$losdays, 0)) %>%
    select(m0.err.sq) %>% 
    unlist() %>% 
    as.vector() %>% 
    mean(., na.rm = TRUE) %>% 
    sqrt()

m4.rmse.filtered <- df4.testing.with.predicted %>% 
    filter(losdays <= quantile(df4.testing.with.predicted$losdays, .75), 
           losdays >= quantile(df4.testing.with.predicted$losdays, 0)) %>%
    select(m4.err.sq) %>% 
    unlist() %>% 
    as.vector() %>% 
    mean(., na.rm = TRUE) %>% 
    sqrt()

m0.rmse.filtered; m4.rmse.filtered
(m4.rmse.filtered - m0.rmse.filtered)/m0.rmse.filtered  # 15% reduction in RMSE




# plot results: --------------
p15.test.data.actual.vs.pred.m4 <- 
    df4.testing.with.predicted %>% 
    ggplot(aes(x = m4.pred, 
               y = losdays)) +
    geom_point(aes(x = m4.pred, 
                   y = losdays), 
               shape = 1, 
               colour = "blue") +
    scale_y_continuous(breaks = seq(0,120, by = 10)) + 
    coord_cartesian(ylim = c(0,30)) + 
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



# calculate RMSE: 


