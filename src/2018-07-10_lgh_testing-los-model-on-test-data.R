


#********************************************************
# TESTING MODEL M4 WITH TEST DATA FROM 2018
#********************************************************

library("tidyverse")
library("here") 
library("magrittr")
library("broom")
library("ggpubr")

# rm(list = ls())

# read in data if necessary: ------
if(!exists("df2.test.data")){
    source(here("src",
                "2018-07-04_clean-data.R"))
}

# load models: 
m0.ols.dow <- readRDS(here("results", "output from src", "m0.ols.dow.Rds"))
m0.ols.unit <- readRDS(here("results", "output from src", "m0.ols.unit.Rds")) 
m3.los.vs.dow <- readRDS(here("results", "output from src", "m3.los.vs.dow.Rds"))
m4.los.vs.dow.age.unit <- readRDS(here("results", "output from src", "m4.los.vs.dow.age.unit.Rds"))


# remove NAs: 
df2.test.data %<>% 
    filter(!is.na(losdays)) 


# add predictions to test data: 
df4.testing.with.predicted <- 
    df2.test.data %>% 
    mutate(m0.dow.pred = predict(m0.ols.dow, 
                                 newdata = df2.test.data),
           m0.unit.pred = predict(m0.ols.unit, 
                                  newdata = df2.test.data),
           m3.pred = exp(predict(m3.los.vs.dow, 
                                 newdata = df2.test.data)), 
           m4.pred = exp(predict(m4.los.vs.dow.age.unit, 
                                 newdata = df2.test.data)),
           m5.pred = exp(predict(m5.los.vs.dow.age.unit.year, 
                                 newdata = df2.test.data)),
           m6.1.pred = exp(predict(m6.1.add.service, 
                                 newdata = df2.test.data)),
           
           # find squared errors: 
           m0.dow.err.sq = (losdays - m0.dow.pred)^2,
           m0.unit.err.sq = (losdays - m0.unit.pred)^2,
           m3.err.sq = (losdays - m3.pred)^2, 
           m4.err.sq = (losdays - m4.pred)^2,
           m5.err.sq = (losdays - m5.pred)^2,
           m6.1.err.sq = (losdays - m6.1.pred)^2,
           
           # find absolute errors: 
           m0.unit.err.abs = abs(losdays - m0.unit.pred), 
           m4.err.abs = abs(losdays - m4.pred), 
           m5.err.abs = abs(losdays - m5.pred), 
           m6.1.err.abs = abs(losdays - m6.1.pred))

summary(df4.testing.with.predicted)

# Find RMSE: ---------------- 
(m0.dow.rmse <- sqrt(mean(df4.testing.with.predicted$m0.dow.err.sq)))  # 8.720378
(m0.unit.rmse <- sqrt(mean(df4.testing.with.predicted$m0.unit.err.sq)))  # 7.567942
(m3.rmse <- sqrt(mean(df4.testing.with.predicted$m3.err.sq)))  # 8.72092
(m4.rmse <- sqrt(mean(df4.testing.with.predicted$m4.err.sq)))  # 7.547195
(m5.rmse <- sqrt(mean(df4.testing.with.predicted$m5.err.sq)))  # 7.471976
(m6.1.rmse <- sqrt(mean(df4.testing.with.predicted$m6.1.err.sq)))  # 7.334444
# 13% reduction of RMSE from OLS using DOW 
# 0.3% reduction of RMSE from OLS using unit.code




# RMSE for shorter/longer stays: -------
quantile.upper <- 0.9
quantile.lower <- 0

m0.dow.rmse.filtered <- df4.testing.with.predicted %>% 
    filter(losdays <= quantile(df4.testing.with.predicted$losdays, quantile.upper), 
           losdays >= quantile(df4.testing.with.predicted$losdays, quantile.lower)) %>%
    select(m0.dow.err.sq) %>% 
    unlist() %>% 
    as.vector() %>% 
    mean(., na.rm = TRUE) %>% 
    sqrt()

m0.unit.rmse.filtered <- df4.testing.with.predicted %>% 
    filter(losdays <= quantile(df4.testing.with.predicted$losdays, quantile.upper), 
           losdays >= quantile(df4.testing.with.predicted$losdays, quantile.lower)) %>%
    select(m0.unit.err.sq) %>% 
    unlist() %>% 
    as.vector() %>% 
    mean(., na.rm = TRUE) %>% 
    sqrt()

m4.rmse.filtered <- df4.testing.with.predicted %>% 
    filter(losdays <= quantile(df4.testing.with.predicted$losdays, quantile.upper), 
           losdays >= quantile(df4.testing.with.predicted$losdays, quantile.lower)) %>%
    select(m4.err.sq) %>% 
    unlist() %>% 
    as.vector() %>% 
    mean(., na.rm = TRUE) %>% 
    sqrt()


m5.rmse.filtered <- df4.testing.with.predicted %>% 
    filter(losdays <= quantile(df4.testing.with.predicted$losdays, quantile.upper), 
           losdays >= quantile(df4.testing.with.predicted$losdays, quantile.lower)) %>%
    select(m5.err.sq) %>% 
    unlist() %>% 
    as.vector() %>% 
    mean(., na.rm = TRUE) %>% 
    sqrt()


m6.1.rmse.filtered <- df4.testing.with.predicted %>% 
    filter(losdays <= quantile(df4.testing.with.predicted$losdays, quantile.upper), 
           losdays >= quantile(df4.testing.with.predicted$losdays, quantile.lower)) %>%
    select(m6.1.err.sq) %>% 
    unlist() %>% 
    as.vector() %>% 
    mean(., na.rm = TRUE) %>% 
    sqrt()


# compare m4 with m0.dow: 
# m0.dow.rmse.filtered; m4.rmse.filtered
(m4.rmse.filtered - m0.dow.rmse.filtered)/m0.dow.rmse.filtered  # 26% reduction in RMSE

# compare m4 with m0.unit: 
# m0.unit.rmse.filtered; m4.rmse.filtered
(m4.rmse.filtered - m0.unit.rmse.filtered)/m0.unit.rmse.filtered  # 0.5% reduction in RMSE

# compare m5 with m0.unit: 
(m5.rmse.filtered - m0.unit.rmse.filtered)/m0.unit.rmse.filtered  # 1.2% reduction in RMSE over entire range; 15% reduction in RMSE over bottom 90% of LOS data!! 

# compare m6.1 with m0.unit: 
(m6.1.rmse.filtered - m0.unit.rmse.filtered)/m0.unit.rmse.filtered  # 1.2% reduction in RMSE over entire range; 16% reduction in RMSE over bottom 90% of LOS data!! 


# compare MAE of m4 and m5 with m0.unit: --------------
(m0.unit.mae <- mean(df4.testing.with.predicted$m0.unit.err.abs))  # 3.271747
(m4.mae <- mean(df4.testing.with.predicted$m4.err.abs))  # 3.238862 
(m5.mae <- mean(df4.testing.with.predicted$m5.err.abs))  # 2.98388 
(m6.1.mae <- mean(df4.testing.with.predicted$m6.1.err.abs))  # 2.934371 

(m4.mae - m0.unit.mae)/m0.unit.mae  # 1.00% reduction in MAE 
(m5.mae - m0.unit.mae)/m0.unit.mae  # 8.799% reduction in MAE!! 
(m6.1.mae - m0.unit.mae)/m0.unit.mae  # 10.3% reduction in MAE!! 


# filtered MAE comparison: 
quantile.upper <- .90
quantile.lower <- 0

m0.unit.mae.filtered <- df4.testing.with.predicted %>% 
    filter(losdays <= quantile(df4.testing.with.predicted$losdays, quantile.upper), 
           losdays >= quantile(df4.testing.with.predicted$losdays, quantile.lower)) %>%
    select(m0.unit.err.abs) %>% 
    unlist() %>% 
    as.vector() %>% 
    mean(., na.rm = TRUE) 

m6.1.mae.filtered <- df4.testing.with.predicted %>% 
    filter(losdays <= quantile(df4.testing.with.predicted$losdays, quantile.upper), 
           losdays >= quantile(df4.testing.with.predicted$losdays, quantile.lower)) %>%
    select(m6.1.err.abs) %>% 
    unlist() %>% 
    as.vector() %>% 
    mean(., na.rm = TRUE) 


(m6.1.mae.filtered - m0.unit.mae.filtered)/m0.unit.mae.filtered  # 17.3% reduction in MAE over bottom 90! of data 



# comparison table for models:
df5.error.comparison <- 
    data.frame(model = c("los ~ unit (OLS)", 
                         "los ~ unit + age + dow (0-trunc. Pois)", 
                         "los ~ unit + age + dow + year (0-trunc. Pois)", 
                         "los ~ unit + age + dow + year + service (0-trunc. Pois)"), 
               rmse = c(m0.unit.rmse, 
                        m4.rmse, 
                        m5.rmse, 
                        m6.1.rmse), 
               mae = c(m0.unit.mae, 
                       m4.mae, 
                       m5.mae, 
                       m6.1.mae), 
               mae.bottom.90perc.los = c(m0.unit.mae.filtered, 
                                         "not calculated yet", 
                                         "not calculated yet", 
                                         m6.1.mae.filtered))

# so, RMSE or MAE? 
# ans. It's complicated? https://www.geosci-model-dev-discuss.net/7/C473/2014/gmdd-7-C473-2014-supplement.pdf 
# This says RMSE more appropriate with normal errors (not the case here)

# https://www.quora.com/How-would-a-model-change-if-we-minimized-absolute-error-instead-of-squared-error-What-about-the-other-way-around 
# says MAE more robust to outliers



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
    coord_cartesian(ylim = c(0,30), 
                    xlim = c(0,30)) + 
    stat_smooth(method = "loess") + 
    geom_abline(intercept = 0, 
                slope = 1, 
                colour = "grey50") + 
    
    labs(title = "Predicting LOS at Lions Gate Hospital (test data from Jan \nto Feb 2018)",
         subtitle = "Actual LOS vs predicted values using zero-truncated Poisson regression model \nModel is performing well up to fitted values of 20 days\nModel uses Age, Day of Week and Nursing Unit as predictors",
         x = "Predicted average LOS", 
         y = "Actual LOS") + 
    
    theme_classic(base_size = 14); p15.test.data.actual.vs.pred.m4

# save output: 
# ggsave(here("results", 
#             "output from src", 
#             "2018-07-10_lgh_test-data-performance-los-predictions-from-full-model.pdf"), 
#        p15.test.data.actual.vs.pred.m4)
# 


# model m5: 
p21.test.data.actual.vs.pred.m5 <- 
    df4.testing.with.predicted %>% 
    ggplot(aes(x = m5.pred, 
               y = losdays)) +
    geom_point(aes(x = m5.pred, 
                   y = losdays), 
               shape = 1, 
               colour = "blue") +
    scale_y_continuous(breaks = seq(0,120, by = 10)) + 
    coord_cartesian(xlim = c(0,25)) +
    stat_smooth(method = "loess") + 
    geom_abline(intercept = 0, 
                slope = 1, 
                colour = "grey50") + 
    
    labs(title = "Predicting LOS at Lions Gate Hospital (test data from Jan \nto Feb 2018)",
         subtitle = "Actual LOS vs predicted values using zero-truncated Poisson regression model \nModel uses Age, Day of Week, Nursing Unit & Year as predictors",
         x = "Predicted average LOS", 
         y = "Actual LOS") + 
    
    theme_classic(base_size = 14); p21.test.data.actual.vs.pred.m5

# save output: 
# ggsave(here("results",
#             "output from src",
#             "2018-07-13_lgh_test-data-performance-los-predictions-model-m5.pdf"),
#        p21.test.data.actual.vs.pred.m5)



# model m6: 
p25.test.data.actual.vs.pred.m6 <- 
    df4.testing.with.predicted %>% 
    ggplot(aes(x = m6.1.pred, 
               y = losdays)) +
    geom_point(aes(x = m6.1.pred, 
                   y = losdays), 
               shape = 1, 
               colour = "blue") +
    scale_y_continuous(breaks = seq(0,120, by = 10)) + 
    coord_cartesian(xlim = c(0,25)) +
    stat_smooth(method = "loess") + 
    geom_abline(intercept = 0, 
                slope = 1, 
                colour = "grey50") + 
    
    labs(title = "Predicting LOS at Lions Gate Hospital (test data from Jan \nto Feb 2018)",
         subtitle = "Actual LOS vs predicted values using zero-truncated Poisson regression model\nModel uses Age, Day of Week, Nursing Unit, Year & Service as predictors",
         x = "Predicted average LOS", 
         y = "Actual LOS") + 
    
    theme_classic(base_size = 14); p25.test.data.actual.vs.pred.m6

# output: 
# ggsave(here("results",
#             "output from src",
#             "2018-07-13_lgh_test-data-performance-los-predictions-model-m6.1.pdf"),
#        p25.test.data.actual.vs.pred.m6)



# ols model for comparison: 
p16.test.data.actual.vs.pred.m0 <- 
    df4.testing.with.predicted %>% 
    ggplot(aes(x = m0.dow.pred, 
               y = losdays)) +
    geom_point(aes(x = m0.dow.pred, 
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
# ggsave(here("results", 
#             "output from src", 
#             "2018-07-10_lgh_test-data-los-predictions-from-null-model-OLS-with-only-DOW.pdf"), 
#        p16.test.data.actual.vs.pred.m0)



p17.test.data.actual.vs.pred.m0.unit <- 
    df4.testing.with.predicted %>% 
    ggplot(aes(x = m0.unit.pred, 
               y = losdays)) +
    geom_point(aes(x = m0.unit.pred, 
                   y = losdays), 
               shape = 1, 
               colour = "blue") +
    
    # scale_y_continuous(breaks = seq(0,120, by = 10)) + 
    coord_cartesian(xlim = c(0,25)) + 
    stat_smooth(method = "gam") +  # loess is acting weird 
    geom_abline(intercept = 0,
                slope = 1,
                color = "grey50") + 
    
    labs(title = "Null model for predicting LOS at Lions Gate Hospital using \nadmission unit", 
         subtitle = "This model simply calculates average by admission unit, and uses that as prediction \nWithout adding predictors, the model cannot account for most of the variation") + 
    
    theme_classic(base_size = 14); p17.test.data.actual.vs.pred.m0.unit

# output: 
# ggsave(here("results",
#             "output from src",
#             "2018-07-11_lgh_test-data-los-predictions-null-model-OLS-nursing-unit.pdf"),
#        p17.test.data.actual.vs.pred.m0.unit)






# write output: -------------------------------
# write_csv(df4.testing.with.predicted, 
#           here("results", 
#                "output from src", 
#                "2018-07-11_lgh_los-modelling-test-data-with-predictions.csv"))


# all graphs in 1 file; 
# pdf(here("results", 
#          "output from src", 
#          "2018-07-16_lgh_training-data-models-m0-4-5-6.pdf"))
# p17.test.data.actual.vs.pred.m0.unit
# p21.test.data.actual.vs.pred.m5
# p25.test.data.actual.vs.pred.m6
# dev.off()
