

#**************************************************
# POISSON REGRESSION FOR LOS on DAY OF WEEK, AGE, ETC.  
#**************************************************

library("tidyverse")
library("here") 
library("magrittr")
library("broom")
library("ggpubr")

# rm(list = ls())

# TODO: ---------
# > Change filename
# > refit model without outliers 
# > Negative Binomial regression instead of Poisson (account for increasing variance as fitted values increase)
# > use zero-truncated Poisson regression?: https://stats.idre.ucla.edu/r/dae/zero-truncated-poisson/
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



# 4) WRITE OUTPUTS: ------------------
write_csv(df1.raw.data, 
          here("results",
               "output from src", 
               "2018-07-09_lgh_clean-data-los-by-dow.csv"))

