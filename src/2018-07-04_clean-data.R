

#**************************************************
# IMPORT, CLEAN DATA 
#**************************************************

library("tidyverse")
library("here") 
library("magrittr")

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
         ishomeless = as.factor(ishomeless)) %>% 
  mutate_if(is.character, factor) %>% 
  select(-admissionnursingunit)


str(df1.raw.data)
summary(df1.raw.data)




