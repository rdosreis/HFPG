results <- GBD %>% 
          select(measure, location, sex, age, cause, metric, year, val, upper, lower) %>% 
          filter(sex == "Both",
                 age == "Age-standardized",
                 cause == "All causes",
                 year %in% c(1990, 2019))

library("xlsx")
write.xlsx(results, file="resultsGBD.xlsx")


SA <- SA_age_stand %>% 
  filter(year %in% c(1990,2019))

write.xlsx(SA, file="SAresults.xlsx")
