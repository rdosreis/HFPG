results <- GBD %>% 
          select(measure, location, sex, age, cause, metric, year, val, upper, lower) %>% 
          filter(sex == "Both",
                 age == "Age-standardized",
                 cause == "All causes",
                 year %in% c(1990, 2019))

library("xlsx")
write.xlsx(results, file="resultsGBD.xlsx")

resultsROC <- GBD_ROC_1990_to_2019 %>% 
  select(measure, location, sex, age, cause, metric, year, ROC_val, ROC_upper, ROC_lower) %>% 
  filter(sex == "Both",
         age == "Age-standardized",
         cause == "All causes",
         metric == "Rate")

SA <- SA_age_stand %>% 
  filter(year %in% c(1990,2019))

write.xlsx(SA, file="SAresults.xlsx")
