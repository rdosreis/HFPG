# -----------------------------------------
# Carrega pacotes
# -----------------------------------------

library(ggplot2)
library(ggthemes)
library(plotly)
library(tidyverse)
library(readr)
library(here)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(hrbrthemes)
library(shadowtext)

# -----------------------------------------
# Carrega arquivos de dados
# -----------------------------------------

#From GBD tools
GBD_3 <-
  readr::read_csv(file = here::here("GBD-dados", "GBD_3.csv"))
GBD_4 <-
  readr::read_csv(file = here::here("GBD-dados", "GBD_4.csv"))
SEV <- readr::read_csv(file = here::here("GBD-dados", "SEV.csv"))

#Rate of changes
SEV_ROC <-
  readr::read_csv(file = here::here("GBD-dados", "SEV_ROC.csv"))
GBD_ROC <-
  readr::read_csv(file = here::here("GBD-dados", "GBD_ROC.csv"))

#From GHDx
SDI <-
  readr::read_csv(file = here::here("GBD-dados", "GBD_SDI_1990_TO_2019.csv"))
GBD_POP_1990 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_1990_Y2020M10D15.CSV"))
GBD_POP_1991 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_1991_Y2020M10D15.CSV"))
GBD_POP_1992 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_1992_Y2020M10D15.CSV"))
GBD_POP_1993 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_1993_Y2020M10D15.CSV"))
GBD_POP_1994 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_1994_Y2020M10D15.CSV"))
GBD_POP_1995 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_1995_Y2020M10D15.CSV"))
GBD_POP_1996 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_1996_Y2020M10D15.CSV"))
GBD_POP_1997 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_1997_Y2020M10D15.CSV"))
GBD_POP_1998 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_1998_Y2020M10D15.CSV"))
GBD_POP_1999 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_1999_Y2020M10D15.CSV"))
GBD_POP_2000 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2000_Y2020M10D15.CSV"))
GBD_POP_2001 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2001_Y2020M10D15.CSV"))
GBD_POP_2002 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2002_Y2020M10D15.CSV"))
GBD_POP_2003 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2003_Y2020M10D15.CSV"))
GBD_POP_2004 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2004_Y2020M10D15.CSV"))
GBD_POP_2005 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2005_Y2020M10D15.CSV"))
GBD_POP_2006 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2006_Y2020M10D15.CSV"))
GBD_POP_2007 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2007_Y2020M10D15.CSV"))
GBD_POP_2008 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2008_Y2020M10D15.CSV"))
GBD_POP_2009 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2009_Y2020M10D15.CSV"))
GBD_POP_2010 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2010_Y2020M10D15.CSV"))
GBD_POP_2011 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2011_Y2020M10D15.CSV"))
GBD_POP_2012 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2012_Y2020M10D15.CSV"))
GBD_POP_2013 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2013_Y2020M10D15.CSV"))
GBD_POP_2014 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2014_Y2020M10D15.CSV"))
GBD_POP_2015 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2015_Y2020M10D15.CSV"))
GBD_POP_2016 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2016_Y2020M10D15.CSV"))
GBD_POP_2017 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2017_Y2020M10D15.CSV"))
GBD_POP_2018 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2018_Y2020M10D15.CSV"))
GBD_POP_2019 <-
  read_csv(file = here::here("GBD-dados", "IHME_GBD_2019_POP_2019_Y2020M10D15.CSV"))

# -----------------------------------------
# Combina e formata objetos de dados
# -----------------------------------------

GBD <- GBD_3 %>%
  bind_rows(GBD_4) %>%
  bind_rows(SEV) %>%
  mutate_at(c('cause'), ~replace_na(.,"All causes"))

rm(GBD_3, GBD_4, SEV) #Dont waste my precious RAM


GBD <- GBD %>%
  mutate(age = recode(age, "80-84" = "80-84 years",
                      "85-89" = "85-89 years",
                      "90-94" = "90-94 years"),
         year = as.factor(year))%>%
  mutate(age = as.factor(age)) %>%
  mutate(measure = recode(measure,
                          "YLDs (Years Lived with Disability)" = "YLDs",
                          "YLLs (Years of Life Lost)" = "YLLs",
                          "DALYs (Disability-Adjusted Life Years)" = "DALYs",
                          "Summary exposure value" = "SEV"),
         location = recode(location,
                           "Bolivia (Plurinational State of)" = "Bolivia",
                           "Venezuela (Bolivarian Republic of)" = "Venezuela"),
         val_cat = cut(x = GBD$val,
                       breaks = c(-Inf, 1, 10, 100, 500,
                                  1000, 2500, 5000, Inf),
                       labels = c("0 - 1",
                                  "1 - 10",
                                  "10 - 100",
                                  "100 - 500",
                                  "500 - 1000",
                                  "1000 - 2500",
                                  "2500 - 5000",
                                  ">5000")))%>% 
  mutate(age = recode(age,  "<5 years"="<5",
                            "5-9 years"="5-9",
                            "10-14 years"="10-14",
                            "15-19 years"="15-19",
                            "20-24 years"="20-24",
                            "25-29 years"="25-29",
                            "30-34 years"="30-34",
                            "35-39 years"="35-39",
                            "40-44 years"="40-44",
                            "45-49 years"="45-49",
                            "50-54 years"="50-54",
                            "55-59 years"="55-59",
                            "60-64 years"="60-64",
                            "65-69 years"="65-69",
                            "70-74 years"="70-74",
                            "75-79 years"="75-79",
                            "80-84 years"="80-84",
                            "85-89 years"="85-89",
                            "90-94 years"="90-94",
                            "95+ years"="95+")) %>% 
  mutate(age = factor(age, levels = c("<5",
                                      "5-9",
                                      "10-14",
                                      "15-19",
                                      "20-24",
                                      "25-29",
                                      "30-34",
                                      "35-39",
                                      "40-44",
                                      "45-49",
                                      "50-54",
                                      "55-59",
                                      "60-64",
                                      "65-69",
                                      "70-74",
                                      "75-79",
                                      "80-84",
                                      "85-89",
                                      "90-94",
                                      "95+",
                                      "All ages",
                                      "Age-standardized")))%>% 
  mutate(measure = factor(measure, levels = c("DALYs",
                                              "YLDs",
                                              "YLLs",
                                              "Deaths",
                                              "SEV")))


GBD_POP <- GBD_POP_1990 %>% 
  bind_rows(GBD_POP_1991,
            GBD_POP_1992,
            GBD_POP_1993,
            GBD_POP_1994,
            GBD_POP_1995,
            GBD_POP_1996,
            GBD_POP_1997,
            GBD_POP_1998,
            GBD_POP_1999,
            GBD_POP_2000,
            GBD_POP_2001,
            GBD_POP_2002,
            GBD_POP_2003,
            GBD_POP_2004,
            GBD_POP_2005,
            GBD_POP_2006,
            GBD_POP_2007,
            GBD_POP_2008,
            GBD_POP_2009,
            GBD_POP_2010,
            GBD_POP_2011,
            GBD_POP_2012,
            GBD_POP_2013,
            GBD_POP_2014,
            GBD_POP_2015,
            GBD_POP_2016,
            GBD_POP_2017,
            GBD_POP_2018,
            GBD_POP_2019) %>% 
  rename(location = location_name,
         sex = sex_name,
         age = age_group_name,
         year = year_id,
         pop = val) %>%
  select(location, sex, age, year, pop) %>%
  mutate(location = recode(location,
                           "Bolivia (Plurinational State of)" = "Bolivia",
                           "Venezuela (Bolivarian Republic of)" = "Venezuela"),
         sex = recode(sex,
                      "both" = "Both",
                      "male" = "Male",
                      "female" = "Female"),
         age = as.factor(age),
         year = as.factor(year))%>%
  filter(location %in% c("Argentina",
                         "Bolivia",
                         "Brazil",
                         "Chile",
                         "Colombia",
                         "Ecuador",
                         "Guyana",
                         "Paraguay",
                         "Peru",
                         "Suriname",
                         "Uruguay",
                         "Venezuela"),
         sex == "Both") %>%
  mutate(age = recode(age,  "Under 5"="<5",
                            "5 to 9"="5-9",
                            "10 to 14"="10-14",
                            "15 to 19"="15-19",
                            "20 to 24"="20-24",
                            "25 to 29"="25-29",
                            "30 to 34"="30-34",
                            "35 to 39"="35-39",
                            "40 to 44"="40-44",
                            "45 to 49"="45-49",
                            "50 to 54"="50-54",
                            "55 to 59"="55-59",
                            "60 to 64"="60-64",
                            "65 to 69"="65-69",
                            "70 to 74"="70-74",
                            "75 to 79"="75-79",
                            "80 to 84"="80-84",
                            "85 to 89"="85-89",
                            "90 to 94"="90-94",
                            "95 plus"="95+",
                      "All Ages" = "All ages")) %>%
  filter(age %in% c("<5",
                    "5-9",
                    "10-14",
                    "15-19",
                    "20-24",
                    "25-29",
                    "30-34",
                    "35-39",
                    "40-44",
                    "45-49",
                    "50-54",
                    "55-59",
                    "60-64",
                    "65-69",
                    "70-74",
                    "75-79",
                    "80-84",
                    "85-89",
                    "90-94",
                    "95+",
                    "All ages")) %>% 
  mutate(age = factor(age, levels = c("<5",
                                      "5-9",
                                      "10-14",
                                      "15-19",
                                      "20-24",
                                      "25-29",
                                      "30-34",
                                      "35-39",
                                      "40-44",
                                      "45-49",
                                      "50-54",
                                      "55-59",
                                      "60-64",
                                      "65-69",
                                      "70-74",
                                      "75-79",
                                      "80-84",
                                      "85-89",
                                      "90-94",
                                      "95+",
                                      "All ages")))

rm(GBD_POP_1990,
   GBD_POP_1991,
   GBD_POP_1992,
   GBD_POP_1993,
   GBD_POP_1994,
   GBD_POP_1995,
   GBD_POP_1996,
   GBD_POP_1997,
   GBD_POP_1998,
   GBD_POP_1999,
   GBD_POP_2000,
   GBD_POP_2001,
   GBD_POP_2002,
   GBD_POP_2003,
   GBD_POP_2004,
   GBD_POP_2005,
   GBD_POP_2006,
   GBD_POP_2007,
   GBD_POP_2008,
   GBD_POP_2009,
   GBD_POP_2010,
   GBD_POP_2011,
   GBD_POP_2012,
   GBD_POP_2013,
   GBD_POP_2014,
   GBD_POP_2015,
   GBD_POP_2016,
   GBD_POP_2017,
   GBD_POP_2018,
   GBD_POP_2019) #Dont waste my precious RAM


SDI <- SDI %>% 
  filter(Location %in% c("Argentina",
                         "Bolivia",
                         "Brazil",
                         "Chile",
                         "Colombia",
                         "Ecuador",
                         "Guyana",
                         "Paraguay",
                         "Peru",
                         "Suriname",
                         "Uruguay",
                         "Venezuela"))

SDI_pivot <- SDI %>%
  pivot_longer(cols = c("1990",
                        "1991",
                        "1992",
                        "1993",
                        "1994",
                        "1995",
                        "1996",
                        "1997",
                        "1998",
                        "1999",
                        "2000",
                        "2001",
                        "2002",
                        "2003",
                        "2004",
                        "2005",
                        "2006",
                        "2007",
                        "2008",
                        "2009",
                        "2010",
                        "2011",
                        "2012",
                        "2013",
                        "2014",
                        "2015",
                        "2016",
                        "2017",
                        "2018",
                        "2019"),
               names_to = "year",
               values_to = "SDI")

SEV_ROC <- SEV_ROC %>% 
  mutate(cause = "All causes")

GBD_ROC_1990_to_2019 <- GBD_ROC %>%
  rbind(SEV_ROC) %>% 
  mutate(measure = recode(measure,
                          "YLDs (Years Lived with Disability)" = "YLDs",
                          "YLLs (Years of Life Lost)" = "YLLs",
                          "DALYs (Disability-Adjusted Life Years)" = "DALYs",
                          "Summary exposure value" = "SEV")) %>%
  mutate(location = recode(location,
                           "Bolivia (Plurinational State of)" = "Bolivia",
                           "Venezuela (Bolivarian Republic of)" = "Venezuela")) %>%
  mutate(year_end = as.factor(year_end)) %>%
  select(-c("rei", "year_start", "year_end")) %>%
  rename(ROC_val = val,
         ROC_upper = upper,
         ROC_lower = lower) %>% 
  mutate(measure = factor(measure, levels = c("DALYs",
                                              "YLDs",
                                              "YLLs",
                                              "Deaths",
                                              "SEV")))


rm(SEV_ROC, GBD_ROC)

SA_map <- ne_countries(scale = "medium",
                       type = "map_units",
                       returnclass = "sf") %>%
  filter(name %in% c("Argentina",
                     "Bolivia",
                     "Brazil",
                     "Chile",
                     "Colombia",
                     "Ecuador",
                     "Guyana",
                     "Paraguay",
                     "Peru",
                     "Suriname",
                     "Uruguay",
                     "Venezuela"))



GBD <- GBD %>%
  full_join(SDI_pivot, by = c("location" = "Location", "year" = "year")) %>%
  full_join(GBD_POP, by=c("location"="location",
                          "age"="age",
                          "sex"="sex",
                          "year"="year")) 

rm(SDI_pivot)

SA_SUM <- GBD %>%
  filter(metric == "Number",age != "Age-standardized") %>%
  group_by(measure,sex,age,cause,year) %>%
  summarise(number = sum(val), pop = sum(pop)) %>%
  mutate(rate = (number/pop)*100000)


SA_age_stand <- GBD %>% select(-c(pop)) %>% 
  filter(metric == "Rate",age == "Age-standardized", cause == "All causes") %>% 
  full_join(GBD_POP %>% filter(age == "All ages") %>% 
                        select(-c(sex, age)), 
            by=c("location"="location",
                          "year"="year")) %>% 
  group_by(measure, year) %>% 
  summarise(val = weighted.mean(val, pop))

