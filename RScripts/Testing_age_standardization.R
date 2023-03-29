
testing <- read_csv("GBD-dados/IHME-GBD_2019_DATA-7f33c71b-1.csv")
POP2019 <- read_csv("GBD-dados/IHME_GBD_2019_POP_2019_Y2020M10D15.CSV")

POPs2019 <- POP2019 %>% 
           rename(sex = sex_name,
                  age = age_group_name,
                  location = location_name,
                  pop = val) %>% 
           select(c(location, sex, age, pop)) %>% 
           mutate(sex = recode(sex, "male" = "Male",
                               "female" = "Female"),
                  age = recode(age, "All Ages" = "All ages")) %>% 
           filter(sex != "both", age == "All ages")


test <- testing %>% 
  inner_join(POPs2019, by = c("sex" = "sex", "location" = "location"))


SUM <- test %>% 
      filter(location != "Tropical Latin America", sex == "Female")


SUM <- SUM %>% 
       summarise(val = weighted.mean(val, pop))

#Testando se eu sei usar o weighted.mean
Prova <- c("1", "2", "3", "4")
Peso <- c(60, 20, 10, 10)
nota <- c(10, 5, 5, 10)

df <- data.frame(Prova, Peso, nota)
df <- df %>% 
      summarise(nota = weighted.mean(nota, Peso))
#pelos meus calculos, funciona...
#e agora, deus?