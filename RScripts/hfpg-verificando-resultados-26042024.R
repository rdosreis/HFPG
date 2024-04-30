library(readr)
library(dplyr)

tab_1 <- read_csv("~/PPGEpi/GBD/HFPG/Verificando-dados/IHME-GBD_2019_DATA-c3385f0a-1.csv")
View(tab_1)

tab_1_sev <- read_csv("~/PPGEpi/GBD/HFPG/Verificando-dados/IHME-GBD_2019_DATA-ff964a75-1.csv")

tab_1_aux <- tab_1 %>% 
  select(measure, location, val, lower, upper)

tab_1_aux <- tab_1_aux %>% 
  mutate(val = round(val, 1),
         lower = round(lower, 1),
         upper = round(upper, 1))

tab_1_aux <- tab_1_sev %>% 
  select(measure, location, val, lower, upper)

tab_1_aux <- tab_1_aux %>% 
  mutate(val = round(val, 1),
         lower = round(lower, 1),
         upper = round(upper, 1))



fig_2 <- read_csv("Verificando-dados/IHME-GBD_2019_DATA-b2711f44-1.csv")

fig_2_aux <- fig_2 %>% 
  select(measure, location, val, lower, upper)

fig_2_aux <- fig_2_aux %>% 
  mutate(val = round(val, 1),
         lower = round(lower, 1),
         upper = round(upper, 1))

library(ggplot2)

p <- ggplot(data = fig_2, mapping = aes(x = location, y = val, fill = location)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(mapping = aes(ymin = lower, ymax = upper)) +
  facet_grid(~measure) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none")

p
