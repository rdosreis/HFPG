# ---------------------------------------
# Carrega pacotes
# ---------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)

# ---------------------------------------
# Tabela 1
# ---------------------------------------

tab1 <- read_csv(
  file = here::here("resultados-2021", "Tab1-P1 - IHME-GBD_2021_DATA-dbd1aedb-1.csv"))
# View(tab_1)

tab1_sev <- read_csv(
  file = here::here("resultados-2021", "Tab1-P2 - IHME-GBD_2021_DATA-77ee89a9-1.csv"))

tab1 <- bind_rows(tab1 %>% select(-cause), tab1_sev) 

tab1 <- tab1 %>% 
  select(measure, location, val, lower, upper)

tab1$location <- factor(tab1$location,
                        levels = c("Argentina",
                                   "Bolivia (Plurinational State of)",
                                   "Brazil",
                                   "Chile",
                                   "Colombia",
                                   "Ecuador",
                                   "Guyana",
                                   "Paraguay",
                                   "Peru",
                                   "Suriname",
                                   "Uruguay",
                                   "Venezuela (Bolivarian Republic of)"),
                        labels = c("Argentina",
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


tab1$measure <- factor(tab1$measure,
                       levels = c("Summary exposure value",
                                  "Deaths",
                                  "YLLs (Years of Life Lost)",
                                  "YLDs (Years Lived with Disability)",
                                  "DALYs (Disability-Adjusted Life Years)"),
                       labels = c("SEV", "Deaths", "YLLs", "YLDs", "DALYs"))


tab1 <- tab1 %>% 
  mutate(val = round(val, 1),
         lower = round(lower, 1),
         upper = round(upper, 1))

tab1$val_lu <- paste0(tab1$val, " (", tab1$lower, " to ", tab1$upper, ")")


tab1 <- tab1 %>% 
  select(measure, location, val_lu) %>% 
  pivot_wider(
    names_from = measure,
    values_from = c(val_lu)
  )

tab1 <- tab1 %>% 
  arrange(location) %>% 
  select(location, SEV, Deaths, YLLs, YLDs, DALYs)

writexl::write_xlsx(x = tab1,
                    path = here::here("output-resultados-2021", "Tabela1.xlsx"))

# ---------------------------------------
# Figura 2
# ---------------------------------------

fig2 <- read_csv(file = here::here("resultados-2021",
                                   "Fig2-P1 -IHME-GBD_2021_DATA-76c61bdc-1.csv"))

fig2_sev <- read_csv(file = here::here("resultados-2021",
                                       "Fig2-P2 - IHME-GBD_2021_DATA-5da96ab0-1.csv"))

fig2 <- bind_rows(fig2 %>% select(-cause), fig2_sev) 

fig2 <- fig2 %>% 
  select(measure, location, val, lower, upper)

fig2$location <- factor(fig2$location,
                        levels = c("Argentina",
                                   "Bolivia (Plurinational State of)",
                                   "Brazil",
                                   "Chile",
                                   "Colombia",
                                   "Ecuador",
                                   "Guyana",
                                   "Paraguay",
                                   "Peru",
                                   "Suriname",
                                   "Uruguay",
                                   "Venezuela (Bolivarian Republic of)"),
                        labels = c("Argentina",
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


fig2$measure <- factor(fig2$measure,
                       levels = c("Summary exposure value",
                                  "Deaths",
                                  "YLLs (Years of Life Lost)",
                                  "YLDs (Years Lived with Disability)",
                                  "DALYs (Disability-Adjusted Life Years)"),
                       labels = c("SEV", "Deaths", "YLLs", "YLDs", "DALYs"))

p <- ggplot(data = fig2,
            mapping = aes(x = location, y = val,
                          fill = location)) +
  geom_bar(position = position_dodge(.9), stat = "identity") +
  geom_errorbar(mapping = aes(ymin = lower, ymax = upper),
                size = 0.5,
                width = 0.5,
                position = position_dodge(.9)) +
  scale_fill_brewer(palette = "Set3") +
  facet_grid(~measure) +
  labs(x = "Country", y = "Annualized Rate of Change (%) from 1990 to 2021",
       fill = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
p

ggsave(filename = "Fig2-annual-rate-change-1990-2021.jpg",
       plot = p, path = here::here("output-resultados-2021"),
       width = 14, height = 7, dpi = 300)

# ---------------------------------------
# Figura 3
# ---------------------------------------

fig3 <- read_csv(file = here::here("resultados-2021",
                                   "Fig3 - IHME_GBD_SDI_2021_SDI_1950_2021_Y2024M05D16.csv"))

fig3 <- fig3 %>% 
  filter(year_id == 2021,
         location_name %in% c("Argentina",
                              "Bolivia (Plurinational State of)",
                              "Brazil",
                              "Chile",
                              "Colombia",
                              "Ecuador",
                              "Guyana",
                              "Paraguay",
                              "Peru",
                              "Suriname",
                              "Uruguay",
                              "Venezuela (Bolivarian Republic of)"))

fig3 <- fig3 %>% 
  select(location_name, mean_value)

names(fig3)[2] <- "SDI"

fig3$location <- factor(fig3$location_name,
                        levels = c("Argentina",
                                   "Bolivia (Plurinational State of)",
                                   "Brazil",
                                   "Chile",
                                   "Colombia",
                                   "Ecuador",
                                   "Guyana",
                                   "Paraguay",
                                   "Peru",
                                   "Suriname",
                                   "Uruguay",
                                   "Venezuela (Bolivarian Republic of)"),
                        labels = c("Argentina",
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
df_fig3 <- fig2 %>% 
  left_join(fig3, by = "location")

p <- ggplot(data = df_fig3,
            mapping = aes(x = SDI, y = val,
                          color = location)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = location, angle = 25, hjust = 0.5, vjust = 1),
            size = 2, color = "black") +
  scale_color_brewer(palette = "Set3") +
  facet_wrap(~measure) +
  labs(y = "Rate of change (%)",
       x = "SDI", color = "Country") +
  theme_bw() +
  theme(legend.position = "none")
p


ggsave(filename = "Fig3-SDI-measures-2021.jpg",
       plot = p, path = here::here("output-resultados-2021"),
       width = 14, height = 7, dpi = 300)

ps <- p + geom_smooth(mapping = aes(color = NULL),
                      method = "loess", se = FALSE, color = "red", size = 0.5)

ggsave(filename = "Fig3-SDI-measures-2021-smooth.jpg",
       plot = ps, path = here::here("output-resultados-2021"),
       width = 14, height = 7, dpi = 300)

# ---------------------------------------
# Figura 4
# ---------------------------------------

# sugar swleetened bev, red Meat, whole grains, processed meat, low Fruit,high BP,low PA, high BMI, smoking
fig4 <- read_csv(file = here::here("resultados-2021",
                                       "Fig4 - IHME-GBD_2021_DATA-393cc592-1.csv"))

fig4 <- fig4 %>% 
  select(location, year, rei, val, lower, upper)

fig4$location <- factor(fig4$location,
                        levels = c("Argentina",
                                   "Bolivia (Plurinational State of)",
                                   "Brazil",
                                   "Chile",
                                   "Colombia",
                                   "Ecuador",
                                   "Guyana",
                                   "Paraguay",
                                   "Peru",
                                   "Suriname",
                                   "Uruguay",
                                   "Venezuela (Bolivarian Republic of)"),
                        labels = c("Argentina",
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


fig4 <- fig4 %>% 
  right_join(fig4 %>% filter(year == 1990), by = c("location", "rei"))

# fig4 %>% arrange(location, rei, year.x)

fig4 <- fig4 %>% 
  mutate(PERCH = (( (val.x / val.y) - 1) * 100 ))

fig4$rei <- factor(fig4$rei)

# df <- SEV_perch %>% 
#   filter(rf %in% c(
#     "Diet high in sugar-sweetened beverages",
#     "Diet high in red meat",
#     "Diet low in whole grains",
#     "Diet low in fruits",
#     "High fasting plasma glucose",
#     "High body-mass index",
#     "High systolic blood pressure",
#     "Diet high in processed meat",
#     "Low physical activity"
#   )) %>% 
#   filter(age == "Age-standardized")

p <- ggplot(data = fig4 %>%
             filter(rei != "High fasting plasma glucose"),
           mapping = aes(x = year.x, y = PERCH, color = rei)) +
  geom_line() +
  geom_line(data = fig4 %>%
              filter(rei == "High fasting plasma glucose"),
            mapping = aes(x = year.x, y = PERCH, color = rei),
                          size = 1.5) +
  scale_x_continuous(breaks = c(seq(1990, 2021, by = 4), 2021)) +
  scale_color_manual(values=c("#ff7f00",
                              "#ff1694",
                              "#0744ab",
                              "#2eff2e",
                              "#fdbf6f",
                              "#6a3d9a",
                              "#006400",
                              "#696969",
                              "#2ab3f3",
                              "#e31a1c")) +
  facet_wrap(vars(location)) +
  labs(x = "Year", y= "SEV annual relative change (%)",
       color = "Risk Factor") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        axis.text.x = element_text(hjust = 1, angle = 45, size = 8))

p

ggsave(filename = "Fig4-annal-rate-SEV-change-1990-2021.jpg",
       plot = p, path = here::here("output-resultados-2021"),
       width = 14, height = 7, dpi = 300)
