# -----------------------------------------
# Gráfico da América do Sul (dados demograficos)
# -----------------------------------------

df <- SA_SUM %>% 
      filter(measure == "DALYs",
             cause == "All causes") %>% 
      mutate(pop_mil = pop/1000000,
             year = as.numeric(year))

p <- ggplot(data = df %>% filter(age != "All ages"),
            mapping = aes(x = age, y = pop_mil,
                          color = year, group = year)) +
  geom_line() + geom_point() +
  scale_color_viridis_b(direction = -1, n.breaks = 29)+
  labs(color = "Measure", y = "Population (in millions)", x = "Year") +
  theme_bw() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 6))

p

ggsave(filename = "pop_years_age_group_sa.jpg",
       plot = p, path = here::here("Figures"),
       width = 10, height = 7, dpi = 300)

p <- ggplot(data = df %>% filter(age == "All ages"),
            mapping = aes(x = year, y = pop_mil,
                          fill = year)) +
      geom_col()+
      theme_bw() +
      coord_cartesian(ylim = c(200,500), xlim = c(1990,2019))+
      labs(color = "Year", y = "Population (in millions)", x = "Year") +
      scale_fill_viridis_c(direction = -1)
  
p
ggsave(filename = "pop_years_all_ages_sa.jpg",
       plot = p, path = here::here("Figures"),
       width = 10, height = 7, dpi = 300)
# -----------------------------------------
# Gráfico da América do Sul (Medidas em número)
# -----------------------------------------

df <- SA_SUM %>%
  filter(sex %in% "Both") %>%
  filter(age %in% "All ages") %>%
  filter(cause == "All causes") %>% 
  mutate(number_millions = number/1000000)

p <- ggplot(data = df,
            mapping = aes(x = year, y = number_millions,
                          color = measure, group = measure)) +
  geom_line() + geom_point() +
  scale_color_brewer(palette="Dark2") +
  labs(color = "Measure", y = "Number (in millions)", x = "Year") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

p

ggsave(filename = "measures_numbers_years_sa.jpg",
       plot = p, path = here::here("Figures"),
       width = 10, height = 7, dpi = 300)

# -----------------------------------------
# Gráfico da América do Sul (Medidas em taxa)
# -----------------------------------------

p <- ggplot(data = df,
            mapping = aes(x = year, y = rate,
                          color = measure, group = measure)) +
  geom_line() + geom_point() +
  scale_color_brewer(palette="Dark2") +
  labs(#caption = "Rate of DALYs, YLDs, YLLs in South America",
    color = "Measure", y = "Rate (100,000)", x = "Year") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

p

ggsave(filename = "measures_rate_years_sa.jpg",
       plot = p, path = here::here("Figures"),
       width = 10, height = 7, dpi = 300)
# -----------------------------------------
# Gráfico da América do Sul por faixa etaria (Medidas em numeros)
# -----------------------------------------

df <- SA_SUM %>%
  filter(sex %in% "Both") %>%
  filter(age != "All ages") %>% 
  filter(cause == "All causes") %>%
  mutate(year = as.numeric(year),
         number_mil = number/1000000) #só assim fiz a escala ficar bonitinha

p <- ggplot(data = df,
            mapping = aes(x = age, y = number_mil,
                          color = year, group = year)) +
  geom_line() + #geom_point() +
  labs(color = "Year", y = "number(in millions)", x = "Age group") +
  theme_bw() +
  scale_color_viridis_b(direction = -1,
                        n.breaks = 29)+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 6)) +
  facet_wrap( ~ measure,  scales = "free_y")
p

#falta conseguir colocar os extremos (1990-2019), mas já ta legal

ggsave(filename = "measures_number_age_groups_years_sa.jpg",
       plot = p, path = here::here("Figures"),
       width = 10, height = 7, dpi = 300)
# -----------------------------------------
# Gráfico da América do Sul por faixa etaria (Medidas em taxas)
# -----------------------------------------

df <- SA_SUM %>%
  filter(sex %in% "Both") %>%
  filter(age != "All ages") %>% 
  filter(cause == "All causes") %>% 
  mutate(year = as.numeric(year)) #só assim fiz a escala ficar bonitinha

p <- ggplot(data = df,
            mapping = aes(x = age, y = rate,
                          color = year, group = year)) +
  geom_line() + geom_point() +
  labs(color = "Year", y = "Rate (100,000)", x = "Age group") +
  theme_bw() +
  scale_color_viridis_b(direction = -1,
                        n.breaks = 29)+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 6)) +
  facet_wrap( ~ measure,  scales = "free_y")
p

#falta conseguir colocar os extremos (1990-2019), mas já ta legal

ggsave(filename = "measures_rates_age_groups_years_sa.jpg",
       plot = p, path = here::here("Figures"),
       width = 10, height = 7, dpi = 300)

# -----------------------------------------
# Gráfico da América do Sul (rate of change)
# -----------------------------------------


df <- GBD_ROC_1990_to_2019 %>% 
  filter(age == "Age-standardized") %>% 
  filter(cause == "All causes") %>% 
  filter(metric == "Rate")

p <- ggplot(data = df,
            mapping = aes(x = location,
                          y = ROC_val,
                          fill = measure)) +
  geom_bar(position = position_dodge(.9), stat = "identity") +
  labs(x = "Country", y = "Annualized Rate of Change (%) from 1990 to 2019",
       fill = "Measure") +
  ylim(c(-0.5, 1.5))+
  theme_bw() +
  scale_fill_viridis_d()+
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 45, hjust = 1))

p
