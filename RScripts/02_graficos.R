# -----------------------------------------
# Gráfico da América do Sul (dados demograficos)
# -----------------------------------------



# graficos populacionais
# piramides SA e cada país AMBOS os SEXOS(piramide clássica)
# tanto em pop total quanto em % da pop


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
  labs(color = "Measure", y = "Population (in millions)", x = "Age-range",
       title = "Age distribution between 1990-2019 in South America") +
  theme_bw() +
  coord_flip()+
  theme(legend.position = "right",
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 6))

p

ggsave(filename = "01_pop_years_age_group_sa.jpg",
       plot = p, path = here::here("Figures"),
       width = 7, height = 10, dpi = 300)

#---------------------------------------------- (eu sei que era pra esperar o prof Rodrigo, mas não me aguentei)

df <- SA_POP_SUM %>% 
  filter(sex != "Both", age != "All ages") %>% 
  mutate(pop_mil = pop/1000000,
         year = as.numeric(as.character(year)))

p <- ggplot(data = df,
            mapping = aes(x = age, y = pop_mil,
                          color = year, group = year)) +
  geom_point(data=subset(df,sex=="Female"),aes(y=(pop_mil*-1))) +
  geom_line(data=subset(df,sex=="Female"),aes(y=(pop_mil*-1))) +
  geom_point(data=subset(df,sex=="Male")) +
  geom_line(data=subset(df,sex=="Male")) +
  scale_color_viridis_b(direction = -1, n.breaks = 29)+
  labs(color = "Measure", y = "Population (in millions)", x = "Age-range",
       title = "Age distribution between 1990-2019 in South America") +
  theme_bw() +
  scale_y_continuous(breaks=seq(-20,20,10),labels=abs(seq(-20,20,10))) + 
  coord_flip()+
  theme(legend.position = "right",
        legend.key.height = unit(2, "cm"),
        legend.text = element_text(size = 6))

p

ggsave(filename = "01.1_pop_years_age_group_sa_male_female.jpg",
       plot = p, path = here::here("Figures"),
       width = 12, height = 10, dpi = 300)


#----------------------------------------------ESSE AQUI

df <- SA_POP_SUM %>%
  filter(!(sex %in% "Both")) %>%
  filter(!(age %in% "All ages")) %>%
  filter(year %in% c("1990", 
                     "2019")) %>% 
  mutate(pop_mil = pop/1000000)

p <- ggplot(df, mapping = aes(x = age, y = pop_mil)) +
  geom_col(data = df %>% filter(sex == "Female", year == "1990"), aes(y = pop_mil*-1),
           alpha = 0.5, fill = "red", width = 0.5) +
  geom_col(data = df %>% filter(sex == "Female", year == "2019"), aes(y = pop_mil*-1),
           alpha = 0.5, fill = "red") +
  geom_col(data = df %>% filter(sex == "Male", year == "1990"),
           alpha = 0.5, fill = "blue", width = 0.5) +  
  geom_col(data = df %>% filter(sex == "Male", year == "2019"),
           alpha = 0.5, fill = "blue") +
  scale_y_continuous(breaks=seq(-20,20,10),labels=abs(seq(-20,20,10))) + 
  scale_fill_hue(direction = 1) +
  coord_flip() +
  labs(y = "Population in millions", x = "Age group")+
  theme_bw()
p 

ggsave(filename = "01.2_pop_years_age_group_sa_male_female.jpg",
       plot = p, path = here::here("Figures"),
       width = 12, height = 10, dpi = 300)

#----------------------------------------------pra cada país

df <- GBD_POP %>%
  filter(!(sex %in% "Both")) %>%
  filter(!(age %in% "All ages")) %>%
  filter(year %in% c("1990", 
                     "2019"))

df1 <- GBD_POP %>% 
      filter(sex == "Both", age == "All ages",
             year %in% c("1990", 
                         "2019"))
df <- df %>% 
  inner_join(df1,
             by = c("year" = "year", "location" = "location")) %>% 
  mutate(pop_percent = (pop.x/pop.y)*100)

p <- ggplot(df, mapping = aes(x = age.x, y = pop_percent)) +
  geom_col(data = df %>% filter(sex.x == "Female", year == "1990"), aes(y = pop_percent*-1),
           alpha = 0.5, fill = "red", width = 0.5) +
  geom_col(data = df %>% filter(sex.x == "Female", year == "2019"), aes(y = pop_percent*-1),
           alpha = 0.5, fill = "red") +
  geom_col(data = df %>% filter(sex.x == "Male", year == "1990"),
           alpha = 0.5, fill = "blue", width = 0.5) +  
  geom_col(data = df %>% filter(sex.x == "Male", year == "2019"),
           alpha = 0.5, fill = "blue") +
  scale_y_continuous(breaks=seq(-10,10,5),labels=abs(seq(-10,10,5))) + 
  scale_fill_hue(direction = 1) +
  coord_flip() +
  facet_wrap(vars(location))+
  labs(y = "Population(%)", x = "Age group")+
  theme_bw()
p 

ggsave(filename = "01.3_pop_years_age_group_male_female_by_countries.jpg",
       plot = p, path = here::here("Figures"),
       width = 12, height = 10, dpi = 300)
rm(df1)


#----------------------------------------
df <- SA_SUM %>% 
  filter(measure == "DALYs",
         cause == "All causes") %>% 
  mutate(pop_mil = pop/1000000,
         year = as.numeric(year))

p <- ggplot(data = df %>% filter(age == "All ages"),
            mapping = aes(x = year, y = pop_mil,
                          fill = year, label = round(pop_mil, digits = 2))) +
      geom_col()+
      theme_bw() +
      geom_text(aes(y = pop_mil + 15), angle = 90)+
      coord_cartesian(ylim = c(200,500), xlim = c(1990,2019))+
      labs(color = "Year", y = "Population (in millions)", x = "Year",
           title = "Population in South America between 1990-2019") +
      scale_fill_viridis_c(direction = -1)

  
  
p

ggsave(filename = "02_pop_years_all_ages_sa.jpg",
       plot = p, path = here::here("Figures"),
       width = 10, height = 7, dpi = 300)

#AJEITAR A ESCALA


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
                          color = measure, group = measure,
                          label = round(number_millions, digits = 2))) +
  geom_line() + geom_point() +
  scale_color_brewer(palette="Dark2") +
  labs(color = "Measure", y = "Number (in millions)", x = "Year",
       title = "Raw number of DALYs, YLDs, YLLs and deaths in South America since 1990") +
  theme_bw() +
  geom_text_repel(data = df %>% filter(year=="1990"),
                  aes(y = number_millions + 0.4)) +
  geom_text_repel(data = df %>% filter(year=="2019"),
                  aes(y = number_millions + 0.4)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

p

ggsave(filename = "03_measures_numbers_years_sa.jpg",
       plot = p, path = here::here("Figures"),
       width = 10, height = 7, dpi = 300)

# -----------------------------------------
# Gráfico da América do Sul (Medidas em taxa)
# -----------------------------------------
df <- SA_SUM %>%
  filter(sex %in% "Both") %>%
  filter(age %in% "All ages") %>%
  filter(cause == "All causes") %>% 
  mutate(number_millions = number/1000000)

p <- ggplot(data = df,
            mapping = aes(x = year, y = rate,
                          color = measure, group = measure,
                          label = round(rate, digits = 2))) +
  geom_line() + geom_point() +
  scale_color_brewer(palette="Dark2") +
  labs(#caption = "Rate of DALYs, YLDs, YLLs in South America",
    color = "Measure", y = "Rate (100,000)(All ages)", x = "Year",
    title = "Rate of DALYs, YLDs, YLLs and Deaths in South America without age-standardizing") +
  theme_bw() +
  geom_text_repel(data = df %>% filter(year=="1990"),
                  aes(y = rate + 100)) +
  geom_text_repel(data = df %>% filter(year=="2019"),
                  aes(y = rate + 100)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

p

ggsave(filename = "04_measures_rate_years_sa.jpg",
       plot = p, path = here::here("Figures"),
       width = 10, height = 7, dpi = 300)
# -----------------------------------------
# Gráfico da América do Sul (Medidas em taxa)
# AGE STANDARDIZED (Rate age-stand de cada pais com media ponderada pelas pop do ano)
# -----------------------------------------
df <- SA_age_stand


p <- ggplot(data = df,
            mapping = aes(x = year, y = val,
                          color = measure, group = measure,
                          label = round(val, digits = 2))) +
  geom_line() + geom_point() +
  scale_color_brewer(palette="Dark2") +
  labs(#caption = "Rate of DALYs, YLDs, YLLs in South America",
    color = "Measure", y = "Rate (100,000)(Age standardized)", x = "Year",
    title = "Age-standardized rate of DALYs, YLDs, YLLs and Deaths in South America") +
  theme_bw() +
  geom_text_repel(data = df %>% filter(year=="1990"),
                  aes(y = val + 100), size = 3) +
  geom_text_repel(data = df %>% filter(year=="2019"),
                  aes(y = val + 100), size = 3) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

p

ggsave(filename = "04.1_measures_rate_AGE_STANDARDIZED_years_sa.jpg",
       plot = p, path = here::here("Figures"),
       width = 10, height = 7, dpi = 300)


# -----------------------------------------
# Junção do 4 e 4.1 conforme orientação do prof Bruce
# -----------------------------------------
df <- SA_SUM %>%
  filter(sex %in% "Both") %>%
  filter(age %in% "All ages") %>%
  filter(cause == "All causes") %>% 
  mutate(number_millions = number/1000000)

p1 <- ggplot(data = df,
            mapping = aes(x = year, y = rate,
                          color = measure, group = measure,
                          label = round(rate, digits = 2))) +
  geom_line() + geom_point() +
  scale_color_brewer(palette="Dark2") +
  labs(#caption = "Rate of DALYs, YLDs, YLLs in South America",
    color = "Measure", y = "", x = "Year",
    title = "") +
  theme_bw() +
  coord_cartesian(ylim = c(0,2500))+
  geom_text_repel(data = df %>% filter(year=="1990"),
                  aes(y = rate + 100)) +
  geom_text_repel(data = df %>% filter(year=="2019"),
                  aes(y = rate + 100)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

p1

df <- SA_age_stand


p2 <- ggplot(data = df,
            mapping = aes(x = year, y = val,
                          color = measure, group = measure,
                          label = round(val, digits = 2))) +
  geom_line() + geom_point() +
  scale_color_brewer(palette="Dark2") +
  labs(#caption = "Rate of DALYs, YLDs, YLLs in South America",
    color = "Measure", y = "Rate per 100.000 population", x = "Year",
    title = "") +
  theme_bw() +
  coord_cartesian(ylim = c(0,2500))+
  geom_text_repel(data = df %>% filter(year=="1990"),
                  aes(y = val + 100)) +
  geom_text_repel(data = df %>% filter(year=="2019"),
                  aes(y = val + 100)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

p2

plot_row <- plot_grid(p2, p1, labels = c('Age-standardized', 'All ages'),
          label_size = 12,
          align = "h")

title <- ggdraw() + 
  draw_label("Rate of DALYs, YLDs, YLLs, Deaths and SEV in South America from 1990 to 2019",
    fontface = 'bold',
    x = 0,
    hjust = 0) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 10))

f <-plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1))

ggsave(filename = "04.2_AS_AA_1990_2019.jpg",
       plot = f, path = here::here("Figures"),
       width = 15, height = 7, dpi = 300)

ggsave(filename = "04.2_AS_AA_1990_2019.svg",
       plot = f, path = here::here("Figures"),
       width = 15, height = 7, dpi = 300)
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
  geom_line() + geom_point() +
  labs(color = "Year", y = "number(in millions)", x = "Age group",
       title = "Number of DALYs, YLDs, YLLs and Deaths in South America by age groups since 1990") +
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

ggsave(filename = "05_measures_number_age_groups_years_sa.jpg",
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
  labs(color = "Year", y = "Rate (100,000)", x = "Age group",
       title = "Rate of DALYs, YLDs, YLLs and Deaths in South America by age groups since 1990") +
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

ggsave(filename = "06_measures_rates_age_groups_years_sa.jpg",
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
  geom_errorbar(aes(ymin = ROC_lower, ymax = ROC_upper),
                size = 0.5,
                width = 0.5,
                position = position_dodge(.9)) +
  labs(x = "Country", y = "Annualized Rate of Change (%) from 1990 to 2019",
       fill = "", caption = "Age-standardized",
       title = "Annualized Rate of Change (%) of DALYs, YLDs, YLLs, Deaths and SEV from 1990 to 2019 in South America countries") +
  theme_bw() +
  scale_fill_viridis_d()+
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 45, hjust = 1))

p

ggsave(filename = "07_ROC_by_countries_1990_2019.jpg",
       plot = p, path = here::here("Figures"),
       width = 12, height = 7, dpi = 300)

# -----------------------------------------
# Gráfico da América do Sul (rate of change) AFTER BRUCE
# -----------------------------------------


df <- GBD_ROC_1990_to_2019 %>% 
  filter(age == "Age-standardized") %>% 
  filter(cause == "All causes") %>% 
  filter(metric == "Rate") %>%
  mutate(measure=fct_relevel(measure,c("SEV","YLDs","YLLs","DALYs","Deaths"))) %>%
  arrange(measure)

p <- ggplot(data = df,
            mapping = aes(x = location,
                          y = ROC_val,
                          fill = location)) +
  geom_bar(position = position_dodge(.9), stat = "identity") +
  geom_errorbar(aes(ymin = ROC_lower, ymax = ROC_upper),
                size = 0.5,
                width = 0.5,
                position = position_dodge(.9)) +
  labs(x = "Country", y = "Annualized Rate of Change (%) from 1990 to 2019",
       fill = "", caption = "Age-standardized",
       title = "Annualized Rate of Change (%) of DALYs, YLDs, YLLs, Deaths and SEV from 1990 to 2019 in South America countries") +
  theme_bw() +
  facet_grid(~ measure)+
  scale_fill_viridis_d()+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

p

ggsave(filename = "07.1_ROC_by_countries_1990_2019_afterBRUCE.jpg",
       plot = p, path = here::here("Figures"),
       width = 12, height = 7, dpi = 300)

# -----------------------------------------
# Rate of measures each country from 1990 to 2019
# -----------------------------------------
df <- GBD %>%
  filter(age == "Age-standardized",
         cause == "All causes",
         metric == "Rate") %>%
#  filter(location %in% c("Guyana", "Suriname"))
  mutate(year = as.numeric(year))

p1 <- ggplot() +
  geom_line(data = df %>% filter(!(location %in% c("Suriname", "Guyana"))), 
            mapping = aes(x = year, y = val, color = measure, group = measure)) +
  facet_wrap( ~ location, scales = "fixed", ncol = 2) +
  labs(x = "", y = "Rate(per 100.000)")+
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, 
                                                              hjust = 1, 
                                                              size = 8))

p2 <- ggplot() +
  geom_line(data = df %>% filter(location %in% c("Suriname", "Guyana")), 
            mapping = aes(x = year, y = val, color = measure, group = measure)) +
  facet_wrap( ~ location, scales = "fixed", ncol = 2) +
  labs(title = "",
       x = "", y = "", caption = "Age-standardized")+
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, 
                                                              hjust = 1, 
                                                              size = 8))


p <- plot_grid(p1, p2)

p

ggsave(filename = "08_countries_measures_year.jpg",
       plot = p, path = here::here("Figures"),
       width = 10, height = 7, dpi = 300)

rm(p1, p2)
# -----------------------------------------
# heatmap countries and causes in 2019
# -----------------------------------------

df <- GBD %>% 
  filter (metric =="Rate", 
          age == "Age-standardized",
          measure != "SEV",
          year == "2019")



p <- ggplot(df, mapping = aes(x = location, y = cause, fill = val_cat))+
  geom_tile(color = "white", lwd = 0)+
  
  geom_shadowtext(aes(label = round(val, digits = 1)), color = "white",
                  size = 2) +
  scale_fill_viridis_d(direction = -1)+
 # ggthemes::theme_base() +
  theme_bw()+
  facet_grid(~measure)+
  labs(title = "Rate of DALYs, YLDs, YLLs, Deaths for each cause by country in 2019, both sexes, age-standardized", 
       fill = "Measure per 100.000",
       x = "", y= "", caption = "per 100.000")+
  theme(
    axis.text.x = element_text(hjust = 1,
                               angle = 45,
                               size = 8),
    plot.title = element_text(hjust = 0.5))


p


ggsave(filename = "09_heatmap_DALYS_countries_2019.jpg",
       plot = p, path = here::here("Figures"),
       width = 18, height = 7, dpi = 300)





# -----------------------------------------
# heatmap countries and causes in 1990
# -----------------------------------------

df <- GBD %>% 
  filter (metric =="Rate", 
          age == "Age-standardized",
          measure != "SEV",
          year == "1990")



p <- ggplot(df, mapping = aes(x = location, y = cause, fill = val_cat))+
  geom_tile(color = "white", lwd = 0)+
  
  geom_shadowtext(aes(label = round(val, digits = 1)), color = "white",
                  size = 2) +
  scale_fill_viridis_d(direction = -1)+
  # ggthemes::theme_base() +
  theme_bw()+
  facet_grid(~measure)+
  labs(title = "Rate of DALYs, YLDs, YLLs, Deaths for each cause by country in 1990, both sexes, age-standardized", 
       fill = "Measure per 100.000",
       x = "", y= "", caption = "per 100.000")+
  theme(
    axis.text.x = element_text(hjust = 1,
                               angle = 45,
                               size = 8),
    plot.title = element_text(hjust = 0.5))


p


ggsave(filename = "09.1_heatmap_DALYS_countries_1990.jpg",
       plot = p, path = here::here("Figures"),
       width = 18, height = 7, dpi = 300)





# -----------------------------------------
# SDI by location and ROC
# -----------------------------------------

df <- GBD_ROC_1990_to_2019 %>%
  filter(age %in% "Age-standardized") %>%
  filter(cause %in% "All causes") %>%
  filter(metric %in% "Rate")

p <-ggplot(df) +
  aes(x = SDI, y = ROC_val, colour = location, label = sov_a3) +
  geom_point(shape = "circle", size = 2) +
  geom_text_repel(size = 3) +
  geom_hline(yintercept = 0)+
  scale_color_viridis_d(option = "viridis", direction = 1) +
  theme_bw()+
  facet_wrap(vars(measure))+
  labs(title = "Rate of change and SDI, both sexes, age-standardized", 
       x = "SDI", y= "Rate of Change(%)")

p

ggsave(filename = "10_ROC_SDI_countries_1990_2019.jpg",
       plot = p, path = here::here("Figures"),
       width = 10, height = 7, dpi = 300)


# -----------------------------------------
# SDI by location and Rate
# -----------------------------------------

df <- GBD %>%
  filter(age %in% "Age-standardized") %>%
  filter(cause %in% "All causes") %>%
  filter(metric %in% "Rate") 

p <-ggplot(df) +
  aes(x = SDI, y = val, colour = location, label = sov_a3) +
  geom_point(shape = "circle", size = 1) +
  scale_color_viridis_d(option = "viridis", direction = 1) +
  theme_bw()+
  facet_wrap(vars(measure), scales = "free_y")+
  labs(title = "Rates of DALYs, YLDs and YLLs by SDI, countries between 1990-2019, both sexes, age-standardized", 
       x = "SDI", y= "Rate per 100.000")

p

ggsave(filename = "10.1_Rate_SDI_countries_1990_2019.jpg",
       plot = p, path = here::here("Figures"),
       width = 10, height = 7, dpi = 300)








# -----------------------------------------
# GRAFICO DE RANKING o que mudou de ranking das causas pra cada país
# -----------------------------------------













# -----------------------------------------
# 
# -----------------------------------------