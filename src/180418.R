
library(tidyverse)
library(imputeTS)
library(forecast)
library(ggfortify)
library(ggrepel)


source("src/180418_lib.R")

# data 
df_global <- readxl::read_xlsx("data/global_mortality.xlsx")
df_global

df_global %>% 
  filter(is.na(country_code)) %>% 
  summarise(n_distinct(country))

df_global %>% 
  filter(is.na(country_code)) %>% 
  .$country %>% 
  unique()


df_global_tidy <- df_global %>% 
  gather(type_death, value, -c(country, country_code, year)) %>% 
  filter(!is.na(country_code)) %>% 
  mutate_if(is.character, factor) %>% 
  group_by(country, type_death) %>% 
  mutate(nas = sum(is.na(value)),
         ceros = sum(value == 0),
         len = length(value), 
         value = ifelse(nas >= len & is.na(value), 0, value),
         value_imp = na.ma(value),
         value_init = lag(value_imp, 26),
         cambio = (value_imp - lag(value_imp, 26))/lag(value_imp, 26) ) %>% 
  ungroup %>% 
  dplyr::select(-len)
df_global_tidy %>% print(n = 30)

df_global_tidy %>% summary()

filter(df_global_tidy, is.na(value)) %>% 
  .$type_death %>% 
  unique()
filter(df_global_tidy, is.na(country_code))
filter(df_global_tidy, type_death == "Conflict (%)")


is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 3 * IQR(x))
}
df_global_tidy %>% 
  filter(!is.na(cambio)) %>% 
  filter(cambio != Inf) %>%
  group_by(type_death) %>% 
  mutate(outlier = ifelse(is_outlier(cambio), 
                          as.character(country), 
                          NA) ) %>% 
  ungroup %>% 
  ggplot(aes(x = fct_reorder(type_death, cambio, median), 
             y = cambio, 
             color = type_death)) + 
  geom_boxplot()  + 
  ylim(c(0, 40)) +
  theme(legend.position = "none") + 
  coord_flip()


df_global_tidy %>% 
  filter(!is.na(cambio)) %>%
  filter(cambio != Inf) %>% 
  filter(type_death == "HIV/AIDS (%)") %>% 
  arrange(desc(cambio))

df_global_tidy %>% 
  filter(!is.na(cambio)) %>%
  filter(cambio != Inf) %>% 
  filter(type_death == "Terrorism (%)") %>% 
  arrange(desc(cambio))


df_global_tidy %>% 
  filter(country_code == 'AGO',
         type_death == "Diarrheal diseases (%)") %>% 
  ggplot(aes(x = year, 
             y = value_imp)) + 
  geom_point(alpha = .1) +
  facet_wrap(~type_death, scales = "free_y")


measures_fun <- function(sub_series){
  print(sub_series[1,])
  ts_serie <- ts(sub_series$value_imp, start = 1990, frequency = 1)
  measures(ts_serie)
}
df_global_tidy %>% 
  filter(nas < 27, 
         ceros < 15) %>% 
  group_by(country, country_code, type_death) %>% 
  do(measures_fun(.))


