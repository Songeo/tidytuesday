
# libraries
library(tidyverse)
library(forcats)
library(ggrepel)

# ggplot theme set
theme_set(theme_bw())


# 1. read data and tidy form ----
df_salaries <- readxl::read_excel("data/tidy_tuesday_week2.xlsx")
df_salaries 

df_salaries_tidy <- df_salaries %>% 
  gather(position, salary, -c(year)) %>% 
  mutate(pos_code = fct_recode(position, 
                               CB_defense = 'Cornerback',
                               DL_defense = "Defensive Lineman",
                               LB_defense = "Linebacker",
                               OL_offense = "Offensive Lineman",
                               QB_offense = "Quarterback",
                               RB_offense = "Running Back",
                               SY_defense = "Safety",
                               ST_defense = "Special Teamer",
                               TE_offense = "Tight End",
                               WR_offense = "Wide Receiver")) %>% 
  separate(pos_code, c("pos_short", "def_off"), sep = "_")
df_salaries_tidy


df_salaries_tidy %>% 
  ggplot(aes(x = position, y = salary, 
             color = factor(year))) +
  geom_boxplot() + 
  scale_y_continuous(labels = function(x)x/1e3)


tab_gg <- df_salaries_tidy %>% 
  na.omit() %>% 
  group_by(year) %>% 
  mutate(total = sum(salary), 
         prop = salary/total) %>% 
  gather(variable, value, c(salary, prop)) 

tab_gg %>% 
  ggplot(aes(x = factor(year), 
             y = value,
             group = position)) +
  geom_smooth(se = F, aes(color = position, linetype = position))  + 
  facet_wrap(~variable, scales = 'free_y', ncol = 1) 


