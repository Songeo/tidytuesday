

library(tidyverse)
library(lubridate)
library(ggrepel)


theme_set(theme_minimal())


# read data
df_tuition <- readxl::read_xlsx("data/us_avg_tuition.xlsx")

# tidy version of data 
df_tuition_tidy <- df_tuition %>% 
  # dates should be rows
  gather(date, avg_tuition, -State) %>% 
  # one observation per year and number of observation separate
  separate(date, c('year', 'obs_num'), sep = "-") %>% 
  # from characters to numbers
  mutate_at(.vars = c('year', 'obs_num'),
            .funs = parse_number)

# how many states
df_tuition_tidy$State %>% n_distinct()

# number of observation per state
df_tuition_tidy %>% 
  group_by(State) %>% 
  tally() %>% 
  print(n = Inf)


# boxplot of tuituon in time 
df_tuition_tidy %>% 
  ggplot(aes(x = factor(year), y = avg_tuition)) + 
  geom_boxplot() + 
  ylab("Tuition (avg)") + 
  xlab("Year") + 
  ggtitle("Average tuition in time", 
          "Prices always go up")
ggsave("graphs/170402_ustuition_01boxplot.png", width = 7, height = 5)


# difference of tuition per annual average
tab_gg <- df_tuition_tidy %>% 
  group_by(year) %>% 
  mutate(year_mean = mean(avg_tuition), 
         cent = avg_tuition-year_mean) %>% 
  arrange(State) %>% 
  group_by(State) %>% 
  mutate(diff_cent = cent - lag(cent), 
         change_avg = mean(diff_cent, na.rm = T),
         change_max = max(diff_cent, na.rm = T), 
         ind = max( abs(diff_cent) > 3000| 
                      change_max > 700 | 
                      abs(cent) > 3000 | 
                      abs(change_avg) > 250, na.rm = T)  == 1) %>% 
  ungroup

# some summaries
tab_gg %>% 
  filter(State == "Wyoming")
tab_gg$change_avg %>% summary()
tab_gg$change_max %>% summary()
tab_gg$year %>% summary()

tab_gg %>% 
  select(State, change_max) %>% 
  unique() %>% 
  arrange(desc(change_max))
filter(State == "Washington")


# change over national avg per state and year
tab_gg %>% 
  ggplot(aes(x = year, y = State, fill = diff_cent)) +
  geom_tile() +
  scale_fill_continuous(low = "#0000FF", high = "#FFFF00") + 
  guides(fill = guide_legend("Difference")) +
  ggtitle("State Tuition (avg) - National Tuition (avg)",
          "Some years have big changes")
ggsave("graphs/170402_ustuition_02heatmap.png", width = 7, height = 8)

# plot of special states
tab_gg %>% 
  # filter(State %in% c("Washington", "New Hampshire", "Wyoming")) %>%
  ggplot(aes(x = year,
             y = cent, 
             group = State,
             color = State)) + 
  geom_line(aes(alpha = ind, 
                size = ind)) +
  scale_alpha_discrete(range = c(.5, 1)) +
  scale_size_discrete(range = c(.5, 1)) +
  xlim(c(2004, 2016)) +
  geom_text_repel(data = filter(tab_gg,
                                ind,
                                 year == 2015),
                   aes(label = State),
                   size = 3,
                   nudge_x = 2,
                   segment.color = NA) + 
  theme(legend.position = "none") + 
  ggtitle("Tuition per State", 
          "Interesting changes in some states") +
  ylab( "State Tuition (avg) - National Tuition (avg)")
ggsave("graphs/170402_ustuition_03trends.png", width = 8, height = 6)
