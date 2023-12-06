# AOC6
library(tidyverse)

## Data
aoc6 = read.delim("aoc6/aoc6.txt", header = FALSE, sep = "") %>% 
  mutate(V1 = c("time", "distance"))

## Prep
aoc6_1 = aoc6 %>% 
  pivot_longer(V2:V5) %>% 
  pivot_wider(names_from = V1, values_from = value) %>% 
  select(-name)

## Part 1
aoc6_1 %>% 
  rowid_to_column() %>% 
  rowwise() %>% 
  mutate(race_speed = seq(0, time) %>% list()) %>% 
  unnest(race_speed) %>% 
  mutate(race_time = time - race_speed,
         race_distance = race_speed * race_time,
         race_win = race_distance > distance) %>% 
  group_by(rowid) %>% 
  summarise(race_win = sum(race_win)) %>% 
  summarise(prod(race_win))

## Part 2
### Brute force probably could be improved upon
aoc6 %>% 
  mutate(value = str_c(V2, V3, V4, V5)) %>% 
  select(V1, value) %>% 
  pivot_wider(names_from = V1, values_from = value) %>% 
  mutate(time = as.numeric(time),
         distance = as.numeric(distance)) %>% 
  mutate(race_speed = seq(0, time) %>% list()) %>% 
  unnest(race_speed) %>% 
  mutate(race_time = time - race_speed,
         race_distance = race_speed * race_time,
         race_win = race_distance > distance) %>% 
  summarise(race_win = sum(race_win)) %>% 
  summarise(prod(race_win))


         