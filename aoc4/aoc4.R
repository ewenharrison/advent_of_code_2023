library(tidyverse)

## Data
aoc4 = read.delim("aoc4/aoc4.txt", header = FALSE, sep = "")

win = aoc4 %>% 
  select(V3:V12) %>%
  as.matrix() %>% 
  t() %>% 
  as_tibble()

me = aoc4 %>% 
  select(V14:V38) %>% 
  as.matrix() %>% 
  t() %>% 
  as_tibble()

# Part 1
map2_df(win, me, ~ .y %in% .x) %>% 
  summarise(across(everything(), sum)) %>% 
  pivot_longer(everything()) %>% 
  mutate(power = ifelse(value != 0, 2 ^ (value-1), 0)) %>% 
  summarise(sum(power))
  
# Part 2
out = map2_df(win, me, ~ .y %in% .x) %>% 
  summarise(across(everything(), sum)) %>% 
  pivot_longer(everything()) %>% 
  mutate(score = 1)

for(i in 1:dim(out)[1]){
  if(out$value[i] > 0){
    out$score[(i+1):(out$value[i] + i)] <- (out$score[(i+1):(out$value[i] + i)]) + out$score[i]
  }
}
sum(out$score)