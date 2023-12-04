# AOC3

library(tidyverse)
library(stringi)

## Data
aoc3 = read.delim("aoc3/aoc3.txt", header = FALSE, sep = "", col.names = "v1")

## Part 1
aoc3 %>% 
  mutate(v_target = str_extract_all(v1, "[:digit:]+"),                      # Value of targets
         v_f_target = str_extract_all(v1, "[:digit:]+(?=[$%#&*//+=@-])"),   # Look forward
         v_b_target = str_extract_all(v1, "(?<=[$%#&*//+=@-])[:digit:]+"),  # Look backward
         v_locate = str_locate_all(v1, "[:digit:]+")) %>%                   # Location of targets
  rowwise() %>% 
  mutate(v_locate = v_locate  %>% {t(.) + c(-1, 1)} %>% t() %>% list()) %>% # 1 step left, 1 step right
  ungroup() %>% 
  mutate(v_lookabove = str_sub_all(lag(v1), v_locate),                      # Look above
         v_lookbelow = str_sub_all(lead(v1), v_locate)) %>%                 # Look below
  rowwise() %>% 
  mutate(v_above_yn = list(str_detect(v_lookabove, "[$%#&*//+=@-]")),
         v_below_yn = list(str_detect(v_lookbelow, "[$%#&*//+=@-]")),
         v_above_target = list(v_target[v_above_yn]),
         v_below_target = list(v_target[v_below_yn])) %>% 
  mutate(v_all_target = list(c(unlist(v_f_target) %>% as.numeric(),
                               unlist(v_b_target) %>% as.numeric(),
                               unlist(v_above_target) %>% as.numeric(),
                               unlist(v_below_target) %>% as.numeric()))) %>% 
  ungroup() %>% 
  reframe(all = c(unlist(v_all_target))) %>% 
  summarise(sum(all, na.rm = TRUE)) 
