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

## Part 2
### Done completely differently. Find stars, find numbers around. 
aoc3 %>% 
  # Index stars
  mutate(star_loc = str_locate_all(v1, "[*]")) %>% 
  rowwise() %>%
  # Locations to look for numbers
  mutate(star_loc_start  = list(star_loc %>% as_tibble() %>% pull(start))) %>%  
  mutate(after = list(tibble(start = star_loc_start + 1, end = star_loc_start + 3) %>% as.matrix())) %>% 
  mutate(before = list(tibble(start = star_loc_start - 3, end = star_loc_start -1) %>% as.matrix())) %>%
  mutate(above_below = list(tibble(start = star_loc_start - 3, end = star_loc_start + 3) %>% as.matrix())) %>%
  mutate(above_below_centre = list(tibble(start = 2, end = 6) %>% as.matrix())) %>%
  mutate(above_below_after = list(tibble(start = 5, end = 7) %>% as.matrix())) %>%
  mutate(above_below_before = list(tibble(start = 1, end = 3) %>% as.matrix())) %>%
  # Look for numbers above and below
  ungroup() %>% 
  mutate(number_after = str_sub_all(v1, after)) %>% 
  mutate(number_before = str_sub_all(v1, before)) %>%
  mutate(string_above = str_sub_all(lag(v1), above_below)) %>%
  mutate(string_below = str_sub_all(lead(v1), above_below)) %>%
  # What is immediately above/below?
  mutate(what_above = str_sub_all(lag(v1), star_loc)) %>% 
  mutate(what_below = str_sub_all(lead(v1), star_loc)) %>% 
  # If a dot, get before and after
  rowwise() %>% 
  mutate(dot_above = list(str_detect(what_above, "[.]{1}"))) %>% 
  mutate(dot_below = list(str_detect(what_below, "[.]{1}"))) %>% 
  mutate(number_above_after = list(ifelse(unlist(dot_above), str_sub_all(string_above, above_below_after), NA) %>% as.character())) %>% 
  mutate(number_below_after = list(ifelse(unlist(dot_below), str_sub_all(string_below, above_below_after), NA) %>% as.character())) %>% 
  mutate(number_above_before = list(ifelse(unlist(dot_above), str_sub_all(string_above, above_below_before), NA) %>% as.character())) %>% 
  mutate(number_below_before = list(ifelse(unlist(dot_below), str_sub_all(string_below, above_below_before), NA) %>% as.character())) %>% 
  mutate(number_above_centre = list(ifelse(unlist(dot_above), NA, str_sub_all(string_above, above_below_centre)) %>% as.character())) %>% 
  mutate(number_below_centre = list(ifelse(unlist(dot_below), NA, str_sub_all(string_below, above_below_centre)) %>% as.character())) %>% 
  select(number_after, number_before, number_above_after, number_above_before, number_above_centre, 
         number_below_after, number_below_before, number_below_centre) %>% 
  rowid_to_column() %>% 
  unnest(c(number_after, number_before, number_above_after, number_above_before, number_above_centre, 
           number_below_after, number_below_before, number_below_centre)) %>% 
  ungroup() %>% 
  # Based on position, remove surrounding rubbish
  mutate(number_after = str_extract(number_after, "^[:digit:]+")) %>% 
  mutate(number_before = str_extract(number_before, "[:digit:]+$")) %>% 
  mutate(number_above_after = str_extract(number_above_after, "^[:digit:]+")) %>% 
  mutate(number_above_before = str_extract(number_above_before, "[:digit:]+$")) %>% 
  mutate(number_below_after = str_extract(number_below_after, "^[:digit:]+")) %>% 
  mutate(number_below_before = str_extract(number_below_before, "[:digit:]+$")) %>% 
  mutate(number_above_centre = case_when(
    str_detect(number_above_centre, "^.{1}[.]{1}") ~ str_extract(number_above_centre, "([:digit:]{2})([.]|[:digit:]{1})$"),
    str_detect(number_above_centre, "^.{3}[.]{1}") ~ str_extract(number_above_centre, "(^([.]|[:digit:]{1})[:digit:]{2})"),
    TRUE ~ number_above_centre) %>% 
      str_remove_all(pattern = "[. ]")) %>% 
  mutate(number_below_centre = case_when(
    str_detect(number_below_centre, "^.{1}[.]{1}") ~ str_extract(number_below_centre, "([:digit:]{2})([.]|[:digit:]{1})$"),
    str_detect(number_below_centre, "^.{3}[.]{1}") ~ str_extract(number_below_centre, "(^([.]|[:digit:]{1})[:digit:]{2})"),
    TRUE ~ number_below_centre) %>% 
      str_remove_all(pattern = "[. ]")) %>% 
  mutate(across(starts_with("number"), parse_number)) %>% 
  rowwise() %>% 
  mutate(count_values = pick(starts_with("number")) %>% is.na(.) %>% {8 - sum(.)}) %>% # Picking up NAs annoying, na.omit doesn't work
  mutate(gears = pick(starts_with("number")) %>% prod(na.rm = TRUE)) %>%
  mutate(gears = ifelse(count_values == 1, 0, gears)) %>% 
  ungroup() %>% 
  summarise(sum(gears))