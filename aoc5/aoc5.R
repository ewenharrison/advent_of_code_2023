# AOC5
library(tidyverse)

## Data ----
seeds = read.delim("aoc5/aoc5.txt", header = FALSE, sep = "", nrows = 1, )
maps = read.delim("aoc5/aoc5.txt", header = FALSE, sep = "", skip = 1, 
                  col.names = c("dest_start", "source_start", "range"))
## Split and prep ----
seeds = seeds %>% 
  select(-1) %>%
  pivot_longer(everything()) %>% 
  select(-1)

fn_prep <- function(.data, slice_low, slice_high){
  .data %>% 
    slice(slice_low:slice_high) %>% 
    mutate(across(everything(), as.numeric)) %>% 
    mutate(dest_end = dest_start + range - 1,
           source_end = source_start + range - 1)
}

seed_soil = fn_prep(maps, 2, 18)
soil_fert = fn_prep(maps, 20, 45)
fert_water = fn_prep(maps, 47, 93)
water_light = fn_prep(maps, 95, 102)
light_temp = fn_prep(maps, 104, 118)
temp_hum = fn_prep(maps, 120, 159)
hum_loc = fn_prep(maps, 161, 184)

## Part 1 ----
### Function to provide location values from seeds
fn_seeds <- function(.seeds, .lookup){
  .seeds %>% 
    left_join(.lookup, join_by(between(value, source_start, source_end))) %>% 
    mutate(dest = value - source_start + dest_start,
           dest = ifelse(is.na(dest), value, dest)) %>% 
    select(value = dest)
}

fn_seeds_wrapper <- function(.seeds, return_min = TRUE){
  out = fn_seeds(.seeds, seed_soil) %>% 
    fn_seeds(soil_fert) %>% 
    fn_seeds(fert_water) %>% 
    fn_seeds(water_light) %>% 
    fn_seeds(light_temp) %>% 
    fn_seeds(temp_hum) %>% 
    fn_seeds(hum_loc)
  if(return_min){
    out %>% slice_min(value)
  } else {
    out
  }
}

## Answer
fn_seeds_wrapper(seeds)

# Part 2 ----
## Do largest and smallest for each pair?
### Wrong answer (same as above)

## Expand out?
### Too big

## Grid search from output values ----
### Ranges of seed values to test against
seed_ranges = seeds %>% 
  mutate(group = rep(c("start", "range"), 10),
         pair = rep(1:10, each = 2)) %>% 
  pivot_wider(names_from = group, values_from = value) %>% 
  mutate(end = start + range - 1) 

### Function to provide seed values from possible locations
fn_seeds_reverse <- function(.locations, .lookup){
  .locations %>% 
    left_join(.lookup, join_by(between(value, dest_start, dest_end))) %>% 
    mutate(source = value - dest_start + source_start,
           source = ifelse(is.na(source), value, source)) %>% 
    select(value = source)
}

fn_seeds_reverse_wrapper <- function(.locations, .ranges){ # .locations to test, .ranges to test against
  fn_seeds_reverse(.locations, hum_loc) %>% 
    fn_seeds_reverse(temp_hum) %>% 
    fn_seeds_reverse(light_temp) %>% 
    fn_seeds_reverse(water_light) %>% 
    fn_seeds_reverse(fert_water) %>% 
    fn_seeds_reverse(soil_fert) %>% 
    fn_seeds_reverse(seed_soil) %>% 
    left_join(.ranges, join_by(between(value, start, end)))
}
### e.g.
fn_seeds_reverse_wrapper(.locations = tibble(value = c(0,1)), seed_ranges)

### Grid search, relies on hitting at given resolution
grid_search <- function(start, end, resolution, .ranges){
  locations = tibble(value = seq(start, end, by = resolution))
  fn_seeds_reverse_wrapper(locations, .ranges) %>% 
    bind_cols(locations %>% select(location = value), .) %>% 
    filter(!is.na(pair)) %>% 
    slice_min(location)
}

## Answer
grid_search(0, 50000000, 1000, seed_ranges)
grid_search(28580000, 28581000, 1, seed_ranges)
