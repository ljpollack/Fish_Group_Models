library(tidyverse)
# read data
d <- read_csv("latency/data/raw/latency_typical_food_group_size.csv")
d <- d[,1:17]

# gather the latency measurements together
d <- gather(d, key = "individual", value = "latency", 8:15)

# strip out some extra words
d$individual <- as.numeric(str_replace_all(d$individual, "novel_bite", ""))

# if the latency value is actually a number, keep a number. If not, it's NA
d <- d %>% 
  mutate(
    latency = case_when(
      str_detect(latency, "^[0-9]+$") ~ as.numeric(latency),
      TRUE ~ NA_real_
    ))
d <- d %>% 
  mutate(max_minus_lat = 300 - latency)

# make some factors
d$group_ID <- as.factor(d$group_ID)
d$tank <- as.factor(d$tank)

# set all instances where fish did not move during trial to 0, this will be the hurdle
d$latency[d$latency == 300] <- 0

saveRDS(d, "latency/data/cleaned/latency_typical_food.rds")


#### novel food data now ####
d <- read_csv("latency_novel_group_size/data/raw/full_latency_novel_food_group_size.csv")
d <- d[,1:17]

# gather the latency measurements together
d <- gather(d, key = "individual", value = "latency", 9:17)

# strip out some extra words
d$individual <- as.numeric(str_replace_all(d$individual, "novel_bite", ""))

# if the latency value is actually a number, keep a number. If not, it's NA
d <- d %>% 
  mutate(
    latency = case_when(
      str_detect(latency, "^[0-9]+$") ~ as.numeric(latency),
      TRUE ~ NA_real_
    ))

d <- d %>% 
  mutate(treatment = as.numeric(str_sub(Group, 1, 1))) %>% 
  rename(tank = Tank, group = Group, novel_food = `Novel Food`) %>% 
  mutate(novel_food = factor(novel_food))

# make some factors
d$group_ID <- as.factor(d$group_ID)
d$tank <- as.factor(d$tank)

# set all instances where fish did not move during trial to 0, this will be the hurdle
d$latency[d$latency == 300] <- 0

saveRDS(d, "latency_novel_group_size/data/cleaned/latency_novel_food.rds")
