library(tidyverse)
# read data
#d <- read_csv("GroupSizeNovelAssay_FinalDataSheet_BitesFood.csv")\
d <- read_csv("latency_novel_group_size/data/raw/latency_typical_food_group_size.csv")
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

write_csv(d, "latency_novel_group_size/data/cleaned_data.csv")
write_csv(d, "latency_novel_group_size/files_for_farm/data/cleaned_data_farm.csv")




#### reading in the first half of a new dataset ####
d <- read_csv("latency_novel_group_size/data/raw/only_half_latency_novel_food_group_size.csv")
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

d %>% ggplot(aes(x=latency)) + geom_histogram()

saveRDS(d, "latency_novel_group_size/data/cleaned/only_half_latency_novel_food_group_size.rds")
saveRDS(d, "latency_novel_group_size/files_for_farm/data/only_half_latency_novel_food_group_size.rds")
