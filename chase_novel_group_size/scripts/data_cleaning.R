library(tidyverse)

d <- read_csv("chase_novel_group_size/data/raw/only_half_chase_group_size.csv")
d <- d[,1:12]

# gather the chase measurements together
# have to add the row ids as columns early on, otherwise the spread function doesn't work, which is stupid. Should make a reproducible example out of this since it seems silly
d <- d %>% rowid_to_column() %>% 
  gather(key = "assay_type", value = "value", 8:13) %>%
  separate(col = assay_type, into = c("assay", "measurement"), sep = "_") 

d <- d %>% 
  spread(key = measurement, value = value) %>% 
  rename(trial = Trial, group = Group, tank = Tank)

d <- d %>% 
  mutate(treatment = as.numeric(str_sub(group, 1, 1)))

d %>% ggplot(aes(x=timechases)) + geom_histogram()

saveRDS(d, "chase_novel_group_size/data/cleaned/only_half_chase_group_size.rds")
saveRDS(d, "chase_novel_group_size/files_for_farm/data/only_half_chase_group_size.rds")



#### proportion food eaten data ####

d <- read_csv("chase_novel_group_size/data/raw/full_proportion_eaten_group_size.csv")

d$food_eaten <- d$`Food Input` - d$`Leftover Food`
d$novel_eaten <- d$`Novel Input` - d$`Leftover Novel`

d$food_eaten <- floor(d$food_eaten)
d$novel_eaten <- floor(d$novel_eaten)

d$food_eaten[d$food_eaten < 0] <- 0
d$novel_eaten[d$novel_eaten < 0] <- 0

d <- d %>% 
  mutate(treatment = as.numeric(str_sub(Group, 1, 1)))

d <- d %>% 
  select(treatment, Trial, Tank, Group, `Novel Food`, food_eaten, novel_eaten, `Proportion Eaten Food`, `Proportion Eaten Novel`, `Food Input`, `Novel Input`)

d_typical <- d %>% 
  select(-c(`Novel Food`, novel_eaten, `Proportion Eaten Novel`))

d_novel <- d %>% 
  filter(`Novel Food` == "Brine") %>% 
  select(-c(food_eaten, `Proportion Eaten Food`))

d_typical <- d_typical %>% 
  mutate(censored = case_when(
    treatment == 2 & food_eaten == 4 ~ "right",
    treatment == 4 & food_eaten == 8 ~ "right",
    treatment == 8 & food_eaten == 16 ~ "right",
    TRUE ~ "none"
  ))

d_novel <- d_novel %>% 
  mutate(censored = case_when(
    treatment == 2 & novel_eaten == 4 ~ "right",
    treatment == 4 & novel_eaten == 8 ~ "right",
    treatment == 8 & novel_eaten == 16 ~ "right",
    TRUE ~ "none"
  ))

d_typical <- d_typical %>% rename(prop_eaten_food = `Proportion Eaten Food`, food_input = `Food Input`)
d_novel <- d_novel %>% rename(prop_eaten_novel = `Proportion Eaten Novel`, novel_input = `Novel Input`)

d_typical$prop_eaten_food[d_typical$prop_eaten_food < 0] <- 0
d_typical$prop_eaten_food[d_typical$prop_eaten_food > 1] <- 1

d_novel$prop_eaten_novel[d_novel$prop_eaten_novel < 0] <- 0
d_novel$prop_eaten_novel[d_novel$prop_eaten_novel > 1] <- 1


saveRDS(d_typical, "chase_novel_group_size/data/cleaned/typical_food_proportion_eaten_group_size.rds")
saveRDS(d_typical, "chase_novel_group_size/files_for_farm/data/typical_food_proportion_eaten_group_size.rds")

saveRDS(d_novel, "chase_novel_group_size/data/cleaned/novel_food_proportion_eaten_group_size.rds")
saveRDS(d_novel, "chase_novel_group_size/files_for_farm/data/novel_food_proportion_eaten_group_size.rds")

