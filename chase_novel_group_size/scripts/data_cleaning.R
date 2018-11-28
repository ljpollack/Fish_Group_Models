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
