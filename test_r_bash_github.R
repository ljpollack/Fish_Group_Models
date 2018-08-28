# this is a test script, gonna see if I can make this script run with a bash script that then pushes to github

library(tidyverse)

# read data
d <- read_csv("GroupSizeNovelAssay_FinalDataSheet_BitesFood.csv")

# gather the latency measurements together
d <- gather(d, key = "individual", value = "latency", 8:15)

# get rid of comments column
d$X__1 <- NULL

# strip out some extra words
d$individual <- as.numeric(str_replace_all(d$individual, "novel_bite", ""))

# if the latency value is actually a number, keep a number. If not, it's NA
d <- d %>% 
  mutate(
    latency = case_when(
      str_detect(latency, "^[0-9]+$") ~ as.numeric(latency),
      TRUE ~ NA_real_
    ))

# let's do some violin plots
d %>% 
  filter(!is.na(latency)) %>%
  ggplot(aes(x=as.factor(treatment), y=latency, group = treatment))+
  geom_violin()+
  geom_jitter(color = "black", stroke = 0, size = 2.1, alpha = 0.1, fill = "transparent")

ggsave("images/violin_plot_latency_data.jpg")