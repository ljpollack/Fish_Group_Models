# the goal here is to estimate the effect of group size on latency to approach novel food
# the model will be predicting indivual latency based on "treatment" of group size. The null model would indicate that an individual's latency is not affected by group size, which means all individuals are drawn from the same underlying distribution of latency values

# have to account for multiple measurements per group

library(tidyverse)
library(brms)
library(ggthemes)
library(ggExtra)

# make a custom theme
theme_custom <-
  theme(text       = element_text(color = "black", family = "Roboto Condensed"),
        strip.text = element_text(color = "black", family = "Roboto Condensed"),
        axis.text  = element_text(color = "black", family = "Roboto Condensed"),
        axis.ticks = element_blank(),
        line       = element_line(color = "black"),
        plot.background   = element_rect(fill = "transparent", color = "transparent"),
        panel.background  = element_rect(fill = "transparent", color = "transparent"),
        strip.background  = element_rect(fill = "transparent", color = "transparent"),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key        = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_blank())


# read data
d <- readxl::read_xlsx("GroupSizeNovelAssay_FinalDataSheet_BitesFood.xlsx")

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
  geom_jitter(color = "black", stroke = 0, size = 2.1, alpha = 0.1, fill = "transparent")+
  theme_custom

ggsave("violin_plot_latency_data.jpg")

# let's just look at mean values by treatment
d %>% 
  group_by(treatment) %>% 
  filter(!is.na(latency)) %>% 
  summarise(mean_latency = mean(latency), n = n())

# let's fit a model! keeping adapt_delta and max_treedepth high
model_1 <- brm(data = d, family = poisson, 
               latency ~ 1 + treatment + trial +
                 (1 + treatment + trial | group_ID) +
                 (1 + treatment + trial | tank),
               prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                         set_prior("normal(0, 1)", class = "b"),
                         set_prior("cauchy(0, 2)", class = "sd"),
                         set_prior("lkj(4)", class = "cor")),
               iter = 5000, warmup = 1000, chains = 3, cores = 3, control = list(adapt_delta = 0.99, max_treedepth = 15), save_model = "stan_model_1")
BRRR::skrrrahh(36)
