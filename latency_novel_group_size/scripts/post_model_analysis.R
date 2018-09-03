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
  geom_jitter(color = "black", stroke = 0, size = 2.1, alpha = 0.1, fill = "transparent")+
  theme_custom

# read in the model fit on the FARM
model_1 <- readRDS("fit_models/model_1_fit.rds")
plot(model_1)

launch_shinystan(model_1)

# only 5 divergent iterations finally!!
model_1$formula

summary(model_1)
model_1$fit
knitr::kable(model_1$fit)
plot(marginal_effects(model_1, effects = "treatment:trial"))
?marginal_effects
conditions <- data.frame(treatment = c(2,4,8), trial = c(1,2,3), cond__ = c("trial 1", "trial 2", "trial 3"))
#make_conditions(conditions, vars = c("treatment", "trial"))

plot(marginal_effects(model_1, effects = "treatment", conditions = conditions), theme = theme_custom)


plot(shinystan_multiparam_gg)


# ran another model, but with a zero-inflated Poisson
model_2 <- readRDS("fit_models/model_2_fit.rds")
launch_shinystan(model_2)

waic(model_1, model_2)
loo(model_1, model_2)

# I just realized the data aren't zero-inflated, they're "max time inflated". We could subtract all the latency values from the max time and then use a zero-inflated model

# # read data
# d <- read_csv("GroupSizeNovelAssay_FinalDataSheet_BitesFood.csv")
# 
# # gather the latency measurements together
# d <- gather(d, key = "individual", value = "latency", 8:15)
# 
# # get rid of comments column
# d$X__1 <- NULL
# 
# # strip out some extra words
# d$individual <- as.numeric(str_replace_all(d$individual, "novel_bite", ""))
# 
# # if the latency value is actually a number, keep a number. If not, it's NA
# d <- d %>% 
#   mutate(
#     latency = case_when(
#       str_detect(latency, "^[0-9]+$") ~ as.numeric(latency),
#       TRUE ~ NA_real_
#     ))
# 
# # let's do some violin plots
# d %>% 
#   filter(!is.na(latency)) %>%
#   ggplot(aes(x=as.factor(treatment), y=latency, group = treatment))+
#   geom_violin()+
#   geom_jitter(color = "black", stroke = 0, size = 2.1, alpha = 0.1, fill = "transparent")+
#   theme_custom
# 
# ggsave("violin_plot_latency_data.jpg")
# 
# # let's just look at mean values by treatment
# d %>% 
#   group_by(treatment) %>% 
#   filter(!is.na(latency)) %>% 
#   summarise(mean_latency = mean(latency), n = n())
# 
# # let's fit a model! keeping adapt_delta and max_treedepth high
# model_1 <- brm(data = d, family = poisson, 
#                latency ~ 1 + treatment + trial +
#                  (1 + treatment + trial | group_ID) +
#                  (1 + treatment + trial | tank),
#                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
#                          set_prior("normal(0, 1)", class = "b"),
#                          set_prior("cauchy(0, 2)", class = "sd"),
#                          set_prior("lkj(4)", class = "cor")),
#                iter = 5000, warmup = 1000, chains = 3, cores = 3, control = list(adapt_delta = 0.99, max_treedepth = 15), save_model = "stan_model_1")
# BRRR::skrrrahh(36)
# 
# system("git add .")
# system("git commit -a -m 'trying the model and pushing to github'")
# system("git push")