# the goal here is to estimate the effect of group size on latency to approach novel food
# the model will be predicting indivual latency based on "treatment" of group size. The null model would indicate that an individual's latency is not affected by group size, which means all individuals are drawn from the same underlying distribution of latency values

# have to account for multiple measurements per group

library(tidyverse)
library(brms)
library(ggthemes)
library(ggExtra)
library(ggeffects)
library(broom)
library(ggridges)

source("../../minimal_ggplot_theme.R")

# make a custom theme


# read data
d <- read_csv("latency_novel_group_size/data/cleaned_data.csv")

# let's do some violin plots
d %>%
  filter(!is.na(latency)) %>%
  ggplot(aes(x=as.factor(treatment), y=max_minus_lat, group = treatment))+
  geom_violin()+
  geom_jitter(color = "black", stroke = 0, size = 2.1, alpha = 0.1, fill = "transparent")+
  custom_minimal_theme()

hist(d$latency)

# read in the model fit on the FARM
model_1 <- readRDS("latency_novel_group_size/fit_models/model_1_fit.rds")
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

plot(marginal_effects(model_1, effects = "treatment", conditions = conditions, spaghetti = T, nsamples = 300), theme = custom_minimal_theme())

int_conditions <- list(
  trial = setNames(c(1,2,3), c("trial 1", "trial 2", "trial 3")),
  treatment = 2:8)
int_conditions

#theme_2 <- theme(legend.background = element_blank(), legend.key = element_rect(fill = "white", colour = "black"))

effects <- marginal_effects(model_1, effects = "treatment:trial", int_conditions = int_conditions)
effects <- effects$`treatment:trial`

effects %>% 
  ggplot(aes(x=treatment, y=estimate__, color=trial)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = trial), color = "transparent", alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  custom_minimal_theme() +
  ggtitle("marginal effects of poisson multilevel model") +
  ylab("estimated latency")

ggsave("latency_novel_group_size/images/marginal_effects.jpg", width = 5, height = 5)


# now spaghetti plots
effects2 <- marginal_effects(model_1, effects = "treatment:trial", int_conditions = int_conditions, spaghetti = T, nsamples = 300)
plot(effects2)
str(effects2)

spag <- attributes(effects2$`treatment:trial`)$spaghetti
str(spag)
spag <- as.tibble(spag)
spag

spag %>% 
  ggplot(aes(x=treatment, y=estimate__, color=trial)) +
  geom_line(aes(group=sample__), alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  custom_minimal_theme() +
  ggtitle("marginal effects of poisson multilevel model") +
  ylab("estimated latency")

ggsave("latency_novel_group_size/images/spaghetti_marginal_effects.jpg", width = 5, height = 5)


ps <- posterior_summary(model_1)
str(ps)
ps <- unlist(ps)
ps <- as.data.frame(ps)
ps
ps2 <- ps %>%
  head(n=15) 
ps2 <- ps2 %>% 
  mutate(variable = rownames(ps2))
ps2 %>% 
  ggplot(aes(y=Estimate, x = variable))+
  geom_pointrange(aes(ymin=Q2.5, ymax=Q97.5, group=variable), size = 2/5, shape = 20) +
  geom_hline(yintercept=0, color = "gray25", alpha = 0.25) +
  coord_flip() +
  custom_minimal_theme() +
  ggtitle("parameter estimates for model 1 with 95% credible intervals")

ggsave("latency_novel_group_size/images/parameter_estimates.jpg", width = 5, height = 5)



preds <- posterior_samples(model_1, pars = ps2$variable)
str(preds)

preds <- as.tibble(preds)
preds <- preds %>% 
  gather(key = "variable", value = "estimate")

plot_dens <- preds %>% 
  ggplot(aes(x=estimate, y = variable)) +
  geom_density_ridges(fill = NA, color = "white") +
  geom_vline(xintercept = 0, color = "white", linetype = 2)

theme_joy_division <-
  theme(text       = element_text(color = "white", family = "Roboto Condensed"),
        strip.text = element_text(color = "white", family = "Roboto Condensed"),
        axis.text  = element_text(color = "white", family = "Roboto Condensed"),
        axis.ticks = element_blank(),
        line       = element_line(color = "white"),
        plot.background   = element_rect(fill = "black", color = "black"),
        panel.background  = element_rect(fill = "black", color = "black"),
        strip.background  = element_rect(fill = "black", color = "black"),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = "black", color = "black"),
        legend.box.background = element_rect(fill = "black", color = "black"),
        axis.line = element_blank())

plot_dens + theme_joy_division

ggsave("latency_novel_group_size/images/parameter_estimates_joy_division.jpg", width = 5, height = 5)




preds <- posterior_samples(model_1)
preds <- preds[,1:75]


preds <- as.tibble(preds)
preds <- preds %>% 
  gather(key = "variable", value = "estimate")

plot_dens <- preds %>% 
  ggplot(aes(x=estimate, y = variable)) +
  geom_density_ridges(fill = NA, color = "white")

plot_dens + theme_joy_division + theme(axis.text.y = element_blank(), axis.text.x = element_blank()) + xlab("") + ylab("")

ggsave("latency_novel_group_size/images/parameter_estimates_abstract_joy_division.jpg", width = 5, height = 5)


dfp <- ggpredict(model_1, terms = c("treatment", "trial"), type = "re") %>% 
  mutate(trial = group) %>% 
  mutate(group = NULL)


dfp %>% 
  ggplot(aes(x=x, y=predicted, color=trial)) +
    geom_line() +
    geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = trial), color = "transparent", alpha = 0.1) +
    scale_fill_viridis_d() +
    scale_color_viridis_d()





# ran another model, but with a zero-inflated Poisson
model_2 <- readRDS("latency_novel_group_size/fit_models/model_2_fit.rds")
launch_shinystan(model_2)

waic(model_1, model_2)
loo(model_1, model_2)

# apparently LOO is problematic for these models, so it recommended this:
kfold(model_1, model_2, K = 10)



# now a model with interaction terms
model_3 <- readRDS("latency_novel_group_size/fit_models/model_3_fit.rds")
launch_shinystan(model_3)





model_4 <- readRDS("latency_novel_group_size/fit_models/model_4_fit.rds")

model_4$fit

launch_shinystan(model_4)


model_5 <- readRDS("latency_novel_group_size/fit_models/model_5_fit.rds")

model_5$family

launch_shinystan(model_5)

ps <- posterior_summary(model_5)
str(ps)
ps <- unlist(ps)
ps <- as.data.frame(ps)
ps
ps2 <- ps %>%
  head(n=15) 
ps2 <- ps2 %>% 
  mutate(variable = rownames(ps2))
ps2 %>% 
  ggplot(aes(y=Estimate, x = variable))+
  geom_pointrange(aes(ymin=Q2.5, ymax=Q97.5, group=variable), size = 2/5, shape = 20) +
  geom_hline(yintercept=0, color = "gray25", alpha = 0.25) +
  coord_flip() +
  custom_minimal_theme() +
  ggtitle("parameter estimates for model 1 with 95% credible intervals")

preds <- posterior_samples(model_5, pars = ps2$variable)
str(preds)

preds <- as.tibble(preds)
preds <- preds %>% 
  gather(key = "variable", value = "estimate")

plot_dens <- preds %>% 
  ggplot(aes(x=estimate, y = variable)) +
  geom_density_ridges(fill = NA, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = 2)

plot_dens + custom_minimal_theme()


dfp <- ggpredict(model_5, terms = c("treatment", "trial"), type = "re") %>% 
  mutate(trial = group) %>% 
  mutate(group = NULL)


dfp %>% 
  ggplot(aes(x=x, y=predicted, color=trial)) +
  geom_line() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = trial), color = "transparent", alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d


int_conditions <- list(
  trial = setNames(c(1,2,3), c("trial 1", "trial 2", "trial 3")),
  treatment = 2:8)
int_conditions


effects <- marginal_effects(model_5, effects = "treatment:trial", int_conditions = int_conditions)
effects <- effects$`treatment:trial`

effects %>% 
  ggplot(aes(x=treatment, y=estimate__, color=trial)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = trial), color = "transparent", alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  custom_minimal_theme() +
  ggtitle("marginal effects of poisson multilevel model") +
  ylab("estimated latency")
# spaghetti plots

effects2 <- marginal_effects(model_5, effects = "treatment:trial", int_conditions = int_conditions, spaghetti = T, nsamples = 300)


spag <- attributes(effects2$`treatment:trial`)$spaghetti

spag <- as.tibble(spag)


spag %>% 
  ggplot(aes(x=treatment, y=estimate__, color=trial)) +
  geom_line(aes(group=sample__), alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  custom_minimal_theme() +
  ggtitle("marginal effects of poisson multilevel model") +
  ylab("estimated latency")

#### Redoing stuff





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
#   custom_minimal_theme()
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