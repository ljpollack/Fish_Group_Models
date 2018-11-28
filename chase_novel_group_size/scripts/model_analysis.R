source("../../minimal_ggplot_theme.R")
library(tidyverse)
library(ggridges)
model1 <- readRDS("chase_novel_group_size/fit_models/fit_numchases_group_size_novel_hur_nbin_farm.rds")
launch_shinystan(model1)
summary(model1)

d$assay %>% unique()
int_conditions <- list(
  assay = setNames(c("activity","novel","food"), c("activity", "novel", "food")),
  treatment = 2:8)

effects <- marginal_effects(model1, effects = "treatment:assay", int_conditions = int_conditions, spaghetti = T, nsamples = 300)
spag <- attributes(effects$`treatment:assay`)$spaghetti %>% as.tibble()

spag %>% 
  ggplot(aes(x=treatment, y=estimate__, color=assay)) +
  geom_line(aes(group=sample__), alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  minimal_ggplot_theme() +
  ggtitle("marginal effects of negbinomial multilevel model") +
  ylab("estimated numchases")

ps <- posterior_summary(model1) %>% unlist() %>% as.data.frame()

ps2 <- ps %>%
  head(n=15) 
ps2 <- ps2 %>% 
  mutate(variable = rownames(ps2))
ps2 %>% 
  ggplot(aes(y=Estimate, x = variable))+
  geom_pointrange(aes(ymin=Q2.5, ymax=Q97.5, group=variable), size = 2/5, shape = 20) +
  geom_hline(yintercept=0, color = "gray25", alpha = 0.25) +
  coord_flip() +
  minimal_ggplot_theme() +
  ggtitle("parameter estimates for model 1 with 95% credible intervals")

preds <- posterior_samples(model1, pars = ps2$variable) %>% as.tibble()
preds <- preds %>% 
  gather(key = "variable", value = "estimate")

plot_dens <- preds %>% 
  ggplot(aes(x=estimate, y = variable)) +
  geom_density_ridges(fill = NA, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = 2)

plot_dens + minimal_ggplot_theme() + ggtitle("Parameter Density Estimates")
ggsave("chase_novel_group_size/images/numchases_half_data_parameters.jpg")

effects <- marginal_effects(model1, effects = "treatment:assay", int_conditions = int_conditions)
effects <- effects$`treatment:assay`

effects %>% 
  ggplot(aes(x=treatment, y=estimate__, color=assay)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = assay), color = "transparent", alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  minimal_ggplot_theme() +
  ggtitle("marginal effects of negbinomial multilevel model") +
  ylab("estimated numchases") + xlab("group size")

ggsave("chase_novel_group_size/images/numchases_half_data_marginal_effects.jpg")
# shit I think I have to redo this model, taking into account trial # as well as treatment and assay


# timechases model that includes trial
model2 <- readRDS("chase_novel_group_size/fit_models/fit_timechases_group_size_novel_hur_nbin_farm.rds")

model2$data
int_conditions <- list(
  assay = setNames(c("activity","novel","food"), c("activity", "novel", "food")),
  treatment = 2:8)

effects <- marginal_effects(model2, effects = "treatment:assay", int_conditions = int_conditions, spaghetti = T, nsamples = 300)
spag <- attributes(effects$`treatment:assay`)$spaghetti %>% as.tibble()

spag %>% 
  ggplot(aes(x=treatment, y=estimate__, color=assay)) +
  geom_line(aes(group=sample__), alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  minimal_ggplot_theme() +
  ggtitle("marginal effects of negbinomial multilevel model") +
  ylab("estimated timechases")

ps <- posterior_summary(model2) %>% unlist() %>% as.data.frame()

ps2 <- ps %>%
  head(n=15) 
ps2 <- ps2 %>% 
  mutate(variable = rownames(ps2))
ps2 %>% 
  ggplot(aes(y=Estimate, x = variable))+
  geom_pointrange(aes(ymin=Q2.5, ymax=Q97.5, group=variable), size = 2/5, shape = 20) +
  geom_hline(yintercept=0, color = "gray25", alpha = 0.25) +
  coord_flip() +
  minimal_ggplot_theme() +
  ggtitle("parameter estimates for model 1 with 95% credible intervals")

preds <- posterior_samples(model2, pars = ps2$variable) %>% as.tibble()
preds <- preds %>% 
  gather(key = "variable", value = "estimate")

plot_dens <- preds %>% 
  ggplot(aes(x=estimate, y = variable)) +
  geom_density_ridges(fill = NA, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = 2)

plot_dens + minimal_ggplot_theme() + ggtitle("Parameter Density Estimates")
ggsave("chase_novel_group_size/images/timechases_half_data_parameters.jpg")

effects <- marginal_effects(model2, effects = "treatment:assay", int_conditions = int_conditions)
effects <- effects$`treatment:assay`

effects %>% 
  ggplot(aes(x=treatment, y=estimate__, color=assay)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = assay), color = "transparent", alpha = 0.1) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  minimal_ggplot_theme() +
  ggtitle("marginal effects of negbinomial multilevel model") +
  ylab("estimated numchases") + xlab("group size")

ggsave("chase_novel_group_size/images/timechases_half_data_marginal_effects.jpg")


#### multivariate now
model3 <- readRDS("chase_novel_group_size/fit_models/fit_multivariate_group_size_novel_nbin_farm.rds")
launch_shinystan(model3)
summary(model3)
