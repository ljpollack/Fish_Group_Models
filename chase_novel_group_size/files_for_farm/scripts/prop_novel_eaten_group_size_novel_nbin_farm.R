.libPaths(.libPaths()[2:3])
.libPaths()
library(package = "brms", lib.loc = "/home/mjculsha/RPackages/R3.4.4")
library(tidyverse)

# read data
d <- readRDS("data/novel_food_proportion_eaten_group_size.rds")
d$group_ID <- as.factor(d$Group)
d$tank <- as.factor(d$Tank)

d$trial <- d$Trial


# fit beta
model <- brm(data = d, family = binomial(link = "logit"), 
               novel_eaten | trials(novel_input) ~ 1 + treatment +
                 (1 + treatment + trial | group_ID) +
                 (1 + treatment + trial | tank),
               prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                         set_prior("normal(0, 1)", class = "b"),
                         set_prior("cauchy(0, 2)", class = "sd"),
                         set_prior("lkj(4)", class = "cor")),
               iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(), 
               control = list(adapt_delta = 0.98, max_treedepth = 15))

# save model
saveRDS(model, "fit_models/fit_prop_novel_eaten_group_size_logistic_farm.rds")
