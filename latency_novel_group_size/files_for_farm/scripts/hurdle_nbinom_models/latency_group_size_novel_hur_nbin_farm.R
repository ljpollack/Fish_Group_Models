.libPaths(.libPaths()[2:3])
.libPaths()
library(package = "brms", lib.loc = "/home/mjculsha/RPackages/R3.4.4")
library(tidyverse)

# read data
d <- readRDS("data/only_half_latency_novel_food_group_size.rds")
d$group_ID <- as.factor(d$group)
d$tank <- as.factor(d$tank)

d$latency[d$latency == 300] <- 0

# fit hurdle-inflated negbinom
 model <- brm(data = d, family = hurdle_negbinomial, 
               latency ~ 1 + treatment + novel_food +
                 (1 + treatment + novel_food | group_ID) +
                 (1 + treatment + novel_food | tank),
               prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                         set_prior("normal(0, 1)", class = "b"),
                         set_prior("cauchy(0, 2)", class = "sd"),
                         set_prior("lkj(4)", class = "cor")),
               iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(), 
               control = list(adapt_delta = 0.98, max_treedepth = 15))

# save model
saveRDS(model, "fit_models/fit_latency_group_size_novel_hur_nbin_farm.rds")
