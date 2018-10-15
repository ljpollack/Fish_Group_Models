library(package = "brms", lib.loc = "/home/mjculsha/RPackages/R3.4.4")
library(tidyverse)

# read data
d <- read_csv("data/cleaned_data_farm.csv")
d$group_ID <- as.factor(d$group_ID)
d$tank <- as.factor(d$tank)

d$latency[d$latency == 300] <- 0

# fit zero-inflated poisson
model_2 <- brm(data = d, family = zero_inflated_poisson, 
               latency ~ 1 + treatment + trial +
                 (1 + treatment + trial | group_ID) +
                 (1 + treatment + trial | tank),
               prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                         set_prior("normal(0, 1)", class = "b"),
                         set_prior("cauchy(0, 2)", class = "sd"),
                         set_prior("lkj(4)", class = "cor")),
               iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(), 
	       control = list(adapt_delta = 0.98, max_treedepth = 15), save_model = "fit_models/stan_model_2")

# save model
saveRDS(model_2, "fit_models/model_2_fit.rds")
