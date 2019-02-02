# get session info
sess <- sessionInfo()

# if running on Ubuntu (on the FARM), then change lib paths and load brms from a specific location, otherwise just load brms
if (stringr::str_detect(sess$running, "Ubuntu")) {
  .libPaths(.libPaths()[2:3])
  .libPaths()
  library(brms, lib.loc = "/home/mjculsha/RPackages/R3.5.1")
} else {library(brms)}

# read data
d <- readRDS("data/cleaned/latency_novel_food.rds")

# fit hurdle-inflated negbinom
 model <- brm(data = d, family = hurdle_negbinomial, 
               latency ~ 1 + treatment + novel_food +
                 (1 + treatment + novel_food | group_ID) +
                 (1 + treatment + novel_food | tank),
               prior = c(set_prior("normal(1, 1)", class = "Intercept"),
                         set_prior("normal(0, 1)", class = "b"),
                         set_prior("cauchy(0, 2)", class = "sd"),
                         set_prior("lkj(4)", class = "cor")),
               iter = 5000, warmup = 2000, chains = 4, cores = 4, 
               control = list(adapt_delta = 0.99999999999, max_treedepth = 35))

# save model
 saveRDS(model, "fit_models/lat_nov_hurd_nbin_fit.rds")
