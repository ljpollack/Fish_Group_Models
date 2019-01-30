# get session info
sess <- sessionInfo()

# if running on Ubuntu (on the FARM), then change lib paths and load brms from a specific location, otherwise just load brms
if (stringr::str_detect(sess$running, "Ubuntu")) {
  .libPaths(.libPaths()[2:3])
  .libPaths()
  library(brms, lib.loc = "/home/mjculsha/RPackages/R3.5.1")
} else {library(brms)}

# read data
d <- readRDS("data/cleaned/only_half_chase_group_size.rds")


# fit negbinom
model <- brm(data = d, family = negbinomial, 
               timechases ~ 1 + treatment + assay + trial +
                 (1 + treatment + assay + trial | group_ID) +
                 (1 + treatment + assay + trial | tank),
               prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                         set_prior("normal(0, 1)", class = "b"),
                         set_prior("cauchy(0, 2)", class = "sd"),
                         set_prior("lkj(4)", class = "cor")),
               iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores(), 
               control = list(adapt_delta = 0.999, max_treedepth = 25))

# save model
saveRDS(model, "fit_models/timechases_nov_nbin_fit.rds")
