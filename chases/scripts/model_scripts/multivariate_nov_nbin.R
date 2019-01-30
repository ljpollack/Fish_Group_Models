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

my_prior <- get_prior(formula = bf(cbind(numchases, timechases) ~ 1 + treatment + assay + trial +
                                  (1 + treatment + assay + trial | group_ID) +
                                  (1 + treatment + assay + trial | tank)), data = d, family = negbinomial)

# fit negbinom
model <- brm(data = d, family = negbinomial, 
               cbind(numchases, timechases) ~ 1 + treatment + assay + trial +
                 (1 + treatment + assay + trial | group_ID) +
                 (1 + treatment + assay + trial | tank),
               prior = my_prior,
               iter = 5000, warmup = 1000, chains = 4, cores = 4, 
               control = list(adapt_delta = 0.98, max_treedepth = 15))

# save model
saveRDS(model, "fit_models/multivariate_nov_nbin_fit.rds")
