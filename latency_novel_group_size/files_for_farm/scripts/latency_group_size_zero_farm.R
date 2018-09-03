library(package = "brms", lib.loc = "/home/mjculsha/RPackages/R3.4.4")
library(tidyverse)

# read data
#d <- read_csv("GroupSizeNovelAssay_FinalDataSheet_BitesFood.csv")\
d <- read_csv("data/for_farm.csv")
d <- d[,1:17]

# gather the latency measurements together
d <- gather(d, key = "individual", value = "latency", 8:15)

# strip out some extra words
d$individual <- as.numeric(str_replace_all(d$individual, "novel_bite", ""))

# if the latency value is actually a number, keep a number. If not, it's NA
d <- d %>% 
  mutate(
    latency = case_when(
      str_detect(latency, "^[0-9]+$") ~ as.numeric(latency),
      TRUE ~ NA_real_
    ))



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

saveRDS(model_2, "fit_models/model_2_fit.rds")
