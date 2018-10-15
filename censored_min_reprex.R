library(package = "brms")
library(tidyverse)

# read data
d <- read_csv("latency_novel_group_size/data/cleaned_data.csv")
d$group_ID <- as.factor(d$group_ID)
d$tank <- as.factor(d$tank)

d$latency[d$latency == 300] <- 0

ggplot(d, aes(x=latency)) + geom_histogram()



set.seed(1)
data <- data.frame(
  response = c(rpois(80,150), rep(300,20)),
  group = rep(1:5, 20),
  treatment = rep(c("a", "b", "b", "c", "c"), 20)
)

data <- data %>% 
  mutate(
    is_censored = case_when(response == 300 ~ 1,
                            TRUE ~ 0)
  )

ggplot(data, aes(x=response)) + geom_histogram()

# fit model
model_1 <- brm(data = data, family = poisson, 
               response | cens(is_censored) ~ 1 + treatment +
                 (1 + treatment | group),
               prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                         set_prior("normal(0, 1)", class = "b"),
                         set_prior("cauchy(0, 2)", class = "sd"),
                         set_prior("lkj(4)", class = "cor")),
               iter = 5000, warmup = 1000, chains = 1, cores = future::availableCores(),
               control = list(adapt_delta = 0.9, max_treedepth = 15))

