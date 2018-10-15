library(tidyverse)
library(brms)
library(ggthemes)
library(ggExtra)
library(ggeffects)
library(broom)

d <- read_csv("latency_novel_group_size/data/cleaned_data.csv")
d$group_ID <- as.factor(d$group_ID)
d$tank <- as.factor(d$tank)
d <- d %>% 
  mutate(
    is_censored = case_when(latency == 300 ~ 1,
              TRUE ~ 0)
  )

# model_cens <- brm(data = d, family = poisson,
#                latency | cens(censored) ~ 1 + treatment + trial,
#                prior = c(set_prior("normal(0, 1)", class = "Intercept"),
#                          set_prior("normal(0, 1)", class = "b")), inits = "0",
#                iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores())

model_cens <- brm(data = d, family = poisson,
             latency | cens(is_censored) ~ 1 + treatment + trial, init_r = 20,
             prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                       set_prior("normal(0, 1)", class = "b")),
             iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores())

model_nocens <- brm(data = d, family = poisson,
                    latency ~ treatment + trial + treatment*trial,
                    iter = 5000, warmup = 1000, chains = 3, cores = future::availableCores())

launch_shinystan(model_cens)
launch_shinystan(model_nocens)

waic(model_cens, model_nocens)


dmin <- d %>% 
  filter(!is.na(latency)) %>% 
  group_by(group_ID) %>% 
  summarise(minlat = min(latency), treatment = mean(treatment))

dmin$treatment <- as.factor(dmin$treatment)
dmin %>% 
  ggplot(aes(x=minlat, fill = treatment)) + geom_histogram() + facet_wrap(aes(treatment))



dmin %>% 
  filter(treatment == 2) %>% 
  sample_n(size = 4)

d %>% 
  filter(!is.na(latency)) %>% 
  filter(treatment == 2) %>% 
  group_by(group_ID) %>% 
  mutate(meanlat = mean(latency)) %>% 
  sample_n()

sample(levels(d$group_ID), 4)
d[d$group_ID %in% sample(as.character(unique(d$group_ID[d$treatment==2])), 4),11] # this takes all rows for 4 random groups of 2




# this will generate as many fake groups of 8 from 4 actual groups of 2 as there are actual groups of 8. So if there are 10 groups of 8 in the data, this'll take 10 random groupings of 4 groups of 2
fake_8_from_2 <- vector(length = length(unique(d$group_ID[d$treatment==8])))
for (i in 1:length(unique(d$group_ID[d$treatment==8]))) {
  fake_8_from_2[i] <- min(d[d$group_ID %in% sample(as.character(unique(d$group_ID[d$treatment==2 & d$trial==1])), 4),11], na.rm = T)
}

actual_8 <- d %>% 
  filter(!is.na(latency)) %>% 
  filter(treatment == 8 & trial==1) %>% 
  group_by(group_ID) %>% 
  summarise(minlat = min(latency))

testdata <- cbind(actual_8, fake_8_from_2)[,2:3]
testdata <- gather(testdata, key = "data_type", value = "minimum_latency")

testdata %>% 
  ggplot(aes(x=minimum_latency, fill = data_type)) + geom_density() + facet_wrap(aes(data_type))



# same thing but we're just gonna make 1000 fake groups
fake_8_from_2 <- vector(length = 1000)
for (i in 1:1000) {
  fake_8_from_2[i] <- min(d[d$group_ID %in% sample(as.character(unique(d$group_ID[d$treatment==2 & d$trial==1])), 4),11], na.rm = T)
}

actual_8 <- d %>% 
  filter(!is.na(latency)) %>% 
  filter(treatment == 8 & trial==1) %>% 
  group_by(group_ID) %>% 
  summarise(minlat = min(latency))

ggplot(as.data.frame(fake_8_from_2), aes(x=fake_8_from_2)) + geom_density() + xlim(0,30) + ylim(0,0.5)
ggplot(actual_8, aes(x=minlat)) + geom_density() + xlim(0,30) + ylim(0,0.5)

# now with 2 groups of 4
fake_8_from_4 <- vector(length = 1000)
for (i in 1:1000) {
  fake_8_from_4[i] <- min(d[d$group_ID %in% sample(as.character(unique(d$group_ID[d$treatment==4 & d$trial==1])), 2),11], na.rm = T)
}

ggplot(as.data.frame(fake_8_from_4), aes(x=fake_8_from_4)) + geom_density() + xlim(0,30) + ylim(0,0.5)
ggplot(actual_8, aes(x=minlat)) + geom_density() + xlim(0,30) + ylim(0,0.5)
