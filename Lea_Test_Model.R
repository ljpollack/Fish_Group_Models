#rm(list=ls())
library(rethinking)
library(tidyverse)

data <- NULL
data$latency <- rpois(120, 4)
data$latency
data$mean_size <- rnorm(120, mean = 4, sd = 1)
data$sd_size <- rnorm(120, mean = 1, sd= 0.2)
data$group_id <- rep(1:40, length=120)
data <- as.data.frame(data)
data <- data %>%
  arrange(group_id)
data$trial_number <- rep(1:3, length=120)
data <- as.data.frame(data)
data

# MODEL TIME BAYBIIIIIEEEEEEE

# this is a toy model, only looking at two predictors and their interaction, plus varying intercepts for rep, and varying intercepts and slopes for group_id

m1 <- map2stan(
  alist(
    # likeliood
    latency ~ dpois(lambda),
    # top-level linear model, where we write down all the predictors and interactions
    log(lambda) <- A + BM*mean_size + BS*sd_size + BMS*mean_size*sd_size,
    # now the sub-models, where we take the intercept and slope and decompose them into their grand means (a, bm, bs, bms) and the "offsets" that occur with each grouping (group or trial)
    A <- a + a_group_id[group_id] + a_trial[trial_number],
    BM <- bm + bm_group_id[group_id],
    BS <- bs + bm_group_id[group_id],
    BMS <- bms + bms_group_id[group_id],
    # adaptive priors
    # this first one is a 4-dimensional multivariate normal prior, where you're saying that within a group_id, the varying intercepts and varying slopes are correlated based on Rho
    c(a_group_id,bm_group_id,bm_group_id,bms_group_id)[group_id] ~
      dmvnorm2(0,sigma_group_id,Rho_group_id),
    a_trial[trial_number] ~ dnorm(0, sigma_trial),
    # fixed priors
    c(a,bm,bs,bms) ~ dnorm(0,1),
    sigma_group_id ~ dcauchy(0,2),
    sigma_trial ~ dcauchy(0,2),
    Rho_group_id ~ dlkjcorr(4)
  ) , data=data , iter=5000 , warmup=1000 , chains=2 , cores=2 )
  

# hell yes this works! however, we've got a lot of divergent chains, so I think it's worth implementing the non-centered paramaterization


p <- precis(m1)
pdat <- as.data.frame(p@output)
pdat$param <- rownames(pdat)

pdat %>% ggplot(aes(x=Mean, y=param))+
  geom_point()

pdat[1:40,] %>% ggplot(aes(x=Mean, y=param))+
  geom_point()

pdat[40:80,] %>% ggplot(aes(x=Mean, y=param))+
  geom_point()

pdat[80:120,] %>% ggplot(aes(x=Mean, y=param))+
  geom_point()

pdat[120:160,] %>% ggplot(aes(x=Mean, y=param))+
  geom_point()

precis(m1)
compare(m1)



# let's try the non-centered paramaterization

m1.NC <- map2stan(
  alist(
    # likeliood
    latency ~ dpois(lambda),
    # top-level linear model, where we write down all the predictors and interactions
    log(lambda) <- A + BM*mean_size + BS*sd_size + BMS*mean_size*sd_size,
    # now the sub-models, where we take the intercept and slope and decompose them into their grand means (a, bm, bs, bms) and their varying intercepts (for A) and varying slopes (for the beta values)
    A <- a + a_group_id[group_id] + a_trial[trial_number],
    BM <- bm + bm_group_id[group_id],
    BS <- bs + bm_group_id[group_id],
    BMS <- bms + bms_group_id[group_id],
    # adaptive priors
    # this first one is a 4-dimensional multivariate normal prior, where you're saying that within a group_id, the varying intercepts and varying slopes are correlated based on Rho
    c(a_group_id,bm_group_id,bm_group_id,bms_group_id)[group_id] ~
      dmvnormNC(sigma_group_id,Rho_group_id),
    a_trial[trial_number] ~ dnorm(0, sigma_trial),
    # fixed priors
    c(a,bm,bs,bms) ~ dnorm(0,1),
    sigma_group_id ~ dcauchy(0,2),
    sigma_trial ~ dcauchy(0,2),
    Rho_group_id ~ dlkjcorr(4)
  ) , data=data , iter=5000 , warmup=1000 , chains=2 , cores=2 )

par(mfrow=c(1,1))
plot(precis(m1.NC, depth = 2))
WAIC(m1.NC)
compare(m1,m1.NC)
