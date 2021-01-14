# Temperature variability, moisture and biotic interactions drive shrub species abundance along a coastal-inland gradient in arctic Greenland

# von Oppen, J. et al. 2020

# Interaction models: Community interactions X temperature

# Jonathan von Oppen, Aarhus University, Sept 2020

# contact: jonathan.vonoppen@bios.au.dk



rm(list = ls())

# Dependencies ----
if (!require('pacman')) install.packages('pacman', repos="https://cloud.r-project.org")
pacman::p_load(tidyverse, # set of packages for data manipulation, exploration and visualisation
               tidylog,   # to document dplyr & tidyr operations
               rjags,     # to link JAGS and R
               R2jags,    # to link JAGS and R
               cowplot)   # combine plot panels


# _____________ ----
# Load & prepare input data ----
load(file = file.path("data", "processed", "model_input_data_twi", "shrub_gradient_species.datasets.Rdata"))


# >> Betula nana ----
BetNan_tXi.tot <- BetNan.tot %>% 
  
    # select relevant columns
    select(plotgroup.NUM,
           plot,
           tempjja_C = tempjjaC,
           interact_C = competC,
           taxon,
           abundance = cover,
           abund_discrete = cover_discrete)


# create subset for discrete and continuous data
BetNan_tXi.dis <- BetNan_tXi.tot %>% 
  filter(abund_discrete == 1)

BetNan_tXi.cont <- BetNan_tXi.tot %>% 
  filter(abund_discrete == 0)


# Assemble data for JAGS 
BetNan_tXi.jags_data <- list(
  
  # plot level, for discrete...
  abund.dis = BetNan_tXi.dis$abundance,
  tempjja.dis = BetNan_tXi.dis$tempjja_C,
  interact.dis = BetNan_tXi.dis$interact_C,
  plotgroup.dis = BetNan_tXi.dis$plotgroup.NUM,
  N_discrete = nrow(BetNan_tXi.dis),
  
  # ...and continuous part of the data
  abund.cont = BetNan_tXi.cont$abundance,
  tempjja.cont = BetNan_tXi.cont$tempjja_C,
  interact.cont = BetNan_tXi.cont$interact_C,
  plotgroup.cont = BetNan_tXi.cont$plotgroup.NUM,
  N_cont = nrow(BetNan_tXi.cont),
  
  # plotgroup level
  tempjja.tot = BetNan_tXi.tot %>% group_by(plotgroup.NUM) %>% summarise(tempjja.tot = mean(tempjja_C)) %>% pull(tempjja.tot),
  N_plotgroups = length(unique(BetNan_tXi.tot$plotgroup.NUM)),
  
  # subset of values for prediction, for each predictor...
  xhat_tempjja = seq(from = min(BetNan_tXi.tot$tempjja_C), 
                     to = max(BetNan_tXi.tot$tempjja_C), 
                     length.out = 100),
  Nxhat = 100,
  
  # ...and for predictions at high/low temperature levels
  xhat_interact2 = as.numeric(c(quantile(BetNan_tXi.tot$interact_C, 0.05),
                                quantile(BetNan_tXi.tot$interact_C, 0.95))), 
  Nxhat2 = 2
)

# check structure
str(BetNan_tXi.jags_data)


# >> Empetrum nigrum ----
EmpNig_tXi.tot <- EmpNig.tot %>% 
  
  # select relevant columns
  select(plotgroup.NUM,
         plot,
         tempjja_C = tempjjaC,
         interact_C = competC,
         taxon,
         abundance = cover,
         abund_discrete = cover_discrete)


# create subset for discrete and continuous data
EmpNig_tXi.dis <- EmpNig_tXi.tot %>% 
  filter(abund_discrete == 1)

EmpNig_tXi.cont <- EmpNig_tXi.tot %>% 
  filter(abund_discrete == 0)


# Assemble data for JAGS
EmpNig_tXi.jags_data <- list(
  
  # plot level, for discrete...
  abund.dis = EmpNig_tXi.dis$abundance,
  tempjja.dis = EmpNig_tXi.dis$tempjja_C,
  interact.dis = EmpNig_tXi.dis$interact_C,
  plotgroup.dis = EmpNig_tXi.dis$plotgroup.NUM,
  N_discrete = nrow(EmpNig_tXi.dis),
  
  # ...and continuous part of the data
  abund.cont = EmpNig_tXi.cont$abundance,
  tempjja.cont = EmpNig_tXi.cont$tempjja_C,
  interact.cont = EmpNig_tXi.cont$interact_C,
  plotgroup.cont = EmpNig_tXi.cont$plotgroup.NUM,
  N_cont = nrow(EmpNig_tXi.cont),
  
  # plotgroup level
  tempjja.tot = EmpNig_tXi.tot %>% group_by(plotgroup.NUM) %>% summarise(tempjja.tot = mean(tempjja_C)) %>% pull(tempjja.tot),
  N_plotgroups = length(unique(EmpNig_tXi.tot$plotgroup.NUM)),
  
  # subset of values for prediction, for each predictor...
  xhat_tempjja = seq(from = min(EmpNig_tXi.tot$tempjja_C), 
                     to = max(EmpNig_tXi.tot$tempjja_C), 
                     length.out = 100),
  Nxhat = 100,
  
  # ...and for predictions at high/low temperature levels
  xhat_interact2 = as.numeric(c(quantile(EmpNig_tXi.tot$interact_C, 0.05),
                                quantile(EmpNig_tXi.tot$interact_C, 0.95))), 
  Nxhat2 = 2
)

# check structure
str(EmpNig_tXi.jags_data)


# >> Rhododendron groenlandicum ----
RhoGro_tXi.tot <- RhoGro.tot %>% 
  
  # select relevant columns
  select(plotgroup.NUM,
         plot,
         tempjja_C = tempjjaC,
         interact_C = competC,
         taxon,
         abundance = cover,
         abund_discrete = cover_discrete)


# create subset for discrete and continuous data
RhoGro_tXi.dis <- RhoGro_tXi.tot %>% 
  filter(abund_discrete == 1)

RhoGro_tXi.cont <- RhoGro_tXi.tot %>% 
  filter(abund_discrete == 0)


# Assemble data for JAGS
RhoGro_tXi.jags_data <- list(
  
  # plot level, for discrete...
  abund.dis = RhoGro_tXi.dis$abundance,
  tempjja.dis = RhoGro_tXi.dis$tempjja_C,
  interact.dis = RhoGro_tXi.dis$interact_C,
  plotgroup.dis = RhoGro_tXi.dis$plotgroup.NUM,
  N_discrete = nrow(RhoGro_tXi.dis),
  
  # ...and continuous part of the data
  abund.cont = RhoGro_tXi.cont$abundance,
  tempjja.cont = RhoGro_tXi.cont$tempjja_C,
  interact.cont = RhoGro_tXi.cont$interact_C,
  plotgroup.cont = RhoGro_tXi.cont$plotgroup.NUM,
  N_cont = nrow(RhoGro_tXi.cont),
  
  # plotgroup level
  tempjja.tot = RhoGro_tXi.tot %>% group_by(plotgroup.NUM) %>% summarise(tempjja.tot = mean(tempjja_C)) %>% pull(tempjja.tot),
  N_plotgroups = length(unique(RhoGro_tXi.tot$plotgroup.NUM)),
  
  # subset of values for prediction, for each predictor...
  xhat_tempjja = seq(from = min(RhoGro_tXi.tot$tempjja_C), 
                     to = max(RhoGro_tXi.tot$tempjja_C), 
                     length.out = 100),
  Nxhat = 100,
  
  # ...and for predictions at high/low temperature levels
  xhat_interact2 = as.numeric(c(quantile(RhoGro_tXi.tot$interact_C, 0.05),
                                quantile(RhoGro_tXi.tot$interact_C, 0.95))), 
  Nxhat2 = 2
)

# check structure
str(RhoGro_tXi.jags_data)


# >> Salix glauca ----
SalGla_tXi.tot <- SalGla.tot %>% 
  
  # select relevant columns
  select(plotgroup.NUM,
         plot,
         tempjja_C = tempjjaC,
         interact_C = competC,
         taxon,
         abundance = cover,
         abund_discrete = cover_discrete)


# create subset for discrete and continuous data
SalGla_tXi.dis <- SalGla_tXi.tot %>% 
  filter(abund_discrete == 1)

SalGla_tXi.cont <- SalGla_tXi.tot %>% 
  filter(abund_discrete == 0)


# Assemble data for JAGS
SalGla_tXi.jags_data <- list(
  
  # plot level, for discrete...
  abund.dis = SalGla_tXi.dis$abundance,
  tempjja.dis = SalGla_tXi.dis$tempjja_C,
  interact.dis = SalGla_tXi.dis$interact_C,
  plotgroup.dis = SalGla_tXi.dis$plotgroup.NUM,
  N_discrete = nrow(SalGla_tXi.dis),
  
  # ...and continuous part of the data
  abund.cont = SalGla_tXi.cont$abundance,
  tempjja.cont = SalGla_tXi.cont$tempjja_C,
  interact.cont = SalGla_tXi.cont$interact_C,
  plotgroup.cont = SalGla_tXi.cont$plotgroup.NUM,
  N_cont = nrow(SalGla_tXi.cont),
  
  # plotgroup level
  tempjja.tot = SalGla_tXi.tot %>% group_by(plotgroup.NUM) %>% summarise(tempjja.tot = mean(tempjja_C)) %>% pull(tempjja.tot),
  N_plotgroups = length(unique(SalGla_tXi.tot$plotgroup.NUM)),
  
  # subset of values for prediction, for each predictor...
  xhat_tempjja = seq(from = min(SalGla_tXi.tot$tempjja_C), 
                     to = max(SalGla_tXi.tot$tempjja_C), 
                     length.out = 100),
  Nxhat = 100,
  
  # ...and for predictions at high/low temperature levels
  xhat_interact2 = as.numeric(c(quantile(SalGla_tXi.tot$interact_C, 0.05),
                                quantile(SalGla_tXi.tot$interact_C, 0.95))), 
  Nxhat2 = 2
)

# check structure
str(SalGla_tXi.jags_data)


# >> Vaccinium uliginosum ----
VacUli_tXi.tot <- VacUli.tot %>% 
  
  # select relevant columns
  select(plotgroup.NUM,
         plot,
         tempjja_C = tempjjaC,
         interact_C = competC,
         taxon,
         abundance = cover,
         abund_discrete = cover_discrete)


# create subset for discrete and continuous data
VacUli_tXi.dis <- VacUli_tXi.tot %>% 
  filter(abund_discrete == 1)

VacUli_tXi.cont <- VacUli_tXi.tot %>% 
  filter(abund_discrete == 0)


# Assemble data for JAGS
VacUli_tXi.jags_data <- list(
  
  # plot level, for discrete...
  abund.dis = VacUli_tXi.dis$abundance,
  tempjja.dis = VacUli_tXi.dis$tempjja_C,
  interact.dis = VacUli_tXi.dis$interact_C,
  plotgroup.dis = VacUli_tXi.dis$plotgroup.NUM,
  N_discrete = nrow(VacUli_tXi.dis),
  
  # ...and continuous part of the data
  abund.cont = VacUli_tXi.cont$abundance,
  tempjja.cont = VacUli_tXi.cont$tempjja_C,
  interact.cont = VacUli_tXi.cont$interact_C,
  plotgroup.cont = VacUli_tXi.cont$plotgroup.NUM,
  N_cont = nrow(VacUli_tXi.cont),
  
  # plotgroup level
  tempjja.tot = VacUli_tXi.tot %>% group_by(plotgroup.NUM) %>% summarise(tempjja.tot = mean(tempjja_C)) %>% pull(tempjja.tot),
  N_plotgroups = length(unique(VacUli_tXi.tot$plotgroup.NUM)),
  
  # subset of values for prediction, for each predictor...
  xhat_tempjja = seq(from = min(VacUli_tXi.tot$tempjja_C), 
                     to = max(VacUli_tXi.tot$tempjja_C), 
                     length.out = 100),
  Nxhat = 100,
  
  # ...and for predictions at high/low temperature levels
  xhat_interact2 = as.numeric(c(quantile(VacUli_tXi.tot$interact_C, 0.05),
                                quantile(VacUli_tXi.tot$interact_C, 0.95))), 
  Nxhat2 = 2
)

# check structure
str(VacUli_tXi.jags_data)


# _____________ ----
# Specify model ----

# >> BetNan, EmpNig, SalGla, VacUli: linear temperature relationship ----
write("
  
  model{
# priors 

  intercept ~ dnorm(0, 0.0001)
  
  b.interact ~ dnorm(0, 0.0001)
  
  b.tempjja.x ~ dnorm(0, 0.001)

  b.tempXinteract ~ dnorm(0, 0.001)

  sigma.plot ~ dunif(0,100)
  tau.plot <- 1/(sigma.plot * sigma.plot)
  
  sigma.plotgroup ~ dunif(0,100)
  tau.plotgroup <- 1/(sigma.plotgroup * sigma.plotgroup)
  
  phi ~ dgamma(0.1, 0.1)
  
  
  # plot level - discrete part
  for (i in 1:N_discrete){ 
    abund.dis[i] ~ dbern(mu[i])
    logit(mu[i]) <- b_plotgroup[plotgroup.dis[i]] + # ~= random effect of plot group
      b.interact * interact.dis[i] + 
      b.tempXinteract * tempjja.dis[i] * interact.dis[i]       # for interaction
  }
  
  
  # plot level - continuous part
  for (j in 1:N_cont){
    abund.cont[j] ~ dbeta(p[j], q[j])
    p[j] <- mu2[j] * phi
    q[j] <- (1 - mu2[j]) * phi
    logit(mu2[j]) <- b_plotgroup[plotgroup.cont[j]] + # ~= random effect of plot group
      b.interact * interact.cont[j] + 
      b.tempXinteract * tempjja.cont[j] * interact.cont[j]       # for interaction
  }
  
  
  # plotgroup level
  for (k in 1:N_plotgroups){ # length of total plotgroup
    b_plotgroup[k] ~ dnorm(mu.plotgroup[k],tau.plotgroup)
    mu.plotgroup[k] <- intercept + 
      
      # plot group level predictors, linear and quadratic term
      b.tempjja.x * tempjja.tot[k]
  }
  
  
  # derived parameters 
  for (m in 1:Nxhat){
    for (p in 1:Nxhat2){
      phat_tempXinteract[m,p] <- intercept +
          b.tempjja.x * xhat_tempjja[m] +
          b.interact * xhat_interact2[p] +
          b.tempXinteract * xhat_tempjja[m] * xhat_interact2[p]       # for interaction
    }
  }
  }
", file.path("models_general", "model_files", "shrub_gradient.tempXinteract.BetNan_EmpNig_SalGla_VacUli.jags"))


# >> RhoGro: unimodal relationship with temperature ----
write("
  
  model{
# priors 

  intercept ~ dnorm(0, 0.0001)
  
  b.interact ~ dnorm(0, 0.0001)
  
  b.tempjja.x ~ dnorm(0, 0.001)
  b.tempjja.x2 ~ dnorm(0, 0.001)

  b.tempXinteract ~ dnorm(0, 0.001)
  b.tempXinteract2 ~ dnorm(0, 0.001)

  sigma.plot ~ dunif(0,100)
  tau.plot <- 1/(sigma.plot * sigma.plot)
  
  sigma.plotgroup ~ dunif(0,100)
  tau.plotgroup <- 1/(sigma.plotgroup * sigma.plotgroup)
  
  phi ~ dgamma(0.1, 0.1)
  
  
  # plot level - discrete part
  for (i in 1:N_discrete){ 
    abund.dis[i] ~ dbern(mu[i])
    logit(mu[i]) <- b_plotgroup[plotgroup.dis[i]] + # ~= random effect of plot group
      b.interact * interact.dis[i] + 
      b.tempXinteract * tempjja.dis[i] * interact.dis[i] +       # for interaction
      b.tempXinteract2 * (tempjja.dis[i]^2) * interact.dis[i]    # for interaction
  }
  
  
  # plot level - continuous part
  for (j in 1:N_cont){
    abund.cont[j] ~ dbeta(p[j], q[j])
    p[j] <- mu2[j] * phi
    q[j] <- (1 - mu2[j]) * phi
    logit(mu2[j]) <- b_plotgroup[plotgroup.cont[j]] + # ~= random effect of plot group
      b.interact * interact.cont[j] + 
      b.tempXinteract * tempjja.cont[j] * interact.cont[j] +       # for interaction
      b.tempXinteract2 * (tempjja.cont[j]^2) * interact.cont[j]    # for interaction
  }
  
  
  # plotgroup level
  for (k in 1:N_plotgroups){ # length of total plotgroup
    b_plotgroup[k] ~ dnorm(mu.plotgroup[k],tau.plotgroup)
    mu.plotgroup[k] <- intercept + 
      
      # plot group level predictors, linear and quadratic term
      b.tempjja.x * tempjja.tot[k] + 
      b.tempjja.x2 * (tempjja.tot[k]^2) 
  }
  
  
  # derived parameters # have to be calculated at taxon level?
  for (m in 1:Nxhat){
    for (p in 1:Nxhat2){
      phat_tempXinteract[m,p] <- intercept +
          b.tempjja.x * xhat_tempjja[m] +
          b.tempjja.x2 * (xhat_tempjja[m]^2) +
          b.interact * xhat_interact2[p] +
          b.tempXinteract * xhat_tempjja[m] * xhat_interact2[p] +       # for interaction
          b.tempXinteract2 * (xhat_tempjja[m]^2) * xhat_interact2[p]    # for interaction
    }
  }
  }
", file.path("models_general", "model_files", "shrub_gradient.tempXinteract.RhoGro.jags"))


# _____________ ----
# Specify parameters ----

params_tempXinteract.BetNan_EmpNig_SalGla_VacUli <- c("intercept",
                                                      "b.tempjja.x",
                                                      "b.interact", 
                                                      "b.tempXinteract",
                                                      "b_plotgroup[1]","b_plotgroup[2]","b_plotgroup[3]",
                                                      "sigma.plotgroup",
                                                      "phi",
                                                      "phat_tempXinteract")

params_tempXinteract.RhoGro <- c("intercept",
                                 "b.tempjja.x", "b.tempjja.x2",
                                 "b.interact", 
                                 "b.tempXinteract", "b.tempXinteract2",
                                 "b_plotgroup[1]","b_plotgroup[2]","b_plotgroup[3]",
                                 "sigma.plotgroup",
                                 "phi",
                                 "phat_tempXinteract")


# _____________ ----
# Run & evaluate models ----

# >> Betula nana ----
model_out.shrub_gradient.tempXinteract.BetNan <- jags(BetNan_tXi.jags_data,              # input data
                                                      inits = NULL,                      # JAGS to create initial values
                                                      params_tempXinteract.BetNan_EmpNig_SalGla_VacUli,              # parameters to be saved
                                                      model.file = file.path("models_general", "model_files", "shrub_gradient.tempXinteract.BetNan_EmpNig_SalGla_VacUli.jags"), 
                                                      n.chains = 3,                      # no. Markov chains
                                                      n.iter = 100000, n.burnin = 70000, # no. iterations & burn-in fraction per chain
                                                      n.thin = 2,                        # thinning rate
                                                      DIC = FALSE,                       # do not compute deviance, pD, and DIC
                                                      working.directory = NULL, 
                                                      progress.bar = "text") 

# plot(model_out.shrub_gradient.tempXinteract.BetNan) #check convergence, etc.


# extract coefficients 
coeff.shrub_gradient.tempXinteract.BetNan <- model_out.shrub_gradient.tempXinteract.BetNan$BUGSoutput$summary %>% 
  as.data.frame %>% 
  select('mean','sd','2.5%','97.5%','Rhat') %>% 
  # add identifying info to data frame
  rownames_to_column(var = "param")

# add 90% CIs
ci_90.tempXinteract.BetNan <- data.frame(q5 = NA, q95 = NA, param = NA)
for (param in 1:(length(model_out.shrub_gradient.tempXinteract.BetNan$BUGSoutput$sims.list)-4)){
  ci_90.tempXinteract.BetNan[param,1:2] <- quantile(data.frame(model_out.shrub_gradient.tempXinteract.BetNan$BUGSoutput$sims.list[param])[,1], probs = c(0.05, 0.95))
  ci_90.tempXinteract.BetNan[param, 3] <- names(data.frame(model_out.shrub_gradient.tempXinteract.BetNan$BUGSoutput$sims.list))[param]
}

# join to coefficients table
coeff.shrub_gradient.tempXinteract.BetNan <- coeff.shrub_gradient.tempXinteract.BetNan %>% 
  left_join(ci_90.tempXinteract.BetNan, by = "param") %>% 
  # reorder and rename cols
  select(param, mean, sd, 
         l95 = "2.5%",
         l90 = q5,
         u90 = q95,
         u95 = "97.5%",
         Rhat)

save(coeff.shrub_gradient.tempXinteract.BetNan, file = file.path("models_general", "model_outputs", "model_output_temp_x_interact.BetNan.Rdata"))


# >> Empetrum nigrum ----
model_out.shrub_gradient.tempXinteract.EmpNig <- jags(EmpNig_tXi.jags_data,              # input data
                                                      inits = NULL,                      # JAGS to create initial values
                                                      params_tempXinteract.BetNan_EmpNig_SalGla_VacUli,              # parameters to be saved
                                                      model.file = file.path("models_general", "model_files", "shrub_gradient.tempXinteract.BetNan_EmpNig_SalGla_VacUli.jags"), 
                                                      n.chains = 3,                      # no. Markov chains
                                                      n.iter = 100000, n.burnin = 70000, # no. iterations & burn-in fraction per chain
                                                      n.thin = 2,                        # thinning rate
                                                      DIC = FALSE,                       # do not compute deviance, pD, and DIC
                                                      working.directory = NULL, 
                                                      progress.bar = "text") 

# plot(model_out.shrub_gradient.tempXinteract.EmpNig) #check convergence, etc.


# extract coefficients 
coeff.shrub_gradient.tempXinteract.EmpNig <- model_out.shrub_gradient.tempXinteract.EmpNig$BUGSoutput$summary %>% 
  as.data.frame %>% 
  select('mean','sd','2.5%','97.5%','Rhat') %>% 
  # add identifying info to data frame
  rownames_to_column(var = "param")

# add 90% CIs
ci_90.tempXinteract.EmpNig <- data.frame(q5 = NA, q95 = NA, param = NA)
for (param in 1:(length(model_out.shrub_gradient.tempXinteract.EmpNig$BUGSoutput$sims.list)-4)){
  ci_90.tempXinteract.EmpNig[param,1:2] <- quantile(data.frame(model_out.shrub_gradient.tempXinteract.EmpNig$BUGSoutput$sims.list[param])[,1], probs = c(0.05, 0.95))
  ci_90.tempXinteract.EmpNig[param, 3] <- names(data.frame(model_out.shrub_gradient.tempXinteract.EmpNig$BUGSoutput$sims.list))[param]
}

# join to coefficients table
coeff.shrub_gradient.tempXinteract.EmpNig <- coeff.shrub_gradient.tempXinteract.EmpNig %>% 
  left_join(ci_90.tempXinteract.EmpNig, by = "param") %>% 
  # reorder and rename cols
  select(param, mean, sd, 
         l95 = "2.5%",
         l90 = q5,
         u90 = q95,
         u95 = "97.5%",
         Rhat)

save(coeff.shrub_gradient.tempXinteract.EmpNig, file = file.path("models_general", "model_outputs", "model_output_temp_x_interact.EmpNig.Rdata"))


# >> Rhododendron groenlandicum ----
model_out.shrub_gradient.tempXinteract.RhoGro <- jags(RhoGro_tXi.jags_data,              # input data
                                                      inits = NULL,                      # JAGS to create initial values
                                                      params_tempXinteract.RhoGro,       # parameters to be saved
                                                      model.file = file.path("models_general", "model_files", "shrub_gradient.tempXinteract.RhoGro.jags"), 
                                                      n.chains = 3,                      # no. Markov chains
                                                      n.iter = 100000, n.burnin = 70000, # no. iterations & burn-in fraction per chain
                                                      n.thin = 2,                        # thinning rate
                                                      DIC = FALSE,                       # do not compute deviance, pD, and DIC
                                                      working.directory = NULL, 
                                                      progress.bar = "text") 

# plot(model_out.shrub_gradient.tempXinteract.RhoGro) #check convergence, etc.


# extract coefficients 
coeff.shrub_gradient.tempXinteract.RhoGro <- model_out.shrub_gradient.tempXinteract.RhoGro$BUGSoutput$summary %>% 
  as.data.frame %>% 
  select('mean','sd','2.5%','97.5%','Rhat') %>% 
  # add identifying info to data frame
  rownames_to_column(var = "param")

# add 90% CIs
ci_90.tempXinteract.RhoGro <- data.frame(q5 = NA, q95 = NA, param = NA)
for (param in 1:(length(model_out.shrub_gradient.tempXinteract.RhoGro$BUGSoutput$sims.list)-4)){
  ci_90.tempXinteract.RhoGro[param,1:2] <- quantile(data.frame(model_out.shrub_gradient.tempXinteract.RhoGro$BUGSoutput$sims.list[param])[,1], probs = c(0.05, 0.95))
  ci_90.tempXinteract.RhoGro[param, 3] <- names(data.frame(model_out.shrub_gradient.tempXinteract.RhoGro$BUGSoutput$sims.list))[param]
}

# join to coefficients table
coeff.shrub_gradient.tempXinteract.RhoGro <- coeff.shrub_gradient.tempXinteract.RhoGro %>% 
  left_join(ci_90.tempXinteract.RhoGro, by = "param") %>% 
  # reorder and rename cols
  select(param, mean, sd, 
         l95 = "2.5%",
         l90 = q5,
         u90 = q95,
         u95 = "97.5%",
         Rhat)

save(coeff.shrub_gradient.tempXinteract.RhoGro, file = file.path("models_general", "model_outputs", "model_output_temp_x_interact.RhoGro.Rdata"))


# >> Salix glauca ----
model_out.shrub_gradient.tempXinteract.SalGla <- jags(SalGla_tXi.jags_data,              # input data
                                                      inits = NULL,                      # JAGS to create initial values
                                                      params_tempXinteract.BetNan_EmpNig_SalGla_VacUli,              # parameters to be saved
                                                      model.file = file.path("models_general", "model_files", "shrub_gradient.tempXinteract.BetNan_EmpNig_SalGla_VacUli.jags"), 
                                                      n.chains = 3,                      # no. Markov chains
                                                      n.iter = 100000, n.burnin = 70000, # no. iterations & burn-in fraction per chain
                                                      n.thin = 2,                        # thinning rate
                                                      DIC = FALSE,                       # do not compute deviance, pD, and DIC
                                                      working.directory = NULL, 
                                                      progress.bar = "text") 

# plot(model_out.shrub_gradient.tempXinteract.SalGla) #check convergence, etc.


# extract coefficients 
coeff.shrub_gradient.tempXinteract.SalGla <- model_out.shrub_gradient.tempXinteract.SalGla$BUGSoutput$summary %>% 
  as.data.frame %>% 
  select('mean','sd','2.5%','97.5%','Rhat') %>% 
  # add identifying info to data frame
  rownames_to_column(var = "param")

# add 90% CIs
ci_90.tempXinteract.SalGla <- data.frame(q5 = NA, q95 = NA, param = NA)
for (param in 1:(length(model_out.shrub_gradient.tempXinteract.SalGla$BUGSoutput$sims.list)-4)){
  ci_90.tempXinteract.SalGla[param,1:2] <- quantile(data.frame(model_out.shrub_gradient.tempXinteract.SalGla$BUGSoutput$sims.list[param])[,1], probs = c(0.05, 0.95))
  ci_90.tempXinteract.SalGla[param, 3] <- names(data.frame(model_out.shrub_gradient.tempXinteract.SalGla$BUGSoutput$sims.list))[param]
}

# join to coefficients table
coeff.shrub_gradient.tempXinteract.SalGla <- coeff.shrub_gradient.tempXinteract.SalGla %>% 
  left_join(ci_90.tempXinteract.SalGla, by = "param") %>% 
  # reorder and rename cols
  select(param, mean, sd, 
         l95 = "2.5%",
         l90 = q5,
         u90 = q95,
         u95 = "97.5%",
         Rhat)

save(coeff.shrub_gradient.tempXinteract.SalGla, file = file.path("models_general", "model_outputs", "model_output_temp_x_interact.SalGla.Rdata"))


# >> Vaccinium uliginosum ----
model_out.shrub_gradient.tempXinteract.VacUli <- jags(VacUli_tXi.jags_data,              # input data
                                                      inits = NULL,                      # JAGS to create initial values
                                                      params_tempXinteract.BetNan_EmpNig_SalGla_VacUli,              # parameters to be saved
                                                      model.file = file.path("models_general", "model_files", "shrub_gradient.tempXinteract.BetNan_EmpNig_SalGla_VacUli.jags"), 
                                                      n.chains = 3,                      # no. Markov chains
                                                      n.iter = 100000, n.burnin = 70000, # no. iterations & burn-in fraction per chain
                                                      n.thin = 2,                        # thinning rate
                                                      DIC = FALSE,                       # do not compute deviance, pD, and DIC
                                                      working.directory = NULL, 
                                                      progress.bar = "text") 

# plot(model_out.shrub_gradient.tempXinteract.VacUli) #check convergence, etc.


# extract coefficients 
coeff.shrub_gradient.tempXinteract.VacUli <- model_out.shrub_gradient.tempXinteract.VacUli$BUGSoutput$summary %>% 
  as.data.frame %>% 
  select('mean','sd','2.5%','97.5%','Rhat') %>% 
  # add identifying info to data frame
  rownames_to_column(var = "param")

# add 90% CIs
ci_90.tempXinteract.VacUli <- data.frame(q5 = NA, q95 = NA, param = NA)
for (param in 1:(length(model_out.shrub_gradient.tempXinteract.VacUli$BUGSoutput$sims.list)-4)){
  ci_90.tempXinteract.VacUli[param,1:2] <- quantile(data.frame(model_out.shrub_gradient.tempXinteract.VacUli$BUGSoutput$sims.list[param])[,1], probs = c(0.05, 0.95))
  ci_90.tempXinteract.VacUli[param, 3] <- names(data.frame(model_out.shrub_gradient.tempXinteract.VacUli$BUGSoutput$sims.list))[param]
}

# join to coefficients table
coeff.shrub_gradient.tempXinteract.VacUli <- coeff.shrub_gradient.tempXinteract.VacUli %>% 
  left_join(ci_90.tempXinteract.VacUli, by = "param") %>% 
  # reorder and rename cols
  select(param, mean, sd, 
         l95 = "2.5%",
         l90 = q5,
         u90 = q95,
         u95 = "97.5%",
         Rhat)

save(coeff.shrub_gradient.tempXinteract.VacUli, file = file.path("models_general", "model_outputs", "model_output_temp_x_interact.VacUli.Rdata"))


# _____________ ----
# __________________________ ----

# THE FARM: model across species ----

# Load and prepare data ----
# see documentation/Nuuk_shrub_drivers_analyses_twi_complete.nb.html for complete documentation

temp_x_interact_data.tot <- read.csv(file.path("data", "processed", "nuuk_env_cover_plots.csv"), header = T) %>% 
  
  # select relevant columns
  select(plotgroup = site_alt_plotgroup_id,
         plot,
         tempjja = tempjja_ts_30,
         interact = compet,
         taxon,
         abundance = cover) %>% 
  
  # filter out functional groups
  filter(!(str_detect(taxon, "^All "))) %>% 
  
  # arrange by plotgroup, taxon, plot
  arrange(plotgroup, plot, taxon) %>% 
  
  # convert plotgroup, plot, and taxon to factor
  mutate_at(vars(plotgroup, plot, taxon), factor) %>% 
  
  # assign numeric identifiers 
  
  # per plotgroup
  group_by(plotgroup) %>% 
  mutate(plotgroup.NUM = cur_group_id()) %>% 
  ungroup() %>% 
  
  # per taxon
  group_by(taxon) %>% 
  mutate(taxon.NUM = cur_group_id()) %>% 
  ungroup() %>% 
  
  # per plotgroup and taxon
  group_by(plotgroup, taxon) %>% 
  mutate(plotgroupXtaxon.NUM = cur_group_id()) %>% 
  ungroup() %>% 
  
  # per plot
  group_by(plot) %>% 
  mutate(plot.NUM = cur_group_id()) %>% 
  ungroup() %>% 
  
  # scale predictors
  mutate(across(c(tempjja, interact), 
                .fns = list(C = ~scale(., scale = TRUE, center = TRUE)))) %>% 
  
  # create discrete (=1) / continuous (=0) variable
  mutate(abund_discrete = case_when(abundance %in% c(0, 1) ~ 1,
                                    TRUE ~ 0))

# save input data (for predictions plot)
save("temp_x_interact_data.tot",
     file = file.path("models_general", "shrub_gradient_temp_x_interact_preddata.Rdata"))

# create subset for discrete and continuous data
temp_x_interact_data.dis <- temp_x_interact_data.tot %>% 
  
  filter(abund_discrete == 1)

temp_x_interact_data.cont <- temp_x_interact_data.tot %>% 
  
  filter(abund_discrete == 0)


# Assemble data for JAGS ----
temp_x_interact.jags_data <- list(
  
  # plot level, for discrete...
  abund.dis = temp_x_interact_data.dis$abundance,
  tempjja.dis = temp_x_interact_data.dis$tempjja_C[,1],
  interact.dis = temp_x_interact_data.dis$interact_C[,1],
  taxon.dis = temp_x_interact_data.dis$taxon.NUM,
  plotgroup_x_taxon.dis = temp_x_interact_data.dis$plotgroupXtaxon.NUM,
  N_discrete = nrow(temp_x_interact_data.dis),
  
  # ...and continuous part of the data
  abund.cont = temp_x_interact_data.cont$abundance,
  tempjja.cont = temp_x_interact_data.cont$tempjja_C[,1],
  interact.cont = temp_x_interact_data.cont$interact_C[,1],
  taxon.cont = temp_x_interact_data.cont$taxon.NUM,
  plotgroup_x_taxon.cont = temp_x_interact_data.cont$plotgroupXtaxon.NUM,
  N_cont = nrow(temp_x_interact_data.cont),
  
  # plotgroup X taxon level
  tempjja.tot = temp_x_interact_data.tot %>% group_by(plotgroup.NUM, taxon.NUM) %>% summarise(tempjja.tot = mean(tempjja_C)) %>% pull(tempjja.tot),
  taxon.tot = temp_x_interact_data.tot %>% group_by(plotgroup.NUM) %>% distinct(taxon.NUM) %>% ungroup() %>% pull(taxon.NUM),
  N_plotgroups_x_taxon = length(unique(temp_x_interact_data.tot$plotgroupXtaxon.NUM)),
  
  # taxon level
  N_taxa = length(unique(temp_x_interact_data.tot$taxon.NUM)),
  
  # subset of values for prediction, for each predictor...
  xhat_tempjja = seq(from = min(temp_x_interact_data.tot$tempjja_C), 
                     to = max(temp_x_interact_data.tot$tempjja_C), 
                     length.out = 100),
  Nxhat = 100,
  
  # ...and for predictions at high/low temperature levels
  xhat_interact2 = as.numeric(c(quantile(temp_x_interact_data.tot$interact_C, 0.05),
                                quantile(temp_x_interact_data.tot$interact_C, 0.95))), 
  Nxhat2 = 2
)

# check structure
str(temp_x_interact.jags_data)


# Specify model ----
write("
  
  model{
# priors 

  for(t in 1:N_taxa){ #AB added 1: to the for statement
    intercept[t] ~ dnorm(0, 0.0001)
  
    b.interact[t] ~ dnorm(0, 0.0001)
  
    b.tempjja.x[t] ~ dnorm(0, 0.001)
    b.tempjja.x2[t] ~ dnorm(0, 0.001)
  
    b.tempXinteract[t] ~ dnorm(0, 0.001)
    b.tempXinteract2[t] ~ dnorm(0, 0.001)
  }

  sigma.plot ~ dunif(0,100)
  tau.plot <- 1/(sigma.plot * sigma.plot)
  
  sigma.plotgroup_x_taxon ~ dunif(0,100)
  tau.plotgroup_x_taxon <- 1/(sigma.plotgroup_x_taxon * sigma.plotgroup_x_taxon)
  
  phi ~ dgamma(0.1, 0.1)
  
  
  # plot level - discrete part
  for (i in 1:N_discrete){ 
    abund.dis[i] ~ dbern(mu[i])
    logit(mu[i]) <- b_plotgroup_x_taxon[plotgroup_x_taxon.dis[i]] + # ~= random effect of plot group
      b.interact[taxon.dis[i]] * interact.dis[i] + 
      b.tempXinteract[taxon.dis[i]] * tempjja.dis[i] * interact.dis[i] +       # for interaction
      b.tempXinteract2[taxon.dis[i]] * (tempjja.dis[i]^2) * interact.dis[i]    # for interaction
  }
  
  
  # plot level - continuous part
  for (j in 1:N_cont){
    abund.cont[j] ~ dbeta(p[j], q[j])
    p[j] <- mu2[j] * phi
    q[j] <- (1 - mu2[j]) * phi
    logit(mu2[j]) <- b_plotgroup_x_taxon[plotgroup_x_taxon.cont[j]] + # ~= random effect of plot group
      b.interact[taxon.cont[j]] * interact.cont[j] + 
      b.tempXinteract[taxon.cont[j]] * tempjja.cont[j] * interact.cont[j] +       # for interaction
      b.tempXinteract2[taxon.cont[j]] * (tempjja.cont[j]^2) * interact.cont[j]    # for interaction
  }
  
  
  # plotgroup X taxon level
  for (k in 1:N_plotgroups_x_taxon){ # length of total plotgroup X taxon
    b_plotgroup_x_taxon[k] ~ dnorm(mu.plotgroup_x_taxon[k],tau.plotgroup_x_taxon)
    mu.plotgroup_x_taxon[k] <- intercept[taxon.tot[k]] + 
      
      # plot group level predictors, linear and quadratic term
      b.tempjja.x[taxon.tot[k]] * tempjja.tot[k] + 
      b.tempjja.x2[taxon.tot[k]] * (tempjja.tot[k]^2) 
  }
  
  
  # derived parameters # have to be calculated at taxon level?
  for (m in 1:Nxhat){
    for (p in 1:Nxhat2){
      for (t in 1:N_taxa){
        phat_tempXinteract[m,p,t] <- intercept[t] +
          b.tempjja.x[t] * xhat_tempjja[m] +
          b.tempjja.x2[t] * (xhat_tempjja[m]^2) +
          b.interact[t] * xhat_interact2[p] +
          b.tempXinteract[t] * xhat_tempjja[m] * xhat_interact2[p] +       # for interaction
          b.tempXinteract2[t] * (xhat_tempjja[m]^2) * xhat_interact2[p]    # for interaction
}
    }
  }
  }
", file.path("models_general", "shrub_gradient.tempXinteract.jags"))


# Specify parameters ----
params_tempXinteract <- c("intercept",
                          "b.tempjja.x", "b.tempjja.x2",
                          "b.interact", 
                          "b_plotgroup_x_taxon[1]","b_plotgroup_x_taxon[2]","b_plotgroup_x_taxon[3]",
                          "sigma.plotgroup_x_taxon",
                          "phi",
                          "phat_tempXinteract")


# Run model ----
model_out.shrub_gradient.tempXinteract <- jags(temp_x_interact.jags_data,         # input data
                                               inits = NULL,                      # JAGS to create initial values
                                               params_tempXinteract,              # parameters to be saved
                                               model.file = file.path("models_general", "shrub_gradient.tempXinteract.jags"), 
                                               n.chains = 3,                      # no. Markov chains
                                               n.iter = 100000, n.burnin = 70000, # no. iterations & burn-in fraction per chain
                                               n.thin = 2,                        # thinning rate
                                               DIC = FALSE,                       # do not compute deviance, pD, and DIC
                                               working.directory = NULL, 
                                               progress.bar = "text") 

# plot(model_out.shrub_gradient.tempXinteract) #check convergence, etc.


# Extract coefficients and save model output ----
# extract coefficients 
coeff.shrub_gradient.tempXinteract <- model_out.shrub_gradient.tempXinteract$BUGSoutput$summary %>% 
  as.data.frame %>% 
  select('mean','sd','2.5%','97.5%','Rhat') %>% 
  # add identifying info to data frame
  rownames_to_column(var = "param")

# add 90% CIs
ci_90.tempXinteract <- data.frame(q5 = NA, q95 = NA, param = NA)
for (param in 1:(length(model_out.shrub_gradient.tempXinteract$BUGSoutput$sims.list)-4)){
  ci_90.tempXinteract[param,1:2] <- quantile(data.frame(model_out.shrub_gradient.tempXinteract$BUGSoutput$sims.list[param])[,1], probs = c(0.05, 0.95))
  ci_90.tempXinteract[param, 3] <- names(data.frame(model_out.shrub_gradient.tempXinteract$BUGSoutput$sims.list))[param]
}

# join to coefficients table
coeff.shrub_gradient.tempXinteract <- coeff.shrub_gradient.tempXinteract %>% 
  left_join(ci_90.tempXinteract, by = "param") %>% 
  # reorder and rename cols
  select(param, mean, sd, 
         l95 = "2.5%",
         l90 = q5,
         u90 = q95,
         u95 = "97.5%",
         Rhat)

save(coeff.shrub_gradient.tempXinteract, file = file.path("models_general", "model_output_temp_x_interact.Rdata"))

# ___________________________ ----
######## End of script ############

