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

# create subset for discrete and continuous data
temp_x_interact_data.dis <- temp_x_interact_data.tot %>% 
  
  filter(abund_discrete == 1)

temp_x_interact_data.cont <- temp_x_interact_data.tot %>% 
  
  filter(abund_discrete == 0)


# Assemble data for JAGS ----
temp_x_interact.jags_data <- list(
  
  # plot level, for discrete...
  abund.dis = temp_x_interact_data.dis$abundance,
  tempjja.dis = temp_x_interact_data.dis$tempjja_C,
  interact.dis = temp_x_interact_data.dis$interact_C,
  taxon.dis = temp_x_interact_data.dis$taxon.NUM,
  plotgroup_x_taxon.dis = temp_x_interact_data.dis$plotgroupXtaxon.NUM,
  N_discrete = nrow(temp_x_interact_data.dis),
  
  # ...and continuous part of the data
  abund.cont = temp_x_interact_data.cont$abundance,
  tempjja.cont = temp_x_interact_data.cont$tempjja_C,
  interact.cont = temp_x_interact_data.cont$interact_C,
  plotgroup_x_taxon.cont = temp_x_interact_data.cont$plotgroupXtaxon.NUM,
  N_cont = nrow(temp_x_interact_data.cont),
  
  # plotgroup X taxon level
  tempjja.tot = temp_x_interact_data.tot %>% group_by(plotgroup.NUM, taxon.NUM) %>% summarise(tempjja.tot = mean(tempjja_C)) %>% pull(tempjja.tot),
  taxon.tot = temp_x_interact_data.tot %>% group_by(plotgroup.NUM) %>% distinct(taxon.NUM) %>% ungroup() %>% pull(taxon.NUM),
  N_plotgroups_x_taxon = length(unique(temp_x_interact_data.tot$plotgroupXtaxon.NUM)),
  
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

  intercept ~ dnorm(0, 0.0001)
  
  b.interact ~ dnorm(0, 0.0001)
  
  sigma.plot ~ dunif(0,100)
  tau.plot <- 1/(sigma.plot * sigma.plot)
  
  sigma.plotgroup_x_taxon ~ dunif(0,100)
  tau.plotgroup_x_taxon <- 1/(sigma.plotgroup_x_taxon * sigma.plotgroup_x_taxon)
  
  b.tempjja.x ~ dnorm(0, 0.001)
  b.tempjja.x2 ~ dnorm(0, 0.001)
  
  b.tempXinteract ~ dnorm(0, 0.001)
  b.tempXinteract2 ~ dnorm(0, 0.001)


  # plot level - discrete part
  for (i in 1:N_discrete){ 
    abund.dis[i] ~ dbern(mu[i])
    logit(mu[i]) <- b_plotgroup[plotgroup_x_taxon.dis[i]] + # ~= random effect of plot group
      b.interact[taxon.dis[i]] * interact.dis[i] + 
      b.tempXinteract[taxon.dis[i]] * tempjja.dis[i] * interact.dis[i] +       # for interaction
      b.tempXinteract2[taxon.dis[i]] * (tempjja.dis[i]^2) * interact.dis[i]    # for interaction
  }
  
  
  # plot level - continuous part
  for (j in 1:N_cont){
    abund.cont[j] ~ dbeta(p[j], q[j])
    p[j] <- mu2[j] * phi
    q[j] <- (1 - mu2[j]) * phi
    logit(mu2[j]) <- b_plotgroup[plotgroup_x_taxon.cont[j]] + # ~= random effect of plot group
      b.interact[taxon.cont[j]] * interact.cont[j] + 
      b.tempXinteract[taxon.cont[j]] * tempjja.cont[j] * interact.cont[j] +       # for interaction
      b.tempXinteract2[taxon.cont[j]] * (tempjja.cont[j]^2) * interact.cont[j]    # for interaction
  }
  
  
  # plotgroup X taxon level
  for (k in 1:N_plotgroups_x_taxon){ # length of total plotgroup X taxon
    b_plotgroup_x_taxon[k] ~ dnorm(mu.plotgroup_x_taxon[k],tau.plotgroup_x_taxon)
    mu.plotgroup_x_taxon[k] <- intercept + 
      
      # plot group level predictors, linear and quadratic term
      b.tempjja.x[taxon.tot[k]] * tempjja.tot[k] + 
      b.tempjja.x2[taxon.tot[k]] * (tempjja.tot[k]^2) 
  }
  
  
  # derived parameters
  for (m in 1:Nxhat){
    for (p in 1:Nxhat2){
      phat_tempXinteract[m,p] <- intercept +
        b.tempjja.x * xhat_tempjja[m] +
        b.tempjja.x2 * (xhat_tempjja[m]^2) +
        b.interact * xhat_interact2[p]
        
    }
  }
  }
", file.path("models_general", "shrub_gradient.tempXinteract.jags"))


# Specify parameters ----
params_tempXinteract <- c("intercept",
                          "b.tempjja.x", "b.tempjja.x2",
                          "b.interact.x", 
                          "b_plotgroup_x_taxon[1]","b_plotgroup_x_taxon[2]","b_plotgroup_x_taxon[3]","b_plotgroup[63]",
                          "sigma.plotgroup_x_taxon",
                          "phat_tempXinteract")


# Run model ----
model_out.shrub_gradient.tempXinteract <- jags(temp_x_interact.jags_data,         # input data
                                               inits = NULL,                      # JAGS to create initial values
                                               params_tempXinteract,              # parameters to be saved
                                               model.file = file.path("models_general", "shrub_gradient.tempXinteract.jags"), 
                                               n.chains = 3,                      # no. Markov chains
                                               n.iter = 10000, n.burnin = 7000,   # no. iterations & burn-in fraction per chain
                                               n.thin = 2,                        # thinning rate
                                               DIC = FALSE,                       # do not compute deviance, pD, and DIC
                                               working.directory = NULL, 
                                               progress.bar = "text") 

# plot(model_out.shrub_gradient.tempXinteract) #check convergence, etc.

