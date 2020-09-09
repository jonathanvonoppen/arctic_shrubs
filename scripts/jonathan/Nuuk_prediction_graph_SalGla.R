# Drivers of shrub abundance across the Nuuk fjord area

# Script to debug model predictions workflow

# Jonathan von Oppen, Aarhus University, jonathan.vonoppen@bio.au.dk
# 09/09/2020

# ____________________________________________________________________________


# Dependencies ----
if (!require('pacman')) install.packages('pacman', repos="https://cloud.r-project.org")
pacman::p_load(tidyverse, # set of packages for data manipulation, exploration and visualisation
               rjags,     # to link JAGS and R
               R2jags)    # to link JAGS and R
              
# ____________________________________________________________________________

# Data preparation ----

# Load data
env_cov_bio <- read.csv(file.path("data", "nuuk_env_cover_plots.csv", header = T))
  
# Filter data for relevant variables
env_cov_bio_sub <- env_cov_bio %>% 
  select(site_alt_plotgroup_id, plot, site, site_alt_id, year, long, lat, alt,  # plot info / metadata
         ends_with("_ts_30"),         # CHELSA predictors averaged over 10-year period prior to study year
         inclin_down, twi_90m, tri, sri, 
         taxon, cover, compet) %>%    # taxon, cover response, competition pressure
  # remove correlated variables
  select(-tempmax_ts_30,
         -tempmin_ts_30,
         -inclin_down,
         -contains("mam"))

# Order data
env_cov_bio_sub <- env_cov_bio_sub[order(env_cov_bio_sub$site_alt_plotgroup_id, env_cov_bio_sub$taxon),]

# assign numeric identifiers:
env_cov_bio_sub$plotgroup.NUM <- as.numeric(factor(env_cov_bio_sub$site_alt_plotgroup_id,
                                                   levels = unique(env_cov_bio_sub$site_alt_plotgroup_id)))
env_cov_bio_sub$plot.NUM <- as.numeric(factor(env_cov_bio_sub$plot,
                                              levels = unique(env_cov_bio_sub$plot)))
env_cov_bio_sub$site_alt.NUM <- as.numeric(factor(env_cov_bio_sub$site_alt_id,
                                                  levels = unique(env_cov_bio_sub$site_alt_id)))
env_cov_bio_sub$site.NUM <- as.numeric(factor(env_cov_bio_sub$site, 
                                              levels = unique(env_cov_bio_sub$site)))
env_cov_bio_sub$taxon.NUM <- as.numeric(factor(env_cov_bio_sub$taxon, 
                                               levels = unique(env_cov_bio_sub$taxon)))

# Scale and center numeric predictors
num_pred <- env_cov_bio_sub %>% select(alt,
                                       ends_with("_ts_30"), 
                                       sri, 
                                       tri,
                                       starts_with("twi"), 
                                       matches("compet"))
for(i in 1:length(num_pred)){
  col <- colnames(num_pred[i])
  env_cov_bio_sub[paste0(col,"C")] <- as.numeric(scale(num_pred[i], scale = TRUE, center = TRUE))
}

# assign discrete/continuous identifier and split dataset accordingly
env_cov_bio_sub$cover_discrete <- ifelse(env_cov_bio_sub$cover == 1 | env_cov_bio_sub$cover == 0, 1, 0)

    # complete SalGla data
    SalGla.tot <- filter(env_cov_bio_sub, taxon == "Salix glauca")
    # discrete cover values
    SalGla.dis <- filter(SalGla.tot, cover_discrete == 1) 
    # continuous cover values
    SalGla.cont <- filter(SalGla.tot, cover_discrete == 0)


# ____________________________________________________________________________


# JAGS model ----

# compile input data
shrub_gradient_jags.SalGla.xhat.data <- list(
  
  # plot level predictors, for discrete...
  cov.dis = SalGla.dis$cover,
  plotgroup.dis = SalGla.dis$plotgroup.NUM,
  sri.dis = SalGla.dis$sriC,
  tri.dis = SalGla.dis$triC,
  twi_90m.dis = SalGla.dis$twi_90mC,
  compet.dis = SalGla.dis$competC,
  N_discrete = nrow(SalGla.dis),
  
  # ...and continuous part of the data
  cov.cont = SalGla.cont$cover,
  plotgroup.cont = SalGla.cont$plotgroup.NUM,
  sri.cont = SalGla.cont$sriC,
  tri.cont = SalGla.cont$triC,
  twi_90m.cont = SalGla.cont$twi_90mC,
  compet.cont = SalGla.cont$competC,
  N_cont = nrow(SalGla.cont),
  
  # plot group level predictors
  tempjja.tot = SalGla.tot$tempjja_ts_30C[!duplicated(SalGla.tot$plotgroup.NUM)], # one value per tXpg
  tempcont.tot = SalGla.tot$tempcont_ts_30C[!duplicated(SalGla.tot$plotgroup.NUM)],
  precipjja.tot = SalGla.tot$precipjja_ts_30C[!duplicated(SalGla.tot$plotgroup.NUM)],
  N_plotgroups = length(unique(SalGla.tot$site_alt_plotgroup_id)),
  
  xhat = seq(from = min(SalGla.tot$tempcont_ts_30C), to = max(SalGla.tot$tempcont_ts_30C), length.out = 100),
  Nxhat = 100
)

str(shrub_gradient_jags.SalGla.xhat.data)

# model script
write("
  
  model{
    
    # priors
      
      intercept ~ dnorm(0, 0.0001)
      
      b.compet ~ dnorm(0, 0.0001)
      b.sri ~ dnorm(0, 0.0001)
      # b.inclin_down ~ dnorm(0, 0.0001)
      b.tri ~ dnorm(0, 0.0001)
      b.twi_90m ~ dnorm(0, 0.0001)

      sigma.plotgroup ~ dunif(0,100)
      tau.plotgroup <- 1/(sigma.plotgroup * sigma.plotgroup)
      
      # sigma.isocline ~ dunif(0,100)
      # tau.isocline <- 1/(sigma.isocline * sigma.isocline)
      # 
      # b.alt.x ~ dnorm(0, 0.001)
      # b.alt.x2 ~ dnorm(0, 0.001)
      b.tempjja.x ~ dnorm(0, 0.001)
      b.tempjja.x2 ~ dnorm(0, 0.001)
      # b.tempmax.x ~ dnorm(0, 0.001)
      # b.tempmax.x2 ~ dnorm(0, 0.001)
      # b.tempmin.x ~ dnorm(0, 0.001)
      # b.tempmin.x2 ~ dnorm(0, 0.001)
      b.tempcont.x ~ dnorm(0, 0.001)
      # b.tempcont.x2 ~ dnorm(0, 0.001)
      b.precipjja.x ~ dnorm(0, 0.001)
      # b.precipjja.x2 ~ dnorm(0, 0.001)
      # b.precipjfmam.x ~ dnorm(0, 0.001)
      # b.precipjfmam.x2 ~ dnorm(0, 0.001)
      
      phi ~ dgamma(0.1, 0.1)
      
      
    # LIKELIHOOD for discrete part

      for (i in 1:N_discrete){ 
        cov.dis[i] ~ dbern(mu[i])
        logit(mu[i]) <- b_plotgroup[plotgroup.dis[i]] + #AB added this, ~= random effect of plot group
                        # b_isocline[isocline.dis[i]] +
                        b.compet * compet.dis[i] + 
                        # b.inclin_down * inclin_down.dis[i] +
                        b.twi_90m * twi_90m.dis[i] + 
                        b.sri * sri.dis[i] +
                        b.tri * tri.dis[i]
      }
      
      
    # LIKELIHOOD for continuous part

      for (j in 1:N_cont){
        cov.cont[j] ~ dbeta(p[j], q[j])
        p[j] <- mu2[j] * phi
        q[j] <- (1 - mu2[j]) * phi
        logit(mu2[j]) <- b_plotgroup[plotgroup.cont[j]] + #AB added this, ~= random effect of plot group
                        # b_isocline[isocline.cont[j]] +
                        b.compet * compet.cont[j] +
                        # b.inclin_down * inclin_down.cont[j] +
                        b.twi_90m * twi_90m.cont[j] + 
                        b.sri * sri.cont[j] +
                        b.tri * tri.cont[j]
      }


      for (k in 1:N_plotgroups){ # length of total plotgroups
        b_plotgroup[k] ~ dnorm(mu.plotgroup[k],tau.plotgroup)
        mu.plotgroup[k] <- intercept + 
                    
                    # plot group level predictors, linear and quadratic term
                    b.tempjja.x * tempjja.tot[k] + 
                      # b.tempjja.x2 * (tempjja.tot[k]^2) +  # eliminated, as n.s.
                    b.tempcont.x * tempcont.tot[k] + 
                      # b.tempcont.x2 * (tempcont.tot[k]^2) +  # eliminated, as n.s.
                    b.precipjja.x * precipjja.tot[k] # + 
                      # b.precipjja.x2 * (precipjja.tot[k]^2)  # eliminated, as n.s.
                    
      }
  
  
      # add predicted values (derived parameters)
      for (m in 1:Nxhat){
        phat[m] <- intercept + b.tempcont.x * xhat[m]
      }
    
      }
  ", file.path("models", "shrub_gradient.SalGla2.xhat.jags"))

# Parameters to be monitored
params_SalGla2.xhat <- c("intercept",
                         # "b.alt.x",
                         "b.tempjja.x", # "b.tempjja.x2",
                         # "b.tempmax.x", "b.tempmax.x2",
                         # "b.tempmin.x", "b.tempmin.x2",
                         "b.tempcont.x", # "b.tempcont.x2",
                         "b.precipjja.x", # "b.precipjja.x2",
                         # "b.precipjfmam.x", "b.precipjfmam.x2",
                         "b.compet", 
                         # "b.inclin_down",
                         "b.sri",
                         "b.tri",
                         "b.twi_90m",
                         "b_plotgroup[1]","b_plotgroup[2]","b_plotgroup[3]","b_plotgroup[63]",
                         # "b_isocline[1]","b_isocline[2]","b_isocline[21]",
                         "sigma.plotgroup",
                         "phi",
                         "phat")

# Run model:
model_out.shrub_gradient.SalGla2.xhat <- jags(shrub_gradient_jags.SalGla.xhat.data,    # input data
                                              inits = NULL,                            # JAGS to create initial values
                                              params_SalGla2.xhat,                     # parameters to be saved
                                              model.file = file.path("..", "models", "shrub_gradient.SalGla2.xhat.jags"), 
                                              n.chains = 3,                            # no. Markov chains
                                              n.iter = 10000, n.burnin = 7000,         # no. iterations & burn-in fraction per chain
                                              n.thin = 2,                              # thinning rate
                                              DIC = FALSE,                             # do not compute deviance, pD, and DIC
                                              working.directory = NULL, 
                                              progress.bar = "text")

  # plot(model_out.shrub_gradient.SalGla2.xhat) #check convergence, etc.


# ____________________________________________________________________________


# Coefficients extraction ----
coeff.shrub_gradient.SalGla2.xhat <- model_out.shrub_gradient.SalGla2.xhat$BUGSoutput$summary %>% 
  as.data.frame %>% 
  select('mean','sd','2.5%','97.5%','Rhat') %>% 
  # add identifying info to data frame
  rownames_to_column(var = "param")


# reorder and rename cols
coeff.shrub_gradient.SalGla2.xhat <- coeff.shrub_gradient.SalGla2.xhat %>% 

  select(param, mean, sd, 
         l95 = "2.5%",
         u95 = "97.5%",
         Rhat) %>% print

# assemble predicted and predictor values
phats <- coeff.shrub_gradient.SalGla2.xhat %>% 
  
  # filter for predicted values
  filter(param %in% c(paste0("phat[", seq(from = 1, to = 100), "]"))) %>% 
  
  # add xhats column
  mutate(xhats = seq(from = min(SalGla.tot$tempcont_ts_30C),
                     to = max(SalGla.tot$tempcont_ts_30C),
                     length.out = 100))

# back-center and back-scale predictor values (xhats)
phats$tempcont <- phats$xhats*attr(scale(SalGla.tot$tempcont_ts_30), 'scaled:scale') + attr(scale(SalGla.tot$tempcont_ts_30), 'scaled:center') 


# GRAPH
ggplot() +
  # tempcont is modelled at plotgroup level, so reduce base data (points layer) to plotgroup level
  geom_point(data = SalGla.tot %>% group_by(site_alt_plotgroup_id) %>% summarise(tempcont = mean(tempcont_ts_30), cover = mean(cover)), 
             aes(x = tempcont, 
                 y = cover), 
             size = 2,
             position = position_jitter(width=0, height=.01),
             alpha=0.5) +
  
  # draw line of predicted values
  geom_line(data = phats, 
            aes(x = tempcont, 
                y = plogis(mean)), 
            colour = "orange",
            alpha = 1,
            size = 3) + 
  
  # draw predicted 95% CI
  geom_ribbon(data = phats,
              aes(x = tempcont, 
                  ymin = plogis(l95), 
                  ymax = plogis(u95)),
              fill = "orange",
              alpha = 0.2) +
  
  # define appearance
  labs(x = "annual temperature variability [°C]",
       y = "rel. no. hits per plot") + 
  theme_bw()