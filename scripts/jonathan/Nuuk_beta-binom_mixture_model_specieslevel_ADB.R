#####################################################################################################################################################
# Drivers of shrub abundance across the Nuuk Fjord inland gradient                                                                                         #
# Code to perform mixture model accounting for proportions of discrete & continuous data                                                            #
#                                                                                                                                                   #
# by Jonathan von Oppen (based on code and workflow by Anne Bjorkman)                                                                               #
# December & January 2019                                                                                                                                     #
#                                                                                                                                                   #
#####################################################################################################################################################
##TO DO!!
# break up model by species
# build model (random effect for site)
# add more env predictors
# extract slope values for plots
# create output graphs
#####################################################################################################################################################

rm(list = ls())

# Dependencies ####
# load pacman package from the repository, if you do not already have it
if (!require('pacman')) install.packages('pacman', repos="https://cloud.r-project.org")
pacman::p_load(tidyverse, # set of packages for data manipulation, exploration and visualisation
               tidylog,
               rjags,     # to link JAGS and R
               R2jags)    # to link JAGS and R

# Import data ####
# >> on plot group level ----
# complete set of species:
# env_cov_bio <- read.csv("I:/C_Write/_User/JonathanVonOppen_au630524/Project/A_NuukFjord_shrub_abundance_controls/aa_Godthaabsfjord/Data/PlotSpecies/Processed/godthaabsfjord_plots_fusion_table_with_pred_spp_rel_cover_compet_per_plot_group.csv", header = T)
# complete set - local path -> check if updated!
# env_cov_bio <- read.csv("//uni.au.dk/Users/au630524/Documents/Jonathan/Project/A_Nuuk_community_competition_controls/Data/processed/godthaabsfjord_plots_fusion_table_with_pred_spp_rel_cover_compet_per_plot_group.csv", header = T)
# subset of species with competition data:
env_cov_bio <- read.csv("data/Nuuk_env_cover_plotgroups.csv", header = T)

# >> on plot level ----
# # subset of species with competition data:
# env_cov_bio <- read.csv("data/Nuuk_env_cover_plots.csv", header = T, stringsAsFactors = F)

# Make plots for cover against predictors for each species ----
env_cov_bio.long <- env_cov_bio %>% 
  select(taxon,
         inclin_down,
         ends_with("ts_30"),
         twi_90m,
         compet,
         cover) %>% 
  # pivot to long format
  pivot_longer(cols = c(inclin_down,
                           ends_with("ts_30"),
                           twi_90m,
                           compet),
               names_to = "predictor",
               values_to = "pred_value")

# build function
pred.plot.grid <- function(df){
  
  taxa <- df %>% pull(taxon) %>% unique() %>% as.character()
  
  for(taxon_nr in 1:length(taxa)){
    plot <- ggplot(data = df %>% filter(taxon == taxa[taxon_nr]), 
                   aes(y = cover, group = taxon)) +
      geom_point(aes(x = pred_value), colour = "darkgrey") +
      geom_smooth(aes(x = pred_value), method = "lm", colour = "black", se = TRUE, na.rm = TRUE) +
      facet_wrap(~predictor, scales = "free", ncol = 3) +
      scale_y_continuous("relative no. pin hits per plot group",
                         limits = c(0, max(df %>% 
                                             filter(taxon == taxa[taxon_nr]) %>% 
                                             pull(cover)))) +
      labs(x = "predictor value") +
      ggtitle(paste0(taxa[taxon_nr], " cover ~ predictors")) +
      theme_bw() +
      theme(legend.position = "none")
    print(plot)
  }
}

# run
pred.plot.grid(env_cov_bio.long)
  
# Prepare data ####
# Select relevant variables: plot info, WorldClim predictors, SRI, yos & long-term mean NDVI, distance to coast, cover, competitive pressure
env_cov_bio_sub <- env_cov_bio %>% 
  select(site_alt_plotgroup_id, site, site_alt_id, year, long, lat,  # plot info / metadata
  ends_with("_ts_10"),   # CHELSA predictors averaged over 10-year period prior to study year
  inclin_down, twi_90m, #sri, 
  #mean_summ_ndvi_yos, cv_mean_summ_ndvi_2001_to_yos, Perc_dist_coast_lines,   # environmental data
  taxon, cover, compet)   # taxon, cover response, competition pressure

# REMOVE Cassiope tetragona & others because it is so low abundance it doesn't converge. Summary of zeros:
# env_cov_bio %>% group_by(taxon) %>% summarise_at(vars(cover), list(~sum(. == 0))) %>% rename(zeros = cover) %>% mutate(zeros_perc = zeros/69)
# !!! Model crashes if >2 taxa removed !!! (JvO 06 Apr 20)
#env_cov_bio_sub <- env_cov_bio_sub[env_cov_bio_sub$taxon %in% c("Cassiope tetragona", "Phyllodoce coerulea")==F,]

# ORDER data

env_cov_bio_sub <- env_cov_bio_sub[order(env_cov_bio_sub$site_alt_plotgroup_id, env_cov_bio_sub$taxon),]

# Create integer versions of factors
env_cov_bio_sub$plotgroup.NUM <- as.numeric(factor(env_cov_bio_sub$site_alt_plotgroup_id, levels=unique(env_cov_bio_sub$site_alt_plotgroup_id)))
env_cov_bio_sub$site.NUM <- as.numeric(factor(env_cov_bio_sub$site, levels = unique(env_cov_bio_sub$site)))
env_cov_bio_sub$taxon.NUM <- as.numeric(factor(env_cov_bio_sub$taxon, levels = unique(env_cov_bio_sub$taxon)))
taxa.num <- data.frame(taxon = levels(as.factor(env_cov_bio_sub$taxon)), # I had to add as.factor here
                       num = env_cov_bio_sub$taxon.NUM[1:nlevels(as.factor(env_cov_bio_sub$taxon))]) #%>% print()

# # Create unique taxon-plotgroup combination variable
# env_cov_bio_sub$taxon_plotgroup <- paste(env_cov_bio_sub$site_alt_plotgroup_id, env_cov_bio_sub$taxon, sep="_")
# env_cov_bio_sub$taxon_plotgroup.NUM <- as.numeric(factor(env_cov_bio_sub$taxon_plotgroup, levels = unique(env_cov_bio_sub$taxon_plotgroup)))

# scale numeric predictors
num_pred <- env_cov_bio_sub %>% select(ends_with("_ts_10"), matches("sri"), 
                                       starts_with("twi"), 
                                       matches("compet"))
for(i in 1:length(num_pred)){
  col <- colnames(num_pred[i])
  env_cov_bio_sub[paste0(col,"C")] <- as.numeric(scale(num_pred[i], scale = TRUE, center = TRUE))
}

# Separate datasets for each species into continuous and discrete:

# assign discrete = 1, continuous = 0 variable
env_cov_bio_sub$cover_discrete <- ifelse(env_cov_bio_sub$cover == 1 | env_cov_bio_sub$cover == 0, 1, 0)

# split dataset for separate species

# Betula nana
BetNan.tot <- filter(env_cov_bio_sub, taxon == "Betula nana")
BetNan.dis <- filter(BetNan.tot, cover_discrete == 1)
BetNan.cont <- filter(BetNan.tot, cover_discrete == 0)

# y.dis <- env_cov_bio_sub[env_cov_bio_sub$cover_discrete == 1,]
# y.cont <- env_cov_bio_sub[env_cov_bio_sub$cover_discrete == 0,]

# # Create unique taxon-plotgroup combination variable, for each group separately:
# y.dis$taxon_plotgroup <- paste(y.dis$site_alt_plotgroup_id,y.dis$taxon,sep="_")
# y.dis$taxon_plotgroup.NUM <- as.numeric(factor(y.dis$taxon_plotgroup, levels = unique(y.dis$taxon_plotgroup)))
# 
# y.cont$taxon_plotgroup <- paste(y.cont$site_alt_plotgroup_id,y.cont$taxon,sep="_")
# y.cont$taxon_plotgroup.NUM <- as.numeric(factor(y.cont$taxon_plotgroup, levels = unique(y.cont$taxon_plotgroup)))


# Compile data into list ####
shrub_gradient_jags.BetNan.data <- list(
  #UV_plotgroup.tot = BetNan.tot$plotgroup.NUM,
  #N_sites = length(unique(BetNan.tot$site)),
  N_plotgroups = length(unique(BetNan.tot$site_alt_plotgroup_id)),
  cov.dis = BetNan.dis$cover,
  plotgroup.dis = BetNan.dis$plotgroup.NUM, #AB added this
  #UV_plotgroup.dis = BetNan.dis$plotgroup.NUM,
  compet.dis = BetNan.dis$competC,
  N_discrete = nrow(BetNan.dis),
  #N_sites.dis = length(unique(BetNan.dis$site)),
  #N_plotgroups.dis = length(unique(BetNan.dis$site_alt_plotgroup_id)),
  cov.cont = BetNan.cont$cover,
  plotgroup.cont = BetNan.cont$plotgroup.NUM, #AB added this
  #UV_plotgroup.cont = BetNan.cont$plotgroup.NUM,
  #UV_site.cont = BetNan.cont$site.NUM,
  #year.cont = BetNan.cont$year,
  compet.cont = BetNan.cont$competC,
  N_cont = nrow(BetNan.cont),
  #N_sites.cont = length(unique(BetNan.cont$site)),
  #N_plotgroups.cont = length(unique(BetNan.cont$site_alt_plotgroup_id)),
  env.tot = BetNan.tot$tempjja_ts_10C[!duplicated(BetNan.tot$plotgroup.NUM)]#, # one value per tXpg, amt = annual mean temperature = bio_1
)

str(shrub_gradient_jags.BetNan.data)

# JAGS model #### 
# adapted from Bayesian course: rain -> env = target variable,
                              # biome -> species, 
                              # continent -> site, 
                              # cell -> plot group

write("
  
  model{
    
    # priors
      
      intercept ~ dnorm(0, 0.0001)
      
      b_compet ~ dnorm(0, 0.0001)
      b_sri ~ dnorm(0, 0.0001)
      # b_slope ~ dnorm(0, 0.0001)

      sigma.plotgroup ~ dunif(0,100)
      tau.plotgroup <- 1/(sigma.plotgroup * sigma.plotgroup)
      
      b.env.x ~ dnorm(0, 0.001)
      b.env.x2 ~ dnorm(0, 0.001)
      
      phi ~ dgamma(0.1, 0.1)
      
      
    # LIKELIHOOD for discrete part

      for (i in 1:N_discrete){ 
        cov.dis[i] ~ dbern(mu[i])
        logit(mu[i]) <- b_plotgroup[plotgroup.dis[i]]+ #AB added this
                        b_compet*compet.dis[i] #+ 
                        # b_slope[i]*slope.dis[i] + 
                        # b_sri[i]*sri.dis[i] 
      }
      
      
    # LIKELIHOOD for continuous part

      for (i in 1:N_cont){
        cov.cont[i] ~ dbeta(p[i], q[i])
        p[i] <- mu2[i] * phi
        q[i] <- (1 - mu2[i]) * phi
        logit(mu2[i]) <- b_plotgroup[plotgroup.cont[i]]+ #AB added this
                        b_compet*compet.cont[i] #+
                        # b_slope[i]*slope.cont[i] + 
                        # b_sri[i]*sri.cont[i] 
      }


      for (j in 1:N_plotgroups){ # length of total plotgroups
        b_plotgroup[j] ~ dnorm(mu.plotgroup[j],tau.plotgroup)
        mu.plotgroup[j] <- intercept + 
                    b.env.x * env.tot[j] + 
                    b.env.x2 * (env.tot[j]^2) # add more plotgroup-level predictors
      }

    
      }
  ","shrub_gradient.BetNan.jags")

# Initial values ####
# not strictly necessary here (?)

# Parameters to monitor ####

params <- c("b.env.x","b.env.x2","intercept","b_compet", "b_plotgroup[1]","b_plotgroup[2]","b_plotgroup[3]","b_plotgroup[63]","sigma.plotgroup","phi") # add b_slope when added to df
# "a.env.x","a.env.x2","b_sri",

# Run model ----

model_out.shrub_gradient.BetNan <- jags(shrub_gradient_jags.BetNan.data, inits = NULL, params, 
                                 model.file = "shrub_gradient.BetNan.jags", n.chains = 3, #AB change file name
                                 n.iter = 8000, n.burnin = 6000, n.thin=2, DIC=FALSE, 
                                 working.directory=NULL, progress.bar = "text") 

plot(model_out.shrub_gradient.BetNan) #check convergence, etc.

# extract coefficients 
coeff.shrub_gradient.BetNan<-as.data.frame(model_out.shrub_gradient.BetNan$BUGSoutput$summary[,c('mean','sd','2.5%','97.5%','Rhat')])

# add identifying info to data frame
coeff.shrub_gradient.BetNan$Param <- as.vector(sapply(strsplit(rownames(coeff.shrub_gradient.BetNan),"[[]",fixed=FALSE), "[", 1))

#compare modeled intercepts with raw-data means - why are they different, especially for the Savanna?
aggregate(env_cov_bio_sub$cover,by=list(env_cov_bio_sub$site,env_cov_bio_sub$taxon),FUN=mean)$x
round(plogis(coeff.shrub_gradient.BetNan[coeff.shrub_gradient.BetNan$Param=="intercept","mean"]),3)
# coeff.shrub_gradient.BetNan[coeff.shrub_gradient.BetNan$Param=="b1.intBT",]
# coeff.shrub_gradient[coeff.shrub_gradient$Param=="b1BT",]

plot(aggregate(env_cov_bio_sub$cover,by=list(env_cov_bio_sub$taxon),FUN=mean)$x,round(plogis(coeff.shrub_gradient.BetNan[coeff.shrub_gradient.BetNan$Param=="intercept","mean"]),4))
