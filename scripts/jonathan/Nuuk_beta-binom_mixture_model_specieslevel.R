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
# subset of species with competition data:
env_cov_bio <- read.csv("data/Nuuk_env_cover_plots.csv", header = T, stringsAsFactors = F)


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
taxa.num <- data.frame(taxon = levels(env_cov_bio_sub$taxon), 
                       num = env_cov_bio_sub$taxon.NUM[1:nlevels(env_cov_bio_sub$taxon)]) #%>% print()

# Create unique taxon-plotgroup combination variable
env_cov_bio_sub$taxon_plotgroup <- paste(env_cov_bio_sub$site_alt_plotgroup_id, env_cov_bio_sub$taxon, sep="_")
env_cov_bio_sub$taxon_plotgroup.NUM <- as.numeric(factor(env_cov_bio_sub$taxon_plotgroup, levels = unique(env_cov_bio_sub$taxon_plotgroup)))

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
shrub_gradient_jags.data <- list(
 # cov.tot = BetNan.tot$cover,
  UV_plotgroup.tot = BetNan.tot$plotgroup.NUM,
 # UV_site.tot = BetNan.tot$site.NUM[!duplicated(BetNan.tot$taxon_plotgroup.NUM)], #one value per tXpg
 # year.tot = BetNan.tot$year,
 # UV_taxon.tot = BetNan.tot$taxon.NUM[!duplicated(BetNan.tot$taxon_plotgroup.NUM)], #one value per tXpg
 # sri.tot = BetNan.tot$sriC,
 # ndvi.yos.tot = BetNan.tot$mean_summ_ndvi_yosC,
 # ndvi.long.tot = BetNan.tot$cv_mean_summ_ndvi_2001_to_yosC,
 # continental.tot = BetNan.tot$Perc_dist_coast_linesC,
 # compet.tot = BetNan.tot$competC,
 # N_tot = nrow(BetNan.tot),
  N_sites = length(unique(BetNan.tot$site)),
  N_plotgroups = length(unique(BetNan.tot$site_alt_plotgroup_id)),
 # N_taxa = length(unique(BetNan.tot$taxon)),
  # UV_taxonXplotgroup.tot = BetNan.tot$taxon_plotgroup.NUM,
 # N_taxonXplotgroup.tot = length(unique(BetNan.tot$taxon_plotgroup.NUM)),
  cov.dis = BetNan.dis$cover,
  UV_plotgroup.dis = BetNan.dis$plotgroup.NUM,
 # UV_site.dis = BetNan.dis$site.NUM,
 # year.dis = BetNan.dis$year,
 # UV_taxon.dis = BetNan.dis$taxon.NUM,
 # sri.dis = BetNan.dis$sriC,
 # ndvi.yos.dis = BetNan.dis$mean_summ_ndvi_yosC,
 # ndvi.long.dis = BetNan.dis$cv_mean_summ_ndvi_2001_to_yosC,
 # continental.dis = BetNan.dis$Perc_dist_coast_linesC,
  compet.dis = BetNan.dis$competC,
  N_discrete = nrow(BetNan.dis),
  N_sites.dis = length(unique(BetNan.dis$site)),
  N_plotgroups.dis = length(unique(BetNan.dis$site_alt_plotgroup_id)),
 # N_taxa.dis = length(unique(BetNan.dis$taxon)),
 # UV_taxonXplotgroup.dis = BetNan.dis$taxon_plotgroup.NUM,
 # N_taxonXplotgroup.dis = length(unique(BetNan.dis$taxon_plotgroup.NUM)),
  cov.cont = BetNan.cont$cover,
  UV_plotgroup.cont = BetNan.cont$plotgroup.NUM,
  UV_site.cont = BetNan.cont$site.NUM,
  year.cont = BetNan.cont$year,
 # UV_taxon.cont = BetNan.cont$taxon.NUM,
 # sri.cont = BetNan.cont$sriC,
  # ndvi.yos.cont = BetNan.cont$mean_summ_ndvi_yosC,
  # ndvi.long.cont = BetNan.cont$cv_mean_summ_ndvi_2001_to_yosC,
  # continental.cont = BetNan.cont$Perc_dist_coast_linesC,
  compet.cont = BetNan.cont$competC,
  N_cont = nrow(BetNan.cont),
  N_sites.cont = length(unique(BetNan.cont$site)),
  N_plotgroups.cont = length(unique(BetNan.cont$site_alt_plotgroup_id)),
 # N_taxa.cont = length(unique(BetNan.cont$taxon)),
 # UV_taxonXplotgroup.cont = BetNan.cont$taxon_plotgroup.NUM,
  # N_taxonXplotgroup.cont = length(unique(BetNan.cont$taxon_plotgroup.NUM)),
  # xhat = seq(from = min(BetNan.tot$cover), to = max(BetNan.tot$cover), by = (1/150)),
  # N_xhat = length(seq(from = min(BetNan.tot$cover), to = max(BetNan.tot$cover), by = (1/150))),
  # how to include multiple continuous predictors? check other lectures too!
  # environmental predictors -> set target predictor name to env.*
  env.tot = BetNan.tot$tempjja_ts_10C[!duplicated(BetNan.tot$plotgroup.NUM)]#, # one value per tXpg, amt = annual mean temperature = bio_1
  # env.dis = BetNan.dis$bio_1C,
  # env.plotgroup.dis = BetNan.dis$bio_1C[!duplicated(BetNan.dis$plot.group.NUM)],
  # env.cont = BetNan.cont$bio_1C,
  # env.plotgroup.cont = BetNan.dis$bio_1C[!duplicated(BetNan.dis$plot.group.NUM)],
  # prec.tot = BetNan.tot$bio_12C,           # prec = annual precipitation = bio_12
  # prec.dis = BetNan.dis$bio_12C,
  # prec.cont = BetNan.cont$bio_12C,
  # isotherm.tot = BetNan.tot$bio_3C,        # isotherm = isothermality (diurnal range / annual range) = bio_3
  # isotherm.dis = BetNan.dis$bio_3C,
  # isotherm.cont = BetNan.cont$bio_3C,
  # pdriest.tot = BetNan.tot$bio_14C,        # pdriest = precip of driest month = bio_14
  # pdriest.dis = BetNan.dis$bio_14C,
  # pdriest.cont = BetNan.cont$bio_14C,
  # tseasonality.tot = BetNan.tot$bio_4C,    # tseasonality = temp seasonality (SD*100) = bio_4
  # tseasonalitBetNan.dis = BetNan.dis$bio_4C,
  # tseasonalitBetNan.cont = BetNan.cont$bio_4C,
  # pseasonality.tot = BetNan.tot$bio_15C,   # pseasonality = precip seasonality (coff. of variation) = bio_15
  # pseasonalitBetNan.dis = BetNan.dis$bio_15C,
  # pseasonalitBetNan.cont = BetNan.cont$bio_15C
)
str(shrub_gradient_jags.data)

# JAGS model #### 
# adapted from Bayesian course: rain -> env = target variable,
                              # biome -> species, 
                              # continent -> site, 
                              # cell -> plot group

write("
  
  model{
    
    # priors
      
      # for (i in 1:N_taxa){
      #   intercept[i] ~ dnorm(0,0.0001) # intercept per taxon instead of site-taxon?
      # }
      # 
      # for (i in 1:N_taxa){   # same level as main effect of interest, i.e. taxon X plotgroup # AB - make this slope per taxon (not taxon-plotgroup?)
      #   b_compet[i] ~ dnorm(0,0.0001)
      #   # b_slope[i] ~ dnorm(0,0.0001)
      #   b_sri[i] ~ dnorm(0,0.0001)
      # }
      # 
      # sigma.taxonXplotgroup~dunif(0,100)
      # tau.taxonXplotgroup<-1/(sigma.taxonXplotgroup*sigma.taxonXplotgroup)
      # 
      # for (i in 1:N_taxa){
      #   b.env.x[i]~dnorm(0, 0.001)
      #   b.env.x2[i]~dnorm(0, 0.001)
      # }
      
      
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
        logit(mu[i]) <- b_compet*compet.dis[i] #+ # slope per taxon NOT per taxon-plotgroup?
                        # b_slope[i]*slope.dis[i] + 
                        # b_sri[i]*sri.dis[i] 
      }
      
      
    # LIKELIHOOD for continuous part

      for (i in 1:N_cont){
        cov.cont[i] ~ dbeta(p[i], q[i])
        p[i] <- mu2[i] * phi
        q[i] <- (1 - mu2[i]) * phi
        logit(mu2[i]) <- b_compet*compet.cont[i] #+ # slope per taxon NOT per taxon-plotgroup?
                        # b_slope[i]*slope.cont[i] + 
                        # b_sri[i]*sri.cont[i] 
      }


      for (j in 1:N_plotgroups){ # length of total plotgroups
        b_plotgroup[j] ~ dnorm(mu.plotgroup[j],tau.plotgroup)
        mu.plotgroup[j] <- intercept + # slope per taxon not site-taxon?
                    b.env.x * env.tot[j] + 
                    b.env.x2 * (env.tot[j]^2) # add more plotgroup-level predictors
      }

    
      }
  ","shrub_gradient.jags")

# Initial values ####
# not strictly necessary here (?)

# Parameters to monitor ####

params <- c("b.env.x","b.env.x2","intercept","b_compet", "b_sri","b_plotgroup[1]","b_plotgroup[2]","b_plotgroup[3]","b_plotgroup[63]","sigma.plotgroup","phi") # add b_slope when added to df
# "a.env.x","a.env.x2",

# 5) RUN MODEL

model_out.shrub_gradient <- jags(shrub_gradient_jags.data, inits = NULL, params, 
                                 model.file = "shrub_gradient.jags", n.chains = 3, 
                                 n.iter = 8000, n.burnin = 6000, n.thin=2, DIC=FALSE, 
                                 working.directory=NULL, progress.bar = "text") 

plot(model_out.shrub_gradient) #check convergence, etc.

# extract coefficients 
coeff.shrub_gradient<-as.data.frame(model_out.shrub_gradient$BUGSoutput$summary[,c('mean','sd','2.5%','97.5%','Rhat')])

# add identifying info to data frame
coeff.shrub_gradient$Param <- as.vector(sapply(strsplit(rownames(coeff.shrub_gradient),"[[]",fixed=FALSE), "[", 1))

#compare modeled intercepts with raw-data means - why are they different, especially for the Savanna?
aggregate(env_cov_bio_sub$cover,by=list(env_cov_bio_sub$site,env_cov_bio_sub$taxon),FUN=mean)$x
round(plogis(coeff.shrub_gradient[coeff.shrub_gradient$Param=="intercept","mean"]),3)
# coeff.shrub_gradient[coeff.shrub_gradient$Param=="b1.intBT",]
# coeff.shrub_gradient[coeff.shrub_gradient$Param=="b1BT",]

plot(aggregate(env_cov_bio_sub$cover,by=list(env_cov_bio_sub$taxon),FUN=mean)$x,round(plogis(coeff.shrub_gradient[coeff.shrub_gradient$Param=="intercept","mean"]),4))
