#####################################################################################################################################################
# Drivers of shrub abundance across the Nuuk Fjord inland gradient                                                                                         #
# Code to perform mixture model accounting for proportions of discrete & continuous data                                                            #
#                                                                                                                                                   #
# by Jonathan von Oppen (based on code and workflow by Anne Bjorkman)                                                                               #
# December & January 2019                                                                                                                                     #
#                                                                                                                                                   #
#####################################################################################################################################################
##TO DO!!
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
               rjags,     # to link JAGS and R
               R2jags)    # to link JAGS and R

# Import data ####
# >> on plot group level ----
# complete set of species:
# env_cov_bio <- read.csv("I:/C_Write/_User/JonathanVonOppen_au630524/Project/A_NuukFjord_shrub_abundance_controls/aa_Godthaabsfjord/Data/PlotSpecies/Processed/godthaabsfjord_plots_fusion_table_with_pred_spp_rel_cover_compet_per_plot_group.csv", header = T)
# complete set - local path -> check if updated!
# env_cov_bio <- read.csv("//uni.au.dk/Users/au630524/Documents/Jonathan/Project/A_Nuuk_community_competition_controls/Data/processed/godthaabsfjord_plots_fusion_table_with_pred_spp_rel_cover_compet_per_plot_group.csv", header = T)
# subset of species with competition data:
env_cov_bio <- read.csv("data/Nuuk_fusion_table_plotgroupXtaxon_level.csv", header = T)

# >> on plot level ----
# subset of species with competition data:
env_cov_bio <- read.csv("data/Nuuk_fusion_table_plotXtaxon_level.csv", header = T)


# Prepare data ####
# Select relevant variables: plot info, WorldClim predictors, SRI, yos & long-term mean NDVI, distance to coast, cover, competitive pressure
env_cov_bio_sub <- env_cov_bio %>% select(plot.group.name, site, date, year, long, lat, location,   # plot info / metadata
                              starts_with("bio_"),   # WorldClim predictors
                              wc_alt, ddeg, sri, mean_summ_ndvi_yos, cv_mean_summ_ndvi_2001_to_yos, Perc_dist_coast_lines,   # environmental data
                              taxon, cover, compet)   # taxon, cover response, competition pressure
# Create integer versions of factors
env_cov_bio_sub$plot.group.NUM <- as.numeric(factor(env_cov_bio_sub$plot.group.name, levels=unique(env_cov_bio_sub$plot.group.name)))
env_cov_bio_sub$site.NUM <- as.numeric(factor(env_cov_bio_sub$site, levels = unique(env_cov_bio_sub$site)))
env_cov_bio_sub$taxon.NUM <- as.numeric(factor(env_cov_bio_sub$taxon, levels = unique(env_cov_bio_sub$taxon)))
taxa.num <- data.frame(taxon = levels(env_cov_bio_sub$taxon), 
                       num = env_cov_bio_sub$taxon.NUM[1:nlevels(env_cov_bio_sub$taxon)]) #%>% print()

# Create unique taxon-plotgroup combination variable
env_cov_bio_sub$taxon_plotgroup <- paste(env_cov_bio_sub$plot.group.name,env_cov_bio_sub$taxon,sep="_")
env_cov_bio_sub$taxon_plotgroup.NUM <- as.numeric(factor(env_cov_bio_sub$taxon_plotgroup, levels = unique(env_cov_bio_sub$taxon_plotgroup)))

# scale numeric predictors
num_pred <- env_cov_bio_sub %>% select(starts_with("bio_"), matches("sri"), 
                                       starts_with("Perc"), ends_with("_yos"), 
                                       matches("compet"))
for(i in 1:length(num_pred)){
  col <- colnames(num_pred[i])
  env_cov_bio_sub[paste0(col,"C")] <- as.numeric(scale(num_pred[i], scale = TRUE, center = TRUE))
}

# Separate datasets for each species into continuous and discrete:
env_cov_bio_sub$cover_discrete <- ifelse(env_cov_bio_sub$cover == 1 | env_cov_bio_sub$cover == 0, 1, 0)
y.dis <- env_cov_bio_sub[env_cov_bio_sub$cover_discrete == 1,]
y.cont <- env_cov_bio_sub[env_cov_bio_sub$cover_discrete == 0,]

# # Create unique taxon-plotgroup combination variable, for each group separately:
# y.dis$taxon_plotgroup <- paste(y.dis$plot.group.name,y.dis$taxon,sep="_")
# y.dis$taxon_plotgroup.NUM <- as.numeric(factor(y.dis$taxon_plotgroup, levels = unique(y.dis$taxon_plotgroup)))
# 
# y.cont$taxon_plotgroup <- paste(y.cont$plot.group.name,y.cont$taxon,sep="_")
# y.cont$taxon_plotgroup.NUM <- as.numeric(factor(y.cont$taxon_plotgroup, levels = unique(y.cont$taxon_plotgroup)))


# Compile data into list ####
shrub_gradient_jags.data <- list(
  cov.tot = env_cov_bio_sub$cover,
  UV_plotgroup.tot = env_cov_bio_sub$plot.group.NUM,
  UV_site.tot = env_cov_bio_sub$site.NUM,
  year.tot = env_cov_bio_sub$year,
  UV_taxon.tot = env_cov_bio_sub$taxon.NUM,
  sri.tot = env_cov_bio_sub$sriC,
  ndvi.yos.tot = env_cov_bio_sub$mean_summ_ndvi_yosC,
  ndvi.long.tot = env_cov_bio_sub$cv_mean_summ_ndvi_2001_to_yosC,
  continental.tot = env_cov_bio_sub$Perc_dist_coast_linesC,
  compet.tot = env_cov_bio_sub$competC,
  N_tot = nrow(env_cov_bio_sub),
  N_sites = length(unique(env_cov_bio_sub$site)),
  N_plotgroups = length(unique(env_cov_bio_sub$plot.group.name)),
  N_taxa = length(unique(env_cov_bio_sub$taxon)),
  UV_taxonXplotgroup.tot = env_cov_bio_sub$taxon_plotgroup.NUM,
  N_taxonXplotgroup.tot = length(unique(env_cov_bio_sub$taxon_plotgroup.NUM)),
  cov.dis = y.dis$cover,
  UV_plotgroup.dis = y.dis$plot.group.NUM,
  UV_site.dis = y.dis$site.NUM,
  year.dis = y.dis$year,
  UV_taxon.dis = y.dis$taxon.NUM,
  sri.dis = y.dis$sriC,
  ndvi.yos.dis = y.dis$mean_summ_ndvi_yosC,
  ndvi.long.dis = y.dis$cv_mean_summ_ndvi_2001_to_yosC,
  continental.dis = y.dis$Perc_dist_coast_linesC,
  compet.dis = y.dis$competC,
  N_discrete = nrow(y.dis),
  N_sites.dis = length(unique(y.dis$site)),
  N_plotgroups.dis = length(unique(y.dis$plot.group.name)),
  N_taxa.dis = length(unique(y.dis$taxon)),
  UV_taxonXplotgroup.dis = y.dis$taxon_plotgroup.NUM,
  N_taxonXplotgroup.dis = length(unique(y.dis$taxon_plotgroup.NUM)),
  cov.cont = y.cont$cover,
  UV_plotgroup.cont = y.cont$plot.group.NUM,
  UV_site.cont = y.cont$site.NUM,
  year.cont = y.cont$year,
  UV_taxon.cont = y.cont$taxon.NUM,
  sri.cont = y.cont$sriC,
  ndvi.yos.cont = y.cont$mean_summ_ndvi_yosC,
  ndvi.long.cont = y.cont$cv_mean_summ_ndvi_2001_to_yosC,
  continental.cont = y.cont$Perc_dist_coast_linesC,
  compet.cont = y.cont$competC,
  N_cont = nrow(y.cont),
  N_sites.cont = length(unique(y.cont$site)),
  N_plotgroups.cont = length(unique(y.cont$plot.group.name)),
  N_taxa.cont = length(unique(y.cont$taxon)),
  UV_taxonXplotgroup.cont = y.cont$taxon_plotgroup.NUM,
  N_taxonXplotgroup.cont = length(unique(y.cont$taxon_plotgroup.NUM)),
  xhat = seq(from = min(env_cov_bio_sub$cover), to = max(env_cov_bio_sub$cover), by = (1/150)),
  N_xhat = length(seq(from = min(env_cov_bio_sub$cover), to = max(env_cov_bio_sub$cover), by = (1/150))),
  # how to include multiple continuous predictors? check other lectures too!
  # environmental predictors -> set target predictor name to env.*
  env.tot = env_cov_bio_sub$bio_1C,             # amt = annual mean temperature = bio_1
  env.dis = y.dis$bio_1C,
  env.plotgroup.dis = y.dis$bio_1C[!duplicated(y.dis$plot.group.NUM)],
  env.cont = y.cont$bio_1C,
  env.plotgroup.cont = y.dis$bio_1C[!duplicated(y.dis$plot.group.NUM)],
  prec.tot = env_cov_bio_sub$bio_12C,           # prec = annual precipitation = bio_12
  prec.dis = y.dis$bio_12C,
  prec.cont = y.cont$bio_12C,
  isotherm.tot = env_cov_bio_sub$bio_3C,        # isotherm = isothermality (diurnal range / annual range) = bio_3
  isotherm.dis = y.dis$bio_3C,
  isotherm.cont = y.cont$bio_3C,
  pdriest.tot = env_cov_bio_sub$bio_14C,        # pdriest = precip of driest month = bio_14
  pdriest.dis = y.dis$bio_14C,
  pdriest.cont = y.cont$bio_14C,
  tseasonality.tot = env_cov_bio_sub$bio_4C,    # tseasonality = temp seasonality (SD*100) = bio_4
  tseasonality.dis = y.dis$bio_4C,
  tseasonality.cont = y.cont$bio_4C,
  pseasonality.tot = env_cov_bio_sub$bio_15C,   # pseasonality = precip seasonality (coff. of variation) = bio_15
  pseasonality.dis = y.dis$bio_15C,
  pseasonality.cont = y.cont$bio_15C
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
    
      for (i in 1:N_sites){
        for (j in 1:N_taxa){
          a1[i,j] ~ dunif(-10,10)
          intercept[i,j] ~ dnorm(0,0.0001)
          b1.int[i,j] ~ dnorm(0,0.0001)     # for the intercepts only-models
        }
      }
      
      for (i in 1:N_taxonXplotgroup.tot){   # same level as main effect of interest, i.e. taxon X plotgroup
        b_compet[i] ~ dnorm(0,0.0001)
        # b_slope[i] ~ dnorm(0,0.0001)
        b_sri[i] ~ dnorm(0,0.0001)
      }


      sigma.plotgroup.a~dunif(0,100)
      tau.plotgroup.a<-1/(sigma.plotgroup.a*sigma.plotgroup.a)

      sigma.taxonXplotgroup~dunif(0,100)
      tau.taxonXplotgroup<-1/(sigma.taxonXplotgroup*sigma.taxonXplotgroup)

      sigma.taxonXplotgroup2~dunif(0,100)
      tau.taxonXplotgroup2<-1/(sigma.taxonXplotgroup2*sigma.taxonXplotgroup2)


      for (i in 1:N_taxa){
        a.env.x[i]~dnorm(0,0.001)
        a.env.x2[i]~dnorm(0, 0.001)
        b.env.x[i]~dnorm(0, 0.001)
        b.env.x2[i]~dnorm(0, 0.001)
      }

      phi~dgamma(0.1,0.1)
      phi.int~dgamma(0.1,0.1)


    # LIKELIHOOD

    # likelihood for alpha (prob of discrete vs. continuous data - i.e. relative influence of discrete vs. continuous data on each parameter - just FYI)

      # for (j in 1:N_plotgroups){
      #   a_plotgroup[j] ~ dnorm(mu.plotgroup.a[j],tau.plotgroup.a)
      #   mu.plotgroup.a[j] <- a.env.x[taxonXplotgroup.tot[j]]*env.tot[j] + a.env.x2[taxonXplotgroup.tot[j]]*(env.tot[j]^2)
      # }
      # 
      # for (i in 1:N_tot){
      #   whether.dis[i] ~ dbern(alpha[i])
      #   logit(alpha[i]) <- a1[site.tot[i],taxon.tot[i]] +  a_plotgroup[plotgroup.tot[i]]
      # }
      
    # likelihood for discrete part
      # >> define intercept of logit-transformed mean
      # >> alphalink is the shared parameter between discrete and continuous part!
        # in the longest part, you need to have an estimate for every level you might eventually be interested in
        # here, estimate slopes for predictors at lowest (= plot) level; higher- (= plotgroup-)level predictors are accounted for by the taxon X plotgroup combinations
        # get rid of alpha stuff altogether for now, comment out
        # other factors additive, same thing within square brackets
        
      # for (j in 1:N_taxonXplotgroup.dis){ # -> length of plotgroup per taxon combinations, SAME FOR CONTINUOUS PART!?
      #   b_taxonXplotgroup[j] ~ dnorm(mu.taxonXplotgroup[j],tau.taxonXplotgroup)
      #   mu.taxonplotgroup[j] <- b.env.x[taxon.dis[j]]*env.dis[j] + b.env.x2[taxon.dis[j]]*(env.dis[j]^2)
      # }

      for (i in 1:N_discrete){ 
        cov.dis[i] ~ dbern(mu[i])
        logit(mu[i]) <- b_compet[UV_taxonXplotgroup.dis[i]]*compet.dis[i] + 
                        # b_slope[UV_taxonXplotgroup.dis[i]]*slope.dis[i] + 
                        b_sri[UV_taxonXplotgroup.dis[i]]*sri.dis[i] + 
                        b_taxonXplotgroup[UV_taxonXplotgroup.dis[i]] # -> beta by taxon X plotgroup combination, iterating over plots
      }
      
      
    # likelihood for continuous part

      # for (j in 1:N_taxonXplotgroup.cont){ # -> make common loop for both discrete and cont part?
      #   b_taxonXplotgroup2[j] ~ dnorm(mu.taxonXplotgroup2[j],tau.taxonXplotgroup2)
      #   mu.taxonXplotgroup2[j] <- b.env.x[taxon.cont[j]]*env.cont[j] + b.env.x2[taxon.cont[j]]*(env.cont[j]^2) # -> beta by taxon X plot combination
      # }

      for (i in 1:N_cont){
        cov.cont[i] ~ dbeta(p[i], q[i])
        p[i] <- mu2[i] * phi
        q[i] <- (1 - mu2[i]) * phi
        logit(mu2[i]) <- b_compet[UV_taxonXplotgroup.dis[i]]*compet.cont[i] + 
                         # b_slope[UV_taxonXplotgroup.cont[i]]*slope.cont[i] + 
                         b_sri[UV_taxonXplotgroup.cont[i]]*sri.cont[i] + 
                         b_taxonXplotgroup[UV_taxonXplotgroup.cont[i]] # -> beta by taxon X plotgroup combination, iterating over plots
      }


      for (j in 1:N_taxonXplotgroup.tot){ # -> length of total plotgroup per taxon combinations
        b_taxonXplotgroup[j] ~ dnorm(mu.taxonXplotgroup[j],tau.taxonXplotgroup)
        mu.taxonXplotgroup[j] <- intercept[UV_site.tot[j], UV_taxon.tot[j]] + b.env.x[UV_taxon.tot[j]]*env.tot[j] + b.env.x2[UV_taxon.tot[j]]*(env.tot[j]^2) # add more plotgroup-level predictors
      }


    # INTERCEPTS ONLY (mean per taxon and site)

      for (i in 1:N_discrete){ 
        cov.dis.int[i] ~ dbern(mu.int[i])
        logit(mu.int[i]) <- b1.int[UV_site.dis[i],UV_taxon.dis[i]]
      }

      for (i in 1:N_cont){
        cov.cont.int[i] ~ dbeta(p.int[i], q.int[i])
        p.int[i] <- mu2.int[i] * phi.int
        q.int[i] <- (1 - mu2.int[i]) * phi.int
        logit(mu2.int[i]) <- b1.int[UV_site.cont[i],UV_taxon.cont[i]]
      }


    # Derived parameters
      # >> BT = BackTransformed
      
      for (i in 1:N_sites){
        for (j in 1:N_taxa){
          a1BT[i,j]<-ilogit(a1[i,j])
          interceptBT[i,j]<-ilogit(intercept[i,j])
          b1.intBT[i,j]<-ilogit(b1.int[i,j])
        }
      }

      int.meanBetNan<-mean(intercept[,1]) # see df taxa.num for numeric taxon codes
      int.meanCasTet<-mean(intercept[,2]) 
      int.meanEmpNig<-mean(intercept[,3]) 
      int.meanLedGro<-mean(intercept[,4]) 
      int.meanLedPal<-mean(intercept[,5]) 
      int.meanPhyCoe<-mean(intercept[,6]) 
      int.meanSalGla<-mean(intercept[,7]) 
      int.meanSalArc<-mean(intercept[,8]) 
      int.meanVacUli<-mean(intercept[,9]) 

      int.meanBetNanBT<-ilogit(int.meanBetNan) # backtransformed intercepts
      int.meanCasTetBT<-ilogit(int.meanCasTet)
      int.meanEmpNigBT<-ilogit(int.meanEmpNig)
      int.meanLedGroBT<-ilogit(int.meanLedGro)
      int.meanLedPalBT<-ilogit(int.meanLedPal)
      int.meanPhyCoeBT<-ilogit(int.meanPhyCoe)
      int.meanSalGlaBT<-ilogit(int.meanSalGla)
      int.meanSalArcBT<-ilogit(int.meanSalArc)
      int.meanVacUliBT<-ilogit(int.meanVacUli)
    
      }
  ","shrub_gradient.jags")

# Initial values ####
# not strictly necessary here (?)

# Parameters to monitor ####

params <- c("a.env.x","a.env.x2","b.env.x","b.env.x2","a1BT","interceptBT","b1.intBT","b_compet", "b_sri") # add b_slope when added to df

# 5) RUN MODEL

model_out.shrub_gradient <- jags(shrub_gradient_jags.data, inits = NULL, params, 
                                 model.file = "shrub_gradient.jags", n.chains = 3, 
                                 n.iter = 20000, n.burnin = 15000, n.thin=2, DIC=FALSE, 
                                 working.directory=NULL, progress.bar = "text") 

plot(model_out.shrub_gradient) #check convergence, etc.

# extract coefficients 
coeff.shrub_gradient<-as.data.frame(model_out.shrub_gradient$BUGSoutput$summary[,c('mean','sd','2.5%','97.5%','Rhat')])

# add identifying info to data frame
coeff.shrub_gradient$Param <- as.vector(sapply(strsplit(rownames(coeff.shrub_gradient),"[[]",fixed=FALSE), "[", 1))

#compare modeled intercepts with raw-data means - why are they different, especially for the Savanna?
aggregate(env_cov_bio_sub$cover,by=list(env_cov_bio_sub$site,env_cov_bio_sub$taxon),FUN=mean)
coeff.shrub_gradient[coeff.shrub_gradient$Param=="b1.intBT",]
coeff.shrub_gradient[coeff.shrub_gradient$Param=="b1BT",]

