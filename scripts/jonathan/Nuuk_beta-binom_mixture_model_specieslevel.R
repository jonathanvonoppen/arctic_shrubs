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
               cowplot,
               rjags,     # to link JAGS and R
               R2jags)    # to link JAGS and R


# Colour scheme ####
theme_red <- "#E64447FF"
theme_blue <- "#12435DFF"
theme_lightblue <- "#1FA3AEFF"
theme_grey <- "#EDEDEDFF"
theme_yellow <- "#EEBE5BFF"

# additional colours
theme_darkblue <- "#1D5799"
theme_darkgreen <- "#13944D"
theme_orange <- "#B56A24"
theme_purple <- "#8757B3"


# Functions ####
model_plot_function <- function(model_coeff_output) {
  target_vars <- c("b.tempjja.x", "b.tempjja.x2",
                   "b.tempcont.x", "b.tempcont.x2",
                   "b.precipjja.x", "b.precipjja.x2",
                   "b.sri",
                   "b.tri",
                   "b.twi_90m", 
                   "b.compet")
  solutions <- model_coeff_output
  names(solutions) <- c("variable", "post.mean", "post.sd", "l95", "l90", "u90", "u95", "Rhat")
  solutions <- solutions %>% 
    filter(variable %in% target_vars)
  # solutions$variable <- factor(solutions$variable,
  #                               levels = c("b.tempjja.x", "b.tempjja.x2",
  #                                          "b.tempcont.x", "b.tempcont.x2",
  #                                          "b.precipjja.x", "b.precipjja.x2",
  #                                          "b.sri",
  #                                          "b.tri",
  #                                          "b.twi_90m",
  #                                          "b.compet"))
  min_value <- floor(min(solutions$l95))
  max_value <- ceiling(max(solutions$u95))
  solutions$sig <- "ns"
  solutions$sig[solutions$l95 < 0 & solutions$u95 < 0] <- "sig"
  solutions$sig[solutions$l95 > 0 & solutions$u95 > 0] <- "sig"
  label_colour <- rep("black", nrow(solutions))
  label_colour[solutions$sig == "sig"] <- theme_darkgreen
  label_face <- rep("plain", nrow(solutions))
  label_face[solutions$sig == "sig"] <- "bold"
  # label_face[response == "T1_mean" & solutions$sig == "sig"] <- "bold"
  title_colour <- "black"
  # if(response == "T1_mean" | response == "T1_amp") title_colour <- theme_red
  # if(response == "T2_mean" | response == "T2_amp") title_colour <- theme_yellow
  # if(response == "T1_mean") response <- "Soil"
  # if(response == "T2_mean") response <- "Ground"
  
  
  model_plot <- ggplot(solutions, aes(x = variable, y = post.mean,
                                      ymin = l95, ymax = u95,
                                      colour = sig)) +
    geom_point() +
    geom_errorbar(width = .8) +
    theme_cowplot(18) +
    ylab("Effect Size (scaled)") +
    xlab("") +
    #ggtitle("Betula nana") + # (paste0(response))
    scale_colour_manual(values = c("black", theme_darkgreen)) +
    scale_y_continuous(limits = c(min_value, max_value), breaks = seq(min_value,max_value,0.5)) +
    # scale_x_discrete(limits = c("b.tempjja.x", "b.tempjja.x2",
    #         "b.tempcont.x", "b.tempcont.x2",
    #         "b.precipjja.x", "b.precipjja.x2",
    #         "b.sri",
    #         "b.tri",
    #         "b.twi_90m",
    #         "b.compet"),
    #                  labels = c("summer temperature", bquote(.("summer") *" "* temperature^2),
    #                             "temperature variability", bquote(.("temperature") *" "* variability^2),
    #                             "summer precipitation", bquote(.("summer") *" "* precipitation^2),
    #                             "solar radiation",
  #                             "terrain ruggedness",
  #                             "moisture availability",
  #                             "competition")) +
  annotate("segment", x = 0, xend = 11, y = 0, yend = 0) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = label_colour, face = label_face),
          plot.title = element_text(colour = title_colour, face = "italic"),
          legend.position = "none")
  return(model_plot)
}

# Import data ####
# >> on plot group level - NOT USED ---
# complete set of species:
# env_cov_bio <- read.csv("I:/C_Write/_User/JonathanVonOppen_au630524/Project/A_NuukFjord_shrub_abundance_controls/aa_Godthaabsfjord/Data/PlotSpecies/Processed/godthaabsfjord_plots_fusion_table_with_pred_spp_rel_cover_compet_per_plot_group.csv", header = T)
# complete set - local path -> check if updated!
# env_cov_bio <- read.csv("//uni.au.dk/Users/au630524/Documents/Jonathan/Project/A_Nuuk_community_competition_controls/Data/processed/godthaabsfjord_plots_fusion_table_with_pred_spp_rel_cover_compet_per_plot_group.csv", header = T)
# subset of species with competition data:
# env_cov_bio <- read.csv("data/Nuuk_env_cover_plotgroups.csv", header = T)

# >> on plot level ---
# subset of species with competition data:
env_cov_bio <- read.csv("data/Nuuk_env_cover_plots.csv", header = T, stringsAsFactors = F)
  
# Prepare data ####
# Select relevant variables: plot info, WorldClim predictors, SRI, yos & long-term mean NDVI, distance to coast, cover, competitive pressure
env_cov_bio_sub <- env_cov_bio %>% 
  select(site_alt_plotgroup_id, site, alt, site_alt_id, year, long, lat,  # plot info / metadata
         ends_with("_ts_30"),   # CHELSA predictors averaged over 30-year period prior to study year
         inclin_down, twi_90m, tri, sri, 
         #mean_summ_ndvi_yos, cv_mean_summ_ndvi_2001_to_yos, Perc_dist_coast_lines,   # environmental data
         taxon, cover, compet) %>% # taxon, cover response, competition pressure
  
  mutate(taxon = as.factor(taxon))
  

# REMOVE Cassiope tetragona & others because it is so low abundance it doesn't converge. Summary of zeros:
# env_cov_bio %>% group_by(taxon) %>% summarise_at(vars(cover), list(~sum(. == 0))) %>% rename(zeros = cover) %>% mutate(zeros_perc = zeros/69)
# !!! Model crashes if >2 taxa removed !!! (JvO 06 Apr 20)
#env_cov_bio_sub <- env_cov_bio_sub[env_cov_bio_sub$taxon %in% c("Cassiope tetragona", "Phyllodoce coerulea")==F,]

# ORDER data

env_cov_bio_sub <- env_cov_bio_sub[order(env_cov_bio_sub$site_alt_plotgroup_id, env_cov_bio_sub$taxon),]

# Create integer versions of factors
env_cov_bio_sub$plotgroup.NUM <- as.numeric(factor(env_cov_bio_sub$site_alt_plotgroup_id, 
                                                   levels=unique(env_cov_bio_sub$site_alt_plotgroup_id)))
env_cov_bio_sub$site_alt.NUM <- as.numeric(factor(env_cov_bio_sub$site_alt_id,
                                                  levels = unique(env_cov_bio_sub$site_alt_id)))
env_cov_bio_sub$site.NUM <- as.numeric(factor(env_cov_bio_sub$site, 
                                              levels = unique(env_cov_bio_sub$site)))
env_cov_bio_sub$taxon.NUM <- as.numeric(factor(env_cov_bio_sub$taxon, 
                                               levels = unique(env_cov_bio_sub$taxon)))
# taxa.num <- data.frame(taxon = levels(env_cov_bio_sub$taxon), 
#                        num = env_cov_bio_sub$taxon.NUM[1:nlevels(env_cov_bio_sub$taxon)]) #%>% print()

# # Create unique taxon-plotgroup combination variable
# env_cov_bio_sub$taxon_plotgroup <- paste(env_cov_bio_sub$site_alt_plotgroup_id, env_cov_bio_sub$taxon, sep="_")
# env_cov_bio_sub$taxon_plotgroup.NUM <- as.numeric(factor(env_cov_bio_sub$taxon_plotgroup, levels = unique(env_cov_bio_sub$taxon_plotgroup)))

# scale numeric predictors
num_pred <- env_cov_bio_sub %>% select(alt,
                                       ends_with("_ts_30"), 
                                       tri,
                                       sri, 
                                       starts_with("twi"), 
                                       matches("compet"))
for(i in 1:length(num_pred)){
  col <- colnames(num_pred[i])
  env_cov_bio_sub[paste0(col,"C")] <- as.numeric(scale(num_pred[i], scale = TRUE, center = TRUE))
}

# Separate datasets for each species into continuous and discrete:

# assign discrete = 1, continuous = 0 variable
env_cov_bio_sub$cover_discrete <- ifelse(env_cov_bio_sub$cover == 1 | env_cov_bio_sub$cover == 0, 1, 0)

# >> split dataset for separate species ----
env_cov_bio_sub_spec.tot <- split(env_cov_bio_sub, env_cov_bio_sub$taxon)

# assign taxon name to list elements
# >> for total datasets
for (taxon_id in 1:nlevels(env_cov_bio_sub$taxon)){
  # extract 3-letter genus name string
  assign(paste0(str_extract(levels(env_cov_bio_sub$taxon)[taxon_id], 
                            "^\\w{3}"),
                # extract and capitalise 3-letter species name string
                str_to_title(str_remove(str_extract(levels(env_cov_bio_sub$taxon)[taxon_id],
                                                    "\\s\\w{3}"),
                                        "\\s")),
                # add extension
                ".tot"),
         # assign to respective list element
         env_cov_bio_sub_spec.tot[[taxon_id]])
}



# >> for discrete datasets
env_cov_bio_sub_spec.dis <- list()
for (taxon_id in 1:nlevels(env_cov_bio_sub$taxon)){
  # filter for discrete response values
  env_cov_bio_sub_spec.dis[[taxon_id]] <- filter(env_cov_bio_sub_spec.tot[[taxon_id]], cover_discrete == 1)
  # extract 3-letter genus name string
  assign(paste0(str_extract(levels(env_cov_bio_sub$taxon)[taxon_id], 
                            "^\\w{3}"),
                # extract and capitalise 3-letter species name string
                str_to_title(str_remove(str_extract(levels(env_cov_bio_sub$taxon)[taxon_id],
                                                    "\\s\\w{3}"),
                                        "\\s")),
                # add extension
                ".dis"),
         # assign to respective list element
         env_cov_bio_sub_spec.dis[[taxon_id]])
}

# >> for continuous datasets
env_cov_bio_sub_spec.cont <- list()
for (taxon_id in 1:nlevels(env_cov_bio_sub$taxon)){
  # filter for continuous response values
  env_cov_bio_sub_spec.cont[[taxon_id]] <- filter(env_cov_bio_sub_spec.tot[[taxon_id]], cover_discrete == 0)
  # extract 3-letter genus name string
  assign(paste0(str_extract(levels(env_cov_bio_sub$taxon)[taxon_id], 
                            "^\\w{3}"),
                # extract and capitalise 3-letter species name string
                str_to_title(str_remove(str_extract(levels(env_cov_bio_sub$taxon)[taxon_id],
                                                    "\\s\\w{3}"),
                                        "\\s")),
                # add extension
                ".cont"),
         # assign to respective list element
         env_cov_bio_sub_spec.cont[[taxon_id]])
}

# # Create unique taxon-plotgroup combination variable, for each group separately:
# y.dis$taxon_plotgroup <- paste(y.dis$site_alt_plotgroup_id,y.dis$taxon,sep="_")
# y.dis$taxon_plotgroup.NUM <- as.numeric(factor(y.dis$taxon_plotgroup, levels = unique(y.dis$taxon_plotgroup)))
# 
# y.cont$taxon_plotgroup <- paste(y.cont$site_alt_plotgroup_id,y.cont$taxon,sep="_")
# y.cont$taxon_plotgroup.NUM <- as.numeric(factor(y.cont$taxon_plotgroup, levels = unique(y.cont$taxon_plotgroup)))


# Compile data into lists ####

# * BetNan ----
shrub_gradient_jags.BetNan.data <- list(
  
  # plot level predictors, for discrete...
  cov.dis = BetNan.dis$cover,
  plotgroup.dis = BetNan.dis$plotgroup.NUM, #AB added this
  # isocline.dis = BetNan.dis$site_alt.NUM,
  # inclin_down.dis = BetNan.dis$inclin_downC,
  sri.dis = BetNan.dis$sriC,
  tri.dis = BetNan.dis$triC,
  twi_90m.dis = BetNan.dis$twi_90mC,
  compet.dis = BetNan.dis$competC,
  N_discrete = nrow(BetNan.dis),
  
  # ...and continuous part of the data
  cov.cont = BetNan.cont$cover,
  plotgroup.cont = BetNan.cont$plotgroup.NUM, #AB added this
  # isocline.cont = BetNan.cont$site_alt.NUM,
  # inclin_down.cont = BetNan.cont$inclin_downC,
  sri.cont = BetNan.cont$sriC,
  tri.cont = BetNan.cont$triC,
  twi_90m.cont = BetNan.cont$twi_90mC,
  compet.cont = BetNan.cont$competC,
  N_cont = nrow(BetNan.cont),
  
  # plot group level predictors
  tempjja.tot = BetNan.tot$tempjja_ts_30C[!duplicated(BetNan.tot$plotgroup.NUM)], # one value per tXpg
  # tempmax.tot = BetNan.tot$tempmax_ts_30C[!duplicated(BetNan.tot$plotgroup.NUM)],
  # tempmin.tot = BetNan.tot$tempmin_ts_30C[!duplicated(BetNan.tot$plotgroup.NUM)],
  tempcont.tot = BetNan.tot$tempcont_ts_30C[!duplicated(BetNan.tot$plotgroup.NUM)],
  precipjja.tot = BetNan.tot$precipjja_ts_30C[!duplicated(BetNan.tot$plotgroup.NUM)],
  # precipjfmam.tot = BetNan.tot$precipjfmam_ts_30C[!duplicated(BetNan.tot$plotgroup.NUM)]
  N_plotgroups = length(unique(BetNan.tot$site_alt_plotgroup_id))
  
  # # site/alt level predictors
  # alt.tot = BetNan.tot$altC[!duplicated(BetNan.tot$site_alt.NUM)],
  # N_isoclines = length(unique(BetNan.tot$site_alt_id))
)
str(shrub_gradient_jags.BetNan.data)

# * CasTet ----
shrub_gradient_jags.CasTet.data <- list(
  
  # plot level predictors, for discrete...
  cov.dis = CasTet.dis$cover,
  plotgroup.dis = CasTet.dis$plotgroup.NUM, #AB added this
  # isocline.dis = CasTet.dis$site_alt.NUM,
  # inclin_down.dis = CasTet.dis$inclin_downC,
  sri.dis = CasTet.dis$sriC,
  tri.dis = CasTet.dis$triC,
  twi_90m.dis = CasTet.dis$twi_90mC,
  compet.dis = CasTet.dis$competC,
  N_discrete = nrow(CasTet.dis),
  
  # ...and continuous part of the data
  cov.cont = CasTet.cont$cover,
  plotgroup.cont = CasTet.cont$plotgroup.NUM, #AB added this
  # isocline.cont = CasTet.cont$site_alt.NUM,
  # inclin_down.cont = CasTet.cont$inclin_downC,
  sri.cont = CasTet.cont$sriC,
  tri.cont = CasTet.cont$triC,
  twi_90m.cont = CasTet.cont$twi_90mC,
  compet.cont = CasTet.cont$competC,
  N_cont = nrow(CasTet.cont),
  
  # plot group level predictors
  tempjja.tot = CasTet.tot$tempjja_ts_30C[!duplicated(CasTet.tot$plotgroup.NUM)], # one value per tXpg
  # tempmax.tot = CasTet.tot$tempmax_ts_30C[!duplicated(CasTet.tot$plotgroup.NUM)],
  # tempmin.tot = CasTet.tot$tempmin_ts_30C[!duplicated(CasTet.tot$plotgroup.NUM)],
  tempcont.tot = CasTet.tot$tempcont_ts_30C[!duplicated(CasTet.tot$plotgroup.NUM)],
  precipjja.tot = CasTet.tot$precipjja_ts_30C[!duplicated(CasTet.tot$plotgroup.NUM)],
  # precipjfmam.tot = CasTet.tot$precipjfmam_ts_30C[!duplicated(CasTet.tot$plotgroup.NUM)]
  N_plotgroups = length(unique(CasTet.tot$site_alt_plotgroup_id))
  
  # # site/alt level predictors
  # alt.tot = CasTet.tot$altC[!duplicated(CasTet.tot$site_alt.NUM)],
  # N_isoclines = length(unique(CasTet.tot$site_alt_id))
)
str(shrub_gradient_jags.CasTet.data)

# * EmpNig ----
shrub_gradient_jags.EmpNig.data <- list(
  
  # plot level predictors, for discrete...
  cov.dis = EmpNig.dis$cover,
  plotgroup.dis = EmpNig.dis$plotgroup.NUM, #AB added this
  # isocline.dis = EmpNig.dis$site_alt.NUM,
  # inclin_down.dis = EmpNig.dis$inclin_downC,
  sri.dis = EmpNig.dis$sriC,
  tri.dis = EmpNig.dis$triC,
  twi_90m.dis = EmpNig.dis$twi_90mC,
  compet.dis = EmpNig.dis$competC,
  N_discrete = nrow(EmpNig.dis),
  
  # ...and continuous part of the data
  cov.cont = EmpNig.cont$cover,
  plotgroup.cont = EmpNig.cont$plotgroup.NUM, #AB added this
  # isocline.cont = EmpNig.cont$site_alt.NUM,
  # inclin_down.cont = EmpNig.cont$inclin_downC,
  sri.cont = EmpNig.cont$sriC,
  tri.cont = EmpNig.cont$triC,
  twi_90m.cont = EmpNig.cont$twi_90mC,
  compet.cont = EmpNig.cont$competC,
  N_cont = nrow(EmpNig.cont),
  
  # plot group level predictors
  tempjja.tot = EmpNig.tot$tempjja_ts_30C[!duplicated(EmpNig.tot$plotgroup.NUM)], # one value per tXpg
  # tempmax.tot = EmpNig.tot$tempmax_ts_30C[!duplicated(EmpNig.tot$plotgroup.NUM)],
  # tempmin.tot = EmpNig.tot$tempmin_ts_30C[!duplicated(EmpNig.tot$plotgroup.NUM)],
  tempcont.tot = EmpNig.tot$tempcont_ts_30C[!duplicated(EmpNig.tot$plotgroup.NUM)],
  precipjja.tot = EmpNig.tot$precipjja_ts_30C[!duplicated(EmpNig.tot$plotgroup.NUM)],
  # precipjfmam.tot = EmpNig.tot$precipjfmam_ts_30C[!duplicated(EmpNig.tot$plotgroup.NUM)]
  N_plotgroups = length(unique(EmpNig.tot$site_alt_plotgroup_id))
  
  # # site/alt level predictors
  # alt.tot = EmpNig.tot$altC[!duplicated(EmpNig.tot$site_alt.NUM)],
  # N_isoclines = length(unique(EmpNig.tot$site_alt_id))
)
str(shrub_gradient_jags.EmpNig.data)

# * PhyCae ----
shrub_gradient_jags.PhyCae.data <- list(
  
  # plot level predictors, for discrete...
  cov.dis = PhyCae.dis$cover,
  plotgroup.dis = PhyCae.dis$plotgroup.NUM, #AB added this
  # isocline.dis = PhyCae.dis$site_alt.NUM,
  # inclin_down.dis = PhyCae.dis$inclin_downC,
  sri.dis = PhyCae.dis$sriC,
  tri.dis = PhyCae.dis$triC,
  twi_90m.dis = PhyCae.dis$twi_90mC,
  compet.dis = PhyCae.dis$competC,
  N_discrete = nrow(PhyCae.dis),
  
  # ...and continuous part of the data
  cov.cont = PhyCae.cont$cover,
  plotgroup.cont = PhyCae.cont$plotgroup.NUM, #AB added this
  # isocline.cont = PhyCae.cont$site_alt.NUM,
  # inclin_down.cont = PhyCae.cont$inclin_downC,
  sri.cont = PhyCae.cont$sriC,
  tri.cont = PhyCae.cont$triC,
  twi_90m.cont = PhyCae.cont$twi_90mC,
  compet.cont = PhyCae.cont$competC,
  N_cont = nrow(PhyCae.cont),
  
  # plot group level predictors
  tempjja.tot = PhyCae.tot$tempjja_ts_30C[!duplicated(PhyCae.tot$plotgroup.NUM)], # one value per tXpg
  # tempmax.tot = PhyCae.tot$tempmax_ts_30C[!duplicated(PhyCae.tot$plotgroup.NUM)],
  # tempmin.tot = PhyCae.tot$tempmin_ts_30C[!duplicated(PhyCae.tot$plotgroup.NUM)],
  tempcont.tot = PhyCae.tot$tempcont_ts_30C[!duplicated(PhyCae.tot$plotgroup.NUM)],
  precipjja.tot = PhyCae.tot$precipjja_ts_30C[!duplicated(PhyCae.tot$plotgroup.NUM)],
  # precipjfmam.tot = PhyCae.tot$precipjfmam_ts_30C[!duplicated(PhyCae.tot$plotgroup.NUM)]
  N_plotgroups = length(unique(PhyCae.tot$site_alt_plotgroup_id))
  
  # # site/alt level predictors
  # alt.tot = PhyCae.tot$altC[!duplicated(PhyCae.tot$site_alt.NUM)],
  # N_isoclines = length(unique(PhyCae.tot$site_alt_id))
)
str(shrub_gradient_jags.PhyCae.data)

# * RhoGro ----
shrub_gradient_jags.RhoGro.data <- list(
  
  # plot level predictors, for discrete...
  cov.dis = RhoGro.dis$cover,
  plotgroup.dis = RhoGro.dis$plotgroup.NUM, #AB added this
  # isocline.dis = RhoGro.dis$site_alt.NUM,
  # inclin_down.dis = RhoGro.dis$inclin_downC,
  sri.dis = RhoGro.dis$sriC,
  tri.dis = RhoGro.dis$triC,
  twi_90m.dis = RhoGro.dis$twi_90mC,
  compet.dis = RhoGro.dis$competC,
  N_discrete = nrow(RhoGro.dis),
  
  # ...and continuous part of the data
  cov.cont = RhoGro.cont$cover,
  plotgroup.cont = RhoGro.cont$plotgroup.NUM, #AB added this
  # isocline.cont = RhoGro.cont$site_alt.NUM,
  # inclin_down.cont = RhoGro.cont$inclin_downC,
  sri.cont = RhoGro.cont$sriC,
  tri.cont = RhoGro.cont$triC,
  twi_90m.cont = RhoGro.cont$twi_90mC,
  compet.cont = RhoGro.cont$competC,
  N_cont = nrow(RhoGro.cont),
  
  # plot group level predictors
  tempjja.tot = RhoGro.tot$tempjja_ts_30C[!duplicated(RhoGro.tot$plotgroup.NUM)], # one value per tXpg
  # tempmax.tot = RhoGro.tot$tempmax_ts_30C[!duplicated(RhoGro.tot$plotgroup.NUM)],
  # tempmin.tot = RhoGro.tot$tempmin_ts_30C[!duplicated(RhoGro.tot$plotgroup.NUM)],
  tempcont.tot = RhoGro.tot$tempcont_ts_30C[!duplicated(RhoGro.tot$plotgroup.NUM)],
  precipjja.tot = RhoGro.tot$precipjja_ts_30C[!duplicated(RhoGro.tot$plotgroup.NUM)],
  # precipjfmam.tot = RhoGro.tot$precipjfmam_ts_30C[!duplicated(RhoGro.tot$plotgroup.NUM)]
  N_plotgroups = length(unique(RhoGro.tot$site_alt_plotgroup_id))
  
  # # site/alt level predictors
  # alt.tot = RhoGro.tot$altC[!duplicated(RhoGro.tot$site_alt.NUM)],
  # N_isoclines = length(unique(RhoGro.tot$site_alt_id))
)
str(shrub_gradient_jags.RhoGro.data)

# * RhoTom ----
shrub_gradient_jags.RhoTom.data <- list(
  
  # plot level predictors, for discrete...
  cov.dis = RhoTom.dis$cover,
  plotgroup.dis = RhoTom.dis$plotgroup.NUM, #AB added this
  # isocline.dis = RhoTom.dis$site_alt.NUM,
  # inclin_down.dis = RhoTom.dis$inclin_downC,
  sri.dis = RhoTom.dis$sriC,
  tri.dis = RhoTom.dis$triC,
  twi_90m.dis = RhoTom.dis$twi_90mC,
  compet.dis = RhoTom.dis$competC,
  N_discrete = nrow(RhoTom.dis),
  
  # ...and continuous part of the data
  cov.cont = RhoTom.cont$cover,
  plotgroup.cont = RhoTom.cont$plotgroup.NUM, #AB added this
  # isocline.cont = RhoTom.cont$site_alt.NUM,
  # inclin_down.cont = RhoTom.cont$inclin_downC,
  sri.cont = RhoTom.cont$sriC,
  tri.cont = RhoTom.cont$triC,
  twi_90m.cont = RhoTom.cont$twi_90mC,
  compet.cont = RhoTom.cont$competC,
  N_cont = nrow(RhoTom.cont),
  
  # plot group level predictors
  tempjja.tot = RhoTom.tot$tempjja_ts_30C[!duplicated(RhoTom.tot$plotgroup.NUM)], # one value per tXpg
  # tempmax.tot = RhoTom.tot$tempmax_ts_30C[!duplicated(RhoTom.tot$plotgroup.NUM)],
  # tempmin.tot = RhoTom.tot$tempmin_ts_30C[!duplicated(RhoTom.tot$plotgroup.NUM)],
  tempcont.tot = RhoTom.tot$tempcont_ts_30C[!duplicated(RhoTom.tot$plotgroup.NUM)],
  precipjja.tot = RhoTom.tot$precipjja_ts_30C[!duplicated(RhoTom.tot$plotgroup.NUM)],
  # precipjfmam.tot = RhoTom.tot$precipjfmam_ts_30C[!duplicated(RhoTom.tot$plotgroup.NUM)]
  N_plotgroups = length(unique(RhoTom.tot$site_alt_plotgroup_id))
  
  # # site/alt level predictors
  # alt.tot = RhoTom.tot$altC[!duplicated(RhoTom.tot$site_alt.NUM)],
  # N_isoclines = length(unique(RhoTom.tot$site_alt_id))
)
str(shrub_gradient_jags.RhoTom.data)

# * SalArc ----
shrub_gradient_jags.SalArc.data <- list(
  
  # plot level predictors, for discrete...
  cov.dis = SalArc.dis$cover,
  plotgroup.dis = SalArc.dis$plotgroup.NUM, #AB added this
  # isocline.dis = SalArc.dis$site_alt.NUM,
  # inclin_down.dis = SalArc.dis$inclin_downC,
  sri.dis = SalArc.dis$sriC,
  tri.dis = SalArc.dis$triC,
  twi_90m.dis = SalArc.dis$twi_90mC,
  compet.dis = SalArc.dis$competC,
  N_discrete = nrow(SalArc.dis),
  
  # ...and continuous part of the data
  cov.cont = SalArc.cont$cover,
  plotgroup.cont = SalArc.cont$plotgroup.NUM, #AB added this
  # isocline.cont = SalArc.cont$site_alt.NUM,
  # inclin_down.cont = SalArc.cont$inclin_downC,
  sri.cont = SalArc.cont$sriC,
  tri.cont = SalArc.cont$triC,
  twi_90m.cont = SalArc.cont$twi_90mC,
  compet.cont = SalArc.cont$competC,
  N_cont = nrow(SalArc.cont),
  
  # plot group level predictors
  tempjja.tot = SalArc.tot$tempjja_ts_30C[!duplicated(SalArc.tot$plotgroup.NUM)], # one value per tXpg
  # tempmax.tot = SalArc.tot$tempmax_ts_30C[!duplicated(SalArc.tot$plotgroup.NUM)],
  # tempmin.tot = SalArc.tot$tempmin_ts_30C[!duplicated(SalArc.tot$plotgroup.NUM)],
  tempcont.tot = SalArc.tot$tempcont_ts_30C[!duplicated(SalArc.tot$plotgroup.NUM)],
  precipjja.tot = SalArc.tot$precipjja_ts_30C[!duplicated(SalArc.tot$plotgroup.NUM)],
  # precipjfmam.tot = SalArc.tot$precipjfmam_ts_30C[!duplicated(SalArc.tot$plotgroup.NUM)]
  N_plotgroups = length(unique(SalArc.tot$site_alt_plotgroup_id))
  
  # # site/alt level predictors
  # alt.tot = SalArc.tot$altC[!duplicated(SalArc.tot$site_alt.NUM)],
  # N_isoclines = length(unique(SalArc.tot$site_alt_id))
)
str(shrub_gradient_jags.SalArc.data)

# * SalGla ----
shrub_gradient_jags.SalGla.data <- list(
  
  # plot level predictors, for discrete...
  cov.dis = SalGla.dis$cover,
  plotgroup.dis = SalGla.dis$plotgroup.NUM, #AB added this
  # isocline.dis = SalGla.dis$site_alt.NUM,
  # inclin_down.dis = SalGla.dis$inclin_downC,
  sri.dis = SalGla.dis$sriC,
  tri.dis = SalGla.dis$triC,
  twi_90m.dis = SalGla.dis$twi_90mC,
  compet.dis = SalGla.dis$competC,
  N_discrete = nrow(SalGla.dis),
  
  # ...and continuous part of the data
  cov.cont = SalGla.cont$cover,
  plotgroup.cont = SalGla.cont$plotgroup.NUM, #AB added this
  # isocline.cont = SalGla.cont$site_alt.NUM,
  # inclin_down.cont = SalGla.cont$inclin_downC,
  sri.cont = SalGla.cont$sriC,
  tri.cont = SalGla.cont$triC,
  twi_90m.cont = SalGla.cont$twi_90mC,
  compet.cont = SalGla.cont$competC,
  N_cont = nrow(SalGla.cont),
  
  # plot group level predictors
  tempjja.tot = SalGla.tot$tempjja_ts_30C[!duplicated(SalGla.tot$plotgroup.NUM)], # one value per tXpg
  # tempmax.tot = SalGla.tot$tempmax_ts_30C[!duplicated(SalGla.tot$plotgroup.NUM)],
  # tempmin.tot = SalGla.tot$tempmin_ts_30C[!duplicated(SalGla.tot$plotgroup.NUM)],
  tempcont.tot = SalGla.tot$tempcont_ts_30C[!duplicated(SalGla.tot$plotgroup.NUM)],
  precipjja.tot = SalGla.tot$precipjja_ts_30C[!duplicated(SalGla.tot$plotgroup.NUM)],
  # precipjfmam.tot = SalGla.tot$precipjfmam_ts_30C[!duplicated(SalGla.tot$plotgroup.NUM)]
  N_plotgroups = length(unique(SalGla.tot$site_alt_plotgroup_id))
  
  # # site/alt level predictors
  # alt.tot = SalGla.tot$altC[!duplicated(SalGla.tot$site_alt.NUM)],
  # N_isoclines = length(unique(SalGla.tot$site_alt_id))
)
str(shrub_gradient_jags.SalGla.data)

# * VacUli ----
shrub_gradient_jags.VacUli.data <- list(
  
  # plot level predictors, for discrete...
  cov.dis = VacUli.dis$cover,
  plotgroup.dis = VacUli.dis$plotgroup.NUM, #AB added this
  # isocline.dis = VacUli.dis$site_alt.NUM,
  # inclin_down.dis = VacUli.dis$inclin_downC,
  sri.dis = VacUli.dis$sriC,
  tri.dis = VacUli.dis$triC,
  twi_90m.dis = VacUli.dis$twi_90mC,
  compet.dis = VacUli.dis$competC,
  N_discrete = nrow(VacUli.dis),
  
  # ...and continuous part of the data
  cov.cont = VacUli.cont$cover,
  plotgroup.cont = VacUli.cont$plotgroup.NUM, #AB added this
  # isocline.cont = VacUli.cont$site_alt.NUM,
  # inclin_down.cont = VacUli.cont$inclin_downC,
  sri.cont = VacUli.cont$sriC,
  tri.cont = VacUli.cont$triC,
  twi_90m.cont = VacUli.cont$twi_90mC,
  compet.cont = VacUli.cont$competC,
  N_cont = nrow(VacUli.cont),
  
  # plot group level predictors
  tempjja.tot = VacUli.tot$tempjja_ts_30C[!duplicated(VacUli.tot$plotgroup.NUM)], # one value per tXpg
  # tempmax.tot = VacUli.tot$tempmax_ts_30C[!duplicated(VacUli.tot$plotgroup.NUM)],
  # tempmin.tot = VacUli.tot$tempmin_ts_30C[!duplicated(VacUli.tot$plotgroup.NUM)],
  tempcont.tot = VacUli.tot$tempcont_ts_30C[!duplicated(VacUli.tot$plotgroup.NUM)],
  precipjja.tot = VacUli.tot$precipjja_ts_30C[!duplicated(VacUli.tot$plotgroup.NUM)],
  # precipjfmam.tot = VacUli.tot$precipjfmam_ts_30C[!duplicated(VacUli.tot$plotgroup.NUM)]
  N_plotgroups = length(unique(VacUli.tot$site_alt_plotgroup_id))
  
  # # site/alt level predictors
  # alt.tot = VacUli.tot$altC[!duplicated(VacUli.tot$site_alt.NUM)],
  # N_isoclines = length(unique(VacUli.tot$site_alt_id))
)
str(shrub_gradient_jags.VacUli.data)

# JAGS model #### 
# adapted from Bayesian course: rain -> env = target variable,
                              # biome -> species, 
                              # continent -> site, 
                              # cell -> plot group

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
      b.tempcont.x2 ~ dnorm(0, 0.001)
      b.precipjja.x ~ dnorm(0, 0.001)
      b.precipjja.x2 ~ dnorm(0, 0.001)
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
                    b.tempjja.x2 * (tempjja.tot[k]^2) + 
                    # b.tempmax.x * tempmax.tot[k] + 
                    # b.tempmax.x2 * (tempmax.tot[k]^2) +
                    # b.tempmin.x * tempmin.tot[k] + 
                    # b.tempmin.x2 * (tempmin.tot[k]^2) +
                    b.tempcont.x * tempcont.tot[k] + 
                    b.tempcont.x2 * (tempcont.tot[k]^2) +
                    b.precipjja.x * precipjja.tot[k] + 
                    b.precipjja.x2 * (precipjja.tot[k]^2) # +
                    # b.precipjfmam.x * precipjfmam.tot[k] + 
                    # b.precipjfmam.x2 * (precipjfmam.tot[k]^2)
      }
      
      
      # for (l in 1:N_isoclines){ #length of total isoclines
      #   b_isocline[l] ~ dnorm(mu.isocline[l],tau.isocline)
      #   mu.isocline[l] <- intercept + 
      #   
      #               # isocline-level predictor
      #               b.alt.x * alt.tot[l]
      # }

    
      }
  ", file.path("..", "..", "models", "shrub_gradient.spec.jags"))

# Initial values ###
# not strictly necessary here (?)

# Parameters to monitor ####

params <- c("intercept",
            # "b.alt.x",
            "b.tempjja.x", "b.tempjja.x2",
            # "b.tempmax.x", "b.tempmax.x2",
            # "b.tempmin.x", "b.tempmin.x2",
            "b.tempcont.x", "b.tempcont.x2",
            "b.precipjja.x", "b.precipjja.x2",
            # "b.precipjfmam.x", "b.precipjfmam.x2",
            "b.compet", 
            # "b.inclin_down", 
            "b.sri",
            "b.tri",
            "b.twi_90m",
            "b_plotgroup[1]","b_plotgroup[2]","b_plotgroup[3]","b_plotgroup[63]",
            # "b_isocline[1]","b_isocline[2]","b_isocline[21]",
            "sigma.plotgroup","phi")
# "a.env.x","a.env.x2",

# 5) Run models ----
 # WRITE FUNCTION TO RUN THIS WHOLE SECTION FOR ALL SPECIES
# * BetNan ----
model_out.shrub_gradient.BetNan <- jags(shrub_gradient_jags.BetNan.data,    # input data
                                        inits = NULL,                       # JAGS to create initial values
                                        params,                             # parameters to be saved
                                        model.file = file.path("..", "..", "models", "shrub_gradient.spec.jags"), 
                                        n.chains = 3,                       # no. Markov chains
                                        n.iter = 50000, n.burnin = 30000,     # no. iterations & burn-in fraction per chain
                                        n.thin = 2,                         # thinning rate
                                        DIC = FALSE,                        # do not compute deviance, pD, and DIC
                                        working.directory = NULL, 
                                        progress.bar = "text") 

# plot(model_out.shrub_gradient.BetNan) #check convergence, etc.

# * EmpNig ----
model_out.shrub_gradient.EmpNig <- jags(shrub_gradient_jags.EmpNig.data,    # input data
                                        inits = NULL,                       # JAGS to create initial values
                                        params,                             # parameters to be saved
                                        model.file = file.path("..", "..", "models", "shrub_gradient.spec.jags"), 
                                        n.chains = 3,                       # no. Markov chains
                                        n.iter = 50000, n.burnin = 30000,     # no. iterations & burn-in fraction per chain
                                        n.thin = 2,                         # thinning rate
                                        DIC = FALSE,                        # do not compute deviance, pD, and DIC
                                        working.directory = NULL, 
                                        progress.bar = "text") 

# plot(model_out.shrub_gradient.EmpNig) #check convergence, etc.

# * RhoGro ----
model_out.shrub_gradient.RhoGro <- jags(shrub_gradient_jags.RhoGro.data,    # input data
                                        inits = NULL,                       # JAGS to create initial values
                                        params,                             # parameters to be saved
                                        model.file = file.path("..", "..", "models", "shrub_gradient.spec.jags"), 
                                        n.chains = 3,                       # no. Markov chains
                                        n.iter = 50000, n.burnin = 30000,     # no. iterations & burn-in fraction per chain
                                        n.thin = 2,                         # thinning rate
                                        DIC = FALSE,                        # do not compute deviance, pD, and DIC
                                        working.directory = NULL, 
                                        progress.bar = "text") 

# plot(model_out.shrub_gradient.RhoGro) #check convergence, etc.

# * SalGla ----
model_out.shrub_gradient.SalGla <- jags(shrub_gradient_jags.SalGla.data,    # input data
                                        inits = NULL,                       # JAGS to create initial values
                                        params,                             # parameters to be saved
                                        model.file = file.path("..", "..", "models", "shrub_gradient.spec.jags"), 
                                        n.chains = 3,                       # no. Markov chains
                                        n.iter = 50000, n.burnin = 30000,     # no. iterations & burn-in fraction per chain
                                        n.thin = 2,                         # thinning rate
                                        DIC = FALSE,                        # do not compute deviance, pD, and DIC
                                        working.directory = NULL, 
                                        progress.bar = "text") 

# plot(model_out.shrub_gradient.SalGla) #check convergence, etc.

# * VacUli ----
model_out.shrub_gradient.VacUli <- jags(shrub_gradient_jags.VacUli.data,    # input data
                                        inits = NULL,                       # JAGS to create initial values
                                        params,                             # parameters to be saved
                                        model.file = file.path("..", "..", "models", "shrub_gradient.spec.jags"), 
                                        n.chains = 3,                       # no. Markov chains
                                        n.iter = 50000, n.burnin = 30000,     # no. iterations & burn-in fraction per chain
                                        n.thin = 2,                         # thinning rate
                                        DIC = FALSE,                        # do not compute deviance, pD, and DIC
                                        working.directory = NULL, 
                                        progress.bar = "text") 

# plot(model_out.shrub_gradient.VacUli) #check convergence, etc.


# 6) Evaluation ----
# * BetNan ----
# extract coefficients - Betula nana 
coeff.shrub_gradient.BetNan <- model_out.shrub_gradient.BetNan$BUGSoutput$summary %>% 
  as.data.frame %>% 
  select('mean','sd','2.5%','97.5%','Rhat') %>% 
# add identifying info to data frame
  rownames_to_column(var = "param")

# add 90% CIs
ci_90 <- data.frame(q5 = NA, q95 = NA, param = NA)
for (param in 1:(length(model_out.shrub_gradient.BetNan$BUGSoutput$sims.list)-4)){
  ci_90[param,1:2] <- quantile(data.frame(model_out.shrub_gradient.BetNan$BUGSoutput$sims.list[param])[,1], probs = c(0.05, 0.95))
  ci_90[param, 3] <- names(data.frame(model_out.shrub_gradient.BetNan$BUGSoutput$sims.list))[param]
}

# join to coefficients table
coeff.shrub_gradient.BetNan <- coeff.shrub_gradient.BetNan %>% 
  left_join(ci_90, by = "param") %>% 
  # reorder and rename cols
  select(param, mean, sd, 
         l95 = "2.5%",
         l90 = q5,
         u90 = q95,
         u95 = "97.5%",
         Rhat) #%>% print

model_plot_function(coeff.shrub_gradient.BetNan)

# * EmpNig ----
# extract coefficients
coeff.shrub_gradient.EmpNig <- model_out.shrub_gradient.EmpNig$BUGSoutput$summary %>% 
  as.data.frame %>% 
  select('mean','sd','2.5%','97.5%','Rhat') %>% 
  # add identifying info to data frame
  rownames_to_column(var = "param")

# add 90% CIs
ci_90 <- data.frame(q5 = NA, q95 = NA, param = NA)
for (param in 1:(length(model_out.shrub_gradient.EmpNig$BUGSoutput$sims.list)-4)){
  ci_90[param,1:2] <- quantile(data.frame(model_out.shrub_gradient.EmpNig$BUGSoutput$sims.list[param])[,1], probs = c(0.05, 0.95))
  ci_90[param, 3] <- names(data.frame(model_out.shrub_gradient.EmpNig$BUGSoutput$sims.list))[param]
}

# join to coefficients table
coeff.shrub_gradient.EmpNig <- coeff.shrub_gradient.EmpNig %>% 
  left_join(ci_90, by = "param") %>% 
  # reorder and rename cols
  select(param, mean, sd, 
         l95 = "2.5%",
         l90 = q5,
         u90 = q95,
         u95 = "97.5%",
         Rhat) #%>% print

# * RhoGro ----
# extract coefficients
coeff.shrub_gradient.RhoGro <- model_out.shrub_gradient.RhoGro$BUGSoutput$summary %>% 
  as.data.frame %>% 
  select('mean','sd','2.5%','97.5%','Rhat') %>% 
  # add identifying info to data frame
  rownames_to_column(var = "param")

# add 90% CIs
ci_90 <- data.frame(q5 = NA, q95 = NA, param = NA)
for (param in 1:(length(model_out.shrub_gradient.RhoGro$BUGSoutput$sims.list)-4)){
  ci_90[param,1:2] <- quantile(data.frame(model_out.shrub_gradient.RhoGro$BUGSoutput$sims.list[param])[,1], probs = c(0.05, 0.95))
  ci_90[param, 3] <- names(data.frame(model_out.shrub_gradient.RhoGro$BUGSoutput$sims.list))[param]
}

# join to coefficients table
coeff.shrub_gradient.RhoGro <- coeff.shrub_gradient.RhoGro %>% 
  left_join(ci_90, by = "param") %>% 
  # reorder and rename cols
  select(param, mean, sd, 
         l95 = "2.5%",
         l90 = q5,
         u90 = q95,
         u95 = "97.5%",
         Rhat) #%>% print

# * SalGla ----
# extract coefficients
coeff.shrub_gradient.SalGla <- model_out.shrub_gradient.SalGla$BUGSoutput$summary %>% 
  as.data.frame %>% 
  select('mean','sd','2.5%','97.5%','Rhat') %>% 
  # add identifying info to data frame
  rownames_to_column(var = "param")

# add 90% CIs
ci_90 <- data.frame(q5 = NA, q95 = NA, param = NA)
for (param in 1:(length(model_out.shrub_gradient.SalGla$BUGSoutput$sims.list)-4)){
  ci_90[param,1:2] <- quantile(data.frame(model_out.shrub_gradient.SalGla$BUGSoutput$sims.list[param])[,1], probs = c(0.05, 0.95))
  ci_90[param, 3] <- names(data.frame(model_out.shrub_gradient.SalGla$BUGSoutput$sims.list))[param]
}

# join to coefficients table
coeff.shrub_gradient.SalGla <- coeff.shrub_gradient.SalGla %>% 
  left_join(ci_90, by = "param") %>% 
  # reorder and rename cols
  select(param, mean, sd, 
         l95 = "2.5%",
         l90 = q5,
         u90 = q95,
         u95 = "97.5%",
         Rhat) #%>% print

# * VacUli ----
# extract coefficients
coeff.shrub_gradient.VacUli <- model_out.shrub_gradient.VacUli$BUGSoutput$summary %>% 
  as.data.frame %>% 
  select('mean','sd','2.5%','97.5%','Rhat') %>% 
  # add identifying info to data frame
  rownames_to_column(var = "param")

# add 90% CIs
ci_90 <- data.frame(q5 = NA, q95 = NA, param = NA)
for (param in 1:(length(model_out.shrub_gradient.VacUli$BUGSoutput$sims.list)-4)){
  ci_90[param,1:2] <- quantile(data.frame(model_out.shrub_gradient.VacUli$BUGSoutput$sims.list[param])[,1], probs = c(0.05, 0.95))
  ci_90[param, 3] <- names(data.frame(model_out.shrub_gradient.VacUli$BUGSoutput$sims.list))[param]
}

# join to coefficients table
coeff.shrub_gradient.VacUli <- coeff.shrub_gradient.VacUli %>% 
  left_join(ci_90, by = "param") %>% 
  # reorder and rename cols
  select(param, mean, sd, 
         l95 = "2.5%",
         l90 = q5,
         u90 = q95,
         u95 = "97.5%",
         Rhat) #%>% print


# ---
# compare modeled intercepts with raw-data means
aggregate(env_cov_bio_sub$cover,by=list(env_cov_bio_sub$WHAT_TO_INSERT_HERE,env_cov_bio_sub$taxon),FUN=mean)$x
# round(plogis(coeff.shrub_gradient.BetNan[coeff.shrub_gradient.BetNan$param=="intercept","mean"]),3)
coeff.shrub_gradient.BetNan[coeff.shrub_gradient.BetNan$param=="b.compet",]
coeff.shrub_gradient[coeff.shrub_gradient$param=="b1BT",]

plot(aggregate(env_cov_bio_sub$cover,by=list(env_cov_bio_sub$taxon),FUN=mean)$x,round(plogis(coeff.shrub_gradient.BetNan[coeff.shrub_gradient.BetNan$param=="intercept","mean"]),4))

