#####################################################################################################################################################
# Drivers of shrub abundance across the Nuuk Fjord inland gradient
# Code to extract data per species and for each scale
#   and to calculate cover per plot
#                                                                                                                                                   #
# by Anne Blach Overgaard                                                                                                                           #
# September/October 2015                                                                                                                            #
#                                                                                                                                                   #
# modified & extended by Jonathan von Oppen, November 2019 - January 2020                                                                           #
#                                                                                                                                                   #
#####################################################################################################################################################
##TO DO!! (from former workflow for GLMM analyses)
# identification of variables and correlations perhaps supported by random forest
# build env data for each scale 
# run GLM/GLMER to indentify important variables/relations
#####################################################################################################################################################
rm(list = ls())

# Import data tables: ----
env<-read.csv("I:/C_Write/_User/JonathanVonOppen_au630524/Project/A_NuukFjord_shrub_abundance_controls/aa_Godthaabsfjord/Data/PlotSpecies/Processed/godthaabsfjord_plots_fusion_table_with_pred_05102015.csv")
#env<-read.csv(file.choose())
spec<-read.csv("I:/C_Write/_User/JonathanVonOppen_au630524/Project/A_NuukFjord_shrub_abundance_controls/aa_Godthaabsfjord/Data/PlotSpecies/Original/Nuuk plant data 150201 - Pin-point data - stacked.csv", sep=";")
#spec<-read.csv(file.choose())
#####################################################################################################################################################
## 1. Create data set at plot scale - extract species occurrences and select variables based on correlation test                                                                                                         
## 2. Create data set at plot group scale (1 per 6 plots per isocline) - extract species occurrences, and median values for NDVI and predictors - select variables based on correlation test  
## 3. Create data set at isocline level (3 plot groups per isocline) - extract species occurrences, and the median values for NDVI and the predictors - select variables based on correlation test
#####################################################################################################################################################

# extract minimum and maximum lat & long values ----
# coord_minmax <- env %>% select(lat, long) %>% summarise_all(funs(min = min, max = max)) %>% print()
# # convert to degree format
# library("OSMscale")
# degree(lat = coord_minmax[1,1], long = coord_minmax[1,2], todms = TRUE) # for minimum values
# degree(lat = coord_minmax[1,3], long = coord_minmax[1,4], todms = TRUE) # for maximum values

# Some data checking ----

library(tidyverse)
as_tibble(spec)
spec %>% group_by(taxon) %>% summarise(sum = sum(presence))
dat <- spec %>% group_by(taxon, site) %>% summarise(sum = sum(presence)) %>% as.data.frame %>% as_tibble %>% print

# Generate species list
spp <- unique(spec$taxon)
# create dataframe with info of presence sum per species per site
spp.data <- unique(subset(dat, select=site))
for (m in 1:length(spp)){
  dat.sub <- subset(dat, taxon == spp[m], select = sum)  
  names(dat.sub) <- spp[m]
  spp.data <- data.frame(spp.data,dat.sub)
}
# setwd("I:\\C_Write\\JonathanVonOppen\\aa_Godthaabsfjord\\Analyses\\Data.analyses\\")
# write.csv(spp.data, "species.site.presences.csv", row.names = FALSE) 
#####################################################################################################################################################
## Create common columns in the "env" and "spec" objects to be able to summarise the spp data for all three scales                                                                                                           
## Create data set at plot scale - extract species occurrences - selec variables based on correlation test 
#####################################################################################################################################################

# Generate unique identifiers on plot, plot group, altitude levels: ----
# Generate a plot/site specific ID (plot.name) in the "spec" and "env" tables
spec$plot.name<- paste(spec$site,spec$plot,sep="_")
env$plot.name <- paste(env$site,env$plot,sep="_")

# Generate a plot group/site specific ID (plot.group.name) in the "spec" and "env" tables
env$plot.group<-rep(c(rep(1,6),rep(2,6),rep(3,6)),nrow(env)/18)
env$plot.group.name<-paste(env$site,env$plot.group,env$alt,sep="_")

spec$plot.group<-rep(c(rep(1,2850),rep(2,2850),rep(3,2850)),nrow(spec)/8550) # Every plot has 19 spp x 25 pins and we want it repeated per 6 plots for each isocline (3 each)
spec$plot.group.name<-NA
plotname <- unique(spec$plot.name)
for (i in 1:length(plotname)){
  env.sub <- subset(env, env$plot.name == plotname[i])
  spec$plot.group.name[spec$plot.name == plotname[i]] <- env.sub$plot.group.name
}

# Generate an isocline group/site specific ID (alt.group.name)
env$alt.group.name<-paste(env$site,env$alt,sep="_")

spec$alt.group.name <- NA
for (j in 1:length(plotname)){
  env.sub.2 <- subset(env, env$plot.name == plotname[j])
  spec$alt.group.name[spec$plot.name == plotname[j]] <- env.sub.2$alt.group.name
}

# Calculate height-dependent measure of competition pressure: ----
# Order the tables according to this variable - very important for the following loop output
spec <- spec[order(spec[,"plot.name"]),]
env <- env[order(env[,"plot.name"]),]


# Generate species list
spp <- unique(spec$taxon)

# Compute species specific occurrence variables in the "env" table
for (i in 1:length(spp)){
  sub<-subset(spec,spec$taxon==spp[i])
  #Sums the number of times a given species is present at a given plot(plot.name)
  col <- paste("occ", "_", gsub(" ", "_",spp[i]), sep="")
  #col <- spp[i]
  env[col]<-as.numeric(tapply(sub$presence,sub$plot.name,sum))
}

# Generate biotic predictors per species:

# Species.study         Acc.name                    Median.height   Rank
# Ledum palustre        Rhododendron tomentosum     1.075	            9
# Ledum groenlandicum	  Rhododendron groenlandicum	0.95	            8
# Salix glauca	        Salix glauca	              0.615	            7
# Betula nana	          Betula nana	                0.455825	        6
# Vaccinium uliginosum	Vaccinium uliginosum	      0.3225	          5
# Empetrum nigrum	      Empetrum nigrum	            0.316833334	      4
# Salix arctophila	    Salix arctophila	          0.15	            3
# Phyllodoce coerulea	  Phyllodoce caerulea	        0.1198	          2
# Cassiope tetragona	  Cassiope tetragona	        0.103653333	      1


env.bio <- mutate(env, occ_graminoids = occ_Juncaceae + occ_Cyperaceae + occ_Poaceae,
                  led.gro.bio = occ_Ledum_palustre,
                  sal.gla.bio = occ_Ledum_palustre + occ_Ledum_groenlandicum,
                  bet.nan.bio = occ_Ledum_palustre + occ_Ledum_groenlandicum + occ_Salix_glauca,
                  vac.uli.bio = occ_Ledum_palustre + occ_Ledum_groenlandicum + occ_Salix_glauca + occ_Betula_nana,
                  emp.nig.bio = occ_Ledum_palustre + occ_Ledum_groenlandicum + occ_Salix_glauca + occ_Betula_nana + occ_Vaccinium_uliginosum,
                  sal.arc.bio = occ_Ledum_palustre + occ_Ledum_groenlandicum + occ_Salix_glauca + occ_Betula_nana + occ_Vaccinium_uliginosum + occ_Empetrum_nigrum,
                  phy.coe.bio = occ_Ledum_palustre + occ_Ledum_groenlandicum + occ_Salix_glauca + occ_Betula_nana + occ_Vaccinium_uliginosum + occ_Empetrum_nigrum + occ_Salix_arctophila,
                  cas.tet.bio = occ_Ledum_palustre + occ_Ledum_groenlandicum + occ_Salix_glauca + occ_Betula_nana + occ_Vaccinium_uliginosum + occ_Empetrum_nigrum + occ_Salix_arctophila + occ_Phyllodoce_coerulea)

env <- as.data.frame(env.bio)

# Calculate abundance measure (IF USING COVER PER PLOT GROUP): ----
# # compute "cover" (= rel. no. hits per plot group)
# # env %>% group_by(plot.group.name) %>% summarise_each("mean") %>% View()
# occ_cols <- env %>% select(starts_with("occ")) %>% colnames()
# env_cov <- env %>% group_by(plot.group.name) %>%
#   # calculate mean of numeric (plot-level) variables, take 1st entry of categorical (plot group-level) variables
#   summarise_each(funs(if(is.numeric(.)) mean(.) else first(.))) %>%
#   mutate_at(occ_cols, funs(cov = ./25)) %>%   # cover = n_hits per 25 pins
#   rename_at(vars(ends_with("cov")), funs(str_replace(.,"occ","cov"))) %>%
#   rename_at(vars(ends_with("cov")), funs(str_remove(.,"_cov")))
#   # %>% View()

# Calculate abundance measure (IF USING COVER PER PLOT): ----
# compute "cover" (= rel. no. hits per plot)
occ_cols <- env %>% select(starts_with("occ")) %>% colnames()
env_cov <- env %>% 
  mutate_at(occ_cols, funs(cov = ./25)) %>%   # cover = n_hits per 25 pins
  rename_at(vars(ends_with("cov")), funs(str_replace(.,"occ","cov"))) %>%
  rename_at(vars(ends_with("cov")), funs(str_remove(.,"_cov")))
# %>% View()

# Make the variable site into a factor to be used as a random factor
env_cov$site <- as.factor(env_cov$site)
env_cov <- as.data.frame(env_cov) %>% droplevels()

# Transform to long format with one observation per species per plot (group): ----
  # for cover values:
env_cov_long_cov <- env_cov %>% select(-c(starts_with("occ"), ends_with("bio"))) %>% 
  pivot_longer(cols = starts_with("cov_"), 
               names_to = "taxon", 
               values_to = "cover", 
               values_drop_na = FALSE) %>% 
  # remove "occ_" & "_" from taxon
  mutate(taxon = str_remove(taxon,"cov_")) %>% 
  mutate(taxon = str_replace(taxon,"_"," "))
env_cov_long_cov$taxon <- as.factor(env_cov_long_cov$taxon)

  # for competition values:
env_cov_long_bio <- env_cov %>% select(plot.name, ends_with("bio")) %>% # for PLOT GROUP level, change to [...] select(plot.group.name, [...])
  pivot_longer(cols = ends_with("bio"), 
               names_to = "taxon", 
               values_to = "compet", 
               values_drop_na = FALSE) %>% 
  # rename taxon column entries
  mutate(taxon = str_remove(taxon, ".bio")) %>% 
  mutate(taxon = recode(taxon, 
                        "led.gro" = "Ledum groenlandicum",
                        "sal.gla" = "Salix glauca",
                        "bet.nan" = "Betula nana",
                        "vac.uli" = "Vaccinium uliginosum",
                        "emp.nig" = "Empetrum nigrum",
                        "sal.arc" = "Salix arctophila",
                        "phy.coe" = "Phyllodoce coerulea",
                        "cas.tet" = "Cassiope tetragona"))
env_cov_long_bio$taxon <- as.factor(env_cov_long_bio$taxon)

# merge both long dataframes, insert NAs for taxa w/o compet values
env_cov_long <- left_join(env_cov_long_cov, env_cov_long_bio, 
                          by = c("plot.name","taxon"))  # for PLOT GROUP level, change to [...] c("plot.group.name", [...])
  # correct species names
env_cov_long$taxon <- env_cov_long$taxon %>% 
  recode("Phyllodoce coerulea" = "Phyllodoce caerulea", 
         "Ledum groenlandicum" = "Rhododendron groenlandicum", 
         "Ledum palustre" = "Rhododendron tomentosum")
env_cov_long$taxon <- as.factor(env_cov_long$taxon)

# insert value 0 for competitive pressure for Ledum palustre (= tallest-growing species)
env_cov_long <- env_cov_long %>% 
  mutate(compet = ifelse(taxon == "Rhododendron tomentosum", 0, compet))

# filter dataset to only include species we have competition values for: ----
spp_compet <- levels(droplevels(env_cov_long$taxon[!is.na(env_cov_long$compet)]))
env_cov_long_spp_compet <- env_cov_long %>% 
  filter(taxon %in% spp_compet) %>% 
  droplevels()

# Write new table: ----
# >> for plot group level: ----
# complete set of species
# write_csv(env_cov_long, path = "I:/C_Write/JonathanVonOppen/Project/A_Nuuk_community_competition_controls/aa_Godthaabsfjord/Data/PlotSpecies/Processed/godthaabsfjord_plots_fusion_table_with_pred_spp_rel_cover_compet_per_plot_group.csv")
# only species with competition data
write_csv(env_cov_long_spp_compet, path = "I:/C_Write/_User/JonathanVonOppen_au630524/Project/A_NuukFjord_shrub_abundance_controls/aa_Godthaabsfjord/Data/PlotSpecies/Processed/godthaabsfjord_plots_fusion_table_with_pred_spp_compet_subset_rel_cover_compet_per_plot_group.csv")
# >> for plot level: ----
# complete set of species:
# write_csv(env_cov_long, path = "I:/C_Write/JonathanVonOppen/Project/A_Nuuk_community_competition_controls/aa_Godthaabsfjord/Data/PlotSpecies/Processed/godthaabsfjord_plots_fusion_table_with_pred_spp_rel_cover_compet_per_plot.csv")
# only species with competition data:
write_csv(env_cov_long_spp_compet, path = "I:/C_Write/_User/JonathanVonOppen_au630524/Project/A_NuukFjord_shrub_abundance_controls/aa_Godthaabsfjord/Data/PlotSpecies/Processed/godthaabsfjord_plots_fusion_table_with_pred_spp_compet_subset_rel_cover_compet_per_plot.csv")
