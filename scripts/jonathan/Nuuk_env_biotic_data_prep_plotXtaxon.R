#####################################################################################################################################################
# Drivers of shrub abundance across the Nuuk Fjord inland gradient
# Code to extract data per species and for each scale
#   and to calculate cover per plot
#                                                                                                                                                   #
# by Jonathan von Oppen, Aarhus University, May 2020                                                                                                #
# based on code by Anne Blach Overgaard (September/October 2015)                                                                                    #
#                                                                                                                                                   #
#####################################################################################################################################################
#' TO DO 
#' - 
#
#####################################################################################################################################################

rm(list = ls())

### 0) Preamble ----
### >> Dependencies ----
if(!require(pacman)){       # provides p_load() as wrapper for require() and library()
  install.packages("pacman")
  library(pacman)
}
pacman::p_load(tidyverse,   # for multiple data wrangling packages
               tidylog,     # to log operations in pipes
               stringr,     # for string wrangling
               skimr)       # to conveniently skim & summarise data frames


### 1) Data import ----
# # set path to files in gl_microclim project folder
# Nuuk_data_path <- file.path("../", "../", "../" , "gl_microclim")

# environmental data (plot level) [compiled by Nathalie Chardon]:
load(file.path("data", "ts_plot.RData"))
env_pred_nuuk <- ts_plot

# former env data at #read.csv("I:/C_Write/_User/JonathanVonOppen_au630524/Project/A_NuukFjord_shrub_abundance_controls/aa_Godthaabsfjord/Data/PlotSpecies/Processed/godthaabsfjord_plots_fusion_table_with_pred_05102015.csv")

# species data (raw pinpoint data) [from Jacob Nabe-Nielsen]
spec_nuuk <- read.csv(file.path("data", "Nuuk plant data 150201 - Pin-point data - stacked.csv")) %>% 
  as_tibble

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

spec_nuuk %>% group_by(taxon) %>% summarise(sum = sum(presence)) %>% arrange(desc(sum))

# dat <- spec_nuuk %>% group_by(taxon, site) %>% summarise(sum = sum(presence)) %>% print
# 
# # Generate species list
# spp <- unique(spec_nuuk$taxon)
# # create dataframe with info of presence sum per species per site
# spp.data <- unique(subset(dat, select=site))
# for (m in 1:length(spp)){
#   dat.sub <- subset(dat, taxon == spp[m], select = sum)  
#   names(dat.sub) <- spp[m]
#   spp.data <- data.frame(spp.data,dat.sub)
# }
# setwd("I:\\C_Write\\JonathanVonOppen\\aa_Godthaabsfjord\\Analyses\\Data.analyses\\")
# write.csv(spp.data, "species.site.presences.csv", row.names = FALSE) 
#####################################################################################################################################################
## Create common columns in the "env" and "spec" objects to be able to summarise the spp data for all three scales                                                                                                           
## Create data set at plot scale - extract species occurrences - selec variables based on correlation test 
#####################################################################################################################################################

# Generate unique identifiers on plot, plot group, altitude levels: ----
# Generate a plot/site specific ID (site_plot_id) in the "spec" and "env" tables
spec_nuuk <- spec_nuuk %>% 
  mutate(site_plot_id = paste(site, plot, sep="_"))

env_pred_nuuk <- env_pred_nuuk %>% 
  rename(site_plot_id = id)

# Generate a plot group/site specific ID (site_alt_plotgroup_id) in the "env_pred_nuuk" and "spec_nuuk" tables
env_pred_nuuk <- env_pred_nuuk %>% 
  # create plot group number (3 x 6 within any isocline)
  mutate(plotgroup = rep(c(rep(1, 6), rep(2, 6), rep(3, 6)), 
                             nrow(env_pred_nuuk) / 18)) %>% 
  # create unique identifier of site_alt_plotgroup
  mutate(site_alt_plotgroup_id = paste(site, alt, plotgroup, sep="_"))


spec_nuuk <- spec_nuuk %>% 
  # Every plot has 19 spp x 25 pins and we want it repeated per 6 plots (19*25*6 = 2850) for each isocline (3 each) (3*2850=)
  mutate(plotgroup = rep(c(rep(1, 2850), rep(2, 2850), rep(3, 2850)), 
                          nrow(spec_nuuk) / 8550)) %>% 
  
  mutate(site_alt_plotgroup_id = NA)

plotname <- unique(spec_nuuk$site_plot_id)
for (i in 1:length(plotname)){
  env_pred_nuuk.sub <- subset(env_pred_nuuk, env_pred_nuuk$site_plot_id == plotname[i])
  spec_nuuk$site_alt_plotgroup_id[spec_nuuk$site_plot_id == plotname[i]] <- env_pred_nuuk.sub$site_alt_plotgroup_id
}

# Generate an isocline group/site specific ID (site_alt_id)
env_pred_nuuk$site_alt_id <- paste(env_pred_nuuk$site, env_pred_nuuk$alt, sep="_")

spec_nuuk$site_alt_id <- NA
for (j in 1:length(plotname)){
  env_pred_nuuk.sub.2 <- subset(env_pred_nuuk, env_pred_nuuk$site_plot_id == plotname[j])
  spec_nuuk$site_alt_id[spec_nuuk$site_plot_id == plotname[j]] <- env_pred_nuuk.sub.2$site_alt_id
}

# Calculate height-dependent measure of competition pressure: ----
# Order the tables according to site_plot_id variable - very important for the following loop output
spec_nuuk <- spec_nuuk %>% 
  arrange(site_plot_id) #[order(spec_nuuk[,"site_plot_id"]),]
env_pred_nuuk <- env_pred_nuuk %>% 
  arrange(site_plot_id) # [order(env_pred_nuuk[,"site_plot_id"]),]


# Generate species list
taxon_list <- unique(spec_nuuk$taxon)

# Compute species specific occurrence variables in the "env_pred_nuuk" table
for (i in 1:length(taxon_list)){
  sub <- subset(spec_nuuk, spec_nuuk$taxon == taxon_list[i])
  #Sums the number of times a given species is present at a given plot(site_plot_id)
  col <- paste("occ", "_", gsub(" ", "_", taxon_list[i]), sep = "")
  #col <- taxon_list[i]
  env_pred_nuuk[col] <- as.numeric(tapply(sub$presence, sub$site_plot_id, sum))
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


env_pred_nuuk.bio <- mutate(env_pred_nuuk, occ_graminoids = occ_Juncaceae + occ_Cyperaceae + occ_Poaceae,
                  led.gro.bio = occ_Ledum_palustre,
                  sal.gla.bio = occ_Ledum_palustre + occ_Ledum_groenlandicum,
                  bet.nan.bio = occ_Ledum_palustre + occ_Ledum_groenlandicum + occ_Salix_glauca,
                  vac.uli.bio = occ_Ledum_palustre + occ_Ledum_groenlandicum + occ_Salix_glauca + occ_Betula_nana,
                  emp.nig.bio = occ_Ledum_palustre + occ_Ledum_groenlandicum + occ_Salix_glauca + occ_Betula_nana + occ_Vaccinium_uliginosum,
                  sal.arc.bio = occ_Ledum_palustre + occ_Ledum_groenlandicum + occ_Salix_glauca + occ_Betula_nana + occ_Vaccinium_uliginosum + occ_Empetrum_nigrum,
                  phy.coe.bio = occ_Ledum_palustre + occ_Ledum_groenlandicum + occ_Salix_glauca + occ_Betula_nana + occ_Vaccinium_uliginosum + occ_Empetrum_nigrum + occ_Salix_arctophila,
                  cas.tet.bio = occ_Ledum_palustre + occ_Ledum_groenlandicum + occ_Salix_glauca + occ_Betula_nana + occ_Vaccinium_uliginosum + occ_Empetrum_nigrum + occ_Salix_arctophila + occ_Phyllodoce_coerulea)

env_pred_nuuk.bio <- as.data.frame(env_pred_nuuk.bio)


## !!!! EXECUTE ONE OF THE NEXT TWO CODE CHUNKS - COMMENT THE OTHER ONE OUT !!!! ## 

# Calculate abundance measure (IF USING COVER PER PLOT GROUP): ----
# compute "cover" (= rel. no. hits per plot group)
# env %>% group_by(plot.group.name) %>% summarise_each("mean") %>% View()
occ_cols <- env_pred_nuuk.bio %>% select(starts_with("occ")) %>% colnames()
env_cov <- env_pred_nuuk.bio %>% group_by(site_alt_plotgroup_id) %>%
  # calculate mean of numeric (plot-level) variables, take 1st entry of categorical (plot group-level) variables
  summarise_each(funs(if(is.numeric(.)) mean(.) else first(.))) %>%
  mutate_at(occ_cols, funs(cov = ./25)) %>%   # cover = n_hits per 25 pins
  rename_at(vars(ends_with("cov")), funs(str_replace(.,"occ","cov"))) %>%
  rename_at(vars(ends_with("cov")), funs(str_remove(.,"_cov"))) %>% 
  # discard ID variables below plotgroup level that have lost information value after averaging over plot groups
  select(-c(plot, site_plot_id)) %>% 
  # %>% View()

# # Calculate abundance measure (IF USING COVER PER PLOT): ----
# # compute "cover" (= rel. no. hits per plot)
# occ_cols <- env_pred_nuuk.bio %>% select(starts_with("occ")) %>% colnames()
# env_cov <- env_pred_nuuk.bio %>% 
#   mutate_at(occ_cols, funs(cov = ./25)) %>%   # cover = n_hits per 25 pins
#   rename_at(vars(ends_with("cov")), funs(str_replace(.,"occ","cov"))) %>%
#   rename_at(vars(ends_with("cov")), funs(str_remove(.,"_cov"))) %>% 
# # %>% View()

# Make the variable site into a factor to be used as a random factor
  mutate(site = as.factor(site)) %>% 
  as.data.frame %>% 
  droplevels()

# Transform to long format with one observation per species per plot (group): ----
  # for cover values:
env_cov_long_cov <- env_cov %>% select(-c(starts_with("occ"), ends_with("bio"))) %>% 
  pivot_longer(cols = starts_with("cov_"), 
               names_to = "taxon", 
               values_to = "cover", 
               values_drop_na = FALSE) %>% 
  # remove "occ_" & "_" from taxon
  mutate(taxon = str_remove(taxon, "cov_")) %>% 
  mutate(taxon = str_replace(taxon, "_", " ")) %>% 
  mutate(taxon = factor(taxon))

  # for competition values:
env_cov_long_bio <- env_cov %>% select(site_alt_plotgroup_id, ends_with("bio")) %>% # for PLOT level, change to [...] select(site_plot_id, [...])
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
                        "cas.tet" = "Cassiope tetragona")) %>% 
  mutate(taxon = factor(taxon))

# merge both long dataframes, insert NAs for taxa w/o compet values
env_cov_long <- left_join(env_cov_long_cov, env_cov_long_bio, 
                          by = c("site_alt_plotgroup_id", "taxon")) %>%   # for PLOT GROUP level, change to [...] c("site_plot_id", [...])
  mutate(taxon = factor(taxon)) %>% 
  # correct species names
  mutate(taxon = recode(taxon, "Phyllodoce coerulea" = "Phyllodoce caerulea",
                        "Ledum groenlandicum" = "Rhododendron groenlandicum", 
                        "Ledum palustre" = "Rhododendron tomentosum")) %>% 
  
# insert value 0 for competitive pressure for Ledum palustre aka Rhododendron tomentosum (= tallest-growing species)
  mutate(compet = ifelse(taxon == "Rhododendron tomentosum", 0, compet))

# filter dataset to only include species we have competition values for: ----
spp_compet <- levels(droplevels(env_cov_long$taxon[!is.na(env_cov_long$compet)]))
env_cov_long_spp_compet <- env_cov_long %>% 
  filter(taxon %in% spp_compet) %>% 
  droplevels()

# Write new table: ----
# >> for plot group level: ----
write_csv(env_cov_long_spp_compet, path = "I:/C_Write/_User/JonathanVonOppen_au630524/Project/A_NuukFjord_shrub_abundance_controls/aa_Godthaabsfjord/Data/PlotSpecies/Processed/nuuk_env_cover_plotgroups.csv")
write_csv(env_cov_long_spp_compet, path = file.path("data", "nuuk_env_cover_plotgroups.csv"))

# # >> for plot level: ----
# write_csv(env_cov_long_spp_compet, path = "I:/C_Write/_User/JonathanVonOppen_au630524/Project/A_NuukFjord_shrub_abundance_controls/aa_Godthaabsfjord/Data/PlotSpecies/Processed/nuuk_env_cover_plots.csv")
# write_csv(env_cov_long_spp_compet, path = file.path("data", "nuuk_env_cover_plots.csv"))
