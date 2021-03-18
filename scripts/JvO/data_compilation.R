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
               skimr,       # to conveniently skim & summarise data frames
               scales)      # for scaling variables to specified range

#_______________________________________________________________________####

### 1) Data import ----

# environmental data (plot level) [compiled by Nathalie Chardon]:
load(file.path("data", "input_data", "climate_sri_data.RData"))
env_pred_nuuk <- ts_plot %>% 
  
  # select variables needed
  select(id, site, alt, plot,                         # site/alt/plot IDs
         long, lat, year,                             # WGS84 coordinates, year of sampling
         starts_with("tempjja_"),                     # summer mean temperatures
         starts_with("tempmax_"),                     # average yearly JJA max temperature
         starts_with("tempmin_"),                     # average yearly JFMA min temperature
         starts_with("tempcont_"),                    # temp. continentality = average yearly amplitude (tempmax - tempmin)
         starts_with("precipjja_"),                   # average yearly cumulative summer (JJA) precipitation
         starts_with("precipjfmam_"),                 # average yearly cumulative winter-spring (JFMAM) precipitation
         starts_with("precipmam_"),                   # average yearly cumulative spring (MAM) precipitation
         inclin_down, inclin_dir,                     # terrain variables: slope, aspect
         sri,                                         # solar radiation
         ndvi)                                        # productivity

# species data (raw pinpoint data) [from Jacob Nabe-Nielsen]
spec_nuuk <- read.csv(file.path("data", "input_data", "plant_pinpoint_data.csv")) %>% 
  as_tibble

#_______________________________________________________________________####

# 2) Data exploration ----
# >> extract minimum and maximum lat & long values ----
(coord_minmax <- env_pred_nuuk %>% select(lat, long) %>% summarise_all(funs(min = min, max = max)))

# convert to degree format
library("OSMscale")
degree(lat = coord_minmax[1,1], long = coord_minmax[1,2], todms = TRUE) # for minimum values
degree(lat = coord_minmax[1,3], long = coord_minmax[1,4], todms = TRUE) # for maximum values

# >> extract total abundance by taxon ----
spec_nuuk %>% group_by(taxon) %>% summarise(sum = sum(presence)) %>% arrange(desc(sum))

# >> calculate each taxon's abundance per site ----
dat <- spec_nuuk %>% group_by(taxon, site) %>% summarise(sum = sum(presence)) %>% print

# Generate species list
spp <- unique(spec_nuuk$taxon)

# create dataframe with info of presence sum per species per site
spp.data <- unique(subset(dat, select=site))
for (m in 1:length(spp)){
  dat.sub <- subset(dat, taxon == spp[m], select = sum)
  names(dat.sub) <- spp[m]
  spp.data <- data.frame(spp.data,dat.sub)
}

#_______________________________________________________________________####

# 3) Data compilation ----

# >> generate unique identifiers on plot, plot group, altitude levels: ----

# Generate a plot/site specific ID (site_plot_id) in the "spec" and "env" tables
spec_nuuk <- spec_nuuk %>% 
  mutate(site_plot_id = paste(site, plot, sep="_"))

env_pred_nuuk <- env_pred_nuuk %>% 
  rename(site_plot_id = id) %>% 

# Generate a plot group/site specific ID (site_alt_plotgroup_id) in the "env_pred_nuuk" and "spec_nuuk" tables
  
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

# >> calculate abundance measure per plot ----

# order the tables according to site_plot_id variable - very important for the following loop output
spec_nuuk <- spec_nuuk %>% 
  arrange(site_plot_id) 
env_pred_nuuk <- env_pred_nuuk %>% 
  arrange(site_plot_id) 

# generate species list
taxon_list <- unique(spec_nuuk$taxon)

# compute species-specific occurrence variables in the "env_pred_nuuk" table
for (i in 1:length(taxon_list)){
  sub <- subset(spec_nuuk, spec_nuuk$taxon == taxon_list[i])
  col <- paste("occ", "_", gsub(" ", "_", taxon_list[i]), sep = "")
  # sum the number of times a given species is present at a given plot(site_plot_id)
  env_pred_nuuk[col] <- as.numeric(tapply(sub$presence, sub$site_plot_id, sum))
}

# compute abundance ("cover", = rel. no. hits per plot)
occ_cols <- env_pred_nuuk %>% select(starts_with("occ")) %>% colnames()
env_cov <- env_pred_nuuk %>%
  mutate_at(occ_cols, funs(cov = ./25)) %>%   # cover = n_hits per 25 pins
  rename_at(vars(ends_with("cov")), funs(str_replace(.,"occ","cov"))) %>%
  rename_at(vars(ends_with("cov")), funs(str_remove(.,"_cov"))) %>%
# %>% View()

# make the variable site into a factor to be used as a random factor
  mutate(site = as.factor(site)) %>% 
  
# create columns for total, deciduous & evergreen shrub cover from focal species covers
  mutate(cov_All_shrubs = cov_Betula_nana + cov_Cassiope_tetragona + cov_Empetrum_nigrum + cov_Phyllodoce_coerulea +
                      cov_Ledum_groenlandicum + cov_Ledum_palustre + cov_Salix_arctophila + cov_Salix_glauca + cov_Vaccinium_uliginosum,
         
         cov_All_deciduous = cov_Betula_nana + cov_Salix_arctophila + cov_Salix_glauca + cov_Vaccinium_uliginosum,
         
         cov_All_evergreens = cov_Cassiope_tetragona + cov_Empetrum_nigrum + cov_Phyllodoce_coerulea + cov_Ledum_groenlandicum + cov_Ledum_palustre,
         
# create graminoid cover column from Cyperaceae, Poaceae, Juncaceae cover
         graminoid_cover = cov_Juncaceae + cov_Cyperaceae + cov_Poaceae) %>% 
  
# copy all shrub cover column into predictor column
  mutate(shrub_cover = cov_All_shrubs,
         BetNan_shrub_cover = cov_All_shrubs - cov_Betula_nana,
         CasTet_shrub_cover = cov_All_shrubs - cov_Cassiope_tetragona,
         EmpNig_shrub_cover = cov_All_shrubs - cov_Empetrum_nigrum,
         PhyCae_shrub_cover = cov_All_shrubs - cov_Phyllodoce_coerulea,
         RhoGro_shrub_cover = cov_All_shrubs - cov_Ledum_groenlandicum,
         RhoTom_shrub_cover = cov_All_shrubs - cov_Ledum_palustre,
         SalArc_shrub_cover = cov_All_shrubs - cov_Salix_arctophila,
         SalGla_shrub_cover = cov_All_shrubs - cov_Salix_glauca,
         VacUli_shrub_cover = cov_All_shrubs - cov_Vaccinium_uliginosum) %>% 
  
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

# >> calculate acquisitiveness difference to community-weighted mean difference for each focal species ----

# load traits scores
traits_scores_nuuk <- read.csv(file = file.path("data", "processed", "nuuk_traits_PCAscores_cleaned.csv"),
                               header = T)

# compile focal shrub species (as in original species data)
focal_species <- c("Betula nana",
                   "Cassiope tetragona",
                   "Empetrum nigrum",
                   "Phyllodoce coerulea",
                   "Ledum groenlandicum",
                   "Ledum palustre",
                   "Salix arctophila",
                   "Salix glauca",
                   "Vaccinium uliginosum")

# calculate relative abundances and weight acquisitiveness
spec_acquis_rel <- spec_nuuk %>% 
  
  # filter for focal species
  filter(taxon %in% focal_species) %>% 
  
  # group by taxon and plot
  group_by(plot, taxon) %>% 
  
  # calculate abundance per taxon per plot
  summarise(abundance_rel = sum(presence, na.rm = TRUE) / 25) %>% 
  ungroup() %>% 
  
  # create new taxon column matching names in trait data
  mutate(taxon_traits = case_when(taxon %in% c("Ledum palustre", "Ledum groenlandicum") ~ "Rhododendron sp.",
                                  taxon %in% c("Salix glauca", "Salix arctophila") ~ "Salix sp.",
                                  taxon == "Phyllodoce coerulea" ~ "Phyllodoce caerulea",
                                  TRUE ~ taxon)) %>% 
  
  # join with acquisitiveness PC score
  left_join(traits_scores_nuuk %>% select(PC1, species),
            by = c("taxon_traits" = "species")) %>% 
  rename(acquisitiveness = PC1) %>% 
  
  # scale acquisitiveness score (so far on the arbitrary scale from -3.6 to -0.9)
  mutate(acquis_scale = rescale(acquisitiveness, to = c(0, 1))) %>% 
  
  # weight acquisitiveness score by relative abundance
  mutate(acquis_rel_spec = acquis_scale * abundance_rel) %>% 
  
  # replace zero values (= species not present) with NAs in absolute and weighted acquisitiveness
  mutate(acquis_scale = case_when(abundance_rel == 0 ~ NA_real_,
                                  TRUE ~ acquis_scale),
         acquis_rel_spec = case_when(abundance_rel == 0 ~ NA_real_,
                                     TRUE ~ acquis_rel_spec))

# loop over focal species to calculate specific community-weighted means

focal_taxa_traits <- c("Betula nana", "Cassiope tetragona", "Empetrum nigrum", "Phyllodoce caerulea", "Rhododendron sp.", "Salix sp.", "Vaccinium uliginosum")
community_acquis <- tibble()
for (focal_taxon_id in 1:length(focal_taxa_traits)) {
  community_acquis_spec <- spec_acquis_rel %>% 
    
    filter(!(taxon_traits == focal_taxa_traits[focal_taxon_id])) %>% 
    
    group_by(plot) %>% 
    
    # calculate mean acquisitiveness score of community species
    summarise(acquis_community = sum(acquis_rel_spec, na.rm = T)) %>% ungroup %>% 
    
    # create new column with focal taxon
    mutate(taxon_focal = focal_taxa_traits[focal_taxon_id])
  
  community_acquis <- bind_rows(community_acquis, community_acquis_spec)
}

# calculate species-specific differences in acquisitiveness to community

spec_dist_acquis <- spec_acquis_rel %>% 
  
  # join community values
  left_join(community_acquis, 
            by = c("plot", "taxon_traits" = "taxon_focal")) %>% 
  
  # calculate differences in acquisitiveness
  mutate(acquis_diff = acquis_scale - acquis_community) %>% 
  
  # assign zero difference if none of the shrub species is present in a plot (n = 73)
  group_by(plot) %>% 
  mutate(acquis_diff = case_when(!any(abundance_rel > 0) ~ 0,
                                 TRUE ~ acquis_diff)) %>% 
     
  # select relevant columns
  select(plot,
         taxon, 
         compet = acquis_diff)

# >> looking meaningfully (few values outside {-1, 1} due to cover > 1 (overlapping vegetation layers))
# qplot(spec_dist_acquis$compet)


# merge environmental with acquisitiveness data
env_cov_long <- left_join(env_cov_long_cov, spec_dist_acquis, 
                          by = c("plot", "taxon")) %>%  
  mutate(taxon = factor(taxon)) %>% 
  # correct species names
  mutate(taxon = recode(taxon, 
                        "Phyllodoce coerulea" = "Phyllodoce caerulea",
                        "Ledum groenlandicum" = "Rhododendron groenlandicum", 
                        "Ledum palustre" = "Rhododendron tomentosum")) %>% 
  
# add extracted values for Terrain Ruggedness Index (by Jakob Assmann, for procedure see scripts/JJA/calc_tri_and_twi.bat)
  left_join(read.csv(file.path("data", "processed", "nuuk_env_cover_plots_topo_variables.csv"), 
                     header = T) %>% 
                                  select(plot, 
                                         tri = tri_arctic_dem,        # Terrain Ruggedness Index based on 2m ArcticDEM
                                         twi_fd8 = kopecky_twi,       # Topographic Wetness Index based on 30m GIMP MEaSUREs DEM
                                                                        # and Freeman FD8 flow algorithm (Kopecky et al. 2020 SciTotEnv)
                                         twi_saga = saga_twi,         # TWI based on 30m GIMP MEaSUREs DEM and SAGA GIS flow algorithm
                                         tcws = TCwet_new             # Tasseled-Cap Wetness Index based on original Landsat imagery
                                         ) %>% 
                                  distinct(plot, .keep_all = T),
            by = c("plot")) %>% 
  
# reorder columns & select variables
  select(site_alt_plotgroup_id, site_alt_id, site, alt, plotgroup, plot,  # site/alt/plotgroup/plot IDs
         long, lat, year,                                           # WGS84 coordinates, year of sampling
         starts_with("tempjja_"),                                   # summer mean temperatures
         starts_with("tempmax_"),                                   # average yearly JJA max temperature
         starts_with("tempmin_"),                                   # average yearly JFMA min temperature
         starts_with("tempcont_"),                                  # temp. continentality = average yearly amplitude (tempmax - tempmin)
         starts_with("precipjja_"),                                 # average yearly cumulative summer (JJA) precipitation
         starts_with("precipjfmam_"),                               # average yearly cumulative winter-spring (JFMAM) precipitation
         starts_with("precipmam_"),                                 # average yearly cumulative spring (MAM) precipitation
         inclin_down, inclin_dir, tri,                              # terrain variables
         twi_fd8, twi_saga, tcws,                               # wetness variables
         sri,                                                       # solar radiation
         ndvi, compet,                                              # biotic variables I: productivity, acquisitiveness difference to CWM
         ends_with("_cover"),                                       # biotic variables II: shrub & graminoid cover
         taxon,                                                     # shrub species or func group (all shrubs/deciduous/evergreens)
         cover)                                                     # response variable: relative no. pin hits per plot group

# filter dataset to only include species we have competition values for: ----
env_cov_long_target_groups <- env_cov_long %>% 
  filter(taxon %in% c("Betula nana", "Cassiope tetragona", "Empetrum nigrum", "Phyllodoce caerulea", "Rhododendron groenlandicum", "Rhododendron tomentosum", 
                      "Salix arctophila", "Salix glauca", "Vaccinium uliginosum", "All shrubs", "All deciduous", "All evergreens")) %>% 
  droplevels()

# save dataset ----
write_csv(env_cov_long_target_groups, path = "I:/C_Write/_User/JonathanVonOppen_au630524/Project/A_NuukFjord_shrub_abundance_controls/aa_Godthaabsfjord/Data/PlotSpecies/Processed/nuuk_env_cover_plots.csv")
write_csv(env_cov_long_target_groups, path = file.path("data", "processed", "nuuk_env_cover_plots.csv"))
