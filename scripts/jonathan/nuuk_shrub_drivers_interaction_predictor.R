# computation of interaction metric

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

# species data (raw pinpoint data) [from Jacob Nabe-Nielsen]
spec_nuuk <- read.csv(file.path("data", "input_data", "Nuuk plant data 150201 - Pin-point data - stacked.csv")) %>% 
  as_tibble

nrow(spec_nuuk) / (19*25)

spec_nuuk %>% 
  
  group_by(plot) %>% 
  
  summarise(nhits = sum(presence))

# average height per species per plot
focal_species <- c("Betula nana",
                   "Cassiope tetragona",
                   "Empetrum nigrum",
                   "Phyllodoce coerulea",
                   "Ledum groenlandicum",
                   "Ledum palustre",
                   "Salix arctophila",
                   "Salix glauca",
                   "Vaccinium uliginosum")

# calculate average and median height at pins with focal species present
avg_height <- spec_nuuk %>% 
  filter(presence == 1 & taxon %in% focal_species) %>% 
  mutate(max.h = as.numeric(max.h),
         taxon = as.factor(taxon)) %>% 
  group_by(taxon) %>% 
  summarise(median_height = median(max.h, na.rm = T),
            avg_height = mean(max.h, na.rm = T))
  # >> great deviation from panarctic medians we previously used for height ranks!

# how many cases (that include focal species) where height measurements cannot be attributed to certain species?
multiple_hits <- spec_nuuk %>% 
  filter(presence == 1 & taxon %in% focal_species) %>% 
  mutate(max.h = as.numeric(max.h),
         taxon = as.factor(taxon)) %>% 
  group_by(plot, pin.no) %>% 
  summarise(no_hits = sum(presence, na.rm = T)) %>% 
  filter(no_hits > 1) %>% ungroup() %>% 
  group_by(no_hits) %>% 
  summarise(n_cases = n())
  # >> total 603 cases (561 w/ 2 possibilities, 41 w/ 3, and 1 w/ 4)

# >>> use community-weighted mean difference in acquisitiveness to focal species instead

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
  
  # weight acquisitiveness score by relative abundance
  mutate(acquis_rel_spec = acquisitiveness * abundance_rel) %>% 
  
  # replace zero values (= species not present) with NAs
  mutate(acquis_rel_spec = case_when(acquis_rel_spec == 0 ~ NA_real_,
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
    
    # replace zero values (= none or no other species than focal species present) with NAs
    mutate(acquis_community = case_when(acquis_community == 0 ~ NA_real_,
                                  TRUE ~ acquis_community)) %>% 
    
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
  mutate(acquis_diff = acquis_rel_spec - acquis_community)

  