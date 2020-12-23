# Temperature variability, moisture and biotic interactions drive shrub species abundance along a coastal-inland gradient in arctic Greenland

# von Oppen, J. et al. 2020

# Data compilation: interaction predictor based on species acquisitiveness (PC1 scores in Thomas et al. 2020 Nat. Comm., https://doi.org/10.1038/s41467-020-15014-4, scores extracted by Anne D. Bjorkman)

# Jonathan von Oppen, Aarhus University, Dec 2020

# contact: jonathan.vonoppen@bios.au.dk



rm(list = ls())

# Dependencies ####
if (!require('pacman')) install.packages('pacman', repos="https://cloud.r-project.org")
pacman::p_load(tidyverse,
               tidylog)

traits_scores <- read.csv(file.path("data", "input_data", "Tundra_species_PCA_scores.csv"),
                          sep = ",",
                          header = TRUE)

# >> clean data ----
# calculate average graminoid score
traits_scores_gram <- traits_scores %>% 
  
  # filter for graminoid genera present in Greenland according to Bøcher et al. 1968 'Flora of Greenland'
  filter(str_starts(species, paste(c("Agrostis",
                                     "Alopecurus",
                                     "Anthoxanthum",
                                     "Arctagrostis",
                                     "Bromus",
                                     "Calamagrostis",
                                     "Colpodium",
                                     "Carex",
                                     "Deschampsia",
                                     "Eriophorum",
                                     "Festuca",
                                     "Hierochloe",
                                     "Juncus",
                                     "Kobresia",
                                     "Luzula",
                                     "Nardus",
                                     "Phleum",
                                     "Poa"), collapse = "|"))) %>% 
  summarise_if(is.numeric, mean) %>% 
  
  # remove meaningless "X" mean, add species "graminoid"
  mutate(X = NA,
         species = "graminoids mean")


# filter for species and combine with graminoids
traits_scores_nuuk <- traits_scores %>% 
  
  # filter for target species
  filter(species %in% c("Betula nana", 
                        "Cassiope tetragona", 
                        "Empetrum nigrum", 
                        "Phyllodoce caerulea", 
                        "Rhododendron groenlandicum", # formerly Ledum palustre subsp. groenlandicum
                        "Rhododendron tomentosum", "Ledum palustre", # formerly L. palustre
                        "Salix arctophila", 
                        "Salix glauca", 
                        "Vaccinium uliginosum")) %>% 
  
  # rename Ledum palustre and Salix glauca to sp.
  mutate(species = fct_recode(species,
                              "Rhododendron sp." = "Ledum palustre",
                              "Salix sp." = "Salix glauca")) %>% droplevels() %>% 
  
  # add averaged graminoid species
  bind_rows(traits_scores_gram) %>%
  
  # add functional group column
  mutate(fgroup = ifelse(species %in% c("Betula nana", "Salix sp.", "Vaccinium uliginosum"), 
                         "deciduous", 
                         ifelse(species %in% c("Cassiope tetragona", 
                                               "Empetrum nigrum", 
                                               "Phyllodoce caerulea", 
                                               "Rhododendron sp."),
                                "evergreen",
                                ifelse(species == "graminoids mean",
                                       "graminoid",
                                       "other"))),
         fgroup = factor(fgroup),
         species = factor(species)) 


# >> save cleaned data ----
write_csv(traits_scores_nuuk, file = file.path("data", "processed", "nuuk_traits_PCAscores_cleaned.csv"))
