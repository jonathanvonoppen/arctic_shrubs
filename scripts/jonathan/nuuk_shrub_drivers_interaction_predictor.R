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


avg_height <- spec_nuuk %>% 
  filter(presence == 1 & taxon %in% focal_species) %>% 
  mutate(max.h = as.numeric(max.h),
         taxon = as.factor(taxon)) %>% 
  group_by(taxon) %>% 
  summarise(med_height = median(max.h, na.rm = T),
            avg_height = mean(max.h, na.rm = T))

  
  RhoGro_height <- spec_nuuk %>% 
    filter(presence == 1) %>% 
    group_by(plot, taxon) %>% 
    summarise(max_height = as.numeric(max(max.h))) %>% 
    filter(taxon == "Salix glauca") %>% 
    pull(max_height) %>% 
    mean(., na.rm = T)
