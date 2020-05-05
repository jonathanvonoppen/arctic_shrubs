#####################################################################################################################################################
# Shrub cover changes across the Nuuk Fjord inland gradient                                                                                         #
# Code for mini-review of literature on drivers of shrub vegetation                                                                                 #
#                                                                                                                                                   #
# Jonathan von Oppen, January 2020                                                                                                                  #
# jonathan.vonoppen@bios.au.dk                                                                                                                      #
#                                                                                                                                                   #
#####################################################################################################################################################

rm(list = ls())

# Dependencies ####
if (!require('pacman')) install.packages('pacman', repos="https://cloud.r-project.org")
pacman::p_load(tidyverse)

# Import data ####
lit <- read.csv("I:/C_Write/_User/JonathanVonOppen_au630524/Project/A_NuukFjord_shrub_abundance_controls/aa_Godthaabsfjord/Data/MiniReview/200123_tundra_shrub_drivers.csv", header = T)

# Format data ####
colnames(lit) <- lit %>% 
  colnames %>% 
  str_replace_all("\\.", "_")
lit <- lit %>% 
  rename(species = Shrub_species, 
         drivers =  Controlling_factor, 
         origin = Factor_origin, 
         response = Shrub_expansion_response_measured, 
         study_type = Study_type, 
         location = Location,
         year = Year_of_study, 
         reference = Reference, 
         comment = Comment)

# Summarise studies ####
n_levels <- lit %>% 
  group_by(reference) %>% 
  summarise_at(vars(species, drivers), n_distinct) %>% 
  pivot_longer(-reference, names_to = "group", values_to = "freq")
n_levels$group <- factor(n_levels$group, levels = c("species","drivers"))
levels(n_levels$group)[levels(n_levels$group) == "species"] <- "no. species studied"
levels(n_levels$group)[levels(n_levels$group) == "drivers"] <- "no. drivers studied"


# Plot ####
ggplot(n_levels) +
  geom_bar(aes(x = freq), stat = "count", width = .9) +
  facet_grid(cols = vars(group), scales = "free_x", space = "free_x") +
  scale_y_continuous(limits = c(0,13), breaks = c(2,4,6,8,10,12)) +
  labs(x = "number of studies") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = rel(1)), 
        strip.background = element_rect(fill = "white", colour = "black"))
 