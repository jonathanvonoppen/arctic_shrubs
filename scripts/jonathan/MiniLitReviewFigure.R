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
pacman::p_load(tidyverse,
               patchwork)

# Import data ####
lit <- read.csv("I:/C_Write/_User/JonathanVonOppen_au630524/Project/A_NuukFjord_shrub_abundance_controls/Data/MiniReview/200716_tundra_shrub_drivers.csv", header = T)

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
# levels(n_levels$group)[levels(n_levels$group) == "species"] <- "no. species studied"
# levels(n_levels$group)[levels(n_levels$group) == "drivers"] <- "no. drivers studied"


# Plot ####
species_count <- ggplot(n_levels %>% filter(group == "species")) +
  geom_bar(aes(x = freq), stat = "count", width = .9) +
  #facet_grid(cols = vars(group), scales = "free_x", space = "free_x") +
  scale_y_continuous(limits = c(0,12), breaks = c(3,6,9,12)) +
  labs(x = "number of taxa studied",
       y = "number of published studies") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 32),
        axis.title = element_text(size = 40),
        axis.ticks = element_line(size = 1.5),
        axis.line = element_line(size = 1.5))

driver_count <- ggplot(n_levels %>% filter(group == "drivers")) +
    geom_bar(aes(x = freq), stat = "count", width = .9) +
    #facet_grid(cols = vars(group), scales = "free_x", space = "free_x") +
    scale_y_continuous(limits = c(0,14), breaks = c(3,6,9,12)) +
    labs(x = "number of drivers studied",
         y = "number of published studies") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text = element_text(size = 32),
          axis.title = element_text(size = 40),
          axis.ticks = element_line(size = 1.5),
          axis.line = element_line(size = 1.5))

(driver_count / species_count) + 
  plot_annotation(tag_levels = "a", 
                  tag_suffix = ")") &
  theme(plot.tag = element_text(size = 40))
