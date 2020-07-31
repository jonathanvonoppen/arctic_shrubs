#####################################################################################################################################################
# Shrub cover changes across the Nuuk Fjord inland gradient                                                                                         #
#                                                                                                                                                   #
# Figures                                                                                                                                           #
#                                                                                                                                                   #
# Jonathan von Oppen, January/August 2020                                                                                                           #
# jonathan.vonoppen@bios.au.dk                                                                                                                      #
#                                                                                                                                                   #
#####################################################################################################################################################

rm(list = ls())

# Dependencies ####
if (!require('pacman')) install.packages('pacman', repos="https://cloud.r-project.org")
pacman::p_load(tidyverse,
               tidylog,
               patchwork,
               cowplot)

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

# ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ ----

# 1) Mini-review of literature on drivers of shrub vegetation ----

# >> import data ####
lit <- read.csv("I:/C_Write/_User/JonathanVonOppen_au630524/Project/A_NuukFjord_shrub_abundance_controls/Data/MiniReview/200716_tundra_shrub_drivers.csv", header = T)

# >> format data ####
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

# >> summarise studies ####
n_levels <- lit %>% 
  group_by(reference) %>% 
  summarise_at(vars(species, drivers), n_distinct) %>% 
  pivot_longer(-reference, names_to = "group", values_to = "freq")
n_levels$group <- factor(n_levels$group, levels = c("species","drivers"))
# levels(n_levels$group)[levels(n_levels$group) == "species"] <- "no. species studied"
# levels(n_levels$group)[levels(n_levels$group) == "drivers"] <- "no. drivers studied"


# >> plot ####
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

# ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ ----

# 2) Predictors across the gradients ----

# >> import data ####
pred_gradient_data <- read.csv("data/Nuuk_env_cover_plots.csv", header = T, stringsAsFactors = F) %>% 
  
  # convert grouping columns to factors
  mutate_at(vars(c(site_alt_plotgroup_id, site_alt_id, site, alt, plotgroup, plot, year)), ~as.factor(.)) %>% 
  mutate(alt = factor(alt, levels = c("20", "100", "200", "300", "400", "500"))) %>% 
  
  # reduce to plot level for env predictors
  distinct(plot, .keep_all = TRUE)

# reduce to plotgroup level for clim predictors
pred_gradient_data_clim <- pred_gradient_data %>% 
  distinct(site_alt_plotgroup_id, .keep_all = TRUE)

# create vector with predictors
predictor_set_tot <- c("tempjja_ts_30",
                       "precipjja_ts_30",
                       "tempcont_ts_30",
                       "sri",
                       "tri",
                       "twi_90m")

predictor_set_clim <- c("tempjja_ts_30",
                        "precipjja_ts_30",
                        "tempcont_ts_30")

predictor_set_env <- c("sri",
                       "tri",
                       "twi_90m")

# >> plot ####

# summer temperature
(plot_tempjja_grad <- ggplot(data = pred_gradient_data_clim,
                            aes(x = site,
                                y = tempjja_ts_30,
                                fill = alt)) +
  geom_boxplot() +
  labs(x = "site",
       y = "mean JJA temperature") +
  theme_bw())

# summer precipitation
(plot_precipjja_grad <- ggplot(data = pred_gradient_data_clim,
                             aes(x = site,
                                 y = precipjja_ts_30,
                                 fill = alt)) +
    geom_boxplot() +
    labs(x = "site",
         y = "mean JJA precipitation") +
    theme_bw())

# temperature variability
(plot_tempcont_grad <- ggplot(data = pred_gradient_data_clim,
                             aes(x = site,
                                 y = tempcont_ts_30,
                                 fill = alt)) +
    geom_boxplot() +
    labs(x = "site",
         y = "mean annual temperature variability") +
    theme_bw())

# solar radiation
(plot_sri_grad <- ggplot(data = pred_gradient_data,
                             aes(x = site,
                                 y = sri,
                                 fill = alt)) +
    geom_boxplot() +
    labs(x = "site",
         y = "mean Solar Radiation Index") +
    theme_bw())

# terrain ruggedness
(plot_tri_grad <- ggplot(data = pred_gradient_data,
                             aes(x = site,
                                 y = tri,
                                 fill = alt)) +
    geom_boxplot() +
    labs(x = "site",
         y = "mean Terrain Ruggedness Index") +
    theme_bw())

# topographic wetness
(plot_twi_grad <- ggplot(data = pred_gradient_data,
                             aes(x = site,
                                 y = twi_90m,
                                 fill = alt)) +
    geom_boxplot() +
    labs(x = "site",
         y = "mean Topographic Wetness Index") +
    theme_bw())



# ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ ----

# 3) Cover against predictors for each species ----
# >> import data ####
env_cov_bio <- read.csv("data/Nuuk_env_cover_plots.csv", header = T, stringsAsFactors = F)

# >> prepare data ####
env_cov_bio.long <- env_cov_bio %>% 
  select(taxon,
         tempjja_ts_30,
         tempcont_ts_30,
         precipjja_ts_30,
         sri,
         tri,
         twi_90m,
         compet,
         cover) %>% 
  # pivot to long format
  pivot_longer(cols = c(tempjja_ts_30,
                        tempcont_ts_30,
                        precipjja_ts_30,
                        sri,
                        tri,
                        twi_90m,
                        compet),
               names_to = "predictor",
               values_to = "pred_value")

# >> build function ####
pred.plot.grid <- function(df){
  
  taxa <- df %>% pull(taxon) %>% unique() %>% as.character()
  
  for(taxon_nr in 1:length(taxa)){
    plot <- ggplot(data = df %>% filter(taxon == taxa[taxon_nr]), 
                   aes(y = cover, group = taxon)) +
      geom_point(aes(x = pred_value), colour = "darkgrey") +
      geom_smooth(aes(x = pred_value), method = "lm", colour = "darkgreen", se = TRUE, na.rm = TRUE) +
      geom_smooth(aes(x = pred_value), method = "lm", formula = y ~ poly(x, 2), colour = "darkorange", se = TRUE, na.rm = TRUE) +
      facet_wrap(~predictor, scales = "free", ncol = 4) +
      scale_y_continuous("relative no. pin hits per plot group",
                         limits = c(0, max(df %>% 
                                             filter(taxon == taxa[taxon_nr]) %>% 
                                             pull(cover)))) +
      labs(x = "predictor value") +
      ggtitle(paste0(taxa[taxon_nr], " cover ~ predictors")) +
      theme_bw() +
      theme(legend.position = "none")
    print(plot)
  }
}

# >> plot ####
pred.plot.grid(env_cov_bio.long)

# ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ ----
