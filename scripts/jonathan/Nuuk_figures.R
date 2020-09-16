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

# 4) Cover and predictions for all species and predictors ----

prediction_plots_species <- function(species) {
  
  predictor_phats <- c("phat_compet", "phat_shrub_cover", "phat_graminoid_cover", 
                       "phat_sri", "phat_tri", "phat_tcws", 
                       "phat_tempjja", "phat_tempcont", "phat_precipjja")
  
  # # test
  # species <- "Rhododendron tomentosum"
  
  if(species == "Betula nana") model_coeff_output <- coeff.shrub_gradient.BetNan2
  if(species == "Cassiope tetragona") model_coeff_output <- coeff.shrub_gradient.CasTet
  if(species == "Empetrum nigrum") model_coeff_output <- coeff.shrub_gradient.EmpNig2
  if(species == "Phyllodoce caerulea") model_coeff_output <- coeff.shrub_gradient.PhyCae
  if(species == "Rhododendron groenlandicum") model_coeff_output <- coeff.shrub_gradient.RhoGro2
  if(species == "Rhododendron tomentosum") model_coeff_output <- coeff.shrub_gradient.RhoTom
  if(species == "Salix arctophila") model_coeff_output <- coeff.shrub_gradient.SalArc
  if(species == "Salix glauca") model_coeff_output <- coeff.shrub_gradient.SalGla2
  if(species == "Vaccinium uliginosum") model_coeff_output <- coeff.shrub_gradient.VacUli2
  
  if(species == "Betula nana") species_df <- BetNan.tot
  if(species == "Cassiope tetragona") species_df <- CasTet.tot
  if(species == "Empetrum nigrum") species_df <- EmpNig.tot
  if(species == "Phyllodoce caerulea") species_df <- PhyCae.tot
  if(species == "Rhododendron groenlandicum") species_df <- RhoGro.tot
  if(species == "Rhododendron tomentosum") species_df <- RhoTom.tot
  if(species == "Salix arctophila") species_df <- SalArc.tot
  if(species == "Salix glauca") species_df <- SalGla.tot
  if(species == "Vaccinium uliginosum") species_df <- VacUli.tot
  
  # define initial predictions df
  phats_long <- as.data.frame(matrix(data = NA, 
                                     nrow = 100 * length(predictor_phats), 
                                     ncol = length(model_coeff_output) + 2))
  names(phats_long)[1:length(model_coeff_output)] <- names(model_coeff_output)
  
  # extract predictions into phats data frame 
  for (phat_predictor in predictor_phats){
    
    predictor <- str_remove(phat_predictor, "phat_")
    predictor_min <- min(species_df[, paste0(predictor, "C")])
    predictor_max <- max(species_df[, paste0(predictor, "C")])
    
    # assemble predicted and predictor values, for 100 rows (one predictor) at a time
    phats_long[(100 * match(phat_predictor, predictor_phats) -99) : (100 * match(phat_predictor, predictor_phats)),] <- model_coeff_output %>% 
      
      # filter for predicted values
      filter(param %in% c(paste0(phat_predictor, "[", seq(from = 1, to = 100), "]"))) %>% 
      
      # add xhats column
      mutate(xhats = seq(from = predictor_min,
                         to = predictor_max,
                         length.out = 100)) %>% 
      
      # add column for back-centered and back-scaled values
      mutate(pred_values = xhats * attr(scale(species_df[, predictor]), 'scaled:scale') + attr(scale(species_df[, predictor]), 'scaled:center'))
    
    # # back-center and back-scale predictor values (xhats), for rows 1:100, 101:200 a.s.o.
    # phats_long$xhats[(100 * match(phat_predictor, predictor_phats) -99) : 100 * match(phat_predictor, predictor_phats)] <- phats_long$xhats[(100 * match(phat_predictor, predictor_phats) -99) : 100 * match(phat_predictor, predictor_phats)]*attr(scale(species_df[, predictor]), 'scaled:scale') + attr(scale(species_df[, predictor]), 'scaled:center') 
  }
  # [(100 * match(phat_predictor, predictor_phats) -99) : 100 * match(phat_predictor, predictor_phats)]
  
  # # pivot data frame to long format
  # phats_long <- phats %>% 
  #   pivot_longer(cols = c(str_remove(predictor_phats, "phat_")),
  #                names_to = "predictor",
  #                values_to = "phat")
  
  phats_long <- phats_long %>% 
    # rename added columns
    rename(xhat = V9,
           pred_value = V10) %>% 
    # extract predictor strings from param column
    mutate(pred_id = factor(str_remove(str_remove(param, "phat_"), "\\[\\d+\\]"),
                            levels = c("tempjja", "tempcont", "precipjja", "sri", "tri", "tcws", "compet", "shrub_cover", "graminoid_cover")))
  
  # >> facet solution (simple) ----
  
  # data for point plots: on plotgroup level for climate vars...
  point_data_pg <- species_df %>% 
    # select necessary columns
    select(site_alt_plotgroup_id,
           plot,
           levels(phats_long$pred_id),
           cover) %>% 
    # arrange vertically
    pivot_longer(cols = levels(phats_long$pred_id),
                 names_to = "pred_id",
                 values_to = "pred_value") %>%
    # filter for climate vars
    filter(pred_id %in% c("tempjja", "tempcont", "precipjja")) %>% 
    # reduce to plotgroup level
    group_by(site_alt_plotgroup_id, pred_id) %>% 
    summarise(pred_value = mean(pred_value), 
              cover = mean(cover)) %>% ungroup() %>% 
    # rename predictors
    mutate(pred_id = fct_recode(pred_id,
                                "summer temperature [°C]" = "tempjja",
                                "annual temperature variability [°C]" = "tempcont",
                                "cumulative summer precipitation [mm]" = "precipjja"),
           pred_id = fct_relevel(pred_id,
                                 "summer temperature [°C]",
                                 "annual temperature variability [°C]",
                                 "cumulative summer precipitation [mm]"))
  
  # ... and on plot level for topo & compet vars
  point_data_plot <- species_df %>% 
    # select necessary columns
    select(site_alt_plotgroup_id,
           plot,
           levels(phats_long$pred_id),
           cover) %>% 
    # arrange vertically
    pivot_longer(cols = levels(phats_long$pred_id),
                 names_to = "pred_id",
                 values_to = "pred_value") %>%
    # filter for climate vars
    filter(!(pred_id %in% c("tempjja", "tempcont", "precipjja"))) %>% 
    # rename predictors
    mutate(pred_id = fct_recode(pred_id,
                                "Solar Radiation Index" = "sri",
                                "Terrain Ruggedness Index" = "tri",
                                "Tasseled-cap Wetness Index" = "tcws",
                                "overgrowing competition" = "compet",
                                "cover of other shrub species" = "shrub_cover",
                                "cover of graminoids" = "graminoid_cover"),
           pred_id = fct_relevel(pred_id,
                                 "Solar Radiation Index",
                                 "Terrain Ruggedness Index",
                                 "Tasseled-cap Wetness Index",
                                 "overgrowing competition",
                                 "cover of other shrub species",
                                 "cover of graminoids"))
  
  # rename factor levels in predictions dataset
  phats_long <- phats_long %>% 
    
    # rename predictors
    mutate(pred_id = fct_recode(pred_id,
                                "summer temperature [°C]" = "tempjja",
                                "annual temperature variability [°C]" = "tempcont",
                                "cumulative summer precipitation [mm]" = "precipjja",
                                "Solar Radiation Index" = "sri",
                                "Terrain Ruggedness Index" = "tri",
                                "Tasseled-cap Wetness Index" = "tcws",
                                "overgrowing competition" = "compet",
                                "cover of other shrub species" = "shrub_cover",
                                "cover of graminoids" = "graminoid_cover"),
           pred_id = fct_relevel(pred_id,
                                 "summer temperature [°C]",
                                 "annual temperature variability [°C]",
                                 "cumulative summer precipitation [mm]",
                                 "Solar Radiation Index",
                                 "Terrain Ruggedness Index",
                                 "Tasseled-cap Wetness Index",
                                 "overgrowing competition",
                                 "cover of other shrub species",
                                 "cover of graminoids"))
  
  # compile plot
  pred_plot <- ggplot(data = phats_long,
                      aes(group = pred_id)) +
    
    # plotgroup level
    geom_point(data = point_data_pg,
               aes(x = pred_value,
                   y = cover),
               size = 2,
               position = position_jitter(width=0, height=.01),
               alpha=0.5) +
    
    # plot level
    geom_point(data = point_data_plot,
               aes(x = pred_value,
                   y = cover),
               size = 2,
               position = position_jitter(width=0, height=.01),
               alpha=0.5) + 
    
    # draw line of predicted values
    geom_line(aes(x = pred_value,
                  y = plogis(mean)), 
              colour = "orange",
              alpha = 1,
              size = 2) + 
    
    # draw predicted 95% CI
    geom_ribbon(aes(x = pred_value,
                    ymin = plogis(l95), 
                    ymax = plogis(u95)),
                fill = "orange",
                alpha = 0.2) +
    
    # make facets for predictors
    facet_wrap(~pred_id, strip.position = "bottom", scales = "free_x", ncol = 3) +
    
    scale_y_continuous("relative no. pin hits per plot group",
                       limits = c(0, max(species_df %>% 
                                           pull(cover)))) +
    
    labs(x = "predictor value") +
    ggtitle(paste0(species, " cover ~ predictors")) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          strip.background = element_blank(),
          strip.placement = "outside",
          panel.spacing.y = unit(1.5, "lines"))
  
  return(pred_plot)
  
  
  # # >> list solution (complicated) ----
  # predictors <- levels(phats_long$pred_id)
  # 
  # prediction_plots_list <- list()
  # 
  # for (predictor in predictors){
  #   
  #   if(predictor == "compet") predictor_label <- "abundance of taller species"
  #   if(predictor == "sri") predictor_label <- "Solar Radiation Index"
  #   if(predictor == "tri") predictor_label <- "Topographic Ruggedness Index"
  #   if(predictor == "twi") predictor_label <- "SAGA Wetness Index"
  #   if(predictor == "tempjja") predictor_label <- "mean summer temperature [°C]"
  #   if(predictor == "tempcont") predictor_label <- "annual temperature variability [°C]"
  #   if(predictor == "precipjja") predictor_label <- "mean summer precipitation [mm]"
  #   
  #   
  #   # GRAPH
  #   prediction_plots_list[predictor] <- ggplot() +
  #     # tempcont is modelled at plotgroup level, so reduce base data (points layer) to plotgroup level
  #     geom_point(data = species_df %>% group_by(site_alt_plotgroup_id) %>% summarise(pg_mean = mean(!!ensym(predictor)), cover = mean(cover)), 
  #                aes(x = pg_mean, 
  #                    y = cover), 
  #                size = 2,
  #                position = position_jitter(width=0, height=.01),
  #                alpha=0.5) +
  #     
  #     # draw line of predicted values
  #     geom_line(data = phats_long %>% filter(pred_id == predictor), 
  #               aes(x = pred_value, 
  #                   y = plogis(mean)), 
  #               colour = "orange",
  #               alpha = 1,
  #               size = 3) + 
  #     
  #     # draw predicted 95% CI
  #     geom_ribbon(data = phats_long %>% filter(pred_id == predictor),
  #                 aes(x = pred_value, 
  #                     ymin = plogis(l95), 
  #                     ymax = plogis(u95)),
  #                 fill = "orange",
  #                 alpha = 0.2) +
  #     
  #     # define appearance
  #     labs(x = predictor_label,
  #          y = "rel. no. hits per plot") + 
  #     theme_bw()
  #   
  # }
  # 
  # # combine graphs
}


# >> load data ----
# load model outputs
model_outputs_focal_species <- file.path("data", "model_outputs", "species", list.files(path = file.path("data", "model_outputs", "species"), pattern = "*.Rdata"))
for (model_output in model_outputs_focal_species){
  load(model_output)
}
# load input data
load(file = file.path("data", "model_input_data", "shrub_gradient_jags.speciesdata.Rdata"))


# >> plot graphs ----
prediction_plots_species(species = "Betula nana")
prediction_plots_species(species = "Cassiope tetragona")
prediction_plots_species(species = "Empetrum nigrum")
prediction_plots_species(species = "Phyllodoce caerulea")
prediction_plots_species(species = "Rhododendron groenlandicum")
prediction_plots_species(species = "Rhododendron tomentosum")     # prediction curves / CIs out of y range
prediction_plots_species(species = "Salix arctophila")
prediction_plots_species(species = "Salix glauca")
prediction_plots_species(species = "Vaccinium uliginosum")


# ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ ----

# 5) Cover and predictions for all groupss and predictors ----


prediction_plots_groups <- function(fgroup) {
  
  predictor_phats <- c("phat_sri", "phat_tri", "phat_tcws", 
                       "phat_tempjja", "phat_tempcont", "phat_precipjja", 
                       "phat_graminoid_cover")
  
  
  if(fgroup == "all shrub") model_coeff_output <- coeff.shrub_gradient.AllShr2
  if(fgroup == "evergreen shrub") model_coeff_output <- coeff.shrub_gradient.AllEve2
  if(fgroup == "deciduous shrub") model_coeff_output <- coeff.shrub_gradient.AllDec2
  
  
  if(fgroup == "all shrub") group_df <- AllShr.tot
  if(fgroup == "evergreen shrub") group_df <- AllEve.tot
  if(fgroup == "deciduous shrub") group_df <- AllDec.tot
  
  
  # define initial predictions df
  phats_long <- as.data.frame(matrix(data = NA, 
                                     nrow = 100 * length(predictor_phats), 
                                     ncol = length(model_coeff_output) + 2))
  names(phats_long)[1:length(model_coeff_output)] <- names(model_coeff_output)
  
  # extract predictions into phats data frame 
  for (phat_predictor in predictor_phats){
    
    predictor <- str_remove(phat_predictor, "phat_")
    predictor_min <- min(group_df[, paste0(predictor, "C")])
    predictor_max <- max(group_df[, paste0(predictor, "C")])
    
    # assemble predicted and predictor values, for 100 rows (one predictor) at a time
    phats_long[(100 * match(phat_predictor, predictor_phats) -99) : (100 * match(phat_predictor, predictor_phats)),] <- model_coeff_output %>% 
      
      # filter for predicted values
      filter(param %in% c(paste0(phat_predictor, "[", seq(from = 1, to = 100), "]"))) %>% 
      
      # add xhats column
      mutate(xhats = seq(from = predictor_min,
                         to = predictor_max,
                         length.out = 100)) %>% 
      
      # add column for back-centered and back-scaled values
      mutate(pred_values = xhats * attr(scale(group_df[, predictor]), 'scaled:scale') + attr(scale(group_df[, predictor]), 'scaled:center'))
    
    # # back-center and back-scale predictor values (xhats), for rows 1:100, 101:200 a.s.o.
    # phats_long$xhats[(100 * match(phat_predictor, predictor_phats) -99) : 100 * match(phat_predictor, predictor_phats)] <- phats_long$xhats[(100 * match(phat_predictor, predictor_phats) -99) : 100 * match(phat_predictor, predictor_phats)]*attr(scale(group_df[, predictor]), 'scaled:scale') + attr(scale(group_df[, predictor]), 'scaled:center') 
  }
  # [(100 * match(phat_predictor, predictor_phats) -99) : 100 * match(phat_predictor, predictor_phats)]
  
  # # pivot data frame to long format
  # phats_long <- phats %>% 
  #   pivot_longer(cols = c(str_remove(predictor_phats, "phat_")),
  #                names_to = "predictor",
  #                values_to = "phat")
  
  phats_long <- phats_long %>% 
    # rename added columns
    rename(xhat = V9,
           pred_value = V10) %>% 
    # extract predictor strings from param column
    mutate(pred_id = factor(str_remove(str_remove(param, "phat_"), "\\[\\d+\\]"),
                            levels = c("tempjja", "tempcont", "precipjja", "sri", "tri", "tcws", "graminoid_cover")))
  
  # >> facet solution (simple) ----
  
  # data for point plots: on plotgroup level for climate vars...
  point_data_pg <- group_df %>% 
    # select necessary columns
    select(site_alt_plotgroup_id,
           plot,
           levels(phats_long$pred_id),
           cover) %>% 
    # arrange vertically
    pivot_longer(cols = levels(phats_long$pred_id),
                 names_to = "pred_id",
                 values_to = "pred_value") %>%
    # filter for climate vars
    filter(pred_id %in% c("tempjja", "tempcont", "precipjja")) %>% 
    # reduce to plotgroup level
    group_by(site_alt_plotgroup_id, pred_id) %>% 
    summarise(pred_value = mean(pred_value), 
              cover = mean(cover)) %>% ungroup() %>% 
    # rename predictors
    mutate(pred_id = fct_recode(pred_id,
                                "summer temperature [°C]" = "tempjja",
                                "annual temperature variability [°C]" = "tempcont",
                                "cumulative summer precipitation [mm]" = "precipjja"),
           pred_id = fct_relevel(pred_id,
                                 "summer temperature [°C]",
                                 "annual temperature variability [°C]",
                                 "cumulative summer precipitation [mm]"))
  
  # ... and on plot level for topo & compet vars
  point_data_plot <- group_df %>% 
    # select necessary columns
    select(site_alt_plotgroup_id,
           plot,
           levels(phats_long$pred_id),
           cover) %>% 
    # arrange vertically
    pivot_longer(cols = levels(phats_long$pred_id),
                 names_to = "pred_id",
                 values_to = "pred_value") %>%
    # filter for climate vars
    filter(!(pred_id %in% c("tempjja", "tempcont", "precipjja"))) %>% 
    # rename predictors
    mutate(pred_id = fct_recode(pred_id,
                                "Solar Radiation Index" = "sri",
                                "Terrain Ruggedness Index" = "tri",
                                "Tasseled-cap Wetness Index" = "tcws",
                                "cover of graminoids" = "graminoid_cover"),
           pred_id = fct_relevel(pred_id,
                                 "Solar Radiation Index",
                                 "Terrain Ruggedness Index",
                                 "Tasseled-cap Wetness Index",
                                 "cover of graminoids"))
  
  # rename factor levels in predictions dataset
  phats_long <- phats_long %>% 
    
    # rename predictors
    mutate(pred_id = fct_recode(pred_id,
                                "summer temperature [°C]" = "tempjja",
                                "annual temperature variability [°C]" = "tempcont",
                                "cumulative summer precipitation [mm]" = "precipjja",
                                "Solar Radiation Index" = "sri",
                                "Terrain Ruggedness Index" = "tri",
                                "Tasseled-cap Wetness Index" = "tcws",
                                "cover of graminoids" = "graminoid_cover"),
           pred_id = fct_relevel(pred_id,
                                 "summer temperature [°C]",
                                 "annual temperature variability [°C]",
                                 "cumulative summer precipitation [mm]",
                                 "Solar Radiation Index",
                                 "Terrain Ruggedness Index",
                                 "Tasseled-cap Wetness Index",
                                 "cover of graminoids"))
  
  # compile plot
  pred_plot <- ggplot(data = phats_long,
                      aes(group = pred_id)) +
    
    # plotgroup level
    geom_point(data = point_data_pg,
               aes(x = pred_value,
                   y = cover),
               size = 2,
               position = position_jitter(width=0, height=.01),
               alpha=0.5) +
    
    # plot level
    geom_point(data = point_data_plot,
               aes(x = pred_value,
                   y = cover),
               size = 2,
               position = position_jitter(width=0, height=.01),
               alpha=0.5) + 
    
    # draw line of predicted values
    geom_line(aes(x = pred_value,
                  y = exp(mean)), 
              colour = "orange",
              alpha = 1,
              size = 2) + 
    
    # draw predicted 95% CI
    geom_ribbon(aes(x = pred_value,
                    ymin = exp(l95), 
                    ymax = exp(u95)),
                fill = "orange",
                alpha = 0.2) +
    
    # make facets for predictors
    facet_wrap(~pred_id, strip.position = "bottom", scales = "free_x", ncol = 3) +
    
    scale_y_continuous("relative no. pin hits per plot group",
                       limits = c(0, max(group_df %>% 
                                           pull(cover)))) +
    
    labs(x = "predictor value") +
    ggtitle(paste0(fgroup, " cover ~ predictors")) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          strip.background = element_blank(),
          strip.placement = "outside",
          panel.spacing.y = unit(1.5, "lines"))
  
  return(pred_plot)
  
}


model_outputs_groups <- file.path("data", "model_outputs", "groups", list.files(path = file.path("data", "model_outputs", "groups"), 
                                                                                 pattern = "*.Rdata"))
for (model_output in model_outputs_groups){
  load(model_output)
}
# load input data
load(file = file.path("data", "model_input_data", "shrub_gradient_jags.speciesdata.Rdata"))


# >> plot graphs ----
prediction_plots_groups(fgroup = "all shrub")
prediction_plots_groups(fgroup = "evergreen shrub")
prediction_plots_groups(fgroup = "deciduous shrub")




# ________________________----

# effect size plot function to draw from
model_plot_sig_function <- function(model_coeff_output, title_string, plot_width) {
  target_vars <- c("b.tempjja.x", "b.tempjja.x2",
                   "b.tempcont.x", "b.tempcont.x2",
                   "b.precipjja.x", "b.precipjja.x2",
                   "b.sri",
                   "b.tri",
                   "b.twi", 
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
  #                                          "b.twi",
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
  title_string <- title_string
  title_colour <- "grey10"
  # if(response == "T1_mean" | response == "T1_amp") title_colour <- theme_red
  # if(response == "T2_mean" | response == "T2_amp") title_colour <- theme_yellow
  # if(response == "T1_mean") response <- "Soil"
  # if(response == "T2_mean") response <- "Ground"
  
  
  model_plot_sig <- ggplot(solutions, aes(x = variable, y = post.mean,
                                          ymin = l95, ymax = u95,
                                          colour = sig)) +
    geom_point() +
    geom_errorbar(width = .8) +
    theme_cowplot(18) +
    ylab("Effect Size (scaled)") +
    xlab("") +
    ggtitle(paste0(title_string)) +
    scale_colour_manual(values = c("black", theme_darkgreen)) +
    scale_y_continuous(limits = c(min_value, max_value), breaks = seq(min_value,max_value,0.5)) +
    # scale_x_discrete(limits = c("b.tempjja.x", "b.tempjja.x2",
    #         "b.tempcont.x", "b.tempcont.x2",
    #         "b.precipjja.x", "b.precipjja.x2",
    #         "b.sri",
    #         "b.tri",
    #         "b.twi",
    #         "b.compet"),
    #                  labels = c("summer temperature", bquote(.("summer") *" "* temperature^2),
    #                             "temperature variability", bquote(.("temperature") *" "* variability^2),
    #                             "summer precipitation", bquote(.("summer") *" "* precipitation^2),
    #                             "solar radiation",
  #                             "terrain ruggedness",
  #                             "moisture availability",
  #                             "competition")) +
  annotate("segment", x = 0, xend = plot_width, y = 0, yend = 0) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = label_colour, face = label_face),
          plot.title = element_text(colour = title_colour, face = "italic"),
          legend.position = "none")
  return(model_plot_sig)
}