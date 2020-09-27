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

# >> load data
lit_raw <- read.csv(file.path("data", "shrub_drivers_lit_review_records.csv"),
                sep = ",",
                header = TRUE)

# clean data
lit <- lit_raw %>% 
  
  rename_all(tolower) %>% 
  
  # drop unnecessary columns
  select(-c(starts_with("book"),
            starts_with("conference"),
            starts_with("funding"),
            starts_with("journal"),
            starts_with("publisher"),
            contains("keywords"),
            contains("addresses"),
            contains("cited"),
            contains("count"),
            contains("abbreviation"),
            contains("number"),
            contains(".id"),
            contains("access"),
            ends_with("status"),
            ends_with("page"),
            "author.full.names",
            "group.authors",
            "language",
            "document.type",
            "orcids",
            "issn",
            "eissn",
            "isbn",
            "publication.date",
            "volume",
            "issue",
            "supplement",
            "special.issue",
            "meeting.abstract",
            "wos.categories",
            "research.areas")) %>% 
  
  # replace dots and spaces in variable names with underscores
  janitor::clean_names() %>% 
  
  # calculate and add number of drivers
  left_join(lit_raw %>% 
              select(study_id, drivers) %>% 
              filter(!(drivers == "")) %>%
              mutate(n_drivers = lengths(str_split(drivers, ";"))),
            by = c("study_id", "drivers")) %>% 
  
  # filter for species-level, empirical studies
  filter(study_level == "species") %>% 
  filter(str_detect(study_type, "empirical")) %>% 
  
  # reorder columns
  select(study_id,
         source,
         publication_type,
         authors,
         publication_year,
         source_title,
         article_title,
         abstract,
         doi,
         date_of_export,
         study_type:drivers,
         n_drivers,
         driver_regime:comment)
  



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

# 4) Species: cover and predictions for all predictors ----

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
    
    phats_long[(100 * match(phat_predictor, predictor_phats) -99) : (100 * match(phat_predictor, predictor_phats)),] <- model_coeff_output %>% 
      
      # filter for predicted values
      filter(param %in% c(paste0(phat_predictor, "[", seq(from = 1, to = 100), "]"))) %>% 
      
      # add xhats column
      mutate(xhats = seq(from = predictor_min,
                         to = predictor_max,
                         length.out = 100)) %>% 
      
      # add column for back-centered and back-scaled values
      mutate(pred_values = xhats * attr(scale(species_df[, predictor]), 'scaled:scale') + attr(scale(species_df[, predictor]), 'scaled:center'))
    
  }
  
  
  phats_long <- phats_long %>% 
    # rename added columns
    rename(xhat = V9,
           pred_value = V10) %>% 
    # extract predictor strings from param column
    mutate(pred_id = factor(str_remove(str_remove(param, "phat_"), "\\[\\d+\\]"),
                            levels = c("tempjja", "tempcont", "precipjja", "sri", "tri", "tcws", "compet", "shrub_cover", "graminoid_cover"))) %>% 
    
    # add significance level column
    mutate(sig = "ns") 
    
  param_lookup <- data.frame(coeffs = c("b.tempjja.x", "b.tempjja.x2", "b.tempcont.x", "b.tempcont.x2", "b.precipjja.x", "b.precipjja.x2", "b.sri", "b.tri", "b.tcws", "b.compet", "b.shrub_cov", "b.gramin_cov"),
                             phats = c("tempjja", "tempjja", "tempcont", "tempcont", "precipjja", "precipjja", "sri", "tri", "tcws", "compet", "shrub_cover", "graminoid_cover"))
  
  lapply(param_lookup$coeffs[param_lookup$coeffs %in% as.character(model_coeff_output$param)], function(coefficient){
    model_coeff_output_sub <- model_coeff_output %>% filter(as.character(param) == coefficient)
    
    if ((model_coeff_output_sub$l95 < 0 & model_coeff_output_sub$u95 < 0) | 
        (model_coeff_output_sub$l95 > 0 & model_coeff_output_sub$u95 > 0)) {
      sig <- "sig"
    } else if ((model_coeff_output_sub$l90 < 0 & model_coeff_output_sub$u90 < 0) | 
               (model_coeff_output_sub$l90 > 0 & model_coeff_output_sub$u90 > 0)) {
      sig <- "marg"
    } else {
      sig <- "ns"
    }
    
    phats_long$sig[as.character(phats_long$pred_id) == param_lookup$phats[param_lookup$coeffs == coefficient]] <<- sig
    
  })
  
  phats_long$sig <- ordered(phats_long$sig, levels = c("ns", "sig", "marg"))
  
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
              cover = mean(cover)) %>% ungroup() 
  
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
    filter(!(pred_id %in% c("tempjja", "tempcont", "precipjja"))) 

  
  # plotting function
  plot_predictor <- function(predictor_id) {
  
    predictor_id <- as.character(predictor_id)
    
    phats_long <- phats_long %>% filter(as.character(pred_id) == predictor_id) %>% 
      
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
    
    point_data_pg <- point_data_pg %>% filter(as.character(pred_id) == predictor_id) %>% 
      # rename predictors
      mutate(pred_id = fct_recode(pred_id,
                                  "summer temperature [°C]" = "tempjja",
                                  "annual temperature variability [°C]" = "tempcont",
                                  "cumulative summer precipitation [mm]" = "precipjja"),
             pred_id = fct_relevel(pred_id,
                                   "summer temperature [°C]",
                                   "annual temperature variability [°C]",
                                   "cumulative summer precipitation [mm]"))
    
    point_data_plot <- point_data_plot %>% filter(as.character(pred_id) == predictor_id) %>% 
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
    
    # >> compile plot ----
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
              colour =  c("black", theme_darkgreen, theme_purple)[as.numeric(unique(phats_long$sig))],
              alpha = 1,
              size = 2) + 
    
    # draw predicted 95% CI
    geom_ribbon(aes(x = pred_value,
                    ymin = plogis(l95), 
                    ymax = plogis(u95)),
                fill =  c("black", theme_darkgreen, theme_purple)[as.numeric(unique(phats_long$sig))],
                alpha = 0.2) +
    
    # set y axis limits so ribbons are not cut off
      coord_cartesian(ylim = c(0, 
                             ifelse(predictor_id == "tempjja" | predictor_id == "tempcont" | predictor_id == "precipjja",
                                    max(point_data_pg %>% 
                                          pull(cover)),
                                    max(species_df %>% 
                                          pull(cover))))) +
    
    labs(x = unique(phats_long$pred_id)) +
    guides(colour = guide_legend(nrow = 1),
           fill = FALSE) +
    
    # define appearance
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = rel(1.6), 
                                      margin = margin(t = 5)),
          axis.text.x = element_text(size = 13,
                                     margin = margin(t = 4)),
          plot.margin = unit(c(2, 1, 1, 1), "lines"),
          legend.position = ifelse(predictor_id == "tempjja", c(0.5, 0.9), "none")
          )
  }
  
# compile all plots
  plot_list <- lapply(unique(param_lookup$phats[param_lookup$phats %in% as.character(phats_long$pred_id)]),
                      plot_predictor)
  
# make title and y-axis label
  title <- ggdraw() +
    draw_label(paste0(species, " cover ~ predictors"),
               hjust = 0.5,
               size = 20)
  
  ylabel_plotgroup <- ggdraw() +
    draw_label("cover per plot group",
               vjust = 0,
               angle = 90,
               size = 20,
               fontface = "bold")
  
  ylabel_plot <- ggdraw() +
    draw_label("cover per plot",
               vjust = 0,
               angle = 90,
               size = 20,
               fontface = "bold")
  
  # combine ylabels
  ylabel <- plot_grid(ylabel_plotgroup,
                      ylabel_plot,
                      ncol = 1,
                      rel_heights = c(0.5, 1))
  
# combine axis label and plots
  pred_plots_row <- plot_grid(ylabel,
                              plot_grid(plotlist = plot_list,
                                        labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)", "i)"),
                                        label_size = 18,
                                        label_fontface = "bold",
                                        label_x = c(rep(0.02, 9)),
                                        ncol = 3,
                                        axis = "lt",
                                        align = "hv"),
                              rel_widths = c(.03, 1)
                              )
  
# add title
  pred_plot <- plot_grid(title,
                         pred_plots_row,
                         ncol = 1,
                         rel_heights = c(0.1, 1))
  
  
  return(pred_plot)

}


# >> load data ----
# load model outputs
model_outputs_species <- file.path("data", "model_outputs", "species", list.files(path = file.path("data", "model_outputs", "species"), pattern = "*.Rdata"))
for (model_output in model_outputs_species){
  load(model_output)
}
# load input data
load(file = file.path("data", "model_input_data", "shrub_gradient_species.datasets.Rdata"))


# >> plot graphs ----
(nuuk_prediction_plot_BetNan <- prediction_plots_species(species = "Betula nana"))
(nuuk_prediction_plot_CasTet <- prediction_plots_species(species = "Cassiope tetragona"))
(nuuk_prediction_plot_EmpNig <- prediction_plots_species(species = "Empetrum nigrum"))
(nuuk_prediction_plot_PhyCae <- prediction_plots_species(species = "Phyllodoce caerulea"))
(nuuk_prediction_plot_RhoGro <- prediction_plots_species(species = "Rhododendron groenlandicum"))
(nuuk_prediction_plot_RhoTom <- prediction_plots_species(species = "Rhododendron tomentosum"))     
(nuuk_prediction_plot_SalArc <- prediction_plots_species(species = "Salix arctophila"))
(nuuk_prediction_plot_SalGla <- prediction_plots_species(species = "Salix glauca"))
(nuuk_prediction_plot_VacUli <- prediction_plots_species(species = "Vaccinium uliginosum"))

# save plots
prediction_plots_path <- file.path("figures", "prediction_plots")
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_BetNan.pdf"),
#           nuuk_prediction_plot_BetNan, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_CasTet.pdf"),
#           nuuk_prediction_plot_CasTet, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_EmpNig.pdf"),
#           nuuk_prediction_plot_EmpNig, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_PhyCae.pdf"),
#           nuuk_prediction_plot_PhyCae, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_RhoGro.pdf"),
#           nuuk_prediction_plot_RhoGro, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_RhoTom.pdf"),
#           nuuk_prediction_plot_RhoTom, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_SalArc.pdf"),
#           nuuk_prediction_plot_SalArc, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_SalGla.pdf"),
#           nuuk_prediction_plot_SalGla, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_VacUli.pdf"),
#           nuuk_prediction_plot_VacUli, base_height = 15, base_aspect_ratio = 1)


# ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ ----

# 5) Groups: cover and predictions for all predictors ----


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
    
  }

  phats_long <- phats_long %>% 
    # rename added columns
    rename(xhat = V9,
           pred_value = V10) %>% 
    # extract predictor strings from param column
    mutate(pred_id = factor(str_remove(str_remove(param, "phat_"), "\\[\\d+\\]"),
                            levels = c("tempjja", "tempcont", "precipjja", "sri", "tri", "tcws", "graminoid_cover"))) %>% 
    
    # add significance level column
    mutate(sig = "ns") 
  
  param_lookup <- data.frame(coeffs = c("b.tempjja.x", "b.tempjja.x2", "b.tempcont.x", "b.tempcont.x2", "b.precipjja.x", "b.precipjja.x2", "b.sri", "b.tri", "b.tcws", "b.compet", "b.shrub_cov", "b.gramin_cov"),
                             phats = c("tempjja", "tempjja", "tempcont", "tempcont", "precipjja", "precipjja", "sri", "tri", "tcws", "compet", "shrub_cover", "graminoid_cover"))
  
  lapply(param_lookup$coeffs[param_lookup$coeffs %in% as.character(model_coeff_output$param)], function(coefficient){
    model_coeff_output_sub <- model_coeff_output %>% filter(as.character(param) == coefficient)
    
    if ((model_coeff_output_sub$l95 < 0 & model_coeff_output_sub$u95 < 0) | 
        (model_coeff_output_sub$l95 > 0 & model_coeff_output_sub$u95 > 0)) {
      sig <- "sig"
    } else if ((model_coeff_output_sub$l90 < 0 & model_coeff_output_sub$u90 < 0) | 
               (model_coeff_output_sub$l90 > 0 & model_coeff_output_sub$u90 > 0)) {
      sig <- "marg"
    } else {
      sig <- "ns"
    }
    
    phats_long$sig[as.character(phats_long$pred_id) == param_lookup$phats[param_lookup$coeffs == coefficient]] <<- sig
    
  })
  
  phats_long$sig <- ordered(phats_long$sig, levels = c("ns", "sig", "marg"))
  
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
              cover = mean(cover)) %>% ungroup() 
  
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
    filter(!(pred_id %in% c("tempjja", "tempcont", "precipjja"))) 
  
  
  # plotting function
  plot_predictor <- function(predictor_id) {
    
    predictor_id <- as.character(predictor_id)
    
    phats_long <- phats_long %>% filter(as.character(pred_id) == predictor_id) %>% 
      
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
    
    point_data_pg <- point_data_pg %>% filter(as.character(pred_id) == predictor_id) %>% 
      # rename predictors
      mutate(pred_id = fct_recode(pred_id,
                                  "summer temperature [°C]" = "tempjja",
                                  "annual temperature variability [°C]" = "tempcont",
                                  "cumulative summer precipitation [mm]" = "precipjja"),
             pred_id = fct_relevel(pred_id,
                                   "summer temperature [°C]",
                                   "annual temperature variability [°C]",
                                   "cumulative summer precipitation [mm]"))
    
    point_data_plot <- point_data_plot %>% filter(as.character(pred_id) == predictor_id) %>% 
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
    
    # >> compile plot ----
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
                colour =  c("black", theme_darkgreen, theme_purple)[as.numeric(unique(phats_long$sig))],
                alpha = 1,
                size = 2) + 
      
      # draw predicted 95% CI
      geom_ribbon(aes(x = pred_value,
                      ymin = plogis(l95), 
                      ymax = plogis(u95)),
                  fill =  c("black", theme_darkgreen, theme_purple)[as.numeric(unique(phats_long$sig))],
                  alpha = 0.2) +
      
      # set y axis limits so ribbons are not cut off
      coord_cartesian(ylim = c(0, 
                               ifelse(predictor_id == "tempjja" | predictor_id == "tempcont" | predictor_id == "precipjja",
                                      max(point_data_pg %>% 
                                            pull(cover)),
                                      max(group_df %>% 
                                            pull(cover))))) +
      
      # define appearance
      labs(x = unique(phats_long$pred_id)) +
      guides(colour = guide_legend(nrow = 1),
             fill = FALSE) +
      theme_bw() +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_text(size = 13),
            axis.title.x = element_text(size = rel(1.6), 
                                        margin = margin(t = 5)),
            axis.text.x = element_text(size = 13,
                                       margin = margin(t = 4)),
            plot.margin = unit(c(2, 1, 1, 1), "lines"),
            legend.position = ifelse(predictor_id == "tempjja", c(0.5, 0.9), "none")
            )
  }
  
  # compile all plots
  plot_list <- lapply(unique(param_lookup$phats[param_lookup$phats %in% as.character(phats_long$pred_id)]),
                      plot_predictor)
  
  # make title and y-axis label
  title <- ggdraw() +
    draw_label(paste0(fgroup, " cover ~ predictors"),
               hjust = 0.5,
               size = 20)
  
  ylabel_plotgroup <- ggdraw() +
    draw_label("cover per plot group",
               vjust = 0,
               angle = 90,
               size = 20,
               fontface = "bold")
  
  ylabel_plot <- ggdraw() +
    draw_label("cover per plot",
               vjust = 0,
               angle = 90,
               size = 20,
               fontface = "bold")
  
  # combine ylabels
  ylabel <- plot_grid(ylabel_plotgroup,
                      ylabel_plot,
                      ncol = 1,
                      rel_heights = c(0.5, 1))
  
  # combine axis label and plots
  pred_plots_row <- plot_grid(ylabel,
                              plot_grid(plotlist = plot_list,
                                        labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)"),
                                        label_size = 18,
                                        label_fontface = "bold",
                                        label_x = c(rep(0.02, 7)),
                                        ncol = 3,
                                        axis = "lt",
                                        align = "hv"),
                              rel_widths = c(.03, 1)
  )
  
  # add title
  pred_plot <- plot_grid(title,
                         pred_plots_row,
                         ncol = 1,
                         rel_heights = c(0.1, 1))
  
  
  return(pred_plot)
  
}


# >> load data ----
model_outputs_groups <- file.path("data", "model_outputs", "groups", list.files(path = file.path("data", "model_outputs", "groups"), 
                                                                                 pattern = "*.Rdata"))
for (model_output in model_outputs_groups){
  load(model_output)
}
# load input data
load(file = file.path("data", "model_input_data", "shrub_gradient_group.datasets.Rdata"))


# >> plot graphs ----
(nuuk_prediction_plot_AllShr <- prediction_plots_groups(fgroup = "all shrub"))
(nuuk_prediction_plot_AllEve <- prediction_plots_groups(fgroup = "evergreen shrub"))
(nuuk_prediction_plot_AllDec <- prediction_plots_groups(fgroup = "deciduous shrub"))

# save plots
prediction_plots_path <- file.path("figures", "prediction_plots")
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_AllShr.pdf"),
#           nuuk_prediction_plot_AllShr, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_AllEve.pdf"),
#           nuuk_prediction_plot_AllEve, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_AllDec.pdf"),
#           nuuk_prediction_plot_AllDec, base_height = 15, base_aspect_ratio = 1)


# ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ ----

# 6) Interaction plots temp X moisture ----

interaction_plots_species <- function(species) {
  
  # # test
  # species <- "Salix glauca"
  
  if(species == "Betula nana") model_coeff_output <- coeff.shrub_gradient.BetNan2
  if(species == "Empetrum nigrum") model_coeff_output <- coeff.shrub_gradient.EmpNig2
  if(species == "Rhododendron groenlandicum") model_coeff_output <- coeff.shrub_gradient.RhoGro2
  if(species == "Salix glauca") model_coeff_output <- coeff.shrub_gradient.SalGla2
  if(species == "Vaccinium uliginosum") model_coeff_output <- coeff.shrub_gradient.VacUli2
  
  if(species == "Betula nana") species_df <- BetNan.tot
  if(species == "Empetrum nigrum") species_df <- EmpNig.tot
  if(species == "Rhododendron groenlandicum") species_df <- RhoGro.tot
  if(species == "Salix glauca") species_df <- SalGla.tot
  if(species == "Vaccinium uliginosum") species_df <- VacUli.tot
  
  # define initial predictions df
  phats <- as.data.frame(matrix(data = NA,
                                nrow = 100, 
                                ncol = length(model_coeff_output) + 2))
  names(phats)[1:length(model_coeff_output)] <- names(model_coeff_output)
  
  # extract predictions into phats data frame 
  phat_predictor <- "phat_tempXmoist"
    
  predictor_min <- min(species_df$tempjjaC)
  predictor_max <- max(species_df$tempjjaC)
    
    # assemble predicted and predictor values, for 100 rows (one predictor) at a time
    phats <- model_coeff_output %>% 
      
      # filter for predicted values
      filter(param %in% c(paste0(phat_predictor,
                                 "[", 
                                 rep(seq(from = 1, to = 100), times = 2),
                                 ",",
                                 rep(seq(from = 1, to = 2), each = 100), 
                                 "]"))) %>% 
      
      # add xhats column
      mutate(xhats = rep(seq(from = predictor_min, 
                             to = predictor_max,
                             length.out = 100), times = 2)) %>% 
      
      # add column for back-centered and back-scaled values
      mutate(tempjja = xhats * attr(scale(species_df$tempjja), 'scaled:scale') + attr(scale(species_df$tempjja), 'scaled:center'),
             
      # add column for low/high moisture level values
             tcws = rep(c("low", "high"),
                        each = 100))
    
    # graph
    
    int_plot <- ggplot() +
      # tempjja is modelled at plotgroup level, so reduce base data (points layer) to plotgroup level
      geom_point(data = species_df %>% group_by(site_alt_plotgroup_id) %>% summarise(tempjja = mean(tempjja), cover = mean(cover)), 
                 aes(x = tempjja, 
                     y = cover), 
                 size = 2,
                 position = position_jitter(width=0, height=.001),
                 alpha = 0.5) +
      
      # draw line of predicted values
      geom_line(data = phats, 
                aes(x = tempjja, 
                    y = plogis(mean),
                    colour = tcws), 
                alpha = 1,
                size = 3) + 
      
      # draw predicted 95% CI
      geom_ribbon(data = phats,
                  aes(x = tempjja, 
                      ymin = plogis(l95), 
                      ymax = plogis(u95),
                      fill = tcws),
                  alpha = 0.2) +
      
      # set y range limits so ribbons are not cut off
      coord_cartesian(ylim = c(0, 
                               max(species_df %>% 
                                     group_by(site_alt_plotgroup_id) %>% 
                                     summarise(tempjja = mean(tempjja), cover = mean(cover)) %>% 
                                     pull(cover)))) +
      
      # define appearance
      ggtitle(paste0(species)) +
      scale_colour_manual("soil\nmoisture", values = c("dodgerblue4", "orange1")) +
      scale_fill_manual("soil\nmoisture", values = c("dodgerblue4", "orange1")) +
      theme_cowplot(18) +
      theme(plot.title = element_text(colour = "grey10", face = "italic", size = 18),
            axis.title = element_blank(),
            legend.position = "none")

    
  return(int_plot)

}

interaction_plots_groups <- function(fgroup) {
  
  # # test
  # fgroup <- "all shrubs"
  
  if(fgroup == "all shrubs") model_coeff_output <- coeff.shrub_gradient.AllShr2
  if(fgroup == "evergreen shrubs") model_coeff_output <- coeff.shrub_gradient.AllEve2
  if(fgroup == "deciduous shrubs") model_coeff_output <- coeff.shrub_gradient.AllDec2
  
  
  if(fgroup == "all shrubs") group_df <- AllShr.tot
  if(fgroup == "evergreen shrubs") group_df <- AllEve.tot
  if(fgroup == "deciduous shrubs") group_df <- AllDec.tot
  
  # define initial predictions df
  phats <- as.data.frame(matrix(data = NA,
                                nrow = 100, 
                                ncol = length(model_coeff_output) + 2))
  names(phats)[1:length(model_coeff_output)] <- names(model_coeff_output)
  
  # extract predictions into phats data frame 
  phat_predictor <- "phat_tempXmoist"
  
  predictor_min <- min(group_df$tempjjaC)
  predictor_max <- max(group_df$tempjjaC)
  
  # assemble predicted and predictor values, for 100 rows (one predictor) at a time
  phats <- model_coeff_output %>% 
    
    # filter for predicted values
    filter(param %in% c(paste0(phat_predictor,
                               "[", 
                               rep(seq(from = 1, to = 100), times = 2),
                               ",",
                               rep(seq(from = 1, to = 2), each = 100), 
                               "]"))) %>% 
    
    # add xhats column
    mutate(xhats = rep(seq(from = predictor_min, 
                           to = predictor_max,
                           length.out = 100), times = 2)) %>% 
    
    # add column for back-centered and back-scaled values
    mutate(tempjja = xhats * attr(scale(group_df$tempjja), 'scaled:scale') + attr(scale(group_df$tempjja), 'scaled:center'),
           
           # add column for low/high moisture level values
           tcws = rep(c("low", "high"),
                      each = 100))
  
  # graph
  
  int_plot <- ggplot() +
    # tempjja is modelled at plotgroup level, so reduce base data (points layer) to plotgroup level
    geom_point(data = group_df %>% group_by(site_alt_plotgroup_id) %>% summarise(tempjja = mean(tempjja), cover = mean(cover)), 
               aes(x = tempjja, 
                   y = cover), 
               size = 2,
               position = position_jitter(width=0, height=.001),
               alpha = 0.5) +
    
    # draw line of predicted values
    geom_line(data = phats, 
              aes(x = tempjja, 
                  y = exp(mean),
                  colour = tcws), 
              alpha = 1,
              size = 3) + 
    
    # draw predicted 95% CI
    geom_ribbon(data = phats,
                aes(x = tempjja, 
                    ymin = exp(l95), 
                    ymax = exp(u95),
                    fill = tcws),
                alpha = 0.2) +
    
    # set y range limits so ribbons are not cut off
    coord_cartesian(ylim = c(0, 
                             max(group_df %>% 
                                   group_by(site_alt_plotgroup_id) %>% 
                                   summarise(tempjja = mean(tempjja), cover = mean(cover)) %>% 
                                   pull(cover)))) +
    
    # define appearance
    ggtitle(paste0(fgroup)) +
    scale_colour_manual("soil\nmoisture", values = c("dodgerblue4", "orange1")) +
    scale_fill_manual("soil\nmoisture", values = c("dodgerblue4", "orange1")) +
    theme_cowplot(18) +
    theme(plot.title = element_text(colour = "grey10", face = "italic", size = 18),
          axis.title = element_blank())
  
  
  return(int_plot)
  
}

# >> load data ----

# species
# load model output data
model_outputs_focal_species <- file.path("data", "model_outputs", "species", list.files(path = file.path("data", "model_outputs", "species"), pattern = "*2.Rdata"))
for (model_output in model_outputs_focal_species){
  load(model_output)
}
# load input data
load(file = file.path("data", "model_input_data", "shrub_gradient_species.datasets.Rdata"))

# groups
# load model output data
model_outputs_groups <- file.path("data", "model_outputs", "groups", list.files(path = file.path("data", "model_outputs", "groups"), pattern = "*2.Rdata"))
for (model_output in model_outputs_groups){
  load(model_output)
}
# load input data
load(file = file.path("data", "model_input_data", "shrub_gradient_group.datasets.Rdata"))


# >> plot graphs ----
(int_plot_AllShr <- interaction_plots_groups(fgroup = "all shrubs"))
(int_plot_AllEve <- interaction_plots_groups(fgroup = "evergreen shrubs"))
(int_plot_AllDec <- interaction_plots_groups(fgroup = "deciduous shrubs"))
(int_plot_BetNan <- interaction_plots_species(species = "Betula nana"))
(int_plot_EmpNig <- interaction_plots_species(species = "Empetrum nigrum"))
(int_plot_RhoGro <- interaction_plots_species(species = "Rhododendron groenlandicum"))
(int_plot_SalGla <- interaction_plots_species(species = "Salix glauca"))
(int_plot_VacUli <- interaction_plots_species(species = "Vaccinium uliginosum"))

# extract legend
legend_int_plot <- get_legend(int_plot_AllShr + theme(legend.box.margin = margin(t = 70, l = 70)))

# make x- and y-axis label
xlabel <- ggdraw() +
  draw_label("mean summer temperature [°C]",
             hjust = 0.5,
             size = 20,
             fontface = "bold")

ylabel <- ggdraw() +
  draw_label("cover per plot group",
             vjust = 0,
             angle = 90,
             size = 20,
             fontface = "bold")

# combine plot and ylabel
(nuuk_interactions_row <- plot_grid(ylabel,
                                   plot_grid(int_plot_AllShr + theme(legend.position = "none"),
                                             int_plot_AllEve + theme(legend.position = "none"),
                                             int_plot_AllDec + theme(legend.position = "none"),
                                             int_plot_BetNan, 
                                             int_plot_EmpNig, 
                                             int_plot_RhoGro, 
                                             int_plot_SalGla,
                                             int_plot_VacUli,
                                             legend_int_plot,
                                             labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)"),
                                             label_size = 18,
                                             label_fontface = "bold",
                                             label_x = c(rep(0.02, 8)),
                                             ncol = 3,
                                             axis = "lt",
                                             align = "hv"),
                                   rel_widths = c(.03, 1)
                                   ))

# add xlabel
(nuuk_interaction_plot_grid_ver <- plot_grid(nuuk_interactions_row,
                                             xlabel,
                                             ncol = 1,
                                             rel_heights = c(1, 0.05)))

# save plot
# save_plot(file.path("figures", "nuuk_shrub_drivers_interaction_panels_vert.pdf"),
#           nuuk_interaction_plot_grid_ver, base_height = 15, base_aspect_ratio = 1)


# ________________________________________ ----

# 7) Effect size plots ----

# for cases with only 'significant' effects
effectsize_plot <- function(species, plot_width) {
  
  if(species == "all shrubs") model_coeff_output <- coeff.shrub_gradient.AllShr2
  if(species == "evergreen shrubs") model_coeff_output <- coeff.shrub_gradient.AllEve2
  if(species == "deciduous shrubs") model_coeff_output <- coeff.shrub_gradient.AllDec2
  if(species == "Betula nana") model_coeff_output <- coeff.shrub_gradient.BetNan2
  if(species == "Empetrum nigrum") model_coeff_output <- coeff.shrub_gradient.EmpNig2
  if(species == "Rhododendron groenlandicum") model_coeff_output <- coeff.shrub_gradient.RhoGro2
  if(species == "Salix glauca") model_coeff_output <- coeff.shrub_gradient.SalGla2
  if(species == "Vaccinium uliginosum") model_coeff_output <- coeff.shrub_gradient.VacUli2
  
  target_vars <- c("b.tempjja.x", "b.tempjja.x2",
                   "b.tempcont.x", "b.tempcont.x2",
                   "b.precipjja.x", "b.precipjja.x2",
                   "b.sri",
                   "b.tri",
                   "b.tcws", 
                   "b.shrub_cov",
                   "b.gramin_cov",
                   "b.compet")
  solutions <- model_coeff_output
  names(solutions) <- c("variable", "post.mean", "post.sd", "l95", "l90", "u90", "u95", "Rhat")
  solutions <- solutions %>% 
    filter(variable %in% target_vars)
  solutions$variable <- fct_relevel(solutions$variable,
                                    "b.tempjja.x", "b.tempjja.x2",
                                    "b.tempcont.x", "b.tempcont.x2",
                                    "b.precipjja.x", "b.precipjja.x2",
                                    "b.sri",
                                    "b.tri",
                                    "b.tcws",
                                    "b.compet",
                                    "b.shrub_cov",
                                    "b.gramin_cov")
  # comment next bit ou if solved the labelling issue for discrete axis in the plotting code below
  solutions$variable <- fct_recode(solutions$variable,
                                   "summer temperature" = "b.tempjja.x",
                                   "summer temperature ^2" = "b.tempjja.x2",
                                   "temperature variability" = "b.tempcont.x",
                                   "temperature variability ^2" = "b.tempcont.x2",
                                   "summer precipitation" = "b.precipjja.x", 
                                   "summer precipitation ^2" = "b.precipjja.x2",
                                   "solar radiation" = "b.sri",
                                   "terrain ruggedness" = "b.tri",
                                   "moisture availability" = "b.tcws",
                                   "competition" = "b.compet",
                                   " other shrub cover" = "b.shrub_cov",
                                   "graminoid cover" = "b.gramin_cov")
  solutions <- solutions[order(solutions$variable),]
  min_value <- floor(min(solutions$l95))
  max_value <- ceiling(max(solutions$u95))
  solutions$sig <- "ns"
  solutions$sig[solutions$l95 < 0 & solutions$u95 < 0] <- "sig"
  solutions$sig[solutions$l95 > 0 & solutions$u95 > 0] <- "sig"
  solutions$sig[solutions$l90 < 0 & solutions$u90 < 0 & solutions$l95 < 0 & solutions$u95 > 0] <- "marg"
  solutions$sig[solutions$l90 > 0 & solutions$u90 > 0 & solutions$l95 < 0 & solutions$u95 > 0] <- "marg"
  solutions$sig <- ordered(solutions$sig, levels = c("ns", "sig", "marg"))
  label_colour <- rep("black", nrow(solutions))
  label_colour[solutions$sig == "sig"] <- theme_darkgreen
  label_colour[solutions$sig == "marg"] <- theme_purple
  label_face <- rep("plain", nrow(solutions))
  label_face[solutions$sig == "sig"] <- "bold"
  label_face[solutions$sig == "marg"] <- "bold"
  title_string <- species
  title_colour <- "grey10"
  
  
  model_plot <- ggplot(solutions, aes(x = variable, y = post.mean,
                                      ymin = l95, ymax = u95,
                                      colour = sig)) +
    geom_point() +
    geom_errorbar(width = .8) +
    theme_cowplot(18) +
    ylab("Effect Size (scaled)") +
    xlab("") +
    ggtitle(paste0(title_string)) +
    scale_colour_manual(values = c("black", theme_darkgreen, theme_purple)) +
    scale_y_continuous(limits = c(min_value, max_value), breaks = seq(min_value,max_value,0.5)) +
    # if including limits, it's not unused variable names are not dropped
    # scale_x_discrete(
    #   limits = c("b.tempjja.x", "b.tempjja.x2",
    #              "b.tempcont.x", "b.tempcont.x2",
    #              "b.precipjja.x", "b.precipjja.x2",
    #              "b.sri",
    #              "b.tri",
    #              "b.tcws",
    #              "b.compet",
    #              "b.shrub_cov",
    #              "b.gramin_cov"),
    # # if specifying labels like below, the same labels are used irrespective of which are plotted
    #   labels = c("summer temperature", bquote(.("summer") *" "* temperature^2),
    #              "temperature variability", bquote(.("temperature") *" "* variability^2),
    #              "summer precipitation", bquote(.("summer") *" "* precipitation^2),
    #              "solar radiation",
    #              "terrain ruggedness",
    #              "moisture availability",
    #              "competition",
    #              "other shrub cover",
    #              "graminoid cover")) +
    annotate("segment", x = 0, xend = plot_width, y = 0, yend = 0) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = label_colour, face = label_face),
          plot.title = element_text(colour = title_colour, face = "italic"),
          legend.position = "none")
  return(model_plot)
}


# >> load data ----

# species
# load model output data
model_outputs_focal_species <- file.path("data", "model_outputs", "species", list.files(path = file.path("data", "model_outputs", "species"), pattern = "*2.Rdata"))
for (model_output in model_outputs_focal_species){
  load(model_output)
}

# groups
# load model output data
model_outputs_groups <- file.path("data", "model_outputs", "groups", list.files(path = file.path("data", "model_outputs", "groups"), pattern = "*2.Rdata"))
for (model_output in model_outputs_groups){
  load(model_output)
}


# >> plot graphs ----
(es_plot_AllShr <- effectsize_plot(species = "all shrubs", plot_width = 7.5))
(es_plot_AllEve <- effectsize_plot(species = "evergreen shrubs", plot_width = 8.5))
(es_plot_AllDec <- effectsize_plot(species = "deciduous shrubs", plot_width = 8.5))
(es_plot_BetNan <- effectsize_plot(species = "Betula nana", plot_width = 9.5))
(es_plot_EmpNig <- effectsize_plot(species = "Empetrum nigrum", plot_width = 9.5))
(es_plot_RhoGro <- effectsize_plot(species = "Rhododendron groenlandicum", plot_width = 10.5))
(es_plot_SalGla <- effectsize_plot(species = "Salix glauca", plot_width = 9.5))
(es_plot_VacUli <- effectsize_plot(species = "Vaccinium uliginosum", plot_width = 9.5))

# extract legend
# legend_int_plot <- get_legend(int_plot_BetNan + theme(legend.box.margin = margin(t = 70, l = 70)))

# 3x2 grid (vertical layout)
(nuuk_effect_size_plot_grid_ver <- plot_grid(es_plot_AllShr + theme(legend.position = "none", 
                                                                     axis.title.x = element_blank()),
                                             es_plot_AllEve + theme(legend.position = "none", 
                                                                     axis.title = element_blank()),
                                             es_plot_AllDec + theme(legend.position = "none", 
                                                                     axis.title.x = element_blank()),
                                             es_plot_BetNan + theme(legend.position = "none", 
                                                                     axis.title = element_blank()), 
                                             es_plot_EmpNig + theme(legend.position = "none", 
                                                                     axis.title.x = element_blank()), 
                                             es_plot_RhoGro + theme(legend.position = "none", 
                                                                     axis.title = element_blank()), 
                                             es_plot_SalGla + theme(legend.position = "none",
                                                                     axis.title.x = element_blank()),
                                             es_plot_VacUli + theme(legend.position = "none",
                                                                     axis.title = element_blank()),
                                             labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)"),
                                             label_size = 20,
                                             label_fontface = "plain",
                                             label_x = c(.05, .05, .05, .05, .05, .05, .05, .05),
                                             ncol = 2,
                                             axis = "lt",
                                             align = "hv"))

# save plot
# save_plot(file.path("figures", "nuuk_shrub_drivers_effect_size_panels_vert_prettylabels.pdf"),
#           nuuk_effect_size_plot_grid_ver, base_height = 22, base_aspect_ratio = .6)


# ________________________________________ ----

# 8) Tundra species traits PCA scores ----
# scores extracted by Anne D. Bjorkman from Thomas et al. 2019 GEB, https://doi.org/10.1111/geb.12783

traits_scores <- read.csv(file.path("data", "Tundra_species_PCA_scores.csv"),
                          sep = ",",
                          header = TRUE)

# calculate average graminoid score
traits_scores_gram <- traits_scores %>% 
  
  # filter for graminoid genera present in Greenland according to Bøcher et al. 1968
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


# create labels for x axis
library(grid)
text_right <- textGrob("acquisitive", gp = gpar(fontsize = 13, fontface = "bold"))
text_left <- textGrob("conservative", gp = gpar(fontsize = 13, fontface = "bold"))


# plot PC1 scores
(traits_scores_plot <- ggplot(data = traits_scores_nuuk,
                              aes(x = PC1,
                                  y = 0,
                                  colour = fgroup)) +
  # plot scores
  geom_point(aes(colour = PC1),
             shape = 18,
             size = 4) +
    
  scale_color_gradientn(colours = c(theme_red, theme_darkblue)) +
    
  # add species names
  geom_text(aes(label = species,
                angle = 90,
                y = .1),
            hjust = 0,
            vjust = ifelse(traits_scores_nuuk$species == "Rhododendron sp.", 0.8, 0.375),
            colour = "grey30",
            fontface = ifelse(traits_scores_nuuk$species == "graminoids mean", "plain", "italic")) +
    
  # add traits labels
  annotation_custom(text_right, 
                    xmin = -0.4, xmax = -0.4, ymin = -0.4, ymax = -0.4) + 
  annotation_custom(text_left,
                    xmin = -3.55, xmax = -3.55, ymin = -0.4, ymax = -0.4) +
    
  # set appearance
  theme_classic() +
    
  coord_cartesian(xlim = c(-3.7, -0.3), 
                  ylim = c(0, 1.2), 
                  clip = "off") +
    
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0, 0.5, 1, 0.5), "cm"))

)


# save plot
# save_plot(file.path("figures", "nuuk_shrub_drivers_species_PCA_scores.pdf"),
#           traits_scores_plot, base_height = 3, base_aspect_ratio = 2.5)


# ________________________________________ ----
# ________________________________________ ----


# The Farm ----

# 3x2 grid (vertical layout) ----
(nuuk_interaction_plot_grid_ver <- plot_grid(int_plot_AllShr, 
                                             int_plot_AllEve, 
                                             int_plot_AllDec, 
                                             labels = c("a)", "b)", "c)"),
                                             label_size = 20,
                                             label_fontface = "plain",
                                             hjust = 0,
                                             ncol = 2))

# save plot
# save_plot(file.path("figures", "nuuk_shrub_drivers_interaction_groups_panels_vert.pdf"),
#           nuuk_interaction_plot_grid_ver, base_height = 15, base_aspect_ratio = 0.8)

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