# Temperature variability, moisture and biotic interactions drive shrub species abundance along a coastal-inland gradient in arctic Greenland

# von Oppen, J. et al. 2020

# Interaction plot function

# Jonathan von Oppen, Aarhus University, Sept 2020



# for species ----

interaction_plots_species <- function(species, wet.var) {
  
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
    scale_colour_manual(paste0("wetness level (", wet.var, ")"), values = c("dodgerblue4", "orange1")) +
    scale_fill_manual(paste0("wetness level (", wet.var, ")"), values = c("dodgerblue4", "orange1")) +
    theme_cowplot(18) +
    theme(plot.title = element_text(colour = "grey10", face = "italic", size = 18),
          axis.title = element_blank(),
          legend.position = "none")
  
  
  return(int_plot)
  
}


# for functional groups ----
interaction_plots_groups <- function(fgroup, wet.var) {
  
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
    scale_colour_manual(paste0("wetness level (", wet.var, ")"), values = c("dodgerblue4", "orange1")) +
    scale_fill_manual(paste0("wetness level (", wet.var, ")"), values = c("dodgerblue4", "orange1")) +
    theme_cowplot(18) +
    theme(plot.title = element_text(colour = "grey10", face = "italic", size = 18),
          axis.title = element_blank(),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18))
  
  
  return(int_plot)
  
}

