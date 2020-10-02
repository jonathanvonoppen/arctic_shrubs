# Temperature variability, moisture and biotic interactions drive shrub species abundance along a coastal-inland gradient in arctic Greenland

# von Oppen, J. et al. 2020

# Prediction plot panel functions

# Jonathan von Oppen, Aarhus University, Sept 2020



# for species ----
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
  
  if(species == "Betula nana") figure_no <- "Fig. S8: "
  if(species == "Cassiope tetragona") figure_no <- "Fig. S9: "
  if(species == "Empetrum nigrum") figure_no <- "Fig. S10: "
  if(species == "Phyllodoce caerulea") figure_no <- "Fig. S11: "
  if(species == "Rhododendron groenlandicum") figure_no <- "Fig. S12: "
  if(species == "Rhododendron tomentosum") figure_no <- "Fig. S13: "
  if(species == "Salix arctophila") figure_no <- "Fig. S14: "
  if(species == "Salix glauca") figure_no <- "Fig. S15: "
  if(species == "Vaccinium uliginosum") figure_no <- "Fig. S16: "
  
  if(species == "Betula nana") convergence_string <- ""
  if(species == "Cassiope tetragona") convergence_string <- " (model not converged)"
  if(species == "Empetrum nigrum") convergence_string <- ""
  if(species == "Phyllodoce caerulea") convergence_string <- " (model not converged)"
  if(species == "Rhododendron groenlandicum") convergence_string <- ""
  if(species == "Rhododendron tomentosum") convergence_string <- " (model not converged)"
  if(species == "Salix arctophila") convergence_string <- " (model not converged)"
  if(species == "Salix glauca") convergence_string <- ""
  if(species == "Vaccinium uliginosum") convergence_string <- ""
  
  # 
  # # back-center and back-scale x data for the complete (not species-subsetted) dataset
  # num_pred <- env_cov_bio_sub %>% select(tempjja,
  #                                         tempcont,
  #                                         precipjja,
  #                                         sri,
  #                                         tcws,
  #                                         tri,
  #                                         ends_with("_cover"),
  #                                         matches("compet"))
  # for(i in 1:length(num_pred)){
  #   col <- colnames(num_pred[i])
  #   env_cov_bio_sub[paste0(col, "R")] <- as.numeric(attr(scale(env_cov_bio_sub[, col]), 'scaled:scale') + attr(scale(env_cov_bio_sub[, col]), 'scaled:center'))
  # }
  
  
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
    # attempt to do this using the back-scaled full (not species-subsetted) dataset
    # mutate(pred_values = xhats * (attr(scale(env_cov_bio_sub[, predictor]), 'scaled:scale')[which(env_cov_bio_sub$taxon == species)]) + (attr(scale(env_cov_bio_sub[, predictor]), 'scaled:center')[which(env_cov_bio_sub$taxon == species)]))
    
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
    draw_label(paste0(figure_no, species, " cover ~ predictors", convergence_string),
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
  
  # draw extra plot to extract legend from
  xplot <- ggplot(data = data.frame(x = c(1:6),
                                    y = c(1:6),
                                    z = c(rep("significant     ",2), rep("marginal     ",2),rep("n.s.",2))) %>% 
                    mutate(z = ordered(z, levels = c("significant     ", "marginal     ", "n.s."))),
                  aes(group = z)) +
    geom_line(aes(x = x,
                  y = y,
                  colour = z),
              size = 1) +
    geom_ribbon(aes(x = x,
                    ymin = y-.5,
                    ymax = y+.5,
                    fill = z),
                alpha = .2) +
    scale_colour_manual(values = c(theme_darkgreen, theme_purple, "black")) +
    scale_fill_manual(values = c(theme_darkgreen, theme_purple, "black")) +
    labs(colour = "significance level     ",
         fill = "significance level     ") +
    guides(colour = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 17))
  
  # extract legend
  legend_hor <- get_legend(xplot)
  
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
  
  # add title and legend
  pred_plot <- plot_grid(title,
                         pred_plots_row,
                         legend_hor,
                         ncol = 1,
                         rel_heights = c(0.1, 1, 0.07))
  
  
  return(pred_plot)
  
}


# for functional groups ----
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
  
  if(fgroup == "all shrub") figure_no <- "Fig. S5: "
  if(fgroup == "evergreen shrub") figure_no <- "Fig. S6: "
  if(fgroup == "deciduous shrub") figure_no <- "Fig. S7: "
  
  
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
    draw_label(paste0(figure_no, fgroup, " cover ~ predictors"),
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
  
  # draw extra plot to extract legend from
  xplot <- ggplot(data = data.frame(x = c(1:6),
                                    y = c(1:6),
                                    z = c(rep("significant     ",2), rep("marginal     ",2),rep("n.s.",2))) %>% 
                    mutate(z = ordered(z, levels = c("significant     ", "marginal     ", "n.s."))),
                  aes(group = z)) +
    geom_line(aes(x = x,
                  y = y,
                  colour = z),
              size = 1) +
    geom_ribbon(aes(x = x,
                    ymin = y-.5,
                    ymax = y+.5,
                    fill = z),
                alpha = .2) +
    scale_colour_manual(values = c(theme_darkgreen, theme_purple, "black")) +
    scale_fill_manual(values = c(theme_darkgreen, theme_purple, "black")) +
    labs(colour = "significance level     ",
         fill = "significance level     ") +
    guides(colour = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 17))
  
  # extract legend
  legend_hor <- get_legend(xplot)
  
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
                         legend_hor,
                         ncol = 1,
                         rel_heights = c(0.1, 1, .07))
  
  
  return(pred_plot)
  
}
