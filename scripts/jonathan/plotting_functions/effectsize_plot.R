# Temperature variability, moisture and biotic interactions drive shrub species abundance along a coastal-inland gradient in arctic Greenland

# von Oppen, J. et al. 2020

# Effect size plot function

# Jonathan von Oppen, Aarhus University, Sept/Dec 2020



# for TWI ----

effectsize_plot_twi <- function(species, plot_width) {
  
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
                   "b.twi", 
                   "b.shrub_cov",
                   "b.gramin_cov",
                   "b.compet",
                   "b.tempXtcws", "b.tempXtcws2",
                   "b.tempXcompet", "b.tempXcompet2")
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
                                    "b.twi",
                                    "b.compet",
                                    "b.shrub_cov",
                                    "b.gramin_cov",
                                    "b.tempXtwi", "b.tempXtwi2",
                                    "b.tempXcompet", "b.tempXcompet2")
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
                                   "topographic wetness" = "b.twi",
                                   "summer temperature X\ntopographic wetness" = "b.tempXtwi", 
                                   "summer temperature ^2\nX topographic wetness" = "b.tempXtwi2",
                                   "dCWA" = "b.compet",
                                   "other shrub abundance" = "b.shrub_cov",
                                   "graminoid abundance" = "b.gramin_cov",
                                   "summer temperature\nX dCWA", "b.tempXcompet", 
                                   "summer temperature ^2\nX dCWA" = "b.tempXcompet2")
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
    #              "b.twi",
    #              "b.compet",
    #              "b.shrub_cov",
    #              "b.gramin_cov"),
  # # if specifying labels like below, the same labels are used irrespective of which are plotted
  #   labels = c("summer temperature", bquote(.("summer") *" "* temperature^2),
  #              "temperature variability", bquote(.("temperature") *" "* variability^2),
  #              "summer precipitation", bquote(.("summer") *" "* precipitation^2),
  #              "solar radiation",
  #              "terrain ruggedness",
  #              "topographic wetness",
  #              "dCWA",
  #              "other shrub cover",
  #              "graminoid cover")) +
  annotate("segment", x = 0, xend = plot_width, y = 0, yend = 0) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = label_colour, face = label_face),
          axis.title = element_blank(),
          plot.title = element_text(colour = title_colour, face = "italic"),
          legend.position = "none")
  return(model_plot)
}


# for TCWS ----

effectsize_plot_tcws <- function(species, plot_width) {
  
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
                   "b.compet",
                   "b.tempXtcws", "b.tempXtcws2",
                   "b.tempXcompet", "b.tempXcompet2")
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
                                    "b.tempXtcws", "b.tempXtcws2",
                                    "b.compet",
                                    "b.shrub_cov",
                                    "b.gramin_cov",
                                    "b.tempXcompet", "b.tempXcompet2")
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
                                   "Tasseled-cap wetness" = "b.tcws",
                                   "summer temperature X\nTasseled-cap wetness" = "b.tempXtwi", 
                                   "summer temperature ^2\nX Tasseled-cap wetness" = "b.tempXtwi2",
                                   "dCWA" = "b.compet",
                                   "other shrub abundance" = "b.shrub_cov",
                                   "graminoid abundance" = "b.gramin_cov",
                                   "summer temperature\nX dCWA", "b.tempXcompet", 
                                   "summer temperature ^2\nX dCWA" = "b.tempXcompet2")
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
  #              "Tasseled-cap wetness",
  #              "dCWA",
  #              "other shrub cover",
  #              "graminoid cover")) +
  annotate("segment", x = 0, xend = plot_width, y = 0, yend = 0) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = label_colour, face = label_face),
          axis.title = element_blank(),
          plot.title = element_text(colour = title_colour, face = "italic"),
          legend.position = "none")
  return(model_plot)
}

