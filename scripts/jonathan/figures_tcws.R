# Temperature variability, moisture and biotic interactions drive shrub species abundance along a coastal-inland gradient in arctic Greenland

# von Oppen, J. et al. 2020

# Figure code

# Jonathan von Oppen, Aarhus University, Sept 2020

# contact: jonathan.vonoppen@bios.au.dk



rm(list = ls())

# Dependencies ####
if (!require('pacman')) install.packages('pacman', repos="https://cloud.r-project.org")
pacman::p_load(tidyverse,
               tidylog,
               patchwork,
               cowplot)

# Colour scheme ####
theme_red <- "#E64447FF"
theme_darkblue <- "#1D5799"
theme_darkgreen <- "#13944D"
theme_lightpurple <- "#b774e8"
theme_purple <- "#8757B3"

# ________________________________________ ----

# Supplementary figures based on TCWS ----
# ________________________________________ ----


# Fig. S XXX.1) Effect size plots (TCWS) ----

# >> load function ----
source(file = file.path("scripts", "jonathan", "plotting_functions", "effectsize_plot.R"))


# >> load data ----

# species
# load model output data
model_outputs_focal_species <- file.path("data", "processed", "model_outputs", "species_tcws", list.files(path = file.path("data", "processed", "model_outputs", "species_tcws"), pattern = "*2.Rdata"))
for (model_output in model_outputs_focal_species){
  load(model_output)
}

# groups
# load model output data
model_outputs_groups <- file.path("data", "processed", "model_outputs", "groups_tcws", list.files(path = file.path("data", "processed", "model_outputs", "groups_tcws"), pattern = "*2.Rdata"))
for (model_output in model_outputs_groups){
  load(model_output)
}


# >> plot graphs ----
(es_plot_AllShr <- effectsize_plot_tcws(species = "all shrubs", plot_width = 8.5))
(es_plot_AllEve <- effectsize_plot_tcws(species = "evergreen shrubs", plot_width = 10.5))
(es_plot_AllDec <- effectsize_plot_tcws(species = "deciduous shrubs", plot_width = 9.5))
(es_plot_BetNan <- effectsize_plot_tcws(species = "Betula nana", plot_width = 11.5))
(es_plot_EmpNig <- effectsize_plot_tcws(species = "Empetrum nigrum", plot_width = 11.5))
(es_plot_RhoGro <- effectsize_plot_tcws(species = "Rhododendron groenlandicum", plot_width = 11.5))
(es_plot_SalGla <- effectsize_plot_tcws(species = "Salix glauca", plot_width = 11.5))
(es_plot_VacUli <- effectsize_plot_tcws(species = "Vaccinium uliginosum", plot_width = 11.5))

# make y label
ylabel_es <- ggdraw() +
  draw_label("Effect size (scaled)",
             vjust = 0,
             angle = 90,
             size = 20,
             fontface = "bold")

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


# >> save plot ----
# # ¨¨¨¨4x2 grid (vertical layout) ----
# (nuuk_effect_size_plot_grid_ver <- plot_grid(es_plot_AllShr + theme(legend.position = "none", 
#                                                                     axis.title = element_blank()),
#                                              es_plot_AllEve + theme(legend.position = "none", 
#                                                                     axis.title = element_blank()),
#                                              es_plot_AllDec + theme(legend.position = "none", 
#                                                                     axis.title = element_blank()),
#                                              es_plot_EmpNig + theme(legend.position = "none", 
#                                                                     axis.title = element_blank()), 
#                                              es_plot_RhoGro + theme(legend.position = "none", 
#                                                                     axis.title = element_blank()), 
#                                              es_plot_BetNan + theme(legend.position = "none", 
#                                                                     axis.title = element_blank()), 
#                                              es_plot_VacUli + theme(legend.position = "none",
#                                                                     axis.title = element_blank()),
#                                              es_plot_SalGla + theme(legend.position = "none",
#                                                                     axis.title = element_blank()),
#                                              labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)"),
#                                              label_size = 20,
#                                              label_fontface = "plain",
#                                              label_x = c(.05, .05, .05, .05, .05, .05, .05, .05),
#                                              ncol = 2,
#                                              axis = "lt",
#                                              align = "hv"))
# 
# # save plot
# # save_plot(file.path("figures", "nuuk_shrub_drivers_effect_size_panels_vert_prettylabels.pdf"),
# #           nuuk_effect_size_plot_grid_ver, base_height = 22, base_aspect_ratio = .6)


# ¨¨¨¨3x3 grid (horizontal layout) ----
(es_plots_row_tcws <- plot_grid(ylabel_es,
                                plot_grid(es_plot_AllShr,
                                          es_plot_AllEve,
                                          es_plot_AllDec,
                                          es_plot_EmpNig, 
                                          es_plot_RhoGro, 
                                          es_plot_BetNan, 
                                          es_plot_VacUli,
                                          es_plot_SalGla,
                                          labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)"),
                                          label_size = 20,
                                          label_fontface = "bold",
                                          label_x = c(rep(.02, 8)),
                                          ncol = 3,
                                          axis = "lt",
                                          align = "hv"),
                                rel_widths = c(0.03, 1)))

# combine with legend
(nuuk_effect_size_plot_grid_hor_tcws <- plot_grid(es_plots_row_tcws,
                                                  legend_hor,
                                                  ncol = 1,
                                                  rel_heights = c(1, 0.07)))

# save plot
save_plot(file.path("figures", "nuuk_shrub_drivers_effect_size_panels_hor_prettylabels_tcws.png"),
          nuuk_effect_size_plot_grid_hor_tcws, base_height = 18, base_aspect_ratio = 1.3)


# ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨-----


# Fig. S XXX.2) Interaction plots temp X moisture (TCWS) ----

# >> load function ----
source(file = file.path("scripts", "jonathan", "plotting_functions", "interaction_plot_tempXwetness.R"))


# >> load data ----

# species
# load model output data
model_outputs_focal_species <- file.path("data", "processed", "model_outputs", "species_tcws", list.files(path = file.path("data", "processed", "model_outputs", "species_tcws"), pattern = "*2.Rdata"))
for (model_output in model_outputs_focal_species){
  load(model_output)
}
# load input data
load(file = file.path("data", "processed", "model_input_data_tcws", "shrub_gradient_species.datasets.Rdata"))

# groups
# load model output data
model_outputs_groups <- file.path("data", "processed", "model_outputs", "groups_tcws", list.files(path = file.path("data", "processed", "model_outputs", "groups_tcws"), pattern = "*2.Rdata"))
for (model_output in model_outputs_groups){
  load(model_output)
}
# load input data
load(file = file.path("data", "processed", "model_input_data_tcws", "shrub_gradient_group.datasets.Rdata"))


# >> plot graphs ----
(int_plot_AllShr <- interaction_plots_groups(fgroup = "all shrubs", wet.var = "TCWS"))
(int_plot_AllEve <- interaction_plots_groups(fgroup = "evergreen shrubs", wet.var = "TCWS"))
(int_plot_AllDec <- interaction_plots_groups(fgroup = "deciduous shrubs", wet.var = "TCWS"))
(int_plot_BetNan <- interaction_plots_species(species = "Betula nana", wet.var = "TCWS"))
(int_plot_EmpNig <- interaction_plots_species(species = "Empetrum nigrum", wet.var = "TCWS"))
(int_plot_RhoGro <- interaction_plots_species(species = "Rhododendron groenlandicum", wet.var = "TCWS"))
(int_plot_SalGla <- interaction_plots_species(species = "Salix glauca", wet.var = "TCWS"))
(int_plot_VacUli <- interaction_plots_species(species = "Vaccinium uliginosum", wet.var = "TCWS"))

# extract legend
legend_int_plot <- get_legend(int_plot_AllShr + 
                                theme(legend.box.margin = margin(t = 70, l = 70)))

# make x- and y-axis label
xlabel <- ggdraw() +
  draw_label("mean summer temperature [°C]",
             hjust = 0.5,
             size = 20,
             fontface = "bold")

ylabel <- ggdraw() +
  draw_label("relative abundance per plot group",
             vjust = 0,
             angle = 90,
             size = 20,
             fontface = "bold")

# combine plot and ylabel
(nuuk_interactions_row <- plot_grid(ylabel,
                                    plot_grid(int_plot_AllShr + theme(legend.position = "none"),
                                              int_plot_AllEve + theme(legend.position = "none"),
                                              int_plot_AllDec + theme(legend.position = "none"),
                                              int_plot_EmpNig, 
                                              int_plot_RhoGro, 
                                              int_plot_BetNan, 
                                              int_plot_VacUli,
                                              int_plot_SalGla,
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
(nuuk_interaction_plot_grid_ver_tcws <- plot_grid(nuuk_interactions_row,
                                             xlabel,
                                             ncol = 1,
                                             rel_heights = c(1, 0.05)))

# # >> save plot ----
# save_plot(file.path("figures", "nuuk_shrub_drivers_interaction_panels_vert_tcws.png"),
#           nuuk_interaction_plot_grid_ver_tcws, base_height = 15, base_aspect_ratio = 1)


# ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨-----


# Fig. S5ff) Groups: cover and predictions for all predictors ----

# >> load function ----
source(file = file.path("scripts", "jonathan", "plotting_functions", "prediction_plot.R"))


# >> load data ----
model_outputs_groups <- file.path("data", "processed", "model_outputs", "groups", list.files(path = file.path("data", "processed", "model_outputs", "groups"), 
                                                                                pattern = "*.Rdata"))
for (model_output in model_outputs_groups){
  load(model_output)
}
# load input data
load(file = file.path("data", "processed", "model_input_data", "shrub_gradient_group.datasets.Rdata"))


# >> plot graphs ----
(nuuk_prediction_plot_AllShr <- prediction_plots_groups(fgroup = "all shrub"))
(nuuk_prediction_plot_AllEve <- prediction_plots_groups(fgroup = "evergreen shrub"))
(nuuk_prediction_plot_AllDec <- prediction_plots_groups(fgroup = "deciduous shrub"))

# >> save plots ----
prediction_plots_path <- file.path("figures", "prediction_plots")
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_AllShr.pdf"),
#           nuuk_prediction_plot_AllShr, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_AllEve.pdf"),
#           nuuk_prediction_plot_AllEve, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_AllDec.pdf"),
#           nuuk_prediction_plot_AllDec, base_height = 15, base_aspect_ratio = 1)


# ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ ----


# Fig. S5.1ff) Groups: cover and predictions for all predictors ----

# >> load function ----
source(file = file.path("scripts", "jonathan", "plotting_functions", "prediction_plot_twi.R"))


# >> load data ----
model_outputs_groups <- file.path("data", "processed", "model_outputs", "groups_twi", list.files(path = file.path("data", "processed", "model_outputs", "groups"), 
                                                                                             pattern = "*.Rdata"))
for (model_output in model_outputs_groups){
  load(model_output)
}
# load input data
load(file = file.path("data", "processed", "model_input_data_twi", "shrub_gradient_group.datasets.Rdata"))


# >> plot graphs ----
(nuuk_prediction_plot_AllShr <- prediction_plots_groups(fgroup = "all shrubs"))
(nuuk_prediction_plot_AllEve <- prediction_plots_groups(fgroup = "evergreen shrubs"))
(nuuk_prediction_plot_AllDec <- prediction_plots_groups(fgroup = "deciduous shrubs"))

# >> save plots ----
prediction_plots_path <- file.path("figures", "prediction_plots_twi")
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_AllShr.png"),
#           nuuk_prediction_plot_AllShr, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_AllEve.png"),
#           nuuk_prediction_plot_AllEve, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_AllDec.png"),
#           nuuk_prediction_plot_AllDec, base_height = 15, base_aspect_ratio = 1)


# ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ ----


# Fig. S8ff) Species: cover and predictions for all predictors ----

# >> load function ----
source(file = file.path("scripts", "jonathan", "plotting_functions", "prediction_plot.R"))


# >> load data ----
# load model outputs
model_outputs_species <- file.path("data", "processed", "model_outputs", "species", list.files(path = file.path("data", "processed", "model_outputs", "species"), pattern = "*.Rdata"))
for (model_output in model_outputs_species){
  load(model_output)
}
# load input data
load(file = file.path("data", "processed", "model_input_data", "shrub_gradient_species.datasets.Rdata"))


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

# >> save plots ----
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


# Fig. S8.1ff) Species: cover and predictions for all predictors (TWI) ----

# >> load function ----
source(file = file.path("scripts", "jonathan", "plotting_functions", "prediction_plot_twi.R"))


# >> load data ----
# load model outputs
model_outputs_species <- file.path("data", "processed", "model_outputs", "species_twi", list.files(path = file.path("data", "processed", "model_outputs", "species_twi"), pattern = "*.Rdata"))
for (model_output in model_outputs_species){
  load(model_output)
}
# load input data
load(file = file.path("data", "processed", "model_input_data_twi", "shrub_gradient_species.datasets.Rdata"))


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

# >> save plots ----
prediction_plots_path <- file.path("figures", "prediction_plots_twi")
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_BetNan.png"),
#           nuuk_prediction_plot_BetNan, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_CasTet.png"),
#           nuuk_prediction_plot_CasTet, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_EmpNig.png"),
#           nuuk_prediction_plot_EmpNig, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_PhyCae.png"),
#           nuuk_prediction_plot_PhyCae, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_RhoGro.png"),
#           nuuk_prediction_plot_RhoGro, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_RhoTom.png"),
#           nuuk_prediction_plot_RhoTom, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_SalArc.png"),
#           nuuk_prediction_plot_SalArc, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_SalGla.png"),
#           nuuk_prediction_plot_SalGla, base_height = 15, base_aspect_ratio = 1)
# save_plot(file.path(prediction_plots_path, "nuuk_shrub_drivers_prediction_plot_VacUli.png"),
#           nuuk_prediction_plot_VacUli, base_height = 15, base_aspect_ratio = 1)



# ________________________________________ ----
# ________________________________________ ----
