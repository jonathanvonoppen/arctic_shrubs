

# load data
model_outputs_focal_species <- file.path("data", "processed", "model_outputs", "species_twi", list.files(path = file.path("data", "processed", "model_outputs", "species_twi"), pattern = "*2.Rdata"))
for (model_output in model_outputs_focal_species){
  load(model_output)
}

# fix row names
var_order <- c("intercept", 
               "b.tempjja.x", "b.tempjja.x2", "b.tempcont.x", "b.tempcont.x2", "b.precipjja.x", "b.precipjja.x2", 
               "b.sri", "b.tri", "b.twi", 
               "b.compet", "b.gramin_cov", "b.shrub_cov", 
               "phi", "sigma.plotgroup")
BetNan_output_table <- coeff.shrub_gradient.BetNan2 %>% 
  
  # filter for coefficients of interest
  filter(param %in% var_order) %>% 
  
  # reorder rows
  mutate(param = factor(param, levels = var_order)) %>% 
  arrange(param) %>%
  mutate(param = as.character(param)) %>% 
  
  # adjust parameter namings
  mutate(param = case_when(param == "b.compet" ~ "dCWA",
                           param == "b.gramin_cov" ~ "graminoid cover",
                           param == "b.precipjja.x" ~ "summer precipitation",
                           param == "b.precipjja.x2" ~ "summer precipitation ^2",
                           param == "b.shrub_cov" ~ "other shrub cover",
                           param == "b.sri" ~ "solar radiation",
                           param == "b.tempcont.x" ~ "temperature variability",
                           param == "b.tempcont.x2" ~ "temperature variability ^2",
                           param == "b.tempjja.x" ~ "summer temperature",
                           param == "b.tempjja.x" ~ "summer temperature ^2",
                           param == "b.tri" ~ "terrain ruggedness",
                           param == "b.twi" ~ "topographic wetness",
                           TRUE ~ param)) %>% 
  
  # build flextable
  flextable() %>% 
  theme_vanilla() %>% 
  colformat_num(j = c("Rhat"), digits = 2)
