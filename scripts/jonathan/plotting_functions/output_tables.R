# Temperature variability, moisture and biotic interactions drive shrub species abundance along a coastal-inland gradient in arctic Greenland

# von Oppen, J. et al. 2020

# Table export functions for model outputs

# Jonathan von Oppen, Aarhus University, Jan 2021 - jonathan.vonoppen [at] bio.au.dk


# species ----
output_table_sp <- function(species){
  
  # define output data
  if(species == "Betula nana") model_coeff_output <- coeff.shrub_gradient.BetNan2
  if(species == "Cassiope tetragona") model_coeff_output <- coeff.shrub_gradient.CasTet
  if(species == "Empetrum nigrum") model_coeff_output <- coeff.shrub_gradient.EmpNig2
  if(species == "Phyllodoce caerulea") model_coeff_output <- coeff.shrub_gradient.PhyCae
  if(species == "Rhododendron groenlandicum") model_coeff_output <- coeff.shrub_gradient.RhoGro2
  if(species == "Rhododendron tomentosum") model_coeff_output <- coeff.shrub_gradient.RhoTom
  if(species == "Salix arctophila") model_coeff_output <- coeff.shrub_gradient.SalArc
  if(species == "Salix glauca") model_coeff_output <- coeff.shrub_gradient.SalGla2
  if(species == "Vaccinium uliginosum") model_coeff_output <- coeff.shrub_gradient.VacUli2
  
  # set row name order
  var_order <- c("intercept", 
                 "b.tempjja.x", "b.tempjja.x2", "b.tempcont.x", "b.tempcont.x2", "b.precipjja.x", "b.precipjja.x2", 
                 "b.sri", "b.tri", "b.twi", "b.tcws", "b.tempXtwi", "b.tempXtwi2", "b.tempXtcws", "b.tempXtcws2",
                 "b.compet", "b.shrub_cov", "b.gramin_cov", "b.tempXcompet", "b.tempXcompet2",
                 "phi", "sigma.plotgroup")
  
  # wrangle output table
  model_coeff_output %>% 
    
    # filter for coefficients of interest
    filter(param %in% var_order) %>% 
    
    # reorder rows
    mutate(param = factor(param, levels = var_order)) %>% 
    arrange(param) %>%
    mutate(param = as.character(param)) %>% 
    
    # adjust parameter namings
    mutate(param = case_when(param == "b.compet" ~ "dCWA",
                             param == "b.gramin_cov" ~ "graminoid abundance",
                             param == "b.precipjja.x" ~ "summer precipitation",
                             param == "b.precipjja.x2" ~ "summer precipitation ^2",
                             param == "b.shrub_cov" ~ "other shrub abundance",
                             param == "b.sri" ~ "solar radiation",
                             param == "b.tempcont.x" ~ "temperature variability",
                             param == "b.tempcont.x2" ~ "temperature variability ^2",
                             param == "b.tempjja.x" ~ "summer temperature",
                             param == "b.tempjja.x2" ~ "summer temperature ^2",
                             param == "b.tempXtwi" ~ "summer temp. X wetness",
                             param == "b.tempXtwi2" ~ "summer temp. ^2 X wetness",
                             param == "b.tempXtcws" ~ "summer temp. X wetness",
                             param == "b.tempXtcws2" ~ "summer temp. ^2 X wetness",
                             param == "b.tempXcompet" ~ "summer temp. X dCWA",
                             param == "b.tempXcompet2" ~ "summer temp. ^2 X dCWA",
                             param == "b.tri" ~ "terrain ruggedness",
                             param == "b.twi" ~ "topographic wetness",
                             param == "b.tcws" ~ "Tasseled-cap wetness",
                             TRUE ~ param)) %>% 
    
    # build flextable
    flextable() %>% 
    
    # set simple b/w theme w/ right-aligned num cols
    theme_vanilla() %>% 
    # define NA string
    # set_formatter_type(na_str = "--") %>% 
    #colformat_num(na_str = "--") %>% 
    # set digits for all cols to 2 and for Rhat col to 3
    set_formatter(mean = function(x) sprintf("%.02f", x),
                  sd = function(x) sprintf("%.02f", x),
                  l95 = function(x) sprintf("%.02f", x),
                  l90 = function(x) sprintf("%.02f", x),
                  u90 = function(x) sprintf("%.02f", x),
                  u95 = function(x) sprintf("%.02f", x),
                  Rhat = function(x) sprintf("%.03f", x)) %>% 
    # define width of table
    set_table_properties(width = .8, layout = "autofit") %>% 
    # remove inner borders
    border_inner_h(border = officer::fp_border(color = "white")) %>% 
    # set padding for better readability
    padding(padding = 6, part = "all") %>% 
    # adjust font size
    fontsize(size = 10, part = "all") #%>% 
  
    # set caption
    #set_caption("")
  
}

# functional groups ----
output_table_gr <- function(fgroup){
  
  # define output data
  if(fgroup == "all shrubs") model_coeff_output <- coeff.shrub_gradient.AllShr2
  if(fgroup == "evergreen shrubs") model_coeff_output <- coeff.shrub_gradient.AllEve2
  if(fgroup == "deciduous shrubs") model_coeff_output <- coeff.shrub_gradient.AllDec2
  
  # set row name order
  var_order <- c("intercept", 
                 "b.tempjja.x", "b.tempjja.x2", "b.tempcont.x", "b.tempcont.x2", "b.precipjja.x", "b.precipjja.x2", 
                 "b.sri", "b.tri", "b.twi", "b.tcws", "b.tempXtwi", "b.tempXtwi2", "b.tempXtcws", "b.tempXtcws2",
                 "b.gramin_cov", 
                 "phi", "sigma.plotgroup")
  
  # wrangle output table
  model_coeff_output %>% 
    
    # filter for coefficients of interest
    filter(param %in% var_order) %>% 
    
    # reorder rows
    mutate(param = factor(param, levels = var_order)) %>% 
    arrange(param) %>%
    mutate(param = as.character(param)) %>% 
    
    # adjust parameter namings
    mutate(param = case_when(param == "b.gramin_cov" ~ "graminoid abundance",
                             param == "b.precipjja.x" ~ "summer precipitation",
                             param == "b.precipjja.x2" ~ "summer precipitation ^2",
                             param == "b.sri" ~ "solar radiation",
                             param == "b.tempcont.x" ~ "temperature variability",
                             param == "b.tempcont.x2" ~ "temperature variability ^2",
                             param == "b.tempjja.x" ~ "summer temperature",
                             param == "b.tempjja.x2" ~ "summer temperature ^2",
                             param == "b.tempXtwi" ~ "summer temp. X wetness",
                             param == "b.tempXtwi2" ~ "summer temp. ^2 X wetness",
                             param == "b.tempXtcws" ~ "summer temp. X wetness",
                             param == "b.tempXtcws2" ~ "summer temp. ^2 X wetness",
                             param == "b.tri" ~ "terrain ruggedness",
                             param == "b.twi" ~ "topographic wetness",
                             param == "b.tcws" ~ "Tasseled-cap wetness",
                             TRUE ~ param)) %>% 
    
    # build flextable
    flextable() %>% 
    
    # set simple b/w theme w/ right-aligned num cols
    theme_vanilla() %>% 
    # define NA string
    # set_formatter_type(na_str = "--") %>% 
    #colformat_num(na_str = "--") %>% 
    # set digits for all cols to 2 and for Rhat col to 3
    set_formatter(mean = function(x) sprintf("%.02f", x),
                  sd = function(x) sprintf("%.02f", x),
                  l95 = function(x) sprintf("%.02f", x),
                  l90 = function(x) sprintf("%.02f", x),
                  u90 = function(x) sprintf("%.02f", x),
                  u95 = function(x) sprintf("%.02f", x),
                  Rhat = function(x) sprintf("%.03f", x)) %>% 
    # define width of table
    set_table_properties(width = .8, layout = "autofit") %>% 
    # remove inner borders
    border_inner_h(border = officer::fp_border(color = "white")) %>% 
    # set padding for better readability
    padding(padding = 6, part = "all") %>% 
    # adjust font size
    fontsize(size = 10, part = "all") #%>% 
  
  # set caption
  #set_caption("")
  
}