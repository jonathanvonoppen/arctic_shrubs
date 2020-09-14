# INPUT DATA
jags.data <- list(
  # ALL THE OTHER DATA INPUT STUFF
  xhat = seq(from = min(SalGla.tot$tempcont_ts_30C), to = max(SalGla.tot$tempcont_ts_30C), length.out = 100), # subset from scaled and centered data with length 100)
  Nxhat = 100 # number of xhats (could define through length())
)


# DERIVED PARAMETERS
# (added to bottom of model script)
  for (m in 1:Nxhat){
    phat[m] <- intercept + b.tempcont.x * xhat[m]
  }


# COEFFICIENTS EXTRACTION
coeff.shrub_gradient.SalGla2.xhat <- model_out.shrub_gradient.SalGla2.xhat$BUGSoutput$summary %>% 
  as.data.frame %>% 
  select('mean','sd','2.5%','97.5%','Rhat') %>% 
  # add identifying info to data frame
  rownames_to_column(var = "param")


# reorder and rename cols
coeff.shrub_gradient.SalGla2.xhat <- coeff.shrub_gradient.SalGla2.xhat %>% 

  select(param, mean, sd, 
         l95 = "2.5%",
         u95 = "97.5%",
         Rhat) %>% print

# assemble predicted and predictor values
phats <- coeff.shrub_gradient.SalGla2.xhat %>% 
  
  # filter for predicted values
  filter(param %in% c(paste0("phat[", seq(from = 1, to = 100), "]"))) %>% 
  
  # add xhats column
  mutate(xhats = seq(from = min(SalGla.tot$tempcont_ts_30C),
                     to = max(SalGla.tot$tempcont_ts_30C),
                     length.out = 100))

# back-center and back-scale predictor values (xhats)
phats$tempcont <- phats$xhats*attr(scale(SalGla.tot$tempcont_ts_30), 'scaled:scale') + attr(scale(SalGla.tot$tempcont_ts_30), 'scaled:center') 


# GRAPH
ggplot() +
  # tempcont is modelled at plotgroup level, so reduce base data (points layer) to plotgroup level
  geom_point(data = SalGla.tot %>% group_by(site_alt_plotgroup_id) %>% summarise(tempcont = mean(tempcont_ts_30), cover = mean(cover)), 
             aes(x = tempcont, 
                 y = cover), 
             size = 2,
             position = position_jitter(width=0, height=.01),
             alpha=0.5) +
  
  # draw line of predicted values
  geom_line(data = phats, 
            aes(x = tempcont, 
                y = plogis(mean)), 
            colour = "orange",
            alpha = 1,
            size = 3) + 
  
  # draw predicted 95% CI
  geom_ribbon(data = phats,
              aes(x = tempcont, 
                  ymin = plogis(l95), 
                  ymax = plogis(u95)),
              fill = "orange",
              alpha = 0.2) +
  
  # define appearance
  labs(x = "annual temperature variability [°C]",
       y = "rel. no. hits per plot") + 
  theme_bw()