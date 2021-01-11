# Temperature variability, moisture and biotic interactions drive shrub species abundance along a coastal-inland gradient in arctic Greenland

# von Oppen, J. et al. 2020

# Figure code

# Jonathan von Oppen, Aarhus University, Sept 2020

# contact: jonathan.vonoppen@bios.au.dk



# load model outputs
model_outputs_species <- file.path("data", "processed", "model_outputs", "species_twi", list.files(path = file.path("data", "processed", "model_outputs", "species_twi"), pattern = "*.Rdata"))
for (model_output in model_outputs_species){
  load(model_output)
}

# write 
source(file = file.path("scripts", "jonathan", "plotting_functions", "output_tables.R"))

(AllShr_output_table <- output_table(species = "All shrubs"))
(AllEve_output_table <- output_table(species = "All evergreens"))
(AllDec_output_table <- output_table(species = "All deciduous"))

(BetNan_output_table <- output_table(species = "Betula nana"))
(CasTet_output_table <- output_table(species = "Cassiope tetragona"))
(EmpNig_output_table <- output_table(species = "Empetrum nigrum"))
(PhyCae_output_table <- output_table(species = "Phyllodoce caerulea"))
(RhoGro_output_table <- output_table(species = "Rhododendron groenlandicum"))
(RhoTom_output_table <- output_table(species = "Rhododendron tomentosum"))
(SalArc_output_table <- output_table(species = "Salix arctophila"))
(SalGla_output_table <- output_table(species = "Salix glauca"))
(VacUli_output_table <- output_table(species = "Vaccinium uliginosum"))
