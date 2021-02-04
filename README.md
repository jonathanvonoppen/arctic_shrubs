# Annual air temperature variability and biotic interactions explain tundra shrub species abundance: Data and Code Repository


## Background
This repository contains code and data for the study on drivers of shrub species abundance in the Nuuk Fjord area, Western Greenland, which is curerntly under review at *Journal of Vegetation Science* under the title **Annual air temperature variability and biotic interactions explain tundra shrub species abundance**. Data was collected by [Jacob Nabe-Nielsen](mailto:jnn@bios.au.dk) et al. during the summers of 2011-2013. Analyses were led by [Jonathan von Oppen](mailto:jonathan.vonoppen@bio.au.dk), [Anne Bjorkman](mailto:annebj@gmail.com), and [Signe Normand](mailto:signe.normand@bio.au.dk), with contributions from [Jakob Assmann](mailto:j.assmann@bio.au.dk) and [Anne Blach Overgaard](mailto:anne.overgaard@bio.au.dk).


## Contact
The repository is currently maintained by [Jonathan von Oppen](mailto:jonathan.vonoppen@bio.au.dk). 


## Data usage guidelines and license

### Data
Any data usage requests should be directed to [Jacob Nabe-Nielsen](mailto:jnn@bios.au.dk) and [Signe Normand](mailto:signe.normand@bio.au.dk).

### Code
All code provided for data preparation and analysis is licensed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/). In accordance with the license the code is available to be shared and adapted, but requires attribution to the authors, e.g. through citation of the above manuscript, and indications where changes were made. Although not mandatory, we additionally suggest that code users contact and collaborate with contributors, particularly those explicitly mentioned in the header of each script, should the code form a substantial proportion of a particular publication or analysis.


## File structure
The repository contains the following folders:

* [analyses_main_twi](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/analyses_main_twi) contains all files related to the *main text analysis using topographic wetness (TWI)* as the wetness variable. This includes 
    + a [workflow documentation](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/blob/master/analyses_main_twi/Nuuk_shrub_drivers_analyses_twi_complete.nb.html) including all code to prepare data and run models, 
    + JAGS [model code files](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/analyses_main_twi/model_files_twi) for both general models species and groups (\*.spec.jags or \*.groups.jags) as well as species- and group-specific 2^nd^-step models (\*2.jags), 
    + [model coefficient tables](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/blob/master/analyses_main_twi/appendix_tables_twi.html), and 
    + [figure code and files](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/analyses_main_twi/figures) (including figures for Appendix S1-S6).

* [analyses_tcws](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/analyses_tcws) contains files related to the *additional analysis using Tasseled-cap wetness (TCWS)* as the wetness variable, specifically
    + a [workflow documentation](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/blob/master/analyses_tcws/Nuuk_shrub_drivers_analyses_tcws_complete.nb.html) including all code to prepare data and run models,
    + JAGS [model code files](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/analyses_tcws/model_files_tcws) for both general models species and groups (\*.spec.jags or \*.groups.jags) as well as species- and group-specific 2^nd^-step models (\*2.jags), 
    + [model coefficient tables](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/blob/master/analyses_tcws/appendix_tables_tcws.html),  
    + [figure code and files](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/analyses_tcws/figures) (for TCWS-related figures in Appendix S7), and
    + a supplementary [analysis of wetness predictors](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/blob/master/analyses_tcws/wetness_predictor_comparison.html) to explain wetness-related vegetation (referred to in Appendix S7).

* [data](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/data) contains 
    + the original and pre-processed [input data](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/data/input_data), including plant trait principal component scores extracted from Thomas et al. (2020) 
    + cleaned and [processed data](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/data/processed), including scaled and centered model input datasets for analyses based on [topographic wetness (TWI)](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/data/processed/model_input_data_twi) and [Tasseled-cap wetness (TCWS)](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/data/processed/model_input_data_tcws), respectively, and corresponding [model output files](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/data/processed/model_outputs) (sorted by functional groups and species for each analysis)
    + and data for calculating [variograms](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/data/variograms) for abiotic variables (Appendix S2, [Fig. S2.3](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/analyses_main_twi/figures/AppendixS2_variograms)) 

* [scripts](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/scripts) contains scripts written for processing data and producing figures, sorted by the respective contributor:
    + [JJA](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/scripts/JJA) ([contact](mailto:j.assmann@bio.au.dk)): calculation and extraction of topographic variables, variable maps and variograms
    + [JvO](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/scripts/JvO) ([contact](mailto:jonathan.vonoppen@bio.au.dk)): general compilation of data, cleaning of trait data
    + [NathalieChardon](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/scripts/NC) ([contact](mailto:nathalie.chardon@gmail.com)): calculation of environmental variables and climate time series for plot locations.


## References

Thomas, H. J. D., Bjorkman, A. D., Myers-Smith, I. H., Elmendorf, S. C., Kattge, J., Diaz, S., Vellend, M., Blok, D., Cornelissen, J. H. C., ..., de Vries, F. T. (2020) Global plant trait relationships extend to the climatic extremes of the tundra biome. *Nature Communications*, **11**(1), pp. 1–12. doi: [10.1038/s41467-020-15014-4](https://doi.org/10.1038/s41467-020-15014-4).