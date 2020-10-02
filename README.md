# Drivers of Shrub Abundance in Arctic Tundra: Data and Code Repository


## Background
This repository contains code and data for the study on drivers of shrub species abundance in the Nuuk Fjord area, Western Greenland, which has been submitted for publication in *Journal of Vegetation Science* under the title **Temperature variability, moisture and biotic interactions drive shrub species abundance along a coastal-inland gradient in arctic Greenland**. Data was collected by [Jacob Nabe-Nielsen](mailto:jnn@bios.au.dk) et al. during the summers of 2011-2013. Analyses were led by [Jonathan von Oppen](mailto:jonathan.vonoppen@bio.au.dk), [Anne Bjorkman](mailto:annebj@gmail.com), and [Signe Normand](mailto:signe.normand@bio.au.dk).


## Contact
The repository is currently maintained by [Jonathan von Oppen](mailto:jonathan.vonoppen@bio.au.dk). 


## Data usage guidelines and license

### Data
Any data usage requests should be directed to [Jacob Nabe-Nielsen](mailto:jnn@bios.au.dk) and [Signe Normand](mailto:signe.normand@bio.au.dk).

### Code
All code provided for data preparation and analysis is licensed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/). In accordance with the license the code is available to be shared and adapted, but requires attribution to the authors, e.g. through citation of the above manuscript, and indications where changes were made. Although not mandatory, we additionally suggest that code users contact and collaborate with contributors, particularly those explicitly mentioned in the header of each script, should the code form a substantial proportion of a particular publication or analysis.


## File structure
The repository contains the following folders:

* [data](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/data) with the original and pre-processed data [input_data](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/data/input_data)([input_data](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/data/input_data)), including plant trait principal component scores extracted from Thomas et al. (2020), as well as cleaned model input data and model outputs ([processed](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/data/processed));

* [documentation](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/documentation) and code of the analysis workflow;

* [figures](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/figures) produced for the manuscript and supplementary material;

* [models](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/models) in JAGS code written during the analysis;

* [scripts](https://github.com/jonathanvonoppen/nuuk_shrub_drivers/tree/master/scripts) written for processing data and producing figures, sorted by the respective contributor.


## References

Thomas, H. J. D., Bjorkman, A. D., Myers-Smith, I. H., Elmendorf, S. C., Kattge, J., Diaz, S., Vellend, M., Blok, D., Cornelissen, J. H. C., ..., de Vries, F. T. (2020) Global plant trait relationships extend to the climatic extremes of the tundra biome. *Nature Communications*, **11**(1), pp. 1–12. doi: [10.1038/s41467-020-15014-4](https://doi.org/10.1038/s41467-020-15014-4).