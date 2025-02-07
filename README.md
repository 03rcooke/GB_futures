# GB_future

This repository contains the R code and files underpinning: 'Plausible futures for British biodiversity under climate and land-use change'. Rob Cooke, Victoria J. Burton, Calum Brown, Colin Harrower, Steven M. White, Rob Dunford-Brown, Richard Fox, Paula A. Harrison, Cang Hui, Chris Huntingford, Dario Massimino, Andy Purvis, Emma Robinson, James Rodger, Nick J. B. Isaac & James M. Bullock

Step 0 - prepare data:

`data_prep.R` contains the code needed to prepare the data for all other steps. This script turns the raw data (links provided) into prepared files. All prepared files are included in this repo, so this script is not necessary to run.

Step 1 - load data:

`gdm_start.R` loads up all the data needed to run the models and analysis. Run this code before moving on to the models.

Step 2 - run GDMs for each taxonomic group:

`gdm_plants.Rmd`, `gdm_butterflies.Rmd` and `gdm_birds.Rmd` are R notebooks running GDMs for each taxonomic group. They first load the data, then run the sample coverage estiamtes using the `iNEXT` package. Then join the biological data and environmental data, and then run the GDMs with the `gdm` package. The individual modelling steps can be skipped as the prepared models are saved.

Step 3 - calculate biodiversity metrics:

`gdm_model_out.Rmd` runs the calculations of the biodiversity metrics: disappearing bioclimates, novel bioclimates, and species heading for extinction for each taxonomic group (choose taxonomic group at top of script). This code is computationally intensive even in parallel. We ran the species heading for extinction calculations on a HPC (JASMIN).

Step 4 - collate and plot:

`gdm_cross_tax.Rmd` combines data across the taxonomic groups and creates the figures
