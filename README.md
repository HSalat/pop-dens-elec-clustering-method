# pop-dens-elec-clustering-method

This repository contains the supporting code for the article: Salat, Smoreda, Schläpfer, A method to estimate population densities and electricity consumption from mobile phone data in developing countries. PLoS One, 2020. DOI: 10.1371/journal.pone.0235224

The project is meant to be used as a single workspace (variables created in one file are called directly from other files).

Only the content of the repository is described here, for a description of the methods, please refer to the article.

## Data

Some of the public and non sensitive data are stored on OSF. The rest can only be accessed by requesting permission from Orange/Sonatel.

Data provided on OSF:
- ...

Data not provided:
- Localisation of the antenna sites.
- Full call details records for the year 2013 in Senegal.

## Files

- `data-clustering-curves.R` creates the distance matrices between mobile phone activity curves to apply the clustering algorithm on. In addition, it contains an alternative method to replace distance matrices by a feature table containing curve characteristics (such as seasonality, skewness...) to operate the clustering. There are also a number of plots that were used to generate illustrations for the article. 
- `data-clustering-network.R` creates basic OD data of mobile phone activity that can be easily tramsformed into adjacency matrices / igraph networks for analysis. It also provides some visualisations of the networks.
- `results-correlations-table1.R` contains the code to compute Table 1 of the article (up to l. 128) and some visualisations, additional tests and interesting alternatives methods that were not included in the final article.

The data loaded by `Data-general.R` is essential to run all other files.
