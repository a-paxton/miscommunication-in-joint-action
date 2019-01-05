# Miscommunication in Joint Action: Code for Roche*, Paxton*, Ibarra, & Tanenhaus (under review)

This repo contains the code for the analyses presented in our manuscript,
"Predictors of miscommunication in verbal communication during collaborative
joint action" (Roche*, Paxton*, Ibarra, & Tanenhaus, under review).

## Overview

The repo contains several analysis files, an R markdown, a markdown file, and
figures.

* `miscommunication_in_joint_action.Rmd`: R markdown with all data preparation,
  analysis, and visualization presented in our manuscript. Note that it includes
  the unstandardized models that are mentioned but (for brevity) not included
  in the manuscript itself.
* `miscommunication_in_joint_action.md`: A markdown file generated by the R
  markdown of the same name. We recommend that you open this version to view in
  your browser.
* `./code/libraries_and_functions-MJA.r`: Loads in necessary libraries and
  creates new functions for our analyses.
* `./dictionaries/assent_negation_dictionary-MJA.csv`: List of assent and
  negation words.
* `./dictionaries/nltk_stopwords_list-MJA.csv`: List of stopwords from the
  NLTK Python toolkit.
* `./dictionaries/spatial_word_list-MJA.csv`: List of spatial words.
* `./figures/`: Figures generated by the `miscommunication_in_joint_action.Rmd`
  file.

## Note on data

Due to IRB restrictions, we are unable to openly share the data for the project.

## Notes on running and viewing

For best viewing in a browser, we recommend selecting the
`coordination-and-miscommunication.md`, rather than the similarly named `.Rmd`
file. (Analyses should be run using the `.Rmd` file of the same name.)

For those unfamiliar with R markdown, we recommend taking a look at
[RStudio's introduction to R markdown](http://rmarkdown.rstudio.com/) before
attempting to run the `.Rmd` file. (Be sure to download
[RStudio](https://www.rstudio.com/) first, if you do not already have it
installed on your machine.)
