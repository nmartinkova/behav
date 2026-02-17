# behav 0.7

* Added new function `plotSocialNetwork` for plotting dyadic adjacency graph.
* Modified `mergeBoris` to unify names in the partner column, and add grouping columns zoo, adult and group.


# behav 0.6.1 (2026-01-05)

* Modified `mergeBoris` to include a column with partners for dyadic interactions.


# behav 0.6 (2025-11-26)

* Added new function `calculateDyadicMetrics` that calculates metrics of dyadic interactions for all partner pairs.
* `plotPCA` now outputs also the variable loadings table.

# behav 0.5.1 (2025-11-12)

* Added new function `plotRarefaction()` for plotting bootstrapped rarefaction curves.
* Added new function `calculateMetrics()` that calculates diversity metrices for individuals and optionally plots metrics boxplots.
* Added new function `plotPCA()` for plotting PCA scatterplot and a heatmap of variable loadings for interpretation.

# behav 0.5 (2025-11-05)

* Added new function `mergeBoris()` for merging BORIS behavioural observation files
  exported as `.xlsx` or `.tsv`.
* Release for Natalie Gabrielova, Faculty of Sciences, Masaryk University.

# behav 0.4.1 (2022-12-20)

* Added recursive file listing for batch processing.


# behav 0.4 (2022-10-26)

* First public release on GitHub.
* Renamed all functions to prevent method registration conflicts.


# behav 0.3.4 (2021-11-22)

* Replaced `sum_events()` with `spocitej()`.


# behav 0.3.3 (2021-11-22)

* Bug fix.


# behav 0.3.2 (2021-11-22)

* Updated `sum_events()` to convert event codes to lowercase and
  accept only the first letter of the code.


# behav 0.3.1 (2021-11-22)

* Renamed `sum.events()` to `sum_events()` to avoid generic method registration.
* Added vignette.


# behav 0.3 (2021-10-18)

* Added function `sum.events()`.
* Updated file name validation in `score.events()`.


# behav 0.2 (2021-10-18)

* Added documentation.
* Release for Ivo Adam, Faculty of Education, Masaryk University.


# behav 0.1 (2016-05-06)

* First version, released for Lucie Jakesova, Faculty of Sciences, Masaryk University.
