Univariate Gaussian Dirichlet Process Mixture Model
===
The Univariate Gaussian DPMM allows the user to estimate the cluster amount or density for a continous variable assuming the clusters are normally distributed.

### Assumptions
---

- Continuous dependent variable.
- The clusters are normally distributed.


### Input
---

#### Variable selection
- Variables: In this box the dependent variable is selected and must be continous.  
- Scale dependend: Scaling the dependend is recommenend to speed up convergence.
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis.

### Prior Options
- Prior distribution is about the shape of the normal distribution within each cluster. Setting the values for mu, K, Alpha and Beta will dertermine the prior shape of each normal distribution. Scaling the data might take the nessacity away and default values could be used.

### Alpha Options (percicion parameter)
- The Alpha values are determent by a gamma distribution. Setting the precicion and rate parameter will determine which alpha numbers are most likely to be sampled. Chosing to display the distribution might help visualize which values are most likely. Lower values promote less clusters higher values promote more clusters.

### Sampler options
- No. samples: The number of samples to draw from the posterior distribution that are used for results (tables, plots).
- No. burnin samples: The number of samples to draw from the posterior distribution and immediately discard.

### Plots and tables
- Prior and posterior plot: Displays the prior and posterior distribution of the Alpha Values.
- Density plot: Show a density plot of the clusters based on the data.
- Trace plots: Show a trace plot of the posterior samples of Alpha, Clusters and Likelihood.
- Table clusters: Show a table of cluster means, standard deviations and group size. Additionally a credible can be displayed, the highest density interval is used.

### Export results
- Add predictions to data: turn on the add predictions to the current dataset
- Column name: setting the column name is nessacary to get the predictions in the data.

### Output
---
#### Plots
- Prior and posterior: Displays the prior (orange line) and posterior (bar plot) distribution of the Alpha values sampled from the Gamma Distribution.







### R Packages
---

