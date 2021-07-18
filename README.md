# plantbreeding_myt
Some functions to analyse agricultural data of a structure that I've seen come up a few times. Basically leveraging nesting/mapping functions from tidyverse to group data and apply lm/lmer/emmeans functions to the grouped data. My hope here is to have a collection of functions that are general enough to relatively easily adapt to new data so that the workflow can be reused without too much trouble.

More specifically, these functions are meant to solve two jobs: 
1. Clean (pivot) field data into a long, but tidy format. 
2. Group this cleaned data into a nested structure so that an analysis technique can be applied to each of the independent groups (field experiments). 

The need for job 1 arose because often our data is recorded in a "wide" format where each multiple phenotypic measurements are taken for a particular genotype, in a specific plot. This format makes sense for data entry, but makes analysis tedious. Pivoting the data for an experiment to a long format makes analysis trivial. Job 2 arose because we typically have many field experiments going on at the same time which are ultimately analyzed with the same statistical techniques. Rather than unnecessarily repeating code for each experiment, it is more time-efficient, and less error-prone to systematically apply one analysis function to a nested data structure.

I have a [short writeup](https://jhgille2.github.io/plantbreeding_myt/Writeup.html) of the code here.

# TODO  
- Add a step to assign roles to the columns of the dataset prior to transformation/model fitting. I want to look into using the [recipes](https://cran.r-project.org/web/packages/recipes/vignettes/Roles.html) package for doing this. Some sort of way to "tag" grouping vs measurement variables so that they can be passed to later steps that use these grouping/measurement variables to transform the data. There may even be a way to use the whole tidymodels framework for this analysis. My gut feeling right now is that would be overkill, but still probably worth looking into at least. I feel like this would help the workflow be more "data agnostic" and make applying it to new datasets easier in the future. 
- Use some functions from packages designed to analyze agricultural data (like metan, agricolae, and need to check around for others). Compare using the functions in these packages to standard linear/mixed models that I have right now. Maybe add functionality that will take a starting data set in some general structure and let you just choose analysis techniques to apply to the data.  
-  Add a step to check for errors that could show up in real-life data. Will need to figure out what errors to check for, and how they should be dealt with.  
