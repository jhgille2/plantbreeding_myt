# plantbreeding_myt
Some functions to analyse agricultural data of a structure that I've seen come up a few times. Basically leveraging nesting/mapping functions from tidyverse to group data and apply lm/lmer/emmeans functions to the grouped data. My hope here is to have a collection of functions that are general enough to relatively easily adapt to new data so that the workflow can be reused without too much trouble.  

I have a [short writeup](https://jhgille2.github.io/plantbreeding_myt/Writeup.html) of the code here.

# TODO  
- Add a step to assign roles to the columns of the dataset prior to transformation/model fitting. I want to look into using the [recipes](https://cran.r-project.org/web/packages/recipes/vignettes/Roles.html) package for doing this. Some sort of way to "tag" grouping vs measurement variables so that they can be passed to later steps that use these grouping/measurement variables to transform the data. There may even be a way to use the whole tidymodels framework for this analysis. My gut feeling right now is that would be overkill, but still probably worth looking into at least. 
- Use some functions from packages designed to analyze agricultural data (like metan, agricolae, and need to check around for others). Compare using the functions in these packages to standard linear/mixed models that I have right now. Maybe add functionality that will take a starting data set in some general structure and let you just choose analysis techniques to apply to the data.  
-  Add a step to check for errors that could show up in real-life data. Will need to figure out what errors to check for, and how they should be dealt with.  
