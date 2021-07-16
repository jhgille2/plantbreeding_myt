## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  
  # Simulate data from an agricultural experiment
  tar_target(SimulateData, 
             SimTraits(nTests = 10, nYears = 3, nTraits = 5, nReps = 3, nLocs = 4, nGeno = 20)), 
  
  # Nest the simulated data into datasets ready to fit models to
  tar_target(MungeData, 
             munge_SimulatedData(SimulateData, meanTraits = tidyselect::starts_with("Trait_"))),
  
  # Calculate within and across location marginal means on the simulated data
  tar_target(MarginalMeans, 
             calc_marginal_means(MungeData)),
  
  # Clean up the marginal mean data
  tar_target(CleanMeans, 
             clean_marginal_means(MarginalMeans)),
  
  # Make some summary plots
  tar_target(Plots, 
             make_summary_plots(MarginalMeans)), 
  
  # Export marginal means to formatted workbooks
  tar_target(ExportWorkbooks, 
             export_mean_workbooks(MarginalMeans)), 
  
  # tar_target(target2, function_to_make2(arg)) ## targets style
  tar_render(Writeup, "doc/Writeup.Rmd")

)
