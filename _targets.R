## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  
  # The path to the input file
  tar_target(YieldFile, 
             here("data", "AllYield.csv"), format = "file"), 
  
  # Read in this file
  tar_target(ReadYield, 
             read_csv(YieldFile)), 
  
  # Data cleaning
  tar_target(MungeData, 
             munge_yieldData(ReadYield)), 

  # Calculate marginal means
  tar_target(MarginalMeans, 
             calc_means(MungeData)), 
  
  # Make some summary plots
  tar_target(Plots, 
             make_summary_plots(MarginalMeans)), 
  
  # Export marginal means to formated workbooks
  tar_target(ExportWorkbooks, 
             export_mean_workbooks(MarginalMeans)), 
  
# target = function_to_make(arg), ## drake style

# tar_target(target2, function_to_make2(arg)) ## targets style
  tar_render(Writeup, "doc/Writeup.Rmd")

)
