#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param SimulateData
#' @param meanTraits
munge_SimulatedData <- function(SimulateData, meanTraits =
                                tidyselect::starts_with("Trait_")) {

  # This function prepares the simulated data for analysis. Two data sets are created for... 
  # Set 1: Marginal means within location
  # Set 2: Means across locations
  
  # Set 1: 
  WithinLocData <- SimulateData %>% 
    pivot_longer(cols = all_of(meanTraits), names_to = "trait") %>% 
    group_by(test, loc, year, trait) %>% 
    nest()
  
  # Set 2: 
  AcrossLocData <- SimulateData %>% 
    pivot_longer(cols = all_of(meanTraits), names_to = "trait") %>% 
    group_by(test, year, trait) %>% 
    nest()
  
  # Return these two dataframes in a list
  return(list(WithinLoc = WithinLocData, 
              AcrossLoc = AcrossLocData))
}
