#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param SimulateData
#' @param meanTrait
base_SplitData <- function(SimulateData, meanTraits =
                           tidyselect::starts_with("Trait_")) {

  PivotedData <- SimulateData %>% 
    pivot_longer(cols = all_of(meanTraits), names_to = "trait")
  
  # Split the pivoted dataframe into chunks based on id variables
  Split_ByLoc      <- split(PivotedData, list(PivotedData$test, PivotedData$loc, PivotedData$year, PivotedData$trait))
  Split_AcrosssLoc <- split(PivotedData, list(PivotedData$test, PivotedData$year, PivotedData$trait))

  # Return these two dataframes in a list
  return(list(WithinLoc = Split_ByLoc, 
              AcrossLoc = Split_AcrosssLoc))
}
