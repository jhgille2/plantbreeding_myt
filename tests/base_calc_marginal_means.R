#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param SplitData
base_calc_marginal_means <- function(SplitData) {

  # A function to fit a linear model to data within a year.
  # This function and the one after have (very lazy) error catching that just return
  # NA if the model fit fails
  LM_within_loc <- function(LocData){
    
    # Variables to convert to a factor
    factvars <- c("test", 
                  "loc", 
                  "geno", 
                  "year", 
                  "rep")
    
    FactorData <- LocData
    FactorData[, factvars] <- apply(FactorData[, factvars], 2, factor)
    
    res <- tryCatch({
      lm(value ~ geno + rep, data = FactorData)
    }, error = function(e){
      return(NA)
    })
    return(res)
  }
  
  # A function to fit a mixed model across locations for a test
  Lmer_by_year <- function(TestData){
    
    # Variables to convert to a factor
    factvars <- c("test", 
                  "loc", 
                  "geno", 
                  "year", 
                  "rep")
    
    FactorData <- LocData
    FactorData[, factvars] <- apply(FactorData[, factvars], 2, factor)
    
    res <- tryCatch({
      lmer(value ~ geno + (1|loc/rep) + (1|loc:geno), data = FactorData)
    }, error = function(e){
      return(NA)
    })
    return(res)
  }
  
  # A function to get a tibble of marginal means from a fit model
  get_marginal_means <- function(model){
    emmeans(model, "geno") |>
      as.data.frame()
  }
  
  # Prepare lists to hold the full results for each set of split data
  AllResults        <- vector("list", length = 2)
  names(AllResults) <- names(SplitData)
  for(i in 1:length(AllResults)){
    
    # Lists to hold models and marginal means for split data
    ModelVec   <- vector("list", length = length(SplitData[[i]]))
    emmeansVec <- vector("list", length = length(SplitData[[i]]))
    
    names(ModelVec) <- names(emmeansVec) <- names(SplitData[[i]])
    
    if(names(AllResults)[[i]] == "WithinLoc"){
      ModelVec   <- pblapply(SplitData[[i]], LM_within_loc)
      emmeansVec <- pblapply(ModelVec, get_marginal_means)
    }else if(names(AllResults)[[i]] == "AcrossLoc"){
      ModelVec   <- pblapply(SplitData[[i]], Lmer_by_year)
      emmeansVec <- pblapply(ModelVec, get_marginal_means)
    }
    
    CurrentResults <- list(model          = ModelVec, 
                           marginal_means = emmeansVec)
    
    AllResults[[names(AllResults)[[i]]]] <- CurrentResults
  }
  
  return(AllResults)
}
