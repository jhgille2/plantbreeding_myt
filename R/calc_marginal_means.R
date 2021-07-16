#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param SimulateData
#' @param meanTraits
#' 
#' 

# Fit models to the munged data, and extract marginal means from the models
calc_marginal_means <- function(MungeData) {
  
  ## Section: Helper functions
  ##################################################
  
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
    
    FactorData <- LocData %>% 
      mutate(across(any_of(factvars), ~ factor(.x)))
    
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
    
    FactorData <- TestData %>% 
      mutate(across(any_of(factvars), ~ factor(.x)))
    
    res <- tryCatch({
      lmer(value ~ geno + (1|loc/rep) + (1|loc:geno), data = FactorData)
    }, error = function(e){
      return(NA)
    })
    return(res)
  }
  
  # A function to get a tibble of marginal means from a fit model
  get_marginal_means <- function(model){
    emmeans(model, "geno") %>%
      as_tibble()
  }
  
  # Fit models on the within and across location data, 
  # and extract marginal means
  WithinLoc_ModelData <- MungeData$WithinLoc %>% 
    mutate(model          = map(data, LM_within_loc), 
           marginal_means = map(model, get_marginal_means))
  
  AcrossLoc_ModelData <- MungeData$AcrossLoc %>% 
    mutate(model          = map(data, Lmer_by_year), 
           marginal_means = map(model, get_marginal_means))
  
  # Return the two dataframes in a list
  return(list(WithinLoc = WithinLoc_ModelData, 
              AcrossLoc = AcrossLoc_ModelData))
}
