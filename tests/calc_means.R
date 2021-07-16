#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param MungeData

## Section: Helper functions
##################################################

# A function to fit a linear model to data within a year.
# This function and the one after have (very lazy) error catching that just return
# NA if the model fit fails
LM_within_loc <- function(LocData){
  
  # Variables to convert to a factor
  factvars <- c("test", 
                "loc", 
                "genotype", 
                "year", 
                "rep")
  
  FactorData <- LocData %>% 
    mutate(across(any_of(factvars), ~ factor(.x)))
  
  res <- tryCatch({
    lm(value ~ genotype + rep, data = FactorData)
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
                "genotype", 
                "year", 
                "rep")
  
  FactorData <- TestData %>% 
    mutate(across(any_of(factvars), ~ factor(.x)))
  
  res <- tryCatch({
    lmer(value ~ genotype + (1|loc/rep) + (1|loc:genotype), data = FactorData)
  }, error = function(e){
    return(NA)
  })
  return(res)
}

# A function to get a tibble of marginal means from a fit model
get_marginal_means <- function(model){
  emmeans(model, "genotype") %>%
    as_tibble()
}

# A function to find what percentage of a given trait is missing for a 
# nested data set
missing_pct <- function(nestedData){
  MissingPct <- sum(is.na(nestedData$value))/nrow(nestedData)
  return(MissingPct)
}

missing_pct_byLoc <- function(nestedData){
  MissingSummary <- nestedData %>%
    group_by(loc) %>%
    summarise(MissingPct = sum(is.na(value))/n())
  
  ifelse(any(MissingSummary$MissingPct == 1), 1, 0)
}

# A function to count how many reps do not have 0 measurements. 
# There is no point in applying contrasts to measurements that 
# were only measured with one rep
OneRepTraits <- function(nestedData){
  
  RepCounts <- nestedData %>% 
    group_by(rep) %>% 
    summarise(repCount = sum(!is.na(value)))
  
  NonZeroSum <- sum(RepCounts$repCount != 0)
  
  ifelse(NonZeroSum == 1, 1, 0)
}

calc_means <- function(MungeData) {

  # Create a set of data for each test within each location, keep only the traits that have 
  # data, fit linear models on each dataset (trait ~ genotype + rep), calculate model summaries, 
  # and marginal means.
  MultiLoc_ByLoc <- MungeData$AllYield %>% 
    group_by(test, year, loc, trait) %>% 
    nest() %>% 
    mutate(missingPct = purrr::map_dbl(data, missing_pct), 
           SingleRep  = purrr::map_dbl(data, OneRepTraits)) %>%
    dplyr::filter(missingPct != 1, SingleRep != 1) %>%        # Remove any traits with all their data missing or were only measured with one rep
    mutate(traitModel     = map(data, LM_within_loc),         # Fit a simple linear model to each data set, and extract model 
           modelSummary   = map(traitModel, broom::glance),   # fit statistics and marginal means from each model
           marginal_means = map(traitModel, get_marginal_means))
  
  # Create a set of data for each test within each location, keep only the traits that have 
  # data, fit linear models on each dataset (trait ~ genotype + rep), calculate model sumamries, 
  # and marginal means.
  MultiLoc_AcrossLoc <- MungeData$MultiLoc %>% 
    group_by(test, year, trait) %>% 
    nest() %>% 
    mutate(missingPct = purrr::map_dbl(data, missing_pct_byLoc), 
           SingleRep  = purrr::map_dbl(data, OneRepTraits)) %>%
    dplyr::filter(missingPct != 1, SingleRep != 1) %>%        # Remove any traits with all their data missing or were only measured with one rep
    mutate(traitModel     = map(data, Lmer_by_year),          # Fit a simple linear model to each data set, and extract model
           traitANOVA     = map(traitModel, anova),           # model anova
           modelSummary   = map(traitModel, broom::glance),   # fit statistics and marginal means from each model
           marginal_means = map(traitModel, get_marginal_means))

  # Return both dataframes in a list
  return(list("WithinLoc" = MultiLoc_ByLoc, 
              "AcrossLoc" = MultiLoc_AcrossLoc))
}


