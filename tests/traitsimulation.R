##################################################
## Project: myt analysis
## Script purpose: Functions to simulate phenotype data
## Date: 2021-07-13
## Author: Jay Gillenwater
##################################################

# Simulate phenotypes following the model ...
# y_ijk = Mu + Geno_i + Loc_j + Year_k + Epsilon_ijk
# Where mu is the global mean for some trait, Geno, Loc, and Year are fixed effects
# and Epsilon is a random error

SimTraits <- function(nTests = 10, nYears = 3, nTraits = 5, nReps = 3, nLocs = 4, nGeno = 20){
  
  # Make a tibble to hold genotype effects
  GenoEffects <- expand_grid(test = LETTERS[1:nTests], code = 1:nGeno) %>%
    mutate(geno   = paste(test, code, sep = "_"), 
           geno_effect = rnorm(n(), 0, 5)) %>% 
    expand_grid(rep = 1:nReps)
  
  LocEffects <- tibble(loc = as.factor(as.character(1:nLocs))) %>% 
    mutate(loc_effect = rnorm(n(), 0, 3))
  
  YearEffects <- tibble(year = 1:nYears) %>%
    mutate(year_effect = rnorm(n(), 0, 2))
  
  # Get trait means and SDs
  TraitEffects <- tibble(trait     = paste("Trait", 1:nTraits, sep = "_"), 
                         traitMean = sample(seq(10, 100, 1), nTraits, replace = TRUE), 
                         traitSD   = sample(seq(1, 5, 0.25), nTraits, replace = TRUE))
  
  FullData <- expand_grid(GenoEffects, 
                          LocEffects, 
                          YearEffects, 
                          TraitEffects) %>%
    mutate(trait_value = geno_effect + loc_effect + year_effect + rnorm(n(), mean = traitMean, sd = traitSD))
  
  FullData %<>%
    select(test, code, geno, rep, loc, year, trait, trait_value) %>% 
    pivot_wider(names_from = trait, values_from = trait_value) %>%
    arrange(year, loc, test, code, rep)
  
  return(FullData)
}
