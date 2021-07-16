#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param MarginalMeans
clean_marginal_means <- function(MarginalMeans) {

  AcrossLocmeans <- MarginalMeans$AcrossLoc %>% 
    select(test, year, trait, marginal_means) %>% 
    unnest(marginal_means) %>% 
    select(test, year, trait, geno, emmean) %>% 
    pivot_wider(names_from = trait, values_from = emmean)
  
  WithinLocMeans <- MarginalMeans$WithinLoc %>% 
    select(test, loc, year, trait, marginal_means) %>% 
    unnest(marginal_means) %>% 
    select(test, loc, year, trait, geno, emmean) %>% 
    pivot_wider(names_from = c(trait, loc), values_from = emmean)
  
  AllMeans <- inner_join(AcrossLocmeans, WithinLocMeans)
  
  return(AllMeans)
}
