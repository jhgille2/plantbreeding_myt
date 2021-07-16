#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ReadYield
munge_yieldData <- function(ReadYield) {

  # Reorder columns 
  ReadYield %<>% 
    select(test, 
           loc, 
           genotype, 
           year, 
           rep, 
           plot, 
           md, 
           lod, 
           ht, 
           yield, 
           sdwt, 
           # sq, 
           pro, 
           oil, 
           p_o) %>%
    group_by(test, year) %>% 
    mutate(nLoc = length(unique(loc))) %>% # How many locations was a test grown in for a year
    ungroup()
  
  # Filter to the set of trials that were grown in more than one location
  MultiLoc <- ReadYield %>% 
    dplyr::filter(nLoc != 1) %>% 
    select(-one_of("nLoc")) %>% 
    pivot_longer(cols = c(md, lod, ht, yield, sdwt, pro, oil, p_o), 
                 names_to = "trait")
  
  # The full dataset, pivoted to a long format
  AllYield <- ReadYield %>% 
    select(-one_of("nLoc")) %>% 
    pivot_longer(cols = c(md, lod, ht, yield, sdwt, pro, oil, p_o), 
                 names_to = "trait")
  
  # Return both dataframes in a list
  return(list("MultiLoc" = MultiLoc, 
              "AllYield" = AllYield))
}
