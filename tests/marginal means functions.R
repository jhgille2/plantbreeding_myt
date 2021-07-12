

Yield <- read_csv(here("data", "AllYield.csv"))

# Variables to convert to a factor
factvars <- c("test", 
              "loc", 
              "genotype", 
              "year", 
              "rep")

# Reorder columns 
Yield %<>% 
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
         sq, 
         pro, 
         oil, 
         p_o) %>%
  group_by(test, year) %>% 
  mutate(nLoc = length(unique(loc)))
  

MultiLoc <- Yield %>% 
  dplyr::filter(nLoc != 1) %>%
  select(-one_of("nLoc")) %>% 
  pivot_longer(cols = c(md, lod, ht, yield, sdwt, sq, pro, oil, p_o), 
               names_to = "trait")

# A fuction to fit a linear model to data within a year
LM_within_loc <- function(LocData){
  res <- tryCatch({
    lm(value ~ genotype + rep, data = LocData)
  }, error = function(e){
    return(NA)
  })
}


get_marginal_means <- function(lm_model){
  emmeans(lm_model, "genotype") %>%
    as_tibble()
}
# A function to fit a mixed model across locations for a test
Lmer_by_year <- function(TestData){
  lmer(value ~ genotype + (1|loc/rep) + (1|loc:genotype))
}

# A function to find what percevtage of a given trait is missing for a 
# nested dataset
missing_pct <- function(nestedData){
  MissingPct <- sum(is.na(nestedData$value))/nrow(nestedData)
  return(MissingPct)
}

MultiLoc_ByLoc <- MultiLoc %>% 
  group_by(test, year, loc, trait) %>% 
  nest() %>% 
  mutate(missingPct = purrr::map_dbl(data, missing_pct)) %>%
  dplyr::filter(missingPct != 1) %>%
  mutate(traitModel = map(data, LM_within_loc), 
         modelSummary = map(traitModel, broom::glance), 
         marginal_means = map(traitModel, get_marginal_means))


