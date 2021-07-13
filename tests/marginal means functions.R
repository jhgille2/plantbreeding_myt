
## Section: Initial data import/cleaning
##################################################

# Read in the full yield data set
Yield <- read_csv(here("data", "AllYield.csv"))

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
         # sq, 
         pro, 
         oil, 
         p_o) %>%
  group_by(test, year) %>% 
  mutate(nLoc = length(unique(loc))) %>% # How many locations was a test grown in for a year
  ungroup()

# Filter to the set of trials that were grown in more than one location
MultiLoc <- Yield %>% 
  dplyr::filter(nLoc != 1) %>% 
  select(-one_of("nLoc")) %>% 
  pivot_longer(cols = c(md, lod, ht, yield, sdwt, pro, oil, p_o), 
               names_to = "trait")

# The full dataset, pivoted to a long format
AllYield <- Yield %>% 
  select(-one_of("nLoc")) %>% 
  pivot_longer(cols = c(md, lod, ht, yield, sdwt, pro, oil, p_o), 
               names_to = "trait")

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

## Sub-section: Emmean formatting functions
##################################################

# First a helper function that just pulls out the marginal means
pull_emmeans <- function(emmean_df){
  emmean_df %>% 
    select(genotype, emmean)
}

# Merge within location and across location marginal means
Format_emmeans_both <- function(AcrossLocData = MultiLoc_AcrossLoc, WithinLocData = MultiLoc_ByLoc, Year = 2018, Test = "LP HOLL"){
  
  # Format the within location marginal means
  WithinLoc_emmeans <- WithinLocData %>% 
    dplyr::filter(year == Year, test == Test) %>% 
    unnest(marginal_means) %>% 
    select(test, loc, year, trait, genotype, emmean) %>% 
    pivot_wider(names_from = c(loc, trait), values_from = emmean)
  
  AcrossLoc_ememans <- AcrossLocData %>%
    unnest(marginal_means) %>% 
    dplyr::filter(year == Year, test == Test) %>%
    select(test, year, trait, genotype, emmean) %>% 
    pivot_wider(names_from = trait, values_from = emmean)
  
  AllMeans <- inner_join(WithinLoc_emmeans, AcrossLoc_ememans)
  
  return(AllMeans)
}

## Section: Main data sets
##################################################

# Create a set of data for each test within each location, keep only the traits that have 
# data, fit linear models on each dataset (trait ~ genotype + rep), calculate model sumamries, 
# and marginal means.
MultiLoc_ByLoc <- AllYield %>% 
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
MultiLoc_AcrossLoc <- MultiLoc %>% 
  group_by(test, year, trait) %>% 
  nest() %>% 
  mutate(missingPct = purrr::map_dbl(data, missing_pct_byLoc), 
         SingleRep  = purrr::map_dbl(data, OneRepTraits)) %>%
  dplyr::filter(missingPct != 1, SingleRep != 1) %>%        # Remove any traits with all their data missing or were only measured with one rep
  mutate(traitModel     = map(data, Lmer_by_year),          # Fit a simple linear model to each data set, and extract model
         traitANOVA     = map(traitModel, anova),           # model anova
         modelSummary   = map(traitModel, broom::glance),   # fit statistics and marginal means from each model
         marginal_means = map(traitModel, get_marginal_means))



# TODO: Formatting emmeans for export to an excel workbook
# A function to find location-level mean columns
TraitLookup <- read_csv(paste0(here(),"/Data/HelperFiles/TraitLookup.csv"))
FormatMeans <- function(MeanData, TraitConversion = TraitLookup){
  
  # Text that comes before a "_" character in the column names (the location if such a character exists)
  LocPrefixes <- str_match(colnames(MeanData), "[^_]+")
  
  # Find all unique location names
  UniqueLocs <- LocPrefixes[str_detect(colnames(MeanData), "_")] %>%
    unique()
  
  # A function that takes a character and finds its minimum and maximum position in a vector
  ValRange <- function(Loc = "CAS", FullVec = LocPrefixes){
    range(which(FullVec == Loc))
  }
  
  # The starting and ending positions of each location in the prefixes vector
  LocRanges <- map(UniqueLocs, ValRange, LocPrefixes)
  names(LocRanges) <- UniqueLocs
  
  FullRange <- range(unlist(LocRanges))
  FullRange <- c((FullRange[[1]]):(FullRange[[2]]))
  
  # The column names corresponding to the by-location data
  ByLocCols <- colnames(MeanData)[FullRange]
  
  # Remove the location code and "_" from these column names
  ByLocCols %>%
    str_remove(., "[^_]+") %>%
    str_remove(., "_") -> ByLoc_Cleaned
  
  NewColNames <- colnames(MeanData)
  NewColNames[FullRange] <- ByLoc_Cleaned
  
  # Add these new column names to the MeanData and then format them to prettier names using the 
  # trait conversion table. 
  NewMeanData <- as.data.frame(MeanData)
  colnames(NewMeanData) <- NewColNames
  colnames(NewMeanData) <- TraitConversion$NewName[match(names(NewMeanData), TraitConversion$OldName)]
  
  # Return a list that that contains both the location names with their column number ranges, 
  # and the new formatted data. 
  return(list("LocationRanges" = LocRanges, "NewMeanData" = NewMeanData))
}

TestData_Mult <- FormatMeans(MultLocs)
TestData_One  <- FormatMeans(OneLoc)

FormattedMeans <- map(Means, FormatMeans)

MakeMeanWorkbooks <- function(MeanData, TestName, ExportDir){
  
  FormattedMeans <- map(MeanData, FormatMeans)
  
  # A function that creates a formatted excel workbook for each of the 
  # "FormattedMeans" dataframes
  CreateFormattedWorkbook <- function(FormattedMeanData){
    
    # Pull the location ranges and mean data from the formatted means list.
    # Also, get the full range of by-location columns
    LocRanges         <- FormattedMeanData$LocationRanges
    MeanData_OneTest  <- FormattedMeanData$NewMeanData
    FullRange         <- LocRanges %>% unlist() %>% range()
    
    # Create an excel workbook to add data to
    TestWb <- createWorkbook()
    
    # Some styles for the main table and the header
    MainTableStyle <- createStyle(halign      = "center",
                                  borderStyle = "thin",
                                  fontName    = "calibri",
                                  fontSize    = 11)
    
    HeaderStyle <- createStyle(halign         = "center", 
                               fontSize       = 11,
                               textDecoration = "bold",
                               borderStyle    = "thick",
                               border = "TopBottomLeftRight")
    
    addWorksheet(TestWb, "LSMeans")
    
    writeData(TestWb, "LSMeans", "LSMEANS by LOCATION", startCol = 2, startRow = 1)
    mergeCells(TestWb, "LSMeans", cols = (FullRange[[1]]):(FullRange[[2]]), rows = 1)
    
    writeData(TestWb, "LSMeans", "Overall LSMEANS", startCol = (FullRange[[2]] + 1), startRow = 2)
    mergeCells(TestWb, "LSMeans", cols = (FullRange[[2]] + 1):ncol(MeanData_OneTest), rows = 2)
    
    # Write the location names to the start of each location's data and then
    # merge the cells over these columns
    for(i in seq_along(LocRanges)){
      writeData(TestWb, "LSMeans", names(LocRanges)[[i]], startCol = LocRanges[[i]][[1]], startRow = 2)
      mergeCells(TestWb, "LSMeans", cols = (LocRanges[[i]][[1]]):(LocRanges[[i]][[2]]), rows = 2)
    }
    
    # Add the header style to the header rows
    addStyle(TestWb, 
             "LSMeans", 
             HeaderStyle,
             cols = 2:ncol(MeanData_OneTest), 
             rows = 1:2, 
             gridExpand = TRUE, 
             stack = TRUE)
    
    # Write and format the LSMean data
    writeData(TestWb, "LSMeans", MeanData_OneTest, startRow = 3, borders = "all", headerStyle = HeaderStyle)
    addStyle(TestWb, "LSMeans", MainTableStyle, cols = 1:ncol(MeanData_OneTest), rows = 3:(nrow(MeanData_OneTest) + 3), gridExpand = TRUE, stack = TRUE)
    
    # This needs to be fixed, ultimately want to define a set of widths so that all the data can be seen 
    # without manually fixing the column widths
    width_vec <- map(names(MeanData_OneTest), function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE)) 
    
    setColWidths(TestWb, 
                 "LSMeans", 
                 cols              = 1:ncol(MeanData_OneTest), 
                 widths            = width_vec,
                 ignoreMergedCells = TRUE)
    
    # Save the workbook
    saveWorkbook(TestWb, file = paste0(ExportDir, "/", TestName, ".xlsx"), overwrite = TRUE)
  }
  
  
}
