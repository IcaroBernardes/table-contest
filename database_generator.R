# 0. Loads packages
library(tidyverse)
library(readr)

# 1. Initial database handling
## Loads database
original <- readr::read_delim("bgg_dataset.csv", delim = ";", escape_double = FALSE,
                              trim_ws = TRUE, locale = locale(decimal_mark = ","),
                              show_col_types = FALSE)

## Converts ID to character
original <- original %>% dplyr::mutate(ID = as.character(ID))

## Dispenses unused variables
original <- original %>% dplyr::select(-Mechanics)

## Handles missing data
original <- original %>%
  dplyr::mutate(Domains = ifelse(is.na(Domains),"Not informed",Domains))

## Eliminates possible duplicates
original <- original %>%
  dplyr::distinct(ID, .keep_all = TRUE)

# 2. Creates new categorical counterparts to numerical variables
## Takes a subset with only the numerical variables from the original database
numerical <- original %>% 
  dplyr::select(where(is.numeric))

## Sets the superior limit for the categories in each variable
`Year Published` <- tibble(
  sup = c(500,1500,1945,Inf),
  cat = c("Ancient Games","Medieval Games","Modern Games","Contemporary Games")
)

`Min Players` <- tibble(
  sup = c(1,2,5,Inf),
  cat = c("By yourself","Paired","With a group","With a party")
)

`Max Players` <- tibble(
  sup = c(1,2,5,Inf),
  cat = c("By yourself","Paired","With a group","With a party")
)

`Play Time` <- tibble(
  sup = c(5,30,60,6000,Inf),
  cat = c("Lightning","Quick","Tipical","Hours","Days")
)

`Min Age` <- tibble(
  sup = c(3,12,18,Inf),
  cat = c("Babies","Childs","Teens","Adults")
)

`Users Rated` <- tibble(
  sup = c(100,1000,Inf),
  cat = c("A few","A bunch","A lot")
)

`Rating Average` <- tibble(
  sup = c(5,6,7,8,Inf),
  cat = c("Terrible","Passable","Reasonable","Good","Excellent")
)

`BGG Rank` <- tibble(
  sup = c(10,100,500,Inf),
  cat = c("Top 10","Top 100","Top 500","Rest of the rank")
)

`Complexity Average` <- tibble(
  sup = c(1,2,3,4,Inf),
  cat = c("Light","Medium light","Medium","Medium heavy","Heavy")
)

`Owned Users` <- tibble(
  sup = c(144,288,480,720,Inf),
  cat = c("Niche","Hipster","Common","Known","Famous")
)

## Creates a function to convert each variable to a categorical format
categorizer <- function (var) {
  
  ### Gets the names of each variable
  varname = names(numerical)[[var]]
  
  ### Gets the object that has the same name as the variable
  levels = get0(varname)
  
  if (is.null(levels)) {
    NULL
  } else {
    #### Breaks the numerical variables according to the
    #### breaks and replaces their values by the labels
    breaks = santoku::brk_manual(levels$sup, rep(FALSE, length(levels$sup)))
    labels = levels$cat
    santoku::chop(numerical[[var]], breaks, labels, extend = TRUE)
  }
  
}

### Applies the function
categorical <- lapply(seq_along(numerical), categorizer)

### Converts the list to a data.frame 
categorical <- purrr::map_dfc(categorical, ~.x)

### Creates a prefix to the categorical variables names 
colnames(categorical) <- paste("CAT",colnames(numerical))

# 3. Unites and saves the data
final <- cbind(original, categorical)
readr::write_rds(final, "database.rds")
