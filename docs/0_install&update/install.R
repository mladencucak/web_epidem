## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)


## -----------------------------------------------------------------------------
R.version.string
#> [1] "R version 4.1.2 (2021-11-01)"


## ----libs, message=FALSE, warning=FALSE---------------------------------------
list.of.packages <-
  c(
    "devtools", # package development 
    "conflicted", #Some functions are named the same way across different packages and they can be conflicts and this package helps sorting this problem 
    "here", #helps with the reproducibility across operating systems=
    "conflicted", #Some functions are named the same way across different packages and they can be conflicts and this package helps sorting this problem 
  
    # data import
    "readr", 
    "readxl", # import of data from excel files
    
    # data mundging
    "dplyr", # data manipulation
    "tidyr", # tidying data
    "tidyr", # "modern" data structures
    "reshape2", # reshaping the data
    "purrr", # tools for working with functions and vectors
    
    # Ploting 
    "ggplot2", # most popular 2D visualisation tool in R
    "plotly", # another advanced visualization tool
    "ggsci", # color palletes 
    "ggthemes", # ggplot themes 
    "patchwork", # combine plots
    "stringr", # string manipulation 
    "stringr", # factor manipulation

    # Analysis/Modelling 
    "minpack.lm", # fitting non-linear models
    "deSolve", # solvers for Initial Value Problems of Differential Equations
    "psych",
    "car",
    "emmeans",
    "agricolae", #diverse functions and datasets for agronomy 
    
    # Visualising outputs of statistical analysis 
    "rcompanion", # Useful tool for summarizing model outputs and several other modeling related tasks
    "ggResidpanel", # Diagnostic plots for several models

    # Parallel computation (using multiple cores to execute R code)
    "pbapply", 
    "parallel",

    # Text formatting 
    "rmarkdown",
    "gt", # package for customizing html table view
    "kableExtra", # Another package for html table customization
    
    # Advanced visualisations/ App building
    "shiny", # Package for visualization 
    "shinyscreenshot", #capture screenshots
    "shinydashboard"
  )


## -----------------------------------------------------------------------------
# You might need to restart R during this process
install.packages(list.of.packages)
# When prompted 


## -----------------------------------------------------------------------------
# uncomment the following line, change the name of package and run
#install.packages("ROCR", repos = c(CRAN="https://cran.r-project.org/")) 


## -----------------------------------------------------------------------------
list.of.packages %in% installed.packages()


## -----------------------------------------------------------------------------
list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]



## -----------------------------------------------------------------------------
update.packages(ask = FALSE, checkBuilt = TRUE)


## -----------------------------------------------------------------------------
## get packages installed
packs = as.data.frame(installed.packages(.libPaths()[1]), stringsAsFactors = F)

## and now re-install install packages using install.packages()
install.packages(packs$Package)


## -----------------------------------------------------------------------------
.rs.restartR()
Sys.sleep(2)
remotes::install_github("bbc/bbplot",dependencies = TRUE)


## ----error_noncran_pck, eval=FALSE--------------------------------------------
## Error: (converted from warning) cannot remove prior installation of package ‘name_of_the_package’


## -----------------------------------------------------------------------------
install.packages("name of the package", type = "source")

