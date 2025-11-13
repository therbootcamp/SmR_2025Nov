# 18 February 2019
# http://therbootcamp.github.io

# Script to install any packages necessary for the BaselRBootcamp

Bootcamp_package_installation <- function() {
  
  installed_packages <- rownames(installed.packages())
  
  
  # Critical packages 
  critical <- c(
    
    # Utilities --------------
    "devtools",
    
    # Tidyverse --------------
    "tidyverse",
    
    # Robust stats
    "gam",
    "boot",
    "moments",
    "quantreg",
    "Rfit",
    
    # New Stats
    "rstanarm",
    "pwr",
    
    # Mixed Models
    "lme4",
    "sjstats",
    
    "broom"
    
  )
  
  # Optional packages
  optional <- c(
    # 
    # # Utilities --------------
    # 
    # "Rcpp",
    # "rvest",
    "lubridate", 
    # "skimr",
    
    # Markdown / Shiny --------------
    
    # "shiny", 
    # "shinyjs",
    # "markdown", 
    # "rmarkdown",
    # "rmdformats",
    # "DT",
    
    
    # Stats --------------
    
    # "lme4",
     "afex",
     "car",
     "BayesFactor",
    # "parfm", 
    # "lava",
    "rsq",
    "broom",
    "pbkrtest",
    
    # ML --------------
    
    # "caret",                       
    # "FFTrees",
    # "rpart", 
    # "randomForest", 
    # "e1071", 
    # "party", 
    # "partykit", 
    # "glmnet",
    # "earth",
    # "tensorflow",
    # "Boruta",
    
    # Plotting --------------
    "yarrr", 
    # "diagram",
    # "ggRandomForests",
    "ggthemes", 
    "ggpubr",
    
    # Datasets --------------
    "speff2trial",
    "ISLR")
  
  
  message("Welcome to the BaselRBootcamp!")
  message("I'm going to install many packages for you now, this may take a few minutes...")
  
  critical_not_installed <- c()
  optional_not_installed <- c()
  
  for(package.i in sort(critical)) {
    
    if((package.i %in% installed_packages) == FALSE) {
      
      if(package.i == "Rcpp"){
        test <- try(install.packages(package.i))
        if(is.null(test)){
        } else {
          message(paste("Error: could not install package Rcpp"))
        }
      } else {
        install.packages(package.i)
      }
      
    }
    
    # If package wasn't installed, then send us an email
    if((package.i %in% rownames(installed.packages())) == FALSE) {
      
      critical_not_installed <- c(critical_not_installed, package.i)
      
    }
    
  }
  
  
  for(package.i in sort(optional)) {
    
    if((package.i %in% installed_packages) == FALSE) {
      
      if(package.i == "Rcpp"){
        test <- try(install.packages(package.i))
        if(is.null(test)){
          # message(paste("Installing", package.i, "..."))
        } else {
          message(paste("Error: could not install package Rcpp"))
        }
      } else {
        install.packages(package.i)
      }
      
    }
    
    
    # If package wasn't installed, then send us an email
    if((package.i %in% rownames(installed.packages())) == FALSE) {
      
      optional_not_installed <- c(optional_not_installed, package.i)
      
    }
    
  }
  
  devtools_packages_to_install <- c("kassambara/ggcorrplot", "therbootcamp/baselers")
  
  for(package.i in sort(devtools_packages_to_install)) {
    
    if((strsplit(package.i, split = "/")[[1]][2] %in% installed_packages) == FALSE) {
      
      devtools::install_github(package.i)
    }
  }
  
  if(length(critical_not_installed) > 0 | length(optional_not_installed) > 0) {
    
    message("Final report:")
    
    warning(paste("The critical packages [", 
                  paste(critical_not_installed, sep = ", "), 
                  "] and the optional packages [", 
                  paste(optional_not_installed, sep = ", "), 
                  "] could not be installed. Please copy and paste this text and send it to us at therbootcamp@gmail.com so we can help!")) 
  } else {
    message("*** Final report ***")
    
    message("All packages were installed without issue!!!")
    
  }
  
  message("--------------------------------")
  message("See you soon at the bootcamp :)")
  message("therbootcamp@gmail.com")
  message("www.therbootcamp.com")
  message("--------------------------------")
  
}


# Run code!
Bootcamp_package_installation()
