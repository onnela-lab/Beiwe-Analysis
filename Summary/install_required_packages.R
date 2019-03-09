# Required packages for this code to run
required_packages <- c("dplyr", "tidyr", "ggplot2", "MASS", "RcppRoll", "zoo", "lubridate", "stringr", "ggpubr", "rcompanion")

# This function is used to check whether a package is installed
is_installed <- function(pkg) {
  is.element(pkg, installed.packages()[,1])
}

# Install required libraries if not installed
for(p in required_packages) {
  if (!is_installed(p)) {
    install.packages(p, repos="https://cran.us.r-project.org")
  }
}
