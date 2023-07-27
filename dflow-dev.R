#for dflow test
#install dflow package from a dev branch
devtools::install_github("DEQepricha/dflowR-fork@main", 
                         host = "https://api.github.com", 
                         dependencies = TRUE, force = TRUE, upgrade = "never")
library(dflowR)
