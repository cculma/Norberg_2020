# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("jsonlite")
# library(data.table)
# library(ggplot2)
# library(jsonlite)
library(Matrix)

install.packages("~/Documents/git/software/asreml-4.1.0.176-macOS-10.13.2-R4.2.tar", repos = NULL, type = "source")
# install.packages("/home/hawkins/Documents/Cesar/blup_data/ASReml/ASReml_purchase/asreml-4.1.0.149-Ubuntu-18-R3.6.tar.gz", repos = NULL)



# install.packages("R/x86_64-pc-linux-gnu-library/3.6/asreml-4.1.0.149-Ubuntu-18-R4.0.tar.gz",repos = NULL)
rm(list = ls())
library(asreml)
# asreml.license.activate()
remove.packages(asreml)
remove.packages(asremlPlus)
# asreml.license.status(quiet = FALSE, task = "checkout", json = "")
# asreml.license.status(quiet = FALSE, task = "checkout", json = "")


# devtools::install_github("AparicioJohan/MrBeanApp")
library(MrBean)
run_app()
dev.off()

