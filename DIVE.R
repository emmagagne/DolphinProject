# Diversity Estimator for Dolphin Repertoire
# 10/06/2021
# EKG
#--------------------------------------------------------------------------

# install iNEXT package from CRAN
install.packages("iNEXT")

# import packages
library(iNEXT)
library(ggplot2)
library(ggthemes)
library(readr)

# import data set
DolphinRepertoireData <- read_csv("DolphinRepertoireData.csv")
View(DolphinRepertoireData)

str(DolphinRepertoireData.csv)
