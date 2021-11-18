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

# import data sets
Precovid <- read_csv("Precovid1.csv")
View(Precovid)


Covid <- read_csv("Covid1.csv")
View(Covid)

# restructure data frame into a list
Precovid <- as.numeric(unlist(Precovid))
Precovid
Covid <- as.numeric(unlist(Covid))
Covid
repertoire <- list(Precovid, 
                    Covid)
my_names <- c("pre-covid","covid")
names(repertoire) <- my_names
str(repertoire)


# the following commands display the sample species abundances and 
# run the iNEXT() function for q=0.
x <- iNEXT(repertoire, q=0, se=TRUE, conf=0.95, nboot=1000, datatype="abundance")

# Built in graphics displays
g <- ggiNEXT(x, type=1, se=TRUE, facet.var="none", color.var="site", grey=FALSE)
g1 <- g + scale_colour_manual(values=c("red", "blue")) +
  scale_fill_manual(values=c("red", "blue")) +
  labs(x="Number of individuals", y="Whistle diversity")
g1


# Graphic incorporating hill numbers 0,1,2
# Hill q=0: diversity of all species. the abundances of individual species 
# do not contribute to the sum. Rather, only presences are counted.
# Hill q=1: diversity of "typical" species. (Shannon index)
# Hill q=2: diversity of dominant species
out <- iNEXT(repertoire, q=c(0, 1, 2), datatype="abundance", endpoint=500,
             se=TRUE, conf=0.95, nboot=1000)
# Sample-size-based R/E curves, separating plots by "site"
g <- ggiNEXT(out, type=1, facet.var="site") + theme_bw(base_size=18)
g2 <- g + scale_colour_manual(values=c("red", "blue", "green")) +
  scale_fill_manual(values=c("red", "blue", "green")) +
  labs(x="Number of individuals", y="Whistle diversity")
g2

# sample completeness curve
g <- ggiNEXT(out, type=2)
g
g3 <- g + scale_colour_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue"))
g3

# Sample-size-based R/E curves, separating plots by "order"
g <- ggiNEXT(out, type=1, facet.var="order")
g
g4 <- g + scale_colour_manual(values=c("red", "blue")) +
  scale_fill_manual(values=c("red", "blue")) +
  labs(x="Number of individuals", y="Whistle diversity")
g4
