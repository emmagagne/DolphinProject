# Almirante Noise dbWav
# 12/17/2021
# EKG
#--------------------------------------------------------------------------

# preliminaries -----------------------------------------------------------
library(ggplot2)
library(ggthemes)
library(patchwork)
library(ggsignif)
library(tidyverse)
library(scales)
library(lubridate)
library(chron)
library(colorspace)

# load data ------------------------------------------------------------

both <- read.csv("wilcoxdbwav.csv", stringsAsFactors = T)
head(both)

timeofday <- read.csv("timeofday.csv", stringsAsFactors = T)
head(timeofday)
typeof(timeofday$Start.Time)

# Mann-Whitney Test for Significance ------------------------------------------

wilcox.test(both$RMS.dB~both$ï..Year, data=both)
# W = 107412, p-value = 8.957e-06

# Boxplot figure ----------------------------------------------------------

noiseplot <- ggplot(both, aes(x=both$ï..Year, y=both$RMS.dB,
                                  fill=both$ï..Year)) +
  ylab("RMS (dB)") + 
  xlab("Year") +
  geom_boxplot() + 
  ggtitle("Ambient Noise (dB)")
noiseplot <- noiseplot + theme_clean(base_size=12,
                                 base_family="serif") +
  theme(legend.position = "none") +
  scale_fill_grey(start = 0.8, end = 0.5) +
  geom_signif(comparisons = list(c("2019 (Pre-Covid)", "2020 (Covid)")), 
              map_signif_level=TRUE)
noiseplot

# time of day figure ------------------------------------------------------
  
timeofday$Start.Time <- as.POSIXct(timeofday$Start.Time,
                                    format = "%H:%M",
                                    tz = "UTC")

p1 <- ggplot(data=timeofday,
             mapping=aes(x=timeofday$Start.Time,
                         y=timeofday$RMS..dB..Average,
                         group=timeofday$ï..Year,
                         color=timeofday$ï..Year)) +
  geom_point() + geom_smooth() +
  theme_clean(base_size=12, base_family="serif") +
  scale_x_datetime(breaks=date_breaks("1 hour"), 
                   labels=date_format("%H:%M"),
                   expand=c(0,0)) +
  ylab("RMS (dB)") +
  theme(legend.position="bottom") +
  scale_colour_manual(name="Year",
                      values = c("red", "blue")) +
  xlab("Time of day (hh:mm)") +
  ggtitle("Ambient Noise Level and Time of Day")
print(p1)

scale_x_datetime()
