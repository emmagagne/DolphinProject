# New Toadfish Analysis Without 2018
# 12/10/2021
# EKG
#--------------------------------------------------------------------------

# preliminaries------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(colorBlindness)
library(colorspace)
library(ggsignif)

# import data sets --------------------------------------------------------
# for some reason I had to convert from a tibble to a data frame
deltaTime <- read.csv("Toadfish Data New - Delta time_New.csv", 
                      stringsAsFactors=T)
deltaTime
levels(deltaTime$Year)


highFrequency <- read.csv("Toadfish Data New - High Frequency_New.csv",
                          stringsAsFactors=T)
highFrequency
levels(highFrequency$Year)


lowFrequency <- read.csv("Toadfish Data New - Low Frequency_New.csv",
                         stringsAsFactors=T)
lowFrequency
levels(lowFrequency$Year)


peakFrequency <- read.csv("Toadfish Data New - Peak Frequency_New.csv",
                          stringsAsFactors=T)
peakFrequency
levels(peakFrequency$Year)

RMSAmp <- read.csv("Toadfish Data New - RMS Amp_New.csv",
                   stringsAsFactors=T)
RMSAmp
levels(RMSAmp$Year)

snr <- read.csv("SNR Calcuations - Final Data_New.csv",
                stringsAsFactors=T)
snr
levels(snr$Year)

# Mann-Whitney Test ---------------------------------------------------------

# delta time
wilcox.test(deltaTime$Delta.Time..s.~deltaTime$Year, data=deltaTime)

# Wilcoxon rank sum test with continuity correction

# data:  deltaTime$Delta.Time..s. by deltaTime$Year
# W = 50520, p-value = 1.13e-07
# alternative hypothesis: true location shift is not equal to 0''

# high frequency
wilcox.test(highFrequency$High.Frequency~highFrequency$Year, data=highFrequency)

# W = 33328, p-value < 2.2e-16

# low frequency
wilcox.test(lowFrequency$Low.Frequency~lowFrequency$Year, data=lowFrequency)

# W = 121587, p-value < 2.2e-16

# peak frequency
wilcox.test(peakFrequency$Peak.Frequency~peakFrequency$Year, data=peakFrequency)

# W = 68597, p-value = 0.3134

# RMS Amp
wilcox.test(RMSAmp$RMS.Amp..U.~RMSAmp$Year, data=RMSAmp)

# W = 131494, p-value < 2.2e-16

# SNR
wilcox.test(snr$SNR..dB.~snr$Year, data=snr)

# W = 17.5, p-value = 0.001233

# Plots ---------------------------------------------------------------------

# delta time
timeplot <- ggplot(deltaTime, aes(x=Year, y=deltaTime$Delta.Time..s.,
                                    fill=deltaTime$Year)) +
  ylab("Delta Time (s)") + 
  xlab("Year") +
  geom_boxplot() + ggtitle("(a) Delta Time")
dtplot <- timeplot + theme_clean(base_size=12,
                           base_family="serif") +
  theme(legend.position = "none") +
  scale_fill_grey(start = 0.8, end = 0.5) +
  geom_signif(comparisons = list(c("2019 (Pre-Covid)", "2020 (Covid)")), 
              map_signif_level=TRUE)
dtplot

# high frequency
highfreqplot <- ggplot(highFrequency, aes(x=Year, y=highFrequency$High.Frequency,
                                  fill=highFrequency$Year)) +
  ylab("High Frequency (Hz)") + 
  xlab("Year") +
  geom_boxplot() + ggtitle("(d) High Frequency")
hfplot <- highfreqplot + theme_clean(base_size=12,
                                 base_family="serif") +
  theme(legend.position = "none") +
  scale_fill_grey(start = 0.8, end = 0.5) +
  geom_signif(comparisons = list(c("2019 (Pre-Covid)", "2020 (Covid)")), 
              map_signif_level=TRUE)
hfplot

# low frequency
lowfreqplot <- ggplot(lowFrequency, aes(x=Year, y=lowFrequency$Low.Frequency,
                                          fill=lowFrequency$Year)) +
  ylab("Low Frequency (Hz)") + 
  xlab("Year") +
  geom_boxplot() + ggtitle("(c) Low Frequency")
lfplot <- lowfreqplot + theme_clean(base_size=12,
                                     base_family="serif") +
  theme(legend.position = "none") +
  scale_fill_grey(start = 0.8, end = 0.5) +
  geom_signif(comparisons = list(c("2019 (Pre-Covid)", "2020 (Covid)")), 
              map_signif_level=TRUE)
lfplot

# RMS AMP
rmsamp <- ggplot(RMSAmp, aes(x=Year, y=RMSAmp$RMS.Amp..U.,
                                        fill=RMSAmp$Year)) +
  ylab("RMS Amplitude (U)") + 
  xlab("Year") +
  geom_boxplot() + ggtitle("(b) RMS Amplitude")
rmsplot <- rmsamp + theme_clean(base_size=12,
                                    base_family="serif") +
  theme(legend.position = "none") +
  scale_fill_grey(start = 0.8, end = 0.5) +
  geom_signif(comparisons = list(c("2019 (Pre-Covid)", "2020 (Covid)")), 
              map_signif_level=TRUE)
rmsplot

# SNR
snrplot <- ggplot(snr, aes(x=Year, y=snr$SNR..dB.,
                             fill=snr$Year)) +
  ylab("Signal-to Noise-Ratio (dB)") + 
  xlab("Year") +
  geom_boxplot() + ggtitle("Signal-to-Noise Ratio")
snrplot <- snrplot + theme_clean(base_size=14,
                                base_family="serif") +
  theme(legend.position = "none") +
  scale_fill_grey(start = 0.8, end = 0.5) +
  geom_signif(comparisons = list(c("2019 (Pre-Covid)", "2020 (Covid)")), 
              map_signif_level=TRUE)
snrplot

# combine figures about acoustic structure
(dtplot | rmsplot)/(lfplot | hfplot)

snrplot
