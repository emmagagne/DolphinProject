# Toadfish Statistical Analysis
# 11/12/2021
# EKG
#--------------------------------------------------------------------------

# preliminaries------------------------------------------------------------
library(tidyverse)
library(dunn.test)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(colorBlindness)
library(colorspace)
library(ggsignif)

# import data sets --------------------------------------------------------
# for some reason I had to convert from a tibble to a data frame
deltaTime <- read.csv("Toadfish Data New - Delta time.csv", stringsAsFactors=T)
deltaTime
levels(deltaTime$Year)


highFrequency <- read.csv("Toadfish Data New - High Frequency.csv",
                          stringsAsFactors=T)
highFrequency
levels(highFrequency$Year)


lowFrequency <- read.csv("Toadfish Data New - Low Frequency.csv",
                         stringsAsFactors=T)
lowFrequency
levels(lowFrequency$Year)


peakFrequency <- read.csv("Toadfish Data New - Peak Frequency.csv",
                          stringsAsFactors=T)
peakFrequency
levels(peakFrequency$Year)


# stats time ----------------------------------------------------------------
# Performing Kruskal-Wallis test
# Bonferroni correction: 0.05/4 = 0.0125

# delta time
result = kruskal.test(deltaTime$Delta.Time..s. ~ deltaTime$Year, 
                      data = deltaTime)
print(result)
# Kruskal-Wallis rank sum test
# data:  deltaTime$`Delta Time (s)` by deltaTime$Year
# Kruskal-Wallis chi-squared = 35.997, df = 2, p-value =  1.525e-08


# high frequency
result = kruskal.test(highFrequency$High.Frequency ~ highFrequency$Year, 
                      data = highFrequency)
print(result)
# Kruskal-Wallis rank sum test
# data:  highFrequency$`High Frequency` by highFrequency$Year
# Kruskal-Wallis chi-squared = 179.84, df = 2, p-value < 2.2e-16


# low frequency
result = kruskal.test(lowFrequency$Low.Frequency ~ lowFrequency$Year, 
                      data = lowFrequency)
print(result)
# Kruskal-Wallis rank sum test
# data:  lowFrequency$`Low Frequency` by lowFrequency$Year
# Kruskal-Wallis chi-squared = 572.32, df = 2, p-value < 2.2e-16


# peak frequency
result = kruskal.test(peakFrequency$Peak.Frequency ~ peakFrequency$Year, 
                      data = peakFrequency)
print(result)
# Kruskal-Wallis rank sum test
# data:  peakFrequency$`Peak Frequency` by peakFrequency$Year
# Kruskal-Wallis chi-squared = 38.829, df = 2, p-value =  3.701e-09

# pairwise tests -------------------------------------------------------------
# Dunn's Test

# delta time
dunn.test(x=deltaTime$Delta.Time..s., g=deltaTime$Year,
         method="bonferroni", label=TRUE, table=TRUE)
# Col Mean-|
# Row Mean |   2018 (Pr   2019 (Pr
# ---------+----------------------
# 2019 (Pr  |   1.471202
#          |     0.2119
#          |
# 2020 (Co |  -4.931112  -5.318151
#          |    0.0000*    0.0000*
                                                



# high frequency
dunn.test(x=highFrequency$High.Frequency, g=highFrequency$Year,
          method="bonferroni", label=TRUE, table=TRUE)
#Col Mean-|
# Row Mean |   2018 (Pr   2019 (Pr
# ---------+----------------------
# 2019 (Pr |   7.841102
#          |    0.0000*
#          |
# 2020 (Co |  -7.437972  -13.33719
#         |    0.0000*    0.0000*


# low frequency
dunn.test(x=lowFrequency$Low.Frequency, g=lowFrequency$Year,
          method="bonferroni", label=TRUE, table=TRUE)
# Col Mean-|
# Row Mean |   2018 (Pr   2019 (Pr
# ---------+----------------------
# 2019 (Pr |  -10.51339
#          |    0.0000*
#          |
# 2020 (Co |   16.43684   23.03053
#         |    0.0000*    0.0000*


# peak frequency
dunn.test(x=peakFrequency$Peak.Frequency, g=peakFrequency$Year,
          method="bonferroni", label=TRUE, table=TRUE)
# Col Mean-|
# Row Mean |   2018 (Pr   2019 (Pr
# ---------+----------------------
# 2019 (Pr |  -5.789494
#          |    0.0000*
#          |
# 2020 (Co |  -4.188709   2.137889
#         |    0.0000*     0.0488


# Graphics ------------------------------------------------------------------

# delta time
dfplot <- ggplot(deltaTime, aes(x=Year, y=deltaTime$'Delta.Time..s.',
                      fill=deltaTime$'Year')) +
  ylab("Delta Time (s)") + 
  xlab("Year") +
  geom_boxplot() + ggtitle("Delta Time (s)")
dt <- dfplot + theme_clean(base_size=12,
                        base_family="serif") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues") +
  geom_signif(comparisons = list(c("2018 (Pre-Covid)", "2020 (Covid)")), 
              map_signif_level=TRUE, y_position=2.6) + 
  geom_signif(comparisons = list(c("2019 (Pre-Covid)", "2020 (Covid)")), 
              map_signif_level=TRUE, y_position=2.49)
dt

# high frequency
hfplot <- ggplot(highFrequency, aes(x=Year, y=highFrequency$'High.Frequency',
                                fill=highFrequency$'Year')) +
  ylab("High Frequency (Hz)") + 
  xlab("Year") +
  geom_boxplot() + ggtitle("High Frequency (Hz)")
hf <- hfplot + theme_clean(base_size=12,
                     base_family="serif") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues") +
  geom_signif(comparisons = list(c("2018 (Pre-Covid)", "2020 (Covid)")), 
              map_signif_level=TRUE, y_position=930) + 
  geom_signif(comparisons = list(c("2019 (Pre-Covid)", "2020 (Covid)")), 
              map_signif_level=TRUE, y_position=890) +
  geom_signif(comparisons = list(c("2018 (Pre-Covid)", "2019 (Pre-Covid)")),
              map_signif_level=TRUE, y_position=890)
hf

# low frequency
lfplot <- ggplot(lowFrequency, aes(x=Year, y=lowFrequency$'Low.Frequency',
                                    fill=highFrequency$'Year')) +
  ylab("Low Frequency (Hz)") + 
  xlab("Year") +
  geom_boxplot() + ggtitle("Low Frequency (Hz)")
lf <- lfplot + theme_clean(base_size=12,
                     base_family="serif") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues") + 
  geom_signif(comparisons = list(c("2018 (Pre-Covid)", "2020 (Covid)")), 
            map_signif_level=TRUE, y_position=255) + 
  geom_signif(comparisons = list(c("2019 (Pre-Covid)", "2020 (Covid)")), 
              map_signif_level=TRUE) +
  geom_signif(comparisons = list(c("2018 (Pre-Covid)", "2019 (Pre-Covid)")),
              map_signif_level=TRUE)
lf

# peak frequency
pfplot <- ggplot(peakFrequency, aes(x=Year, y=peakFrequency$'Peak.Frequency',
                                    fill=peakFrequency$'Year')) +
  ylab("Peak Frequency (Hz)") + 
  xlab("Year") +
  geom_boxplot() + ggtitle("Peak Frequency (Hz)")
pf <- pfplot + theme_clean(base_size=12,
                     base_family="serif") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues") +
  geom_signif(comparisons = list(c("2018 (Pre-Covid)", "2020 (Covid)")), 
              map_signif_level=TRUE, y_position=530) + 
  geom_signif(comparisons = list(c("2018 (Pre-Covid)", "2019 (Pre-Covid)")),
              map_signif_level=TRUE, y_position=500)
pf

# combine plots ----------------------------------------------------------------
(dt | hf)/(lf | pf)





# Signal-to-noise ratio------------------------------------------------------
# import data
noise <- read.csv("SNR Calcuations - Final Data.csv", stringsAsFactors=T)
noise

# perform kruskal-wallis test
result = kruskal.test(noise$SNR..dB. ~ noise$Year, 
                      data = noise)
print(result)
# data:  noise$SNR..dB. by noise$Year
# Kruskal-Wallis chi-squared = 12.57, df = 2, p-value = 0.001864

# dunn's test
dunn.test(x=noise$SNR..dB., g=noise$Year,
          method="bonferroni", label=TRUE, table=TRUE)
# Col Mean-|
# Row Mean |   2018 (Pr   2019 (Pr
# ---------+----------------------
# 2019 (Pr |   0.080028
#          |     1.0000
#          |
# 2020 (Co |  -3.125675  -3.153302
#         |    0.0027*    0.0024*
                                                
# boxplot
snrplot <- ggplot(noise, aes(x=Year, y=noise$SNR..dB.,
                                    fill=noise$Year)) +
  ylab("Signal-to-Noise Ratio (dB)") + 
  xlab("Year") +
  geom_boxplot() + ggtitle("Signal-to-Noise Ratio (dB) Across Years")
snr <- snrplot + theme_clean(base_size=12,
                           base_family="serif") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues") +
  geom_signif(comparisons = list(c("2018 (Pre-Covid)", "2020 (Covid)")), 
              map_signif_level=TRUE, y_position=19) + 
  geom_signif(comparisons = list(c("2019 (Pre-Covid)", "2020 (Covid)")),
              map_signif_level=TRUE, y_position=17)
snr
