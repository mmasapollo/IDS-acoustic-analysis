#setwd("C:\\Users\\zugi\\Dropbox\\IPLP\\Vowel space\\Ads\\W03(ads)\\")
#read.table("W03(Ads)_formant_point.txt",skip=0,comment="#", header=T) -> ads
#read.table("W03(Ads)_formant.txt",skip=0,comment="#", header=T) -> ads


#setwd("C:\\Users\\zugi\\Dropbox\\IPLP\\Vowel space\\Ids\\W03(Ids)\\")
#read.table("W03(ids)_formant_point.txt",skip=0,comment="#", header=T) -> ids
#read.table("W03(ids)_formant.txt",skip=0,comment="#", header=T) -> ids

setwd("C:/Users/chosun/Desktop/Vowel/W03(ads)/")
read.table("W03(Ads)_formant(7000).txt",skip=0,comment="#", header=T) -> ads

setwd("C:/Users/chosun/Desktop/Vowel/W03(ids)/")
read.table("W03(ids)_formant(7000).txt",skip=0,comment="#", header=T) -> ids

vowel<-rbind(ads,ids)
library(ggplot2)
x11()
ggplot(data = vowel, aes(x = F2, y = F1, color = register, label = vowel)) + 
  geom_text() + 
  scale_y_reverse() + 
  scale_x_reverse() 

#ggplot(data = vowel, aes(x = F2, y = F1, color = factor(register), label = vowel)) + 
  geom_text() + 
  scale_y_reverse() + 
  scale_x_reverse() +
  theme(legend.position = "none")
###########################################################
#basic plot
ads->vowel
ids->vowel
ggplot(data = vowel, aes(x = F2, y = F1, color = vowel, label = vowel)) + 
  geom_text() + 
  scale_y_reverse() + 
  scale_x_reverse() +
  theme(legend.position = "none")

ggplot(data = vowel, aes(x = F2, y = F1, color = vowel, label = vowel)) + 
  geom_text() + 
  scale_y_reverse(position = "right") + 
  scale_x_reverse(position = "top") +
  theme(legend.position = "none")

####
#error bars
####

# First, create summary table (tibble) with means and standard errors
# I'm using dplyr here (since I loaded tidyverse above)
#library(tidyverse)
install.packages("dplyr")
library(dplyr)
means = vowel %>% group_by(vowel) %>% summarize(meanF1 = mean(F1),
                                                 meanF2 = mean(F2),
                                                 seF1 = sd(F1)/sqrt(n()),
                                                 seF2 = sd(F2)/sqrt(n()))
x11()
library(scales)
ggplot(data = means, aes(x = meanF2, y = meanF1, label = vowel)) + 
  geom_errorbar(aes(ymin = meanF1 - seF1, ymax = meanF1 + seF1), width = 0) + 
  geom_errorbarh(aes(xmin = meanF2 - seF2, xmax = meanF2 + seF2), height = 0) +
  geom_text(position = position_nudge(x = 50, y = 50), size = 5) + 
  scale_y_reverse(position = "right", labels = unit_format(unit = "Hz", sep = "")) + 
  scale_x_reverse(position = "top", labels = unit_format(unit = "Hz", sep = "")) + 
  labs(x = "F2\n",
       y = "F1\n") + 
  theme_light()

#semi-transparent ellipses
ggplot(data = vowel, aes(x = F2, y = F1, color = vowel, label = vowel)) + 
  geom_text(size = 6) + # Font size for vowels
  scale_y_reverse(position = "right", 
                  labels = unit_format(unit = "Hz", sep = ""),
                  breaks = seq(100, 1000, 250)) + 
  scale_x_reverse(position = "top", 
                  labels = unit_format(unit = "Hz", sep = ""),
                  breaks = seq(200, 3000, 500)) + 
  labs(x = "F2\n",
       y = "F1\n",
       title = "IDS") + 
  stat_ellipse(type = "norm", alpha = 0.3) +
  coord_cartesian(xlim = c(200, 3000), 
                  ylim = c(100, 1200)) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5), # Center plot title
        text = element_text(size = 13))         # Font size for plot
#############
#https://drammock.github.io/phonR/
############

