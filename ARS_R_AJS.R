setwd("C:/Users/asomm/OneDrive - UW-Madison/Work_Documents/Projects/Antibiotic Resistance Testing/R")

library(dplyr)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(tidyverse)
library(ggpubr)

df1 <- read.csv("ARS_Data_Summary.csv")

#EC
df1_EC <- df1 %>% filter(Taxa == "EC")
df1_EC_long <- df1_EC %>%
  pivot_longer(cols = c(R, I, S), names_to = "Resistance", values_to = "Count")


df1_EC_long$Resistance <- factor(df1_EC_long$Resistance, levels=c("S", "I", "R"))

p1 <- ggplot(df1_EC_long, aes(x=Source, y=Count, fill=Resistance)) +
  geom_bar(stat='identity', position='fill', color = "Black") +
  facet_grid(~Antibiotic) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
  theme_half_open() +
  scale_fill_manual(values=c("#D6D6D8", "#C0E6F5", "#D07D8D")) +
  theme(strip.background = element_rect(colour="white", fill="white"))  +
  scale_x_discrete(expand=c(1,0)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) + theme(legend.position="none")
p1

#KP
df1_Kleb <- df1 %>% filter(Taxa == "Kleb")
df1_Kleb_long <- df1_Kleb %>%
  pivot_longer(cols = c(R, I, S), names_to = "Resistance", values_to = "Count")
df1_Kleb_long$Resistance <- factor(df1_Kleb_long$Resistance, levels=c("S", "I", "R"))
p2<- ggplot(df1_Kleb_long, aes(x=Source, y=Count, fill=Resistance)) +
  geom_bar(stat='identity', position='fill', color = "Black") +
  facet_grid(~Antibiotic) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
  theme_half_open() +
  scale_fill_manual(values=c("#D6D6D8", "#C0E6F5", "#D07D8D")) +
  theme(strip.background = element_rect(colour="white", fill="white"))  +
  scale_x_discrete(expand=c(1,0))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) + theme(legend.position="none")
p2

#Other
df1_Other <- df1 %>% filter(Taxa == "Other")
df1_Other_long <- df1_Other %>%
  pivot_longer(cols = c(R, I, S), names_to = "Resistance", values_to = "Count")
df1_Other_long$Resistance <- factor(df1_Other_long$Resistance, levels=c("S", "I", "R"))
p3<- ggplot(df1_Other_long, aes(x=Source, y=Count, fill=Resistance)) +
  geom_bar(stat='identity', position='fill', color = "Black") +
  facet_grid(~Antibiotic) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
  theme_half_open() +
  scale_fill_manual(values=c("#D6D6D8", "#C0E6F5", "#D07D8D")) +
  theme(strip.background = element_rect(colour="white", fill="white"))  +
  scale_x_discrete(expand=c(1,0))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) + theme(legend.position="none")
p3

#Staph
df1_Staph <- df1 %>% filter(Taxa == "Staph")
df1_Staph_long <- df1_Staph %>%
  pivot_longer(cols = c(R, I, S), names_to = "Resistance", values_to = "Count")
df1_Staph_long$Resistance <- factor(df1_Staph_long$Resistance, levels=c("S", "I", "R"))
p4<- ggplot(df1_Staph_long, aes(x=Source, y=Count, fill=Resistance)) +
  geom_bar(stat='identity', position='fill', color = "Black") +
  facet_grid(~Antibiotic) +
  scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
  theme_half_open() +
  scale_fill_manual(values=c("#D6D6D8", "#C0E6F5", "#D07D8D")) +
  theme(strip.background = element_rect(colour="white", fill="white"))  +
  scale_x_discrete(expand=c(1,0))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) + theme(legend.position="none")
p4


ggarrange(p1, p2, p3, p4 + rremove("x.text"), 
          labels = c("A", "B", "C","D"),
          ncol = 2, nrow = 2)

