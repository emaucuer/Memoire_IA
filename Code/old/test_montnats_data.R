setwd("G:/Drive partagés/BU_ACT_FR/4 - Mémoires/Mémoires 2022/Mémoire de MAUCUER Eurydice/Code")


# Libraries
library(tidyverse)
library(hrbrthemes)
library(kableExtra)
library(patchwork)
library(readxl)

# Données
# faire ctrl+c sur les donner à importer
my_data <- read.table(file = "clipboard", 
                      sep = "\t", header=TRUE)


# Plot
my_data %>%
  arrange(Montant) %>%
  ggplot( aes(x=Péril, y=Montant) ) +
  geom_segment( aes(x=Péril ,xend=Péril, y=0, yend=Montant), color="grey") +
  geom_point(size=3, color="#69b3a2") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("") +
  ylab("Montant cumulé de sinistre")
