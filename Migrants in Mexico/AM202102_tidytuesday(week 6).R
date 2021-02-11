
rm(list = ls())
cls <- function() cat(rep("\n",100))
wd <- "E:/Azael Personal/Documentos/R/tidytuesday"
setwd(wd)
cls()


#/************************************************************************************************************************************************
# Filename: AM202101_tidytuesday(week 6)
# Author: Azael Mateo
# Date: 09/February 2021
#
# Purpose: This file generates the script to build a vis of the main causes of migration in Mexico.
# Data source: Censo de Población y Vivienda 2020, INEGI.
#
# Created files: -
#**************************************************************************************************************************************************/


library(tidyverse)
library(haven)


# Import data ------------------------------------------------------------------------------------------------------------------------------------
data <- read_csv("https://github.com/AzaelMateo/tidytuesday/raw/master/Migrants%20in%20Mexico/INEGI_Exporta_20210209205620.csv",
                 col_names = TRUE, col_types = "cddddddddddddd")


# Build sub-data ---------------------------------------------------------------------------------------------------------------------------------
data <- data %>% 
  select(`Buscar trabajo`:`Le deportaron (regresaron)`) %>% 
  slice(1) %>% 
  t() %>% 
  data.frame(causa = row.names(data), tot = .) %>% 
  mutate(n = -3:4) %>% 
  rowwise() %>%
  mutate(x = list(c(-300, 0, 0, -300)),
         y = list(c(n*4 - 1.4, n*2 - 0.7, n*2 + 0.7, n*4 + 1.4))) %>% 
  unnest(cols = c(x, y)) 


# Plot -------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data) +
  # first rectangle
  geom_rect(aes(xmin = -1200, ymin = n*4 - 1.4,
                xmax = -300, ymax = n*4 + 1.4), fill = "#D45837", color = NA) +
  # second rectangle
  geom_polygon(aes(x, y, group = n), fill = "#723424", color = NA) +
  # third rectangle
  geom_rect(aes(xmin = 0, ymin = n*2 - 0.7,
                xmax = tot/1000, ymax = n*2 + 0.7), fill = "#46555C", color = NA) +
  geom_text(aes(-1190, n*4, label = causa), family = "JetBrains Mono Bold", color = "white", hjust = 0, size = 12, check_overlap = TRUE) +
  geom_text(aes(tot/1000+5, n*2, label = format(tot, big.mark=",", small.interval=3)), family = "JetBrains Mono Medium", color = "#46555C", hjust = 0, size = 10, check_overlap = TRUE) +
  # legends
  annotate("text", 2700, 16, label = "Principales causas de la migración en México", family = "IBM Plex Sans Bold", color = "black", hjust = 1, size = 25) +
  annotate("text", 2700, 14, label = "Incluye migración intraestatal, interestatal e internacional", family = "IBM Plex Sans Bold", color = "black", hjust = 1, size = 15) +
  annotate("text", 2700, 10, label = "Fuente: Censos 2020, INEGI\nElaborado por Azael Mateo (@xzxxlmxtxx)", family = "IBM Plex Sans Bold", color = "black", hjust = 1, size = 10, lineheight = 0.3) +
  scale_x_continuous(limits = c(-1200, 2700), labels = NULL) +
  theme_minimal(base_family = "JetBrains Mono Medium") +
  theme(
    plot.background = element_rect(fill = "#D3E1E6", colour = "#D3E1E6"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.margin = margin(20, 5, 20, 5)
  ) 


# Export vis -------------------------------------------------------------------------------------------------------------------------------------------
ggsave("AM202002_migrants(week 6).png", width = 12, height = 6)

