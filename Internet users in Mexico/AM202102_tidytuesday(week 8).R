
rm(list = ls())
cls <- function() cat(rep("\n",100))
wd <- "E:/Azael Personal/Documentos/R/tidytuesday"
setwd(wd)
cls()


#/************************************************************************************************************************************************
# Filename: AM202101_tidytuesday(week 8)
# Author: Azael Mateo
# Date: 24/February 2021
#
# Purpose: This file generates the script to build a vis of ...
#
# Created files: -
#**************************************************************************************************************************************************/


library(tidyverse)
library(treemapify)
library(haven)
library(ggthemes)
library(sf)
library(patchwork)


# Import data ------------------------------------------------------------------------------------------------------------------------------------
data <- read_csv("https://github.com/AzaelMateo/tidytuesday/raw/master/Income%20poverty%20in%20Mexico/CONEVAL_ITLP.csv",
                 col_names = TRUE, col_types = "cdddd")


# Sub-set ----------------------------------------------------------------------------------------------------------------------------------------
data <- data %>% 
  filter(ent != "NACIONAL") %>% 
  mutate(v_alpha = contr/max(contr), ent_clave = as.factor(seq(01,32,01))) %>%
  select(ent_clave, ent, `4t`, contr, v_alpha)


# Mx maps ----------------------------------------------------------------------------------------------------------------------------------------
shapes_ent <- "C:/Users/dell/Documents/Stata/Ejercicios/mapeodedatos/mge2005v_1_0/Entidades_2005.shp"
mex_map <- st_read(shapes_ent)

map_ie <- mex_map %>% 
  mutate(CVE_EDO = as.factor(as.numeric(CVE_EDO))) %>% 
  left_join(data, by = c("CVE_EDO" = "ent_clave"))

p1 <- map_ie %>% 
  ggplot(aes(fill = `4t`)) +
  geom_sf(colour = "white", size = 0.3) +
  labs(title = "Porcentaje de la población en pobreza laboral",
       fill = "Pct") +
  scale_fill_gradient(low = "#B4CCD3", high = "#46555C", name = "Porcentaje", breaks = c(.3, .4, .5, .6), labels = c("30%", "40%", "50%", "60%")) +
  theme_void() +
  theme(
    legend.position = c(0.15, 0.26),
    legend.key.size = unit(1, "cm"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 15),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = -4),
    )


# Treemap ----------------------------------------------------------------------------------------------------------------------------------------
p2 <- data %>% 
  ggplot2::ggplot(ggplot2::aes(area = contr, label = paste(ent,"\n", paste(round(contr/sum(contr)*100, 0),"%",sep = "")))) + 
  geom_treemap(aes(alpha = v_alpha), fill = "#46555C") +
  geom_treemap_text(fontface = "italic", colour = "white", place = "center", grow = TRUE) +
  # theme_tufte() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    plot.caption = element_text(size = 10)
    ) +
  labs(title ="Contribución subnacional al total nacional")


# Patchwork --------------------------------------------------------------------------------------------------------------------------------------
vis <- patchwork::wrap_plots(p1, p2, heights = c(0.8,0.5), ncol = 1) +
  plot_annotation(caption = "\nINEGI y CONEVAL (2020)\r\nElaborado por Azael Mateo (@xzxxlmxtxx)",
                  theme = theme(plot.caption = element_text(size = 10))
                  )
                    
  
# Export vis -------------------------------------------------------------------------------------------------------------------------------------
ggsave("AM202102_PobLab (week 8).png", width = 9, height = 12)

