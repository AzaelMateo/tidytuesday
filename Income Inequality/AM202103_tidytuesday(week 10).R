
rm(list = ls())
cls <- function() cat(rep("\n",100))
wd <- "E:/Azael Personal/Documentos/R/tidytuesday"
setwd(wd)
cls()


#/************************************************************************************************************************************************
# Filename: AM202103_tidytuesday(week 10)
# Author: Azael Mateo
# Date: 9/March 2021
#
# Purpose: This file generates the script to build a vis of ...
#
# Created files: -
#**************************************************************************************************************************************************/


library(tidyverse)
library(ggtext)
library(janitor)
library(glue)
library(ggthemes)


# Import data ------------------------------------------------------------------------------------------------------------------------------------
data <- read_csv("https://github.com/AzaelMateo/tidytuesday/raw/master/Income%20Inequality/inglabsexo.csv",
                 col_names = TRUE, col_types = "ddd")


# Sub-set ----------------------------------------------------------------------------------------------------------------------------------------
line_data <- data %>% 
  pivot_longer(-per) %>% 
  filter(name %in% c('masc', 'fem'))

ribbon_data <- data %>% 
  select(per, masc, fem)

pal <- c("#e01a4f","#53b3cb")


# Dots & labels ---------------------------------------------------------------------------------------------------------------------------------
dots <- line_data %>% 
  filter(per %in% range(per))

labels <- dots %>% 
  group_by(per) %>% 
  summarize(diff = max(value) - min(value),
            value = mean(value))


# Building glt function --------------------------------------------------------------------------------------------------------------------------
highlight_text <- function(text, colour = "#000000", style = "", size = 13) {
  
  out <- switch(style,
                "i" = glue::glue("*{text}*"),
                "b" = glue::glue("**{text}**"),
                "ib" = glue::glue("***{text}***"),
                "bi" = glue::glue("***{text}***"),
                text)
  
  as.character(glue::glue("<span style = 'color:{colour}; font-size:{size}px;'>{out}</span>"))
  
}


# Plot --------------------------------------------------------------------------------------------------------------------------------------------
ggplot() +
  geom_ribbon(data = ribbon_data, aes(x = per, ymin = fem, ymax = masc), fill = "grey70", alpha = 0.5) +
  geom_line(data = line_data, aes(x = per, y = value, color = name), size = 2) +
  geom_point(data = dots, aes(x = per, y = value, color = name)) +
  geom_text(data = labels, aes(x = per, y = value, label = scales::dollar(diff)), family = "Goldman Sans Condensed Regular", nudge_x = c(-0.7, 0.7), size = 7) +
  annotate(GeomRichtext, x = 2009.5, y = 5100, label = glue("**Cerca, Pero No Lo Suficiente**: Brecha salarial entre<br>{highlight_text('Mujeres', '#e01a4f', 'b', size = 40)} y {highlight_text('Hombres', '#53b3cb', 'b', size = 40)} en México de 2005 a 2020"), 
           hjust = 0, vjust = 1, size = 10, family = "Goldman Sans Condensed", label.color = NA) +
  labs(x = NULL, 
       y = NULL,
       caption = "Fuente: Coneval 2021. Precios Reales. | Elaborado por Azael Mateo (@xzxxlmxtxx)") +
  scale_x_continuous(limits = c(2004, 2021), breaks = seq(2005, 2020, 1)) +
  scale_y_continuous(limits = c(3300, 5250), breaks = seq(3500, 5000, 300), labels = c("$3,500", "$3,800", "$4,100", "$4,400", "$4,700", "$5,000")) +
  scale_color_manual(values = pal) +
  theme_tufte()+
  theme(
    plot.caption = element_text(size = 12, family = "Goldman Sans Condensed Regular", vjust = -1.3),
    legend.position = "none",
    axis.text = element_text(size = 12, family = "Goldman Sans Condensed Regular"),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(linetype = 1, color = alpha("#E5E7E9", 0.6), size = 0.5)
  )


# Export --------------------------------------------------------------------------------------------------------------------------------------------
ggsave("AM202103_BrechSal (week 10).png", width = 18, height = 9)
