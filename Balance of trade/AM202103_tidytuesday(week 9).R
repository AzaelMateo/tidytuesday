
rm(list = ls())
cls <- function() cat(rep("\n",100))
wd <- "E:/Azael Personal/Documentos/R/tidytuesday"
setwd(wd)
cls()


#/************************************************************************************************************************************************
# Filename: AM202103_tidytuesday(week 9)
# Author: Azael Mateo
# Date: 3/March 2021
#
# Purpose: This file generates the script to build a vis of ...
#
# Created files: -
#**************************************************************************************************************************************************/


library(tidyverse)
library(packcircles)
library(ggtext)
library(ggforce)
library(scales)
library(colorspace)
library(ggthemes)



# Import data ------------------------------------------------------------------------------------------------------------------------------------
data <- read_csv("https://github.com/AzaelMateo/tidytuesday/raw/master/Balance%20of%20trade/BalanzaAgrop.csv",
                 col_names = TRUE, col_types = "cddcdd")


# Sub-set ----------------------------------------------------------------------------------------------------------------------------------------
data <- data %>% 
  select(concepto_exp, `2020_e`) %>% 
  mutate(id = row_number())

top_5 <- data %>% 
  top_n(5, `2020_e`) %>% 
  pull(concepto_exp)

data <- data %>% 
  mutate(fill = if_else(concepto_exp %in% top_5, "#A61C3C" , "#E6DFD8"),
         alpha = if_else(concepto_exp %in% top_5, 0.9 , 0.8))

big_colors <- c("#4D5656", "#A93226", "#BA4A00", "#145A32", "#117A65")

data[data$fill == "#A61C3C", 4] <- big_colors

packing <- circleProgressiveLayout(data$`2020_e`, sizetype = "area")

packing$radius <- 0.95*packing$radius

label_data <- bind_cols(data, packing)

circles <- circleLayoutVertices(packing, npoints = 50) %>% 
  left_join(select(label_data, -x, -y), layout, by = "id")

labels <- filter(label_data, concepto_exp %in% top_5)


# Legend circles ---------------------------------------------------------------------------------------------------------------------------------
legend_circles <- label_data %>% 
  mutate(groups = cut(radius, 4)) %>% 
  split(.$groups) %>% 
  map_dfr(~top_n(.x, -1, radius)) %>%
  select(radius, `2020_e`) %>% 
  mutate(x = 0,
         y = max(radius) + radius,
         y_label = max(radius) + 2*radius)

legend <- ggplot(legend_circles, aes(x0 = x, y0 = y, r = radius)) +
  geom_circle(color = "#E6DFD8", size = 0.75, fill = "#E4E9EE", alpha = 0.5) + 
  geom_text(aes(x = 0, y = y_label, label = dollar(round(`2020_e`), suffix = " Mdd",accuracy = NULL)), 
            family = "Helvetica Bold", color = lighten("black", amount = 0.5), vjust = -0.2) +
  labs(x = NULL,
       y = NULL) +
  coord_equal(clip = "off") +
  theme_tufte() +
  theme(legend.position = "none",
        plot.background = element_rect(color = NA, fill = "transparent"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())

legend_grob <- ggplotGrob(legend)


# Plot ---------------------------------------------------------------------------------------------------------------------------------------------
ggplot() + 
  geom_polygon(data = circles, aes(x = x, y = y, group = id, fill = fill, color = darken(fill, 0.4), alpha = alpha),  size = 0.2) +
  geom_text(data = labels, aes(x, y,  label = str_wrap(concepto_exp, 12), size = 10), color = lighten("#808183", amount = 0.9), family = "Helvetica Bold") +
  annotation_custom(legend_grob, xmin = 65, xmax = 130, ymin = 15, ymax = 80) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_alpha_identity() +
  coord_equal(clip = "off") +
  labs(x = NULL,
       y = NULL,
       title = "Los 5 productos agropecuarios más exportados de México en el 2020",
       subtitle = str_wrap("Los círculos debajo representan los 26 productos agropecuarios (incluye productos pesqueros) que 
       integran la estadística de la balanza comercial de mercancías en el país.", 95), 
       caption = "\nSistema de información económica (2021), Banxico\r\nElaborado por Azael Mateo (@xzxxlmxtxx)") +
  theme_tufte() +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold", family = "Helvetica"),    # subtitle_size = 16,
    plot.subtitle = element_text(size = 18, hjust = 0.5, family = "Helvetica"),
    plot.caption = element_text(size = 12, family = "Helvetica"),
    legend.position = "none",    # plot.background = element_rect(fill = "white"),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank())


# Export --------------------------------------------------------------------------------------------------------------------------------------------
ggsave("AM202103_ExpAgro (week 9).png", width = 11.6, height = 13)


