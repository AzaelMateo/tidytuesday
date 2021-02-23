
rm(list = ls())
cls <- function() cat(rep("\n",100))
wd <- "E:/Azael Personal/Documentos/R/tidytuesday"
setwd(wd)
cls()


#/************************************************************************************************************************************************
# Filename: AM202101_tidytuesday(week 7)
# Author: Azael Mateo
# Date: 22/February 2021
#
# Purpose: This file generates the script to build a vis of ...
#
# Created files: -
#**************************************************************************************************************************************************/


library(tidyverse)
library(haven)
library(ggthemes)


# Import data ------------------------------------------------------------------------------------------------------------------------------------
data <- read_csv("https://github.com/AzaelMateo/tidytuesday/raw/master/Income%20poverty%20in%20Mexico/CONEVAL_ITLP.csv",
                 col_names = TRUE, col_types = "cddd")


# Sub-set ----------------------------------------------------------------------------------------------------------------------------------------
data$ent <- factor(data$ent, levels = data$ent[order(data$`3t`)]) 

data <- data %>% 
  mutate(dif = ((`3t`-`4t`)*-100/(`1t`-`3t`)))


# Geom segment -----------------------------------------------------------------------------------------------------------------------------------
p1 <- data %>% 
  ggplot(aes(x = ent, y = `3t`)) +
  geom_segment(aes(x = ent, xend = ent, y = `4t`, yend = `3t`), size = 3, color = "#E4E8F0", alpha = 0.8) +
  geom_point(aes(y = `1t`, x = ent), size = 3, shape = 17, color = "#CFC5C0", alpha = 0.8) +
  geom_point(aes(y = `3t`, x = ent), size = 3, color = "#A39FA8", alpha = 1) +
  geom_point(aes(y = `4t`, x = ent), size = 3, color = "#7580A1", alpha = 1) +
  geom_text(data = subset(data, dif > 0), aes(label = paste(round(dif*-1, 1), "%", sep = "")), color = "#2F3336", size = 2.5, hjust = 1.8, vjust = 0.4) +
  geom_text(data = subset(data, dif < 0), aes(label = paste(round(dif*-1, 1), "%", sep = "")), color = "#2F3336", size = 2.5, hjust = -0.5, vjust = 0.4) +
  scale_y_continuous("", expand = c(0,0), limits = c(.1, .83), breaks = seq(.1,.8,.1), labels = scales::percent_format()) +
  coord_flip() +
  theme_tufte() +
  theme(panel.grid.major.y = element_line(linetype = 1, color = alpha("#E5E7E9", 0.5), size = 0.3),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(color = "black", size = 11.4),
        axis.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 10.5),
        plot.caption = element_text(size = 9)) +
  labs(title = "Efectos de la pandemia sobre la pobreza laboral",
       subtitle = str_wrap("Cada viñeta indica el porcentaje de la población de cada entidad federativa con un ingreso inferior al costo de la canasta alimentaria
                          (pobreza laboral)  durante el 2020. La etiqueta de valor representa la profundización o reversión de la magnitud de los efectos de la pandemia (1T-3T)", 150),
       caption = "\nENOE y ENOE-N (2020), INEGI\r\nElaborado por Azael Mateo (@xzxxlmxtxx)",
       y = "Porcentaje de la población con ingreso inferior al costo de la canasta alimentaria")


# Annotations -----------------------------------------------------------------------------------------------------------------------------------
p2 <- p1 +  annotate("curve", x = "Hgo.", y = 0.79, xend = "Chis.", yend = 0.75,
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"), color = "grey40") +
      annotate("text", x = "Ver.", y = 0.74,
               label = str_wrap("Chiapas fue la entidad con mayor reversión de los efectos de la pandemia sobre la pobreza laboral con (-)170%. Al 4T se ubicó
               incluso en un menor nivel que en el 1T.", 30, indent = 0), size = 2, color = "grey40") +
      annotate("text", x = "Mex.", y = 0.58,
              label = str_wrap("El Edomex y la CDMX revirtieron los efectos en un (-)34% y (-)22% respectivamente.", 40), size = 2, color = "grey40", vjust = 0.8) +
      annotate("curve", x = "Yuc.", y = 0.54, xend = "NACIONAL", yend = 0.50, curvature = .4,
              arrow = arrow(length = unit(0.2, "cm"), type = "closed"), color = "grey40") +
      annotate("text", x = "Yuc.", y = 0.61,
               label = str_wrap("A nivel nacional, los efectos de la pandemia sobre la pobreza laboral que se presentaron del 1T al 3T se revirtieron en un (-)42.7%
                                al pasar de tener al 45% de la población bajo esta condición en el 3T a tener el 41% para el 4T.", 40), size = 2, color = "grey40") +
      annotate("curve", x = "Coah.", y = 0.44, xend = "Dgo.", yend = 0.40, curvature = .4,
              arrow = arrow(length = unit(0.2, "cm"), type = "closed"), color = "grey40") +
      annotate("text", x = "Coah.", y = 0.50,
              label = str_wrap("Durango profundizó en un 139% los efectos de la pandemia.", 30), size = 2, color = "grey40") 

  
# Bullets --------------------------------------------------------------------------------------------------------------------------------------
p3 <- p2 +  geom_point(aes(y = 0.72, x = "Dgo."), size = 4, shape = 17, color = "#CFC5C0", alpha = 0.8) +
            annotate("text", x = "Dgo.", y = 0.735, label = "1T", size = 4, color = "black") +
            geom_point(aes(y = 0.72, x = "Col."), size = 4, color = "#A39FA8", alpha = 1) +
            annotate("text", x = "Col.", y = 0.735, label = "2T", size = 4, color = "black") +
            geom_point(aes(y = 0.72, x = "BCS"), size = 4, color = "#7580A1", alpha = 1) +
            annotate("text", x = "BCS", y = 0.735, label = "3T", size = 4, color = "black") 
                     
  
# Export vis -------------------------------------------------------------------------------------------------------------------------------------
ggsave("AM202102_PobLab (week 7).png", width = 9.4, height = 8)

