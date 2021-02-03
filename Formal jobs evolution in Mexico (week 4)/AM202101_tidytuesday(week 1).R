
rm(list = ls())
cls <- function() cat(rep("\n",100))
wd <- "E:/Azael Personal/Documentos/R/tidytuesday"
setwd(wd)
cls()


#/************************************************************************************************************************************************
# Filename: AM202101_tidytuesday(week 4)
# Author: Azael Mateo
# Date: 26/January 2021
#
# Purpose: This file generates the script to build a vis on the variation of formal jobs in each federal entity with respect to February 2020.
#
# Used files: -
#
# Created files: -
#**************************************************************************************************************************************************/

library(tidyverse)
library(lubridate)
library(geofacet)


# Import data
data <- read_csv('https://github.com/AzaelMateo/tidytuesday/raw/master/Formal%20jobs%20evolution%20in%20Mexico%20(week%204)/data.csv',
                  col_names = TRUE)

# Date variable
data$PERIODO <- data$PERIODO %>%
  fast_strptime("%Y/%b") %>%  
  as.Date()

# Build sub-data
dt_estados <- data %>%
  gather(ESTADO, SUBNACIONAL, -PERIODO)

# facet_geo
dt_estados <- 
  dt_estados %>% 
  mutate(name = str_to_title(ESTADO),
         name = if_else(name == "Cdmx", "Ciudad de México", name))

# Dataviz 
dt_estados %>% ggplot() +
  geom_area(aes(PERIODO, SUBNACIONAL/100, group = ESTADO), fill = "#008080") +
  geom_line(aes(PERIODO, NACIONAL/100), data = data, size = 0.3, col = "#A9A8AC", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "#E8EADC", linetype = 1) +
  scale_x_date(breaks = as.Date(c("2020-02-20", "2020-11-10")), labels = c("Feb", "Dic 20"), position = "bottom") +
  scale_y_continuous(n.breaks = 5, labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Evolución del empleo registrado ante el IMSS durante 2020",
       subtitle = "Variación porcentual respecto a Febrero",
       x = "", y = element_blank(),
       caption = "Fuente: Datos de IMSS Cubos de información\nElaborado por Azael Mateo (@xzxxlmxtxx)") +
  theme_tufte() +
  theme(
       plot.background = element_rect(fill = "#1E1D23"),
       panel.background = element_rect(fill = "#26262E", colour = "#26262E", size = 0.5, linetype = "solid"),
       strip.text = element_text(color = "#E8EADC", size = 10.2),
       axis.ticks = element_blank(),
       axis.text = element_text(color = "white", size = 7),
       panel.grid.major.y = element_line(colour = alpha("#566573", 0.5), linetype = 2),
       plot.title = element_text(size = 40, face = "bold", hjust = 1, vjust = 3.2, color = "#E8EADC"),
       plot.subtitle = element_text(size = 24, hjust = 1, vjust = 3.2, color = "#E8EADC"),
       plot.caption = element_text(size = 14, hjust = 0, vjust = 2, color = "#E8EADC"),
       plot.margin = margin(60, 30, 30, 30)) +
  facet_geo(~ name, grid = 'mx_state_grid3') 

# Export vis
ggsave("AM202011_mx_state_grid.png", height = 12, width = 15, units = "in", type = "cairo")
