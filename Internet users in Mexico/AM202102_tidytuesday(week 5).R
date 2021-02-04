
rm(list = ls())
cls <- function() cat(rep("\n",100))
wd <- "E:/Azael Personal/Documentos/R/tidytuesday"
setwd(wd)
cls()


#/************************************************************************************************************************************************
# Filename: AM202101_tidytuesday(week 1)
# Author: Azael Mateo
# Date: 26/January 2021
#
# Purpose: This file generates the script to build a vis of the percentage of users who use the internet by age and state in Mexico.
# Data source: Encuesta Nacional sobre Disponibilidad y Uso de Tecnologías de la Información en los Hogares (ENDUTIH) 2018, INEGI.
#
# Created files: -
#**************************************************************************************************************************************************/


library(tidyverse)
library(haven)
library(patchwork)
library(ggthemes)
library(ggrepel)


# Import data
if (file.exists(paste(wd,"tr_endutih_usuario_anual_2018.csv", sep = "/"))){
  usuario_anual <- read_csv(paste(wd, "tr_endutih_usuario_anual_2018.csv", sep = "/"), col_names = TRUE)
  } else {
  dir.create(paste(wd, "download", sep="/"))
  download.file("https://www.inegi.org.mx/contenidos/programas/dutih/2018/datosabiertos/conjunto_de_datos_endutih_2018_csv.zip", paste(wd, "download/conjunto.zip", sep = "/"))
  unzip(zipfile = paste(wd, "download/conjunto.zip", sep = "/"), exdir = "./download")
  file.copy(from = paste(wd, "download/conjunto_de_datos/tr_endutih_usuario_anual_2018.csv", sep = "/"),
            to   = paste(wd, "tr_endutih_usuario_anual_2018.csv", sep = "/"))
  unlink(paste(wd, "download", sep = "/"), recursive = TRUE)
  usuario_anual <- read_csv(paste(wd, "tr_endutih_usuario_anual_2018.csv", sep = "/"), col_names = TRUE)
  file.remove(paste(wd, "tr_endutih_usuario_anual_2018.csv", sep = "/"))
  }

# Build sub-data
sub1 <- usuario_anual %>% 
  transmute(state = as.factor(ENT), yearsold = as.numeric(EDAD), internet = P7_1, reason = P7_2, weight = FAC_PER) %>% 
  arrange(state, yearsold) %>% 
  group_by(state, yearsold, internet) %>% 
  mutate(weightparc = sum(weight)) %>% 
  count(weightparc) %>% 
  ungroup() %>% 
  group_by(state, yearsold) %>% 
  mutate(weighttot = sum(weightparc), percent = weightparc/weighttot) %>% 
  filter(internet == 1, yearsold <= 80)

state_labels <- read_csv("https://github.com/AzaelMateo/tidytuesday/raw/master/Internet%20users%20in%20Mexico/AGEEML.csv",
                         col_names = TRUE) %>% 
  transmute(state = as.factor(`Clave De AGEE`), names = `Nombre Abreviado De AGEE`)

sub1 <- left_join(sub1, state_labels, by = c("state" = "state")) 

highs <- sub1 %>% 
  group_by(yearsold) %>% 
  summarize(mean_percent = mean(percent)) %>% 
  filter(yearsold %in% c(17, 60))

# Prime
prime <- sub1 %>% 
  group_by(yearsold) %>% 
  summarize(mean_percent = mean(percent)) %>% 
  ggplot(aes(yearsold, mean_percent)) +
  geom_line(color = "#3b88b5", size = 1.2) +
  scale_x_continuous(expand = c(0,0)) +
  geom_point(data = highs, color = "#3b88b5", size = 3) +
  geom_text_repel(data = highs, aes(label = paste(round(mean_percent*100, 1), "%", sep = "")), 
                  color = "#3b88b5", size = 4, nudge_y = -0.1, nudge_x = -0.1, segment.color = 'transparent') +
  labs(x = NULL, 
       y = NULL,
       title = "Uso de internet por edad y entidad federativa",
       subtitle = str_wrap("El gráfico superior muestra el porcentaje promedio de usuarios que utilizan internet por edad en México: a los 17 años el 92% de los usuarios acceden a internet, 
       a partir de entonces el porcentaje comienza a reducirse. A los 60 años, solo es el 35%. El gráfico inferior ilustra el porcentaje por edad y entidad federativa.", 120)) +
  theme_tufte() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 25, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        panel.grid = element_line(linetype = 1, color = alpha("#E5E7E9", 0.6), size = 0.5))

# Heatmap
heatmap <- ggplot(sub1, aes(x = yearsold, y = state, fill = percent)) +
  geom_tile(color = "white", size = 0.05) +
  geom_vline(xintercept = 60, color = "black", linetype = 1, size = 0.7) +
  scale_x_continuous(breaks = seq(6, 90, 1), position = "top", expand = c(0,0)) +
  scale_y_discrete(limits = rev(levels(sub1$state)), labels = rev(state_labels$names))  +
  scale_fill_gradient(low = "#f9f8c9", high = "#2f77af", 
                      breaks = c(0.01, 0.25, 0.50, 0.75, 1), labels = c("0%", "25%", "50%", "75%", "100%"),
                      name = "Porcentaje de usuarios que acceden a internet") +
  scale_colour_gradient2() +
  labs(x = NULL, y = NULL, caption = "Fuente: ENDUTIH 2018, INEGI\nElaborado por Azael Mateo (@xzxxlmxtxx)") +
  theme_tufte() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.ticks.y = element_blank(),
        plot.caption = element_text())

# Patchwork 
vis <- patchwork::wrap_plots(prime, heatmap, heights = c(0.3,1), ncol = 1)

# Export vis
ggsave("AM202102_internet_users.png", vis, width = 10, height = 12)

