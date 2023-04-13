library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Set default folder
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

#Load the data
data <- read.csv(file.path(folder, "image_processing_data.csv"))


#####################################
##plot1 = Period of day performance##
#####################################
df = data %>%
  group_by(band, flight) %>%
  summarize(mean = mean(accuracy),
            sd = sd(accuracy))

time_of_day <- ggplot(df, aes(x = flight, y = mean, fill = band)) +
  geom_bar(stat = "identity",
           position = position_dodge(),
           width = 0.98) +
  geom_errorbar(
    aes(ymin = mean - sd/10,
        ymax = mean + sd/10),
    width = .2,
    position = position_dodge(.98),
    color = 'black',
    size = 0.2
  ) + scale_fill_brewer(palette = "Dark2") + theme_minimal() + 
  theme(legend.position = "right") + labs(
    colour = NULL,
    title = "(A) Period-of-Day Model Performance: IP vs no IP",
    subtitle = NULL,
    x = NULL,
    y = "mAP",
    fill = "Band"
  ) + scale_y_continuous(limits = c(0, 1),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0)
  ) +   theme(plot.title = element_text(face = "bold")) + 
  theme(legend.position = "none", axis.title = element_text(size = 6)) + 
  theme(axis.line = element_line(colour = "black"),
        strip.text.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        plot.subtitle = element_text(size = 6),
        axis.line.x  = element_line(size = 0.15),
        axis.line.y  = element_line(size = 0.15),
        plot.title = element_text(size = 8)
  )

#####################################
##plot2 = elevation##
#####################################
df = data %>%
  group_by(band, flight) %>%
  summarize(mean = mean(accuracy), means =elevation, 
            sd = sd(accuracy), sds = sd(elevation))

elevation <- ggplot(df, aes(x = means, y = mean, fill = band)) +
  geom_bar(stat = "identity",
           position = position_dodge(),
           width = 40) + scale_fill_brewer(palette = "Dark2") + theme_minimal() + 
  theme(legend.position = "right") + labs(
    colour = NULL,
    title = "(B) Elevation Model Performance: IP vs no IP",
    subtitle = NULL,
    x = NULL,
    y = "mAP",
    fill = "Band"
  ) + scale_y_continuous(#limits = c(0, 8500),
                         labels = function(y)
                           format(y, scientific = FALSE),
                         expand = c(0, 0)
  ) +   theme(plot.title = element_text(face = "bold")) + 
  theme(legend.position = "none", axis.title = element_text(size = 6)) + 
  theme(axis.line = element_line(colour = "black"),
        strip.text.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        plot.subtitle = element_text(size = 6),
        axis.line.x  = element_line(size = 0.15),
        axis.line.y  = element_line(size = 0.15),
        plot.title = element_text(size = 8)
  )








