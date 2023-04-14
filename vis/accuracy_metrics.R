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
  group_by(image_processing, flight) %>%
  summarize(mean = mean(accuracy),
            sd = sd(accuracy))
df$flight = factor(df$flight, levels = 
                       c("pre-sunrise", "post-sunrise", "noon", 
                         "pre-sunset", "post-sunset"),
                     labels = c("Pre-Sunrise", "Post-Sunrise", 
                         "Noon", "Pre-Sunset", "Post-Sunset"))

time_of_day <- ggplot(df, aes(x = flight, y = mean, fill = image_processing)) +
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
    title = "(B) Model Performance by Period of Day",
    subtitle = NULL,
    x = NULL,
    y = "mAP",
    fill = "Image Processing?"
  ) + scale_y_continuous(limits = c(0, 1),
    labels = function(y)
      format(y, scientific = FALSE),
    expand = c(0, 0)
  ) +   theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title = element_text(size = 6)) + 
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

elevation <- ggplot(data, aes(x = elevation, y = accuracy, fill = band)) +
  geom_bar(stat = "identity",
           position = position_dodge(),
           width = 40) + scale_fill_brewer(palette = "Dark2") + theme_minimal() + 
  theme(legend.position = "right") + labs(
    colour = NULL,
    title = "(C) Model Performance by Elevation",
    subtitle = NULL,
    x = NULL,
    y = "mAP",
    fill = "Band"
  ) + scale_y_continuous(limits = c(0, 1),
                         labels = function(y)
                           format(y, scientific = FALSE),
                         expand = c(0, 0)
  ) +   theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title = element_text(size = 6)) + 
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
##plot3 = period of day and band##
#####################################
df = data %>%
  group_by(band, flight) %>%
  summarize(mean = mean(accuracy),
            sd = sd(accuracy))
df$flight = factor(df$flight, levels = 
                     c("pre-sunrise", "post-sunrise", "noon", 
                       "pre-sunset", "post-sunset"),
                   labels = c("Pre-Sunrise", "Post-Sunrise", 
                              "Noon", "Pre-Sunset", "Post-Sunset"))

band_period <- ggplot(df, aes(x = flight, y = mean, fill = band)) +
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
    title = "(A) Model Performance by Band",
    subtitle = NULL,
    x = NULL,
    y = "mAP",
    fill = "Band?"
  ) + scale_y_continuous(limits = c(0, 1),
                         labels = function(y)
                           format(y, scientific = FALSE),
                         expand = c(0, 0)
  ) +   theme(plot.title = element_text(face = "bold")) + 
  theme(axis.title = element_text(size = 6)) + 
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



