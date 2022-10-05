#Making mulitple ggplot plots at the same time on the same panel

#libraries
library(tidyverse)
library(plotly)
library(trelliscopejs)

#Data
mpg

#Simple ggplot
mpg %>%
  ggplot(aes(displ, hwy)) +
  geom_point(size = 4) +
  geom_smooth(se = FALSE, span =1)

#Trelliscope plots
mpg %>%
  ggplot(aes(displ, hwy)) +
  geom_point(size = 4) +
  geom_smooth(se = FALSE, span =1)+
  facet_trelliscope(~ manufacturer,
                    ncol = 4,
                    nrow = 3)

#Interactive with plotly
mpg %>%
  ggplot(aes(displ, hwy)) +
  geom_point(size = 4) +
  geom_smooth(se = FALSE, span =1)+
  facet_trelliscope(~ manufacturer,
                    ncol = 4,
                    nrow = 3,
                    as_plotly = TRUE)
