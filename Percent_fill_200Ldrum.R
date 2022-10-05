#Calculate degree of filling % over range of temperatures for Aspen Sport 200L
#drum capacity = 216.5L
#alpha (coefficient of thermal exp) = 0.00095


library(tidyverse)
dt <- tibble(
  "tf" = 0:35,
  "alpha" = 0.00095,
  "tdiff" = 50-tf,  
  "percent_fill" = 98/(1+(alpha*tdiff)),
  "fill_vol" = percent_fill*216.5
)

ggplot(data = dt, aes(tf, percent_fill)) +
  geom_line() + labs(x = "Temp at fill (C)",
                     y = "% Fill",
                     title = "Percent fill over range of fill temps",
                     caption = "alpha = 0.00095")+
  theme_bw()
