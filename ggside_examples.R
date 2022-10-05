library(ggside)
library(tidyverse)
library(tidyquant)

p2<-mpg %>%
  ggplot(aes(hwy, cty, color = class)) +
  geom_point(size = 2, alpha = 0.3) +
  geom_smooth(aes(color = NULL), se=TRUE) +
  geom_xsidedensity(
    aes(
      y    = after_stat(density),
      fill = class
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  geom_ysidedensity(
    aes(
      x    = after_stat(density),
      fill = class
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  labs(title = "Fuel Economy by Vehicle Type" ,
       subtitle = "Density Plot",
       x = "Highway", y = "City") +  theme(
         ggside.panel.scale.x = 0.4,
         ggside.panel.scale.y = 0.4
       )
plot(p2)

#Xside placed bottom, yside placed left
p2 + ggside(x.pos = "bottom", y.pos = "left") +
  labs(title = "FacetNull", subtitle = "Xside placed bottom, Yside placed left")

#Collapsing X side panels
p2 + facet_wrap(drv~fl) +
  labs(title = "FacetWrap", subtitle = "Collapsing X side Panels") +
  ggside(collapse = "x")

#Collapse all side panels
p2 + facet_grid(drv~fl, space = "free", scales = "free") +
  labs(title = "FacetGrid", subtitle = "Collapsing All Side Panels") +
  
  ggside(collapse = "all")

#Side boxplot
mpg %>%
  ggplot(aes(x = cty, y = hwy, color = class)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_xsideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  facet_grid(cols = vars(cyl), scales = "free_x") +
  labs( title = "Fuel Economy by Engine Size")
