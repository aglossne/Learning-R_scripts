library(tidyverse)
library(cowplot)
library(ggstatsplot)
##36 Pall
Thirtysix <- filter(Production_master3, Pall == "36", Startad_dt >= "2020-01-01 00:00", Produktionsplats == "FL 5")
Thirtysixb <-distinct(Thirtysix, Order, .keep_all = TRUE)


ggscatterstats(
  data = Thirtysixb,
  x = Producerad_mangd,
  y = Produktionstid_tim,
  marginal.type = "histogram",
  method = "lm",
  formula = y~x,
  centrality.para = "mean",
  ggtheme = ggthemes::theme_par(),
  messages = FALSE
)

lm_36pall <- lm(Thirtysixb$Produktionstid_tim ~ Thirtysixb$Producerad_mangd)
summary(lm_36pall)

##54 pall
Fiftyfour <- filter(Production_master3, Pall == "54", 
            Startad_dt >= "2020-01-01 00:00", Produktionsplats == "FL 5")
Fiftyfourb <- distinct(Fiftyfour, Order, .keep_all = TRUE)
Fiftyfour_Q8 <- filter(Fiftyfourb, Country == "SWE")

ggscatterstats(
  data = OneoEightb,
  x = Producerad_mangd,
  y = Produktionstid_tim,
  marginal.type = "histogram",
  method = "lm",
  formula = y~x,
  centrality.para = "mean",
  ggtheme = ggthemes::theme_par(),
  messages = FALSE
)

lm_54pall <- lm(Fiftyfourb$Produktionstid_tim ~ Fiftyfourb$Producerad_mangd)
summary(lm_54pall)

##108 pall
OneoEight <- filter(Production_master3, Pall == "108", Startad_dt >= "2020-01-01 00:00", 
                        Produktionsplats == "FL 5")
OneoEightb <- distinct(OneoEight, Order, .keep_all = TRUE)
OneoEight_Q8 <- filter(OneoEightb, Country == "SWE")

grouped_ggscatterstats(
  data = OneoEightb,
  x = Producerad_mangd,
  y = Produktionstid_tim,
  grouping.var = Produktionsplats,
  ggstatsplot.layer = FALSE,
  ggplot.component = list(theme(text = element_text(size = 8))),
  marginal = FALSE,
  method = "lm",
  formula = y~x,
  centrality.para = "mean",
  ggtheme = ggthemes::theme_par(),
  messages = FALSE
)

lm_108_Q8 <- lm(OneoEight_Q8$Produktionstid_tim ~ OneoEight_Q8$Producerad_mangd)
summary(lm_108_Q8)


##1l flaskor på FL5 
Oneliter <- filter(Production_master3, Pall == "12", Startad_dt >= "2020-01-01 00:00")
Oneliterb <-distinct(Oneliter, Order, .keep_all = TRUE)

lm_Oneliter <- lm(Oneliterb$Produktionstid_tim ~ Oneliterb$Kvantitet)
summary(lm_Oneliter)

ggscatterstats(
  data = Oneliterb,
  x = Kvantitet,
  y = Produktionstid_tim,
  marginal.type = "histogram",
  method = "lm",
  formula = y~x,
  centrality.para = "mean",
  ggtheme = ggthemes::theme_par(),
  messages = FALSE
)
