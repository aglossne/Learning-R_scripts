library(tidyverse)
install.packages("ISLR")
library(ISLR)
data("Wage")

ggplot(Wage, mapping=aes(age, wage)) + geom_point() + geom_smooth(method = "lm")+ theme_bw()+ 
  labs(title = "Lön för 3000 arbetare i midatlantic USA, 2003-2009", caption = "Source:http://thedataweb.rm.census.gov/TheDataWeb")+
  facet_wrap("race")

ggplot(Wage, mapping=aes(year, wage)) + geom_point() + geom_smooth(method = "lm")+ theme_bw()+ 
  labs(title = "Lön för 3000 arbetare i midatlantic USA, 2003-2009", caption = "Source:http://thedataweb.rm.census.gov/TheDataWeb")

ggplot(Wage, mapping = aes(education, wage)) + geom_boxplot()+ theme_bw()+ 
  labs(title = "Lön för 3000 arbetare i midatlantic USA, 2003-2009", caption = "Source:http://thedataweb.rm.census.gov/TheDataWeb")
