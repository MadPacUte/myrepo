#This is only a test to see if I can get a proper push.

install.packages("tidyverse")
library(tidyverse)
library(gapminder)

head(gapminder)

ggplot(gapminder, aes(x = country, y = pop, color = gdpPercap))+
  geom_point()
