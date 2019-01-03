#This is only a test to see if I can get a proper push.

install.packages("mapproj")
install.packages("gganimate")
install.packages("tidyverse")
library(tidyverse)
library(gapminder)

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Scatter plot comparing pop and lifeExp, with color representing continent
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()


##
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent,
                           size = gdpPercap))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()

##
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent,
                           size = gdpPercap))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+ 
  facet_wrap(~continent)

##
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent,
                      size = pop))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~year)

##
#library(gapminder)
#library(dplyr)

# Filter for 1957 then summarize the median life expectancy and the maximum GDP per capita
#filter will allow you to filter a subset of the data and
#group_by will group the results by that cateogory or multiple categories and
#summarize gives a summary of useful things like min, max, median, etc.
gapminder %>%
  filter(year == 1957)%>%
  group_by(continent)%>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

##
#library(gapminder)
#library(dplyr)
#library(ggplot2)

# Summarize medianGdpPercap within each continent within each year: by_year_continent
by_year_continent <- gapminder%>%
  group_by(continent, year)%>%
  summarize(medianGdpPercap = median(gdpPercap))

# Plot the change in medianGdpPercap in each continent over time
ggplot(by_year_continent, aes(x= year, y= medianGdpPercap, color = continent)) +
  geom_point()+
  expand_limits(y=0)

##
#library(gapminder)
#library(dplyr)
#library(ggplot2)

# Summarize the median GDP and median life expectancy per continent in 2007
by_continent_2007 <- gapminder%>%
  filter(year == 2007)%>%
  group_by(continent)%>%
  summarize(medianLifeExp = median(lifeExp), medianGdpPercap = median(gdpPercap))

# Use a scatter plot to compare the median GDP and median life expectancy
ggplot(by_continent_2007, aes(x = medianGdpPercap, y = medianLifeExp, color = continent)) +
  geom_point()

## Line chart
#library(gapminder)
#library(dplyr)
#library(ggplot2)

# Summarize the median gdpPercap by year, then save it as by_year
by_year <- gapminder %>%
  group_by(year)%>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap over time
ggplot(by_year, aes(x=year, y=medianGdpPercap))+
  geom_line()+
  expand_limits(y=0)

##
#library(gapminder)
#library(dplyr)
#library(ggplot2)

# Summarize the median gdpPercap by year & continent, save as by_year_continent
by_year_continent <- gapminder%>%
  group_by(year, continent)%>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap by continent over time
ggplot(by_year_continent, aes(x=year, y=medianGdpPercap, color=continent))+
  geom_line()+
  expand_limits(y=0)

## Bar chart
#library(gapminder)
#library(dplyr)
#library(ggplot2)

# Summarize the median gdpPercap by year and continent in 1952
by_continent <- gapminder %>%
  filter(year == 1952)%>%
  group_by(continent)%>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a bar plot showing medianGdp by continent
ggplot(by_continent, aes(x = continent, y = medianGdpPercap))+
  geom_col()

##
#library(gapminder)
#library(dplyr)
#library(ggplot2)

# Filter for observations in the Oceania continent in 1952
oceania_1952 <- gapminder%>%
  filter(continent == 'Oceania', year == 1952)

# Create a bar plot of gdpPercap by country
ggplot(oceania_1952, aes(x = country, y = gdpPercap))+
  geom_col()

## Histogram
#library(gapminder)
#library(dplyr)
#library(ggplot2)

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a histogram of population (pop), with x on a log scale
ggplot(gapminder_1952, aes(x = pop))+
  geom_histogram()+
  scale_x_log10()

## Boxplot
#library(gapminder)
#library(dplyr)
#library(ggplot2)

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a boxplot comparing gdpPercap among continents
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap))+
  geom_boxplot()+
  scale_y_log10()

## Add title using labs(x, y, title, subtitle, caption = "")
#library(gapminder)
#library(dplyr)
#library(ggplot2)

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Add a title to this graph: "Comparing GDP per capita across continents"
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  scale_y_log10() + 
  labs(title = "Comparing GDP per capita across continents")

# view tibble data
glimpse(gapminder)

library(skimr)
skim(gapminder)

# Filter and skim
bakeoff %>%
  filter(!is.na(us_season)) %>% 
  skim()

# Edit to filter, group by, and skim
bakeoff %>% 
  filter(!is.na(us_season)) %>% 
  group_by(us_season)  %>% 
  skim()

gapminder %>%
  #filter(!is.na(year))%>%
  #group_by(year)%>%
  skim()


install.packages("janitor")
library(janitor)


gapminder %>%
  clean_names()

ratings

tidy_ratings <- ratings %>%
  # Gather and convert episode to factor
  gather(key = "episode", value = "viewers_7day", -series, 
         factor_key = TRUE, na.rm = TRUE) %>%
  # Sort in ascending order by series and episode
  arrange(series, episode) %>% 
  # Create new variable using row_number()
  mutate(episode_count = row_number())

# Plot viewers by episode and series
ggplot(tidy_ratings, aes(x = episode_count, 
                         y = viewers_7day, 
                         fill = series)) +
  geom_col()

