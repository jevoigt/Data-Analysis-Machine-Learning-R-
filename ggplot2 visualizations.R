#Week 6 Videos
require(dplyr)
require(ggplot2)
require(tidyr)
mtcars = read.csv("mtcars.csv")
head(mtcars)
summary(mtcars)

ggplot(mtcars, aes(x = cyl, y = mpg)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm")

#Video 2
ggplot(mtcars, aes(x = brand)) +
  geom_bar(fill = "red")

ggplot(mtcars, aes(x = brand)) +
  geom_bar(aes(fill = as.factor(am)))

mpg_table = mtcars %>%
  group_by(cyl) %>%
  summarize(avg_mpg = mean (mpg))
mpg_table

mpg_table$cyl = as.factor(mpg_table$cyl)

ggplot(mpg_table, aes(x = as.factor(cyl), y = avg_mpg, fill = cyl)) +
  geom_bar(stat = "identity") +
  coord_flip()

brands_table = mtcars %>%
  group_by(brand) %>%
  summarize(num = n())
brands_table

ggplot(brands_table, aes(x = "", y = "num_brands", fill = brand)) +
  geom_bar(stat = "identity", color = "white")

ggplot(mtcars, aes(x=mpg)) +
  geom_histogram(bins = 7)

ggplot(mtcars, aes(y=mpg))+
  geom_boxplot()

ggplot(mtcars, aes(x = as.factor(cyl), y = hp))+
  geom_boxplot()

ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point(size = 3, aes(color = as.factor(am)))


#Video 3

ggplot(mtcars, aes(x= mpg, y = hp)) +
  geom_point(size = 3, aes(color=as.factor(am))) +
  labs(title = "Fuel Efficiency vs. Horsepower", subtitle = "Data Visualization",
       x = "Miles per Gallon", y = "Horsepower", caption = "Note: ISM 645 ? IAF 601 Week 6", 
       color = "Automatic (0) vs. Manual (1)") +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) +
  geom_text(aes(label = model))

require(ggrepel)

ggplot(mtcars, aes(x= mpg, y = hp)) +
  geom_point(size = 3, aes(color=as.factor(am))) +
  labs(title = "Fuel Efficiency vs. Horsepower", subtitle = "Data Visualization",
       x = "Miles per Gallon", y = "Horsepower", caption = "Note: ISM 645 ? IAF 601 Week 6", 
       color = "Automatic (0) vs. Manual (1)") +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) +
  geom_text_repel(aes(label = model), force = 15, data = subset(mtcars, am == 0))

#Add Text and a geom_smooth
ggplot(mtcars, aes(x= mpg, y = hp)) +
  geom_point(size = 3, aes(color=as.factor(am))) +
  labs(title = "Fuel Efficiency vs. Horsepower", subtitle = "Data Visualization",
       x = "Miles per Gallon", y = "Horsepower", caption = "Note: ISM 645 ? IAF 601 Week 6", 
       color = "Automatic (0) vs. Manual (1)") +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) +
  geom_text(label = "IAF 601", x = 30, y = 300, size = 5) + 
  geom_smooth(se = FALSE, method = "lm", color = "green") 


#Seperate into multiple plots
ggplot(mtcars, aes(x= mpg, y = hp)) +
  geom_point(size = 3, aes(color=as.factor(am))) +
  labs(title = "Fuel Efficiency vs. Horsepower", subtitle = "Data Visualization",
       x = "Miles per Gallon", y = "Horsepower", caption = "Note: ISM 645 ? IAF 601 Week 6", 
       color = "Automatic (0) vs. Manual (1)") +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) +
  geom_text(label = "IAF 601", x = 20, y = 300, size = 5) + 
  geom_smooth(se = FALSE, method = "lm", color = "green") +
  facet_grid(cyl ~ am)

install.packages('ggpubr')
require(ggpubr)

auto_cars = mtcars%>%
  filter(am == 0)

manual_cars = mtcars%>%
  filter(am== 1)

plot1 = ggplot(auto_cars, aes(x= mpg, y = hp)) +
  geom_point(size = 3, aes(color=as.factor(am))) +
  labs(title = "Fuel Efficiency vs. Horsepower", subtitle = "Data Visualization",
       x = "Miles per Gallon", y = "Horsepower", caption = "Note: ISM 645 ? IAF 601 Week 6", 
       color = "Automatic (0)") +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) +
  geom_text(label = "IAF 601", x = 20, y = 300, size = 5) + 
  geom_smooth(se = FALSE, method = "lm", color = "green")
plot1

plot2 = ggplot(manual_cars, aes(x= mpg, y = hp)) +
  geom_point(size = 3, aes(color=as.factor(am))) +
  labs(title = "Fuel Efficiency vs. Horsepower", subtitle = "Data Visualization",
       x = "Miles per Gallon", y = "Horsepower", caption = "Note: ISM 645 / IAF 601 Week 6", 
       color = "Manual") +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(label = "IAF 601", x = 25, y = 300, size = 5) + 
  geom_smooth(se = FALSE, method = "lm", color = "green")
plot2


ggarrange(plot1, plot2)
arranged_plot = ggarrange(plot1, plot2, plot1, plot2, ncol=2, nrow = 2)

annotate_figure(arranged_plot, top = "top", bottom = "bottom", left = "left", right = "right")



#Hands on lab
countries = read.csv("Countries.csv")
head(countries)
str(countries)
summary(countries)

#Bar Chart
countries2010 = countries%>%
  filter(year == 2010)

#Approach 1

countries_table = countries2010%>%
  group_by(continent)%>%
  summarize(num_obs = n())

head(countries_table)

ggplot(countries_table, aes(x = continent, y = num_obs, fill = continent)) +
  geom_bar(stat = "identity")+
  labs(x = "Continent", y = "Number of countries")

#Approach 2 (more limited)

ggplot(countries2010, aes(x = continent)) +
  geom_bar(stat = "count")

#Line Plot
US_data = countries %>%
  filter(country == "United States")

head(US_data)

ggplot(US_data, aes(x = year, y = life_expectancy)) +
  geom_point() +
  geom_line()

#Line plot for life expectancy for each continent over years

continent_table = countries %>%
  group_by(continent, year)%>%
  summarize(avg_LE = mean(life_expectancy))

continent_table

ggplot(continent_table, aes(x= year, y = avg_LE)) +
  geom_point(aes(color = continent))+
  geom_line(aes(color = continent))

#Scatterplot of GDP on x and LE on Y

countries_decade = countries%>%
  filter(year %in% c(1960, 1970, 1980, 1990, 2000, 2010))

head(countries_decade)

ggplot(countries_decade, aes(x=gdp, y=life_expectancy)) +
  geom_point(aes(color = continent)) +
  geom_smooth(color = "red", method = "lm", se = F) +
  facet_grid(.~year)

ggplot(countries_decade, aes(x=gdp, y=life_expectancy)) +
  geom_point(aes(color = continent)) +
  geom_smooth(color = "red", method = "lm", se = F) +
  facet_wrap(.~year, nrow = 3, ncol = 2)

#log10 of GDP

countries_decade = countries_decade %>%
  mutate(log10_gdp = log10(gdp))

str(countries_decade)

ggplot(countries_decade, aes(x=log10_gdp, y=life_expectancy)) +
  geom_point(aes(color = continent)) +
  geom_smooth(color = "red", method = "lm", se = F)+
  facet_wrap(.~year, nrow = 3, ncol = 2)
