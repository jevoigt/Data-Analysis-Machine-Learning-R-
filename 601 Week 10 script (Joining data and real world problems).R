require(dplyr)
require(tidyverse)

df1 = data.frame(CustomerId = c(1:6), Product = c("Oven", "Television", "Mobile", "WashingMachine", "Lightings", "Ipad"))
df2 = data.frame(CustomerId = c(2,4,6,7,8), State = c("california", "New York", "Santiago", "Texas", "Indiana"))

#Joining data

inner_join_df = df1 %>%
  inner_join(df2, by = "CustomerId")

inner_join_df

left_join_df = df1 %>%
  left_join(df2, by = "CustomerId")

left_join_df

right_join_df = df1 %>%
  right_join(df2, by = "CustomerId")

right_join_df

full_join_df = df1 %>%
  full_join(df2, by = "CustomerId")

full_join_df


#Political Analysis

president = read.csv("gallup_approval_polls.csv")

str(president)

president = president %>%
  mutate(Net_Approval = Approve - Disapprove)

library(lubridate)
install.packages("ggtheme")
install.packages("plotly")

ggplot(president, aes(x = ymd(Date), y = Net_Approval)) +
  geom_line(data = subset(president, President == "Trump")) +
  labs(title = "Net Approval Ratings for Donald Trump", x = "Poll Date", y = "Average Net Approval", caption = "source Gallup")

approval_plot = ggplot(president, aes(x = ymd(Date), y = Net_Approval)) +
  geom_line(data = subset(president, President == "Trump")) +
  labs(title = "Net Approval Ratings for Donald Trump", x = "Poll Date", y = "Average Net Approval", caption = "source Gallup")

library(plotly)  

ggplotly(approval_plot)

ggplot(president, aes(x = ymd(Date), y = Net_Approval)) +
  geom_line(aes(color = President)) +
  labs(title = "Net Approval Ratings for all Presidents", x = "Poll Date", y = "Average Net Approval", caption = "source Gallup")

str(president)


president = president %>%
  mutate(Days = ymd(Date) - mdy(Inauguration.Date))

str(president)

ggplot(president, aes(x = Days, y = Net_Approval)) +
  geom_line(aes(color = President)) +
  labs(title = "Net Approval Ratings for all Presidents", x = "Poll Date", y = "Average Net Approval", caption = "source Gallup")

approval_plot_all = ggplot(president, aes(x = Days, y = Net_Approval)) +
  geom_line(aes(label = President), data = subset(president, President == "Trump"), size = 1, color = "red") +
  geom_line(data = subset(president, President != "Trump"), aes(color = President)) +
  labs(title = "Net Approval Ratings for all Presidents", x = "Poll Date", y = "Average Net Approval", caption = "source Gallup")

ggplotly(approval_plot_all)


#Political Analysis Vid 2

county_demo = read.csv("county_demographics.csv")
county_votes = read.csv("county_votes.csv")

str(county_demo)
str(county_votes)

head(county_votes)

require(dplyr)

county_votes2 = county_votes %>%
  spread(key = party, value = vote.count)

head(county_votes2)

county_votes2 = county_votes2 %>%
  mutate(Rep_percent = Republican / (Democrats + Others + Republican)) 

county_merged = county_votes2 %>%
  inner_join(county_demo, by = "Region")

head(county_merged)
str(county_merged)

plot1 = ggplot(county_merged, aes(x = per_capita_income, y = Rep_percent)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "red")

plot2 = ggplot(county_merged, aes(x = median_rent, y = Rep_percent)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "red")

plot3 = ggplot(county_merged, aes(x = median_age, y = Rep_percent)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "red")

require(ggpubr)

ggarrange(plot1, plot2, plot3, ncol = 2, nrow = 2)

regression1 = lm(Rep_percent ~ per_capita_income + 
                   median_rent + median_age,
                  data = county_merged)

regression2 = lm(Rep_percent ~ per_capita_income + 
                   median_rent + median_age + percent_white + 
                   percent_black + percent_asian + 
                   percent_hispanic, data = county_merged)

regression3 = lm(Rep_percent ~ per_capita_income + 
                   median_rent + percent_white + 
                   percent_black + percent_asian + 
                   percent_hispanic, data = county_merged)

summary(regression1)
summary(regression2)
summary(regression3)

google = read.csv("google_trend.csv")

str(google)
head(google)

google2 = google %>%
  spread(key = Keyword, value = Google.Search.Volume)

head(google2)
str(county_merged)

county_merged2 = county_merged %>%
  inner_join(google2, by = 'State')

str(county_merged2)

regression4 = lm(Rep_percent ~ per_capita_income + 
                   median_rent + percent_white + 
                   percent_black + percent_asian + 
                   percent_hispanic + Donald_Trump + Hillary_Clinton, data = county_merged2)

summary(regression4)


