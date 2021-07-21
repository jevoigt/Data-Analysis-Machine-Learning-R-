setwd("/Users/JackVoigt/Documents/MSIA Files")
library(readxl)
library(ggplot2)
library(dplyr)
detach(package:plyr)
library(lubridate)
library(gridExtra)
library(grid)
library(gridExtra)

df = read_excel("InusranceData.xlsx")


head(df)
names(df)

#Histogram of variable BMI
ggplot(df, aes(x=BMI)) + 
	geom_histogram(colour="black", fill="red", bins = 10)+
	geom_density(alpha=.2, fill="#FF6666")

#Barplots of mean BMI and weight
bmi = df

bmi = bmi %>%
	group_by(Response) %>%
	summarize(mean_BMI = mean(BMI, na.rm = TRUE))
			  
head(bmi, 8)

bmi = bmi %>%
	arrange(by_group=mean_BMI)

ggplot(data=bmi, aes(x=reorder(Response, -mean_BMI), y=mean_BMI, fill = mean_BMI)) +
	geom_bar(stat="identity", position=position_dodge())+
	geom_text(aes(label=mean_BMI), vjust=1.6, color="black",
			  position = position_dodge(2), size=3.5) +
	scale_x_discrete("Response Group") 

weight_df = df

weight_df = weight_df %>%
	group_by(Response) %>%
	summarize(mean_Weight = mean(Wt, na.rm = TRUE))

head(weight_df, 8)

ggplot(data=weight_df, aes(x=reorder(Response, -mean_Weight), y=mean_Weight, fill = mean_Weight)) +
	geom_bar(stat="identity", position=position_dodge())+
	geom_text(aes(label=mean_Weight), vjust=1.6, color="black",
			  position = position_dodge(2), size=3.5) +
	scale_x_discrete("Response Group") 


#Boxplot of BMI
str(df$Response)
df$Response = as.factor(df$Response)
ggplot(df, aes(group = Response, x=Response, y=BMI, fill=Response)) + 
	geom_boxplot()







#Final Project

acme = read_excel("Acme Inc Sales.xls")

head(acme)			  
names(acme)

#Count and drop country
table(acme$Country)

acme = subset(acme, select = -c(Country))

AnnualSalesByLocation = acme %>%
	group_by(Year=year(`Order Date`),Region, State)%>%
	summarize(SumSales = sum(Sales))

view(AnnualSalesByLocation)

AnnualSalesByCustomer = acme %>%
	group_by(Year=year(`Order Date`), `Customer Name`) %>%
	summarize(SumSales = sum(Sales))
view(AnnualSalesByCustomer)	


#Highest Grossing by Year
ByYear = AnnualSalesByCustomer %>%
	group_by(Year) %>%
	arrange(Year, desc(SumSales)) %>%
	top_n(10, SumSales)

view(ByYear)
	

#Highest Grossing Overall
arrange(AnnualSalesByCustomer,desc(SumSales), n = 10)



#Profit
AnnualProfitByCustomer = acme %>%
	group_by(Year=year(`Order Date`), `Customer Name`) %>%
	summarize(SumProfit = sum(Profit))
view(AnnualProfitByCustomer)

#Highest Profit by Year
ProfitByYear = AnnualProfitByCustomer %>%
	group_by(Year) %>%
	arrange(Year, desc(SumProfit)) %>%
	top_n(10, SumProfit)
view(ProfitByYear)

#Highest Profit Overall
arrange(AnnualProfitByCustomer,desc(SumProfit), n = 10)
view(AnnualProfitByCustomer)


#Sales By Segment Over Region

SalesData = acme %>%
	group_by(Segment, Region) %>%
	summarise(SumSales = sum(Sales))
view(SalesData)

SalesData %>%
	ggplot(aes(Segment, SumSales))+
	scale_color_brewer(palette = "Paired")+
	geom_smooth(aes(color=Region), se = FALSE)+
	geom_col(aes(fill=Region), position = "dodge")+
	labs(y="Sales", x="Segments")+
	facet_wrap(~Region, nrow(2))

#Over Months
SalesData2 = acme %>%
	group_by(Segment, `Order Date`) %>%
	summarise(SumSales = sum(Sales))
view(SalesData2)

SalesData2 %>%
	ggplot(., aes(`Order Date`, SumSales))+
	scale_color_brewer(palette = "Paired")+
	geom_smooth(aes(color=Segment), se = FALSE)+
	geom_col(aes(fill=Segment), position = "dodge")+
	geom_smooth(aes(`Order Date`, SumSales, color=Segment), se = FALSE)+
	labs(y="Sales", x="Months Across Segments")+
	facet_wrap(~Segment, ncol(3))

#Question 11

BestItems = acme %>%
	select(`Product Name`, `Customer Name`) %>%
	filter(`Customer Name` == c("Tamara Chand", "Raymond Buch", "Sanjit Chand",
		   "Hunter Lopez", "Adrian Barton", "Tom Ashbrook", "Christopher Martinez",
		   "Keith Dawkins", "Sanjit Engle", "Andy Reiter"))
	

view(BestItems)
grid.table(BestItems)


acme2 = acme %>%
	filter(`Customer Name` == c("Tamara Chand", "Raymond Buch", "Sanjit Chand",
									   "Hunter Lopez", "Adrian Barton", "Tom Ashbrook", "Christopher Martinez",
									   "Keith Dawkins", "Sanjit Engle", "Andy Reiter"))
view(acme2)	

acme2 %>%
	ggplot(aes(x=reorder(`Product Name`, -Quantity), Quantity)) +
	geom_col(aes(fill=`Quantity`), position = "dodge") +
	labs(y="Quantity of Sales", x="Product Name") + 
	scale_x_discrete(guide = guide_axis(n.dodge = 10))



acme2 = acme %>%
	filter(`Customer Name` == c("Tamara Chand", "Raymond Buch", "Sanjit Chand",
								"Hunter Lopez", "Adrian Barton", "Tom Ashbrook", "Christopher Martinez",
								"Keith Dawkins", "Sanjit Engle", "Andy Reiter"))
acme2 %>%
	ggplot(aes(`Order Date`, Sales)) +
	geom_line(stat = "identity")
