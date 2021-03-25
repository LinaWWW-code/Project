library(ggplot2)
library(tidyverse)
library(dplyr) 
library(Amelia)
library(scales)
### dataset
FullDS = read.csv("FAO.csv", header = T)
head(FullDS)
summary(FullDS) # check NA
missmap(FullDS)

# overall production
tidyALL = FullDS %>%
  group_by(Area.Abbreviation) %>%
  gather(Y1961:Y2013, key="Year", value="thousands.of.tonnes")
tidyALL$Year <- str_replace(tidyALL$Year, "[Y]", "")
tidyALL$Year <- as.integer(tidyALL$Year)
tidyALL <- tidyALL %>%
  select(Area.Abbreviation:Area, Year, Item.Code:Element, 
         thousands.of.tonnes, latitude:longitude)
tidyALL %>%
  group_by(Year, Element) %>%
  summarise(Total=sum(thousands.of.tonnes, na.rm = TRUE)) %>%
  ggplot(aes(x=Year,y=Total))+
  geom_line(aes(group=Element, color=Element))+
  xlab(label = "Year") +
  ylab(label = "Total Production (thousands of tonnes)")+
  ggtitle(label = "Worldwide Production of Feed and Food from 1961 to 2013\nby log transformation")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(
    trans = "log10",
    breaks = trans_breaks("log10", function(x) 10^x, n=4),
    labels = trans_format("log10", math_format(10^.x)))

# production increase 
countryTotals = tidyALL %>%
  group_by(Area, Year, Element) %>%
  summarise(element.total = sum(thousands.of.tonnes)) 

countryTotals1961 = (countryTotals %>% filter(Year == 1961))[, c(1,3,4)]
countryTotals2013 = (countryTotals %>% filter(Year == 2013))[, c(1,3,4)]

countryProdGrowth = countryTotals1961[, c(1,2)]
countryProdGrowth$Prod1961 = countryTotals1961$element.total
countryProdGrowth$Prod2013 = countryTotals2013$element.total
countryProdGrowth$ProdChange = countryProdGrowth$Prod2013 - countryProdGrowth$Prod1961

countryProdGrowth %>%
  filter(Element == "Feed") %>%
  top_n(n=10) %>%
  ggplot(aes(x = reorder(Area, log(ProdChange)), y = ProdChange, fill = ProdChange)) + 
  geom_bar(stat = "Identity") + 
  xlab("Country") + 
  ylab("Change in Production (thousands of tonnes)") +
  ggtitle(label = "Highest Increases in Feed Production from 1961 to 2013")+
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5))

countryProdGrowth %>%
  filter(Element == "Food") %>%
  top_n(n=10) %>%
  ggplot(aes(x = reorder(Area, log(ProdChange)), y = ProdChange, fill = ProdChange)) + 
  geom_bar(stat = "Identity") + 
  xlab("Country") + 
  ylab("Change in Production (thousands of tonnes)") +
  ggtitle(label = "Highest Increases in Food Production from 1961 to 2013") +
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5))

# time series animation
library(gganimate)
theme_set(theme_bw())
library(gifski)

FoodFeed = read.csv("FoodFeedDS.csv", header = T)

p <- ggplot(FoodFeed,aes(x = log(Food), y=log(Feed), size = Total, colour = Area)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  labs(x = "Food", y = "Feed")
p + transition_time(Year) +
  labs(title = "Year: {frame_time}")

#### animation 
FoodFeedContinent = read.csv("FoodFeedContinent.csv", header = T)

Timeseriestry <- ggplot(FoodFeedContinent,aes(x = log(Food), y=log(Feed), size = Total, colour = Continent)) +
  geom_point(show.legend = TRUE, alpha = 0.7) +
  scale_color_manual(values = c("#963D97","#F5821F","#61BB46","#FDB827","#E03A3E","#009DDC","#0000ff")) +
  scale_size(range = c(2, 12)) +
  transition_time(Year) +
  labs(title = "Year: {frame_time}")
animate(Timeseriestry, fps = 10, width = 750, height = 450)
anim_save("TimeseriestryAnimation.gif")

septry = ggplot(FoodFeedContinent,aes(x = log(Food), y=log(Feed), size = Total, colour = Continent)) +
  geom_point(show.legend = TRUE, alpha = 0.7) +
  scale_color_manual(values = c("#963D97","#F5821F","#61BB46","#FDB827","#E03A3E","#009DDC","#0000ff")) +
  scale_size(range = c(2, 12))
seven = septry + facet_wrap(~Continent) +
  transition_time(Year) +
  labs(title = "Year: {frame_time}")
animate(seven, fps = 10, width = 750, height = 450)
anim_save("TimeseriesSevAnimation.gif")

f = FoodFeedContinent %>%
  group_by(Continent, Year) %>%
  summarise(TotalFood=sum(Food, na.rm = TRUE),TotalFeed=sum(Feed, na.rm = TRUE)) 
d = f %>%
  ggplot(aes(x=log(TotalFood),y=log(TotalFeed), size = TotalFood+TotalFeed ,color = Continent)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("#963D97","#F5821F","#61BB46","#FDB827","#E03A3E","#009DDC","#0000ff")) +
  scale_size(range = c(2, 12)) +
  transition_time(Year) +
  labs(title = "Year: {frame_time}") +
  shadow_mark(alpha = 0.3, size = 0.5)
animate(d, fps = 10, width = 750, height = 450)
anim_save("f2Animation.gif")

# Parallel Coordinates
library(GGally)

FoodItem = read.csv("foodOrg.csv", header = T)
FeedItem = read.csv("feedOrg.csv", header = T)

ggparcoord(FoodItem)
ggparcoord(FeedItem)

ggparcoord(FeedItem,
           columns = 3:7, 
           groupColumn = "Area", 
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the top 5 Feed Produced Items",
           alphaLines = 0.3) + 
  scale_color_viridis_d() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  xlab(NULL)

ggparcoord(FoodItem,
           columns = 3:7, 
           groupColumn = "Area", 
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the top 5 Food Produced Items",
           alphaLines = 0.3) + 
  scale_color_viridis_d() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  xlab(NULL)



