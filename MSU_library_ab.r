library(tidyverse)
library(ggthemes)
library(broom)

#import the click data for each page version
page0 <- read.csv("crazyegg_data\\page_v0.csv")
page1 <- read.csv("crazyegg_data\\page_v1.csv")
page2 <- read.csv("crazyegg_data\\page_v2.csv")
page3 <- read.csv("crazyegg_data\\page_v3.csv")
page4 <- read.csv("crazyegg_data\\page_v4.csv")

#import the file containing total clicks and visits for each page
agg_data <- read.csv("crazyegg_data\\agg_data.csv")

#concatenate the page dataframes so that they can be merged with agg_data
page_data <- rbind(page0, page1, page2, page3, page4)

#merge the two dataframes on page_data$name and agg_data$name
joined <- merge(agg_data, page_data, by.x = 'name', by.y = 'Name', all.x = TRUE)

#select the name, visits, clicks, and number of clicks cols
joined <- subset(joined, select = c(1:5, 8))
joined

#renames the columns
colnames(joined) = c('name', 'page_version', 'live_time', 'page_visits', 'page_clicks', 'element_clicks')
joined

#create the two calcuated columns about the target element
joined <- joined %>%
    mutate(visit_conversion = element_clicks / page_visits, click_rate = element_clicks / page_clicks)

joined

#plot the different visit conversion rates for the page
ggplot(joined, aes(x = reorder(name, page_version), y = visit_conversion))+
    geom_col(fill = 'LightBlue')+
    theme_few()+
    labs(y = "Visitor Conversion Rate", x = "")+
    theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1))

#plot the different click rates for the target elements
ggplot(joined, aes(x = reorder(name, page_version), y = click_rate))+
    geom_col(fill = 'LightBlue')+
    theme_few()+
    labs(y = "Click Rate", x = "")+
    theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1))

#create the five dataframes for click rate
click <- c(rep(1, times=42), rep(0, times = 3714-42))
page <- rep('INTERACT', times = 3714)
proxy0 <- data.frame(page, click)

click <- c(rep(1, times=53), rep(0, times = 1587-53))
page <- rep('CONNECT', times = 1587)
proxy1 <- data.frame(page, click)

click <- c(rep(1, times=21), rep(0, times = 1652-21))
page <- rep('LEARN', times = 1652)
proxy2 <- data.frame(page, click)

click <- c(rep(1, times=38), rep(0, times = 1717-38))
page <- rep('HELP', times = 1717)
proxy3 <- data.frame(page, click)

click <- c(rep(1, times=45), rep(0, times = 1348-45))
page <- rep('SERVICES', times = 1348)
proxy4 <- data.frame(page, click)

#vertically concatenate the five dfs together into one
proxy_df <- rbind(proxy0, proxy1, proxy2, proxy3, proxy4)

str(proxy_df)

#use a glm to determine the p-value of the difference in click rates
glm(click~page, family = 'binomial', data = proxy_df)%>%
    tidy()


