library(tidyverse)
library(scales)
library(highcharter)
library(lubridate)
library(htmlwidgets)

dataset <- read.csv('/media/antonela/4E21EC9619AB84D1/R scripts/script_251120_Google_Mobility/Google Mobility/mobility_report_241120.csv')
dataset <- dataset[, -c(3:7)]
df <- as.data.frame(dataset)
colnames(df)[4] <- 'Retail and recreation'
colnames(df)[5] <- 'Grocery and pharmacy'
colnames(df)[6] <- 'Parks'
colnames(df)[7] <- 'Transit stations'
colnames(df)[8] <- 'Workplaces'
colnames(df)[9] <- 'Residential'

#datos ausentes
any(is.na(df))
df2 <- df[complete.cases(df), ]

#formato fecha y agrupar
df2$date <- as.Date(df2$date)
glimpse(df2)
df3 <- df2 %>%
  group_by(date) %>%
  summarise(across(c(3:8), mean))

#trabajo con numeros
#df4 <- data.frame(diff(as.matrix(df3[, c(2:7)])))
#df5 <- df3
#df5 <- df5[-1, ]
#df6 <- cbind(df5$date, df4)
#colnames(df6)[1] <- 'Date'
#glimpse(df6)
df7 <- df3 %>% group_by(month = floor_date(date, 'month')) %>%
  summarise(across(c(2:7), mean))

df7 <- df7 %>% mutate_if(is.numeric, ~round(., 2))
df7$month <- format(df7$month, '%m/%Y')
colnames(df7)[1] <- 'Date'

p <- hchart(df7, type = 'spline') %>%
  hc_xAxis(categories = df7$Date, type = 'datetime') %>%
  hc_add_series(df7$`Retail and recreation`, name = 'Retail and recreation', showInLegend = TRUE) %>%
  hc_add_series(df7$`Grocery and pharmacy`, name = 'Grocery and pharmacy', showInLegend = TRUE) %>%
  hc_add_series(df7$Parks, name = 'Parks', showInLegend = TRUE) %>%
  hc_add_series(df7$`Transit stations`, name = 'Transit stations', showInLegend = TRUE) %>%
  hc_add_series(df7$Workplaces, name = 'Workplaces', showInLegend = TRUE) %>%
  hc_add_series(df7$Residential, name = 'Residential', showInLegend = TRUE) %>%
  hc_yAxis(labels = FALSE) %>%
  hc_tooltip(useHTML = TRUE, crosshairs = TRUE, headerFormat = '<b> Date: {point.x} <br>', 
             pointFormat = '{series.name}: {point.y}%') %>%
  hc_title(text = 'Mobility in pandemic', 
           style = list(fontWeight = 'bold', fontSize = '20px'),
           align = 'left') %>%
  hc_subtitle(text = 'Period from February to November 2020',
              style = list(fontWeight = 'bold', fontSize = '15px'),
              align = 'left') %>%
  hc_credits(enabled = TRUE, text = 'By Antonela Tamagnini
             <br> Source: Google') %>%
  hc_add_theme(hc_theme_bloom()) %>%
  hc_legend(align = 'right', verticalAlign = 'top')
p
saveWidget(widget = p, file = "plot.html")
