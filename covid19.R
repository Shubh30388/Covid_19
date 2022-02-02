rm(list=ls()) #remove all variables stored previously 
library(Hmisc) #import
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(gganimate)
library(tidyverse)

# data 

data <- read.csv("C:/Users/AKASH LALIT/Desktop/covid-data.csv")

#  Total number of cases in continents
total_cases_content_wise <- data %>% group_by(continent) %>% 
 summarise(total_cases_in_millions=sum(total_cases,na.rm = TRUE)/1000000) 

#pie chart representation of total number of cases each content have
 
 ggplot(total_cases_content_wise_labels,aes(x='',y=total_cases_in_millions,
  fill= paste0(id,":",continent,'(',round(total_cases_in_millions/sum(total_cases_in_millions)*100),'%)')))+
   geom_bar(stat = "identity")+
   geom_text(aes(x=1,label=id ),position = position_stack(vjust = 0.5))+
   theme_classic()+
   theme(legend.position="top")+
   coord_polar("y",start =0)+
   theme(axis.line =element_blank())+
   theme(axis.text  =element_blank())+
   theme(axis.ticks  =element_blank())+
   labs(x=NULL,y=NULL,fill = NULL)+
   ggtitle('Total cases in continents')
 
 total_cases_content_wise_labels <- total_cases_content_wise %>% 
   mutate(id=LETTERS[row_number()])
 
 #total number of Deaths in continents
 data %>% group_by(continent) %>% summarise(total_deaths_in_millions=sum(total_deaths,na.rm = TRUE)/1000000)
 
 # Percentage of death to covid19 cases in continents
 data %>% group_by (continent) %>% summarise(percent_death=100 * sum(total_deaths,na.rm = TRUE)/sum(total_cases,na.rm = TRUE))
 
 #total population of world by content 
 
 World_population = subset(data,select = c( "continent","location","population"))
 world_population_unique<- World_population %>% unique  
 world_population_unique_continent <- world_population_unique %>% 
   group_by(continent) %>% 
   summarise( total_population = sum(population,na.rm = TRUE))
 
 #bar graph world population   
 ggplot(world_population_unique_continent,aes(continent,total_population,label=total_population)) + 
   geom_bar(stat="identity")+
   theme_classic()+
   geom_text()+
   ggtitle('Total populatin in continents')


 #handling Date
 datanew <- data %>% mutate(date=dmy(date))
 
 #Animated time series slot
 datanew %>% group_by(date) %>%
   summarise(total_cases=sum(total_cases,na.rm=TRUE)) %>% 
 mutate (cuml= cumsum(total_cases)) %>% 
ggplot(aes(x=date,y=cuml))+
geom_line(color= 'green')+
  geom_point(size=1.5)+
   geom_area(fill='blue')+
   theme_bw()+
   ggtitle('Daily Cumulative Cases')+
   transition_reveal(cuml)
 
   
  # Africa
 Africa = subset(data, continent == 'Africa') #subset
 data_new_Africa <- Africa %>% mutate(date=dmy(date)) #handling dae
 
 #Animated time series slot by continents
 data_new_Africa %>% group_by(date) %>%
   summarise(total_cases=sum(total_cases,na.rm=TRUE)) %>% 
   mutate (cuml= cumsum(total_cases)) %>% 
   ggplot(aes(x=date,y=cuml))+
   geom_line(color= 'green')+
   geom_point(size=1.5)+
   geom_area(fill='blue')+
   theme_bw()+
   ggtitle('Daily Cumulative Cases in AFRICA')+ 
   transition_reveal(cuml)
 
 #total population of Africa
 Africa_population = subset(Africa,select = c( "continent","location","population"))
 Africa_population_unique<- Africa_population %>% unique  
 Africa_population_unique %>% group_by(continent) %>% 
   summarise( total_pupulation_of_Africa = sum(population,na.rm = TRUE))
 
 #Asia
 
 Asia = subset(data, continent == 'Asia') #subset
 data_new_Asia <- Asia %>% mutate(date=dmy(date)) #handling date
 
 #Animated time series slot by continents
 data_new_Asia %>% group_by(date) %>%
   summarise(total_cases=sum(total_cases,na.rm=TRUE)) %>% 
   mutate (cuml= cumsum(total_cases)) %>% 
   ggplot(aes(x=date,y=cuml))+
   geom_line(color= 'green')+
   geom_point(size=1.5)+
   geom_area(fill='blue')+
   theme_bw()+
   ggtitle('Daily Cumulative Cases in Asia')+
   transition_reveal(cuml)
 
 #total population of Asia
 Asia_population = subset(Asia,select = c( "continent","location","population"))
 Asia_population_unique<- Asia_population %>% unique  
 Asia_population_unique %>% group_by(continent) %>% 
   summarise( total_pupulation_of_Asia = sum(population,na.rm = TRUE))
 
 #Europe
 
 Europe = subset(data, continent == 'Europe') #subset
 data_new_Europe <- Europe %>% mutate(date=dmy(date)) # handling date
 
 #Animated time series slot by continents
 data_new_Europe %>% group_by(date) %>%
   summarise(total_cases=sum(total_cases,na.rm=TRUE)) %>% 
   mutate (cuml= cumsum(total_cases)) %>% 
   ggplot(aes(x=date,y=cuml))+
   geom_line(color= 'green')+
   geom_point(size=1.5)+
   geom_area(fill='blue')+
   theme_bw()+
   ggtitle('Daily Cumulative Cases in Europe')
   transition_reveal(cuml)
   
   #total population of Europe
   Europe_population = subset(Europe,select = c( "continent","location","population"))
   Europe_population_unique<- Europe_population %>% unique  
   Europe_population_unique %>% group_by(continent) %>% 
     summarise( total_pupulation_of_Europe = sum(population,na.rm = TRUE))   

   # North America
   
 North_America = subset(data, continent == 'North America') #subset
 data_new_North_America <- North_America %>% mutate(date=dmy(date))#date handling
 
 #Animated time series slot by continents
 data_new_North_America %>% group_by(date) %>%
   summarise(total_cases=sum(total_cases,na.rm=TRUE)) %>% 
   mutate (cuml= cumsum(total_cases)) %>% 
   ggplot(aes(x=date,y=cuml))+
   geom_line(color= 'green')+
   geom_point(size=1.5)+
   geom_area(fill='blue')+
   theme_bw()+
   ggtitle('Daily Cumulative Cases in North America')+
   transition_reveal(cuml)
 
 #total population of North America
 North_America_population = subset(North_America,select = c( "continent","location","population"))
 North_America_population_unique<- North_America_population %>% unique  
 North_America_population_unique %>% group_by(continent) %>% 
   summarise( total_pupulation_of_North_America = sum(population,na.rm = TRUE))
 
 #South America
 
 South_America = subset(data, continent == 'South America') #subset
 data_new_South_America <- South_America %>% mutate(date=dmy(date)) #date handling
 
 #Animated time series slot by continents
 data_new_South_America %>% group_by(date) %>%
   summarise(total_cases=sum(total_cases,na.rm=TRUE)) %>% 
   mutate (cuml= cumsum(total_cases)) %>% 
   ggplot(aes(x=date,y=cuml))+
   geom_line(color= 'green')+
   geom_point(size=1.5)+
   geom_area(fill='blue')+
   theme_bw()+
   ggtitle('Daily Cumulative Cases in South America')+
   transition_reveal(cuml)
 
 #total population of South America
 South_America_population = subset(South_America,select = c( "continent","location","population"))
 South_America_population_unique<- South_America_population %>% unique  
 South_America_population_unique %>% group_by(continent) %>% 
   summarise( total_pupulation_of_South_America = sum(population,na.rm = TRUE))
 
 #Oceania
 
 Oceania = subset(data, continent == 'Oceania') #subset
 data_new_Oceania <- Oceania %>% mutate(date=dmy(date)) #date handling
 
 #Animated time series slot by continents
 
 data_new_Oceania %>% group_by(date) %>%
   summarise(total_cases=sum(total_cases,na.rm=TRUE)) %>% 
   mutate (cuml= cumsum(total_cases)) %>% 
   ggplot(aes(x=date,y=cuml))+
   geom_line(color= 'green')+
   geom_point(size=1.5)+
   geom_area(fill='blue')+
   theme_bw()+
   ggtitle('Daily Cumulative Cases in Oceania')+
   transition_reveal(cuml)
 
 #total population of oceania
 Oceania_population = subset(Oceania,select = c( "continent","location","population"))
 Oceania_population_unique<- Oceania_population %>% unique  
 Oceania_population_unique %>% group_by(continent) %>% 
 summarise( total_pupulation_of_oceania = sum(population)) 