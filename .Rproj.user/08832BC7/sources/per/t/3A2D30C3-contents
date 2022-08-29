library(dplyr)
library(tidyverse)
library(ggplot2)



df <- read.csv(file = "coursera_final.csv", 
               header=T,
               sep=",",
               dec=".")

df %>% map(~ sum(is.na(.)))
unique(df["member_casual"])

summary(df)
str(df)



df %>%
  group_by(member_casual) %>%
  summarise(proportion = n(), .groups="drop") %>%
  mutate(freq = proportion/sum(proportion)) %>% 
  ggplot(aes(x = "", y = freq, fill = member_casual)) + geom_bar(stat = "identity", width=1)+
  geom_text(aes(label = round(freq,2)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") + theme_void()
  

ggplot(df, aes(x = Elapsed)) +
  geom_histogram(aes(color = member_casual, fill = member_casual), 
                 position = "identity", bins = 30, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  xlim(NA, 150)


df %>%
  group_by(member_casual) %>%
  count(rideable_type)


df %>%  
  group_by(member_casual, Day_start) %>% 
  summarise(number_of_rides = n(), .groups="drop") %>% 
  arrange(member_casual, Day_start) %>% 
  ggplot(aes(x = Day_start, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides of Members and Casual riders Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


df %>%
  group_by(member_casual, start_station_name) %>% 
  summarise(number_of_rides = n(), .groups="drop") %>%
  filter(member_casual == "casual") %>% 
  arrange(desc(number_of_rides)) %>% 
  slice(2:4) %>% 
  ggplot(aes(x = start_station_name, y = number_of_rides, fill = member_casual)) +
  labs(title ="Top start stations for casuals") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


df %>%
  group_by(member_casual, end_station_name) %>% 
  summarise(number_of_rides = n(), .groups="drop") %>%
  filter(member_casual == "casual") %>% 
  arrange(desc(number_of_rides)) %>% 
  slice(2:4) %>% 
  ggplot(aes(x = end_station_name, y = number_of_rides, fill = member_casual)) +
  labs(title ="Top end stations for casuals") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


df %>%
  group_by(member_casual, start_station_name) %>% 
  summarise(number_of_rides = n(), .groups="drop") %>%
  filter(member_casual == "member") %>% 
  arrange(desc(number_of_rides)) %>% 
  slice(2:4) %>% 
  ggplot(aes(x = start_station_name, y = number_of_rides, fill = member_casual)) +
  labs(title ="Top start stations for members") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


df %>%
  group_by(member_casual, end_station_name) %>% 
  summarise(number_of_rides = n(), .groups="drop") %>%
  filter(member_casual == "member") %>% 
  arrange(desc(number_of_rides)) %>% 
  slice(2:4) %>% 
  ggplot(aes(x = end_station_name, y = number_of_rides, fill = member_casual)) +
  labs(title ="Top end stations for members") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))



df %>% group_by(member_casual) %>% summarise_at(vars(Elapsed), list(avg_time = mean)) %>% 
  ggplot(aes(x = member_casual, y = avg_time, fill = member_casual)) + geom_col(width=0.5, position = position_dodge(width=0.5)) +
  geom_text(aes(label = round(avg_time, 2)),
            position = position_stack(vjust = 0.5))



df %>% group_by(member_casual) %>% summarise_at(vars(Elapsed), list(avg_time = mean)) %>% 
  ggplot(aes(x = "", y = avg_time, fill = member_casual)) + geom_bar(stat = "identity", width=1)+
  geom_text(aes(label = round(avg_time, 2)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") + theme_void()



df %>% group_by(member_casual, rideable_type) %>% summarise(num_rides = n(), .groups="drop") %>% 
  ggplot(aes(x = rideable_type, y = num_rides, fill = member_casual)) +
  labs(title ="Number of rides by bike type") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

