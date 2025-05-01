#install.packages("tidyverse")
library(tidyverse)

news_days <- read_csv("https://github.com/Dargel/R_AI_news/raw/refs/heads/main/AI_Comnews_days.csv")

#Ч.1 Посмотрим, связано ли количество публикаций с днями недели

#используем фактор, чтобы на графике дни недели шли по порядку


AI_news <- news_days %>%
  mutate(`days of week` = factor(`days of week`, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))) %>%
  arrange(date) %>%
  mutate(cumulative_count = row_number())


#рисуем график


ggplot(data = AI_news) +
  geom_bar(aes(x = `days of week`, fill = `days of week`)) +
  theme_classic() +
  labs(x="Week days",
       y="Number of publications",
       title = "Publication of news on the topic of AI",
       subtitle = "Source: Comnews.ru") +
  guides(size="none", colour = 'none', fill = FALSE) +
  theme(plot.title = element_text(size = 18, face = "bold", vjust = 5),
        plot.subtitle = element_text(size = 10, vjust = 8),
        axis.title.x = element_text(size = 12, vjust= -2), 
        axis.title.y = element_text(size = 12, vjust= 3), 
        axis.text.x = element_text(size = 10, vjust=1, hjust= 0.5),
        axis.text.y = element_text(size = 9, angle = 90, vjust=1, hjust= 0.5),
        plot.margin = margin(rep(30, 30))) +
  scale_fill_viridis_d()

  
#Вывод: существенной разницы в будние дни практически нет. Немного выделяется понедельник


#Ч.2 Посмотрим, как менялось количество публикаций в течение 3 лет

#сгруппируем публикации по дням

art_per_days <- news_days %>% 
  mutate(dates = ymd(date)) %>% 
  group_by(dates) %>%
  summarise(n = n())


#сгруппируем публикации по годам

art_per_years <- art_per_days %>% 
  mutate(Years = year(dates))


#рисуем линейный график + добавим линейную регрессию

ggplot(data = art_per_years, aes(x = dates, y = n, color = Years)) +
  geom_line() +
  theme_classic() +
  labs(x="Years",  ,
       y="Number of publications",
       title = "Publication of news on the topic of AI",
       subtitle = "Source: Comnews.ru") +
  guides(size="none", colour = FALSE, fill = FALSE) +
  scale_y_continuous(limits = c(0, 10), breaks = c(2, 4, 6, 8)) +
  theme(plot.title = element_text(size = 18, face = "bold", vjust = 5),
        plot.subtitle = element_text(size = 10, vjust = 8),
        axis.title.x = element_text(size = 10, vjust= -2), 
        axis.title.y = element_text(size = 10, vjust= 3), 
        axis.text.x = element_text(size = 10, vjust=1, hjust= 0.5),
        axis.text.y = element_text(size = 9, angle = 90, vjust=1, hjust= 0.5),
        plot.margin = margin(rep(30, 30)))+
  geom_smooth(method = "lm", color = "#EDFF21") 



# Построим кумулятивный график


ggplot(AI_news, aes(x = date)) +
  geom_area(aes(y=cumulative_count, fill="cumulative_count")) + 
  labs(title = "Cumulative Count of Articles Over Time",
       x = "Years",
       y = "Articles") +
  scale_y_continuous(limits = c(0, 1500),breaks = c(300, 600, 900, 1200)) +
  guides(size="none", colour = FALSE, fill = FALSE) +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", vjust = 5),
        axis.title.x = element_text(size = 10, vjust= -2), 
        axis.title.y = element_text(size = 10, vjust= 4), 
        axis.text.x = element_text(size = 10, vjust=1, hjust= 0.5),
        axis.text.y = element_text(size = 8, angle = 90, vjust=1, hjust= 0.5),
        plot.margin = margin(rep(30, 30))) +
  scale_fill_brewer()


