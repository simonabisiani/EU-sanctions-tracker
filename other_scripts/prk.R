library(tidyverse)

tb_fs <- read_csv("merged_dataset.csv")

prk <- tb_fs %>% 
  filter(Programme == "PRK")

prk %>% 
  mutate(year = year(Leba_publication_date)) %>% 
  ggplot(aes(x = year)) +
  geom_bar(stat = "count",
           position = "stack",
           fill = "coral",
           width = 1) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2017,2022,1))+
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks = seq(0,120,10))


mockup_person <- prk |> filter(Leba_numtitle == "2017/1509 (OJ L224)")

prk %>% 
  mutate(year = year(Leba_publication_date)) %>% 
  filter(year == 2016)

prk %>% 
  mutate(year = year(Leba_publication_date)) %>% 
  filter(year == 2017) %>% 
  count()

prk %>% 
  ggplot(aes(x = Leba_publication_date, y = 0)) +
  geom_point(alpha = 0.4)+
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,1,1))+
  scale_x_date(date_labels = "%b %Y", 
               breaks = seq(min(prk$Leba_publication_date),
                          max(prk$Leba_publication_date), 200)) +
  xlab("")+
  ylab("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

prk %>% 
  group_by(Leba_publication_date) %>% 
  count() %>% 
  ggplot(aes(x = Leba_publication_date, y = 0, size = n)) +
  geom_point(alpha = 0.4)+
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,1,1))+
  scale_x_date(date_labels = "%b %Y", 
               breaks = seq(min(prk$Leba_publication_date),
                            max(prk$Leba_publication_date), 200)) +
  xlab("")+
  ylab("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# library(zoo)
# 
# prk %>% 
#   mutate(month = as.yearmon(Leba_publication_date, "%m/%Y")) %>% 
#   ggplot(aes(x = month)) +
#   geom_bar(stat = "count",
#            fill = "coral",
#            width = 0.3,
#            alpha = 0.5) +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(2017,2022,1))+
#   xlab("")+
#   ylab("")+
#   scale_y_continuous(breaks = seq(0,120,10))



################### PRK one company

entities_prk <- tb_fs %>% filter(Programme == "PRK" & Subject_type == "E")
write.csv(entities_prk, "entities_prk.csv")
