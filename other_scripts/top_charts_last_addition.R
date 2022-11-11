# script for new header graphics 31/10/2022

d <- tb_fs |>
  mutate(year = year(Leba_publication_date)) |>
  group_by(year) |>
  summarise(n = n())

spline_int <- as.data.frame(spline(d$year, d$n))

ggplot(d) + 
  geom_point(aes(x = year, y = n, colour = "coral"), size = 2) +
  geom_line(data = spline_int, aes(x = x, y = y), size = 0.7) +
  theme_minimal() +
  ylab("")+
  xlab("")+
  theme(legend.position="none")
  

e <- tb_fs |>
  mutate(year = year(Leba_publication_date)) |>
  group_by(Programme, year) |>
  summarise(n = n()) |>
  group_by(year) |>
  summarise(n = n()) |>
  na.omit()

ggplot(e) + 
  geom_bar(aes(x = year, y = n), size = 2, stat = "identity") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 20, 2))+
  ylab("")+
  xlab("")+
  theme(legend.position="none")


f <- tb_fs |>
  mutate(year = year(Leba_publication_date)) |>
  count(citizenship.countryDescription, year) |>
  filter(citizenship.countryDescription != "UNKNOWN", !is.na(citizenship.countryDescription)) |>
  count(year) |>
  na.omit()

ggplot(f) + 
  geom_bar(aes(x = year, y = n), fill = "cadetblue", size = 2, stat = "identity") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 20, 2))+
  ylab("")+
  xlab("")+
  theme(legend.position="none")
