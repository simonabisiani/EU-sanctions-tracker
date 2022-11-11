################################################################################
#                  SINGLE NATIONALITY SEARCH (afghanian)
################################################################################

library(readr)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(viridis)
library(hrbrthemes)
library(plotly)
library(treemapify)

################################################################################
#                               TRAVEL BANS
################################################################################

travel_bans <- read_csv("travel_bans_massi.csv")

################################################################################
#                          FINANCIAL SANCTIONS
################################################################################

File1_0 <- read_delim(
  "20220913-FULL-1_0.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

################################################################################
#                              MERGED
################################################################################

tb_filtered <-
  travel_bans %>% select(
    logicalId,
    remark,
    nameAlias,
    nameAlias.wholeName,
    subjectType.classificationCode,
    birthdate.year,
    nameAlias.function,
    citizenship.countryDescription,
    regulation.publicationUrl,
    regulation.numberTitle,
    regulation.regulationType
  )

File1_0_filtered <-
  File1_0 %>% select(
    Entity_logical_id...2,
    Subject_type,
    Leba_numtitle,
    Leba_publication_date,
    Leba_url,
    Programme,
    Entity_remark,
    Naal_logical_id,
    Naal_leba_numtitle,
    Naal_leba_publication_date,
    Naal_leba_url,
    Naal_wholename,
    Naal_function,
    Naal_programme,
    Addr_country,
    Birt_country,
    Birt_date,
    EU_ref_num,
  )

File1_0_unique <- File1_0_filtered %>%
  distinct(Entity_logical_id...2, .keep_all = TRUE)

tb_unique <- tb_filtered %>% 
  distinct(logicalId, .keep_all = TRUE)

afganian <-
  full_join(File1_0_unique,
            tb_unique,
            by = c("Entity_logical_id...2" = "logicalId"), keep = TRUE) %>% 
  filter(citizenship.countryDescription == "AFGHANISTAN")




################################################################################
#                          TIMELINE ENTITIES
################################################################################

afganian %>% 
  mutate(year = year(Leba_publication_date)) %>% 
  ggplot(aes(x = year, fill = Subject_type)) +
  geom_bar(stat = "count",
           position = "stack",
           width = 1) +
  theme_minimal() +
  ggtitle("Number of sanctioned (both tb and fs) enterprises and individuals on any given year")

afganian %>% count(Subject_type)

################################################################################
#             SINGLE SANCTION COUNT (double count of individual)
################################################################################

tb_fs <- afganian %>% 
  mutate(type = case_when(
    is.na(Entity_logical_id...2) & !is.na(logicalId) ~ "travel ban",
    !is.na(Entity_logical_id...2) & is.na(logicalId) ~ "financial sanction",
    !is.na(Entity_logical_id...2) & !is.na(logicalId) ~ "both travel and financial sanction")) 

tb_fs %>% count(type)


tb_fs %>% 
  mutate(year = year(Leba_publication_date)) %>% 
  ggplot(aes(x = year, fill = type)) +
  geom_bar(stat = "count",
           position = "stack",
           width = 1) +
  theme_minimal() +
  xlab("")+
  ylab("")



################################################################################
#                          MONITOR REGIMES CHART 
################################################################################

# tb_fs %>% 
#   mutate(year = year(Leba_publication_date)) %>% 
#   filter(year %in% (2006:2012)) %>% 
#   count(Programme) %>% 
#   ggplot(aes(x = reorder(Programme, -n), y = n)) +
#   geom_bar(stat = "identity",
#            width = 1) +
#   theme_minimal() +
#   xlab("") +
#   ylab("numero di nuove sanzioni") +
#   scale_fill_viridis(discrete = TRUE) +
#   ggtitle("Ranking numero di sanzioni per regime per range di anni specifico (qua dal 2006 al 2012)")
# 
# 
# tb_fs %>% 
#   mutate(year = year(Leba_publication_date)) %>% 
#   filter(year == 2022) %>% 
#   count(Programme) %>% 
#   ggplot(aes(x = reorder(Programme, -n), y = n)) +
#   geom_bar(stat = "identity",
#            width = 1) +
#   theme_minimal() +
#   xlab("") +
#   ylab("numero di nuove sanzioni") +
#   scale_fill_viridis(discrete = TRUE) +
#   ggtitle("Ranking numero di sanzioni per regime per anni specifici (qua 2022)")
# 
# 
# tooltip_ukr_2022 <- tb_fs %>% 
#   mutate(year = year(Leba_publication_date)) %>% 
#   filter(year == 2022 & Programme == "UKR") %>% 
#   count(Subject_type)



################################################################################
#                       TREEMAP ENTITY TYPE
################################################################################

# TREEMAP ENTITY TYPE
tb_fs %>% count(Subject_type)

#install.packages("treemapify")
library(treemapify)

ggplot(treemap_entities,
       aes(
         area = n,
         fill = Subject_type,
         label = paste(Subject_type, n, perc, sep = "\n")
       )) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none") +
  ggtitle(
    "Number of individuals/enterprises between the financial sanctions and travel bans datasets"
  )


tooltip_treemap_entities <- tb_fs %>% 
  group_by(Subject_type) %>% 
  count(Programme)




################################################################################
#                          TREEMAP NATIONALITIES
################################################################################



tb_fs %>% 
  filter(Subject_type == "P") %>% 
  count(citizenship.countryDescription) %>% 
  mutate(perc =  round(n / sum(n), 2)) %>% 
  #filter(!is.na(citizenship.countryDescription)) %>% 
  ggplot(aes(
    area = n,
    fill = citizenship.countryDescription,
    label = paste(citizenship.countryDescription, n, perc, sep = "\n")
  )) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none") +
  ggtitle(
    "Number of individuals of each nationality in the financial sanctions and travel bans datasets"
  )



################################################################################
#                       TREEMAP PROGRAMME
################################################################################

tb_fs %>% 
  count(Programme) %>% 
  mutate(perc =  round(n / sum(n), 2)) %>% 
  ggplot(aes(
    area = n,
    fill = Programme,
    label = paste(Programme, n, perc, sep = "\n")
  )) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none") +
  ggtitle(
    "Number of individuals/enterprises in each regime in the financial sanctions and travel bans datasets"
  )


tooltip_treemap_programme <- tb_fs %>% 
  filter(Programme == "UKR") %>% 
  count(type, Subject_type)




################################################################################
#                 LATEST SANCTIONED INDIVIDUALS / ENTERPRISES
################################################################################


enterprises_latest <- tb_fs %>% filter(Subject_type == "E") %>% arrange(desc(Leba_publication_date)) %>% select(
  Naal_wholename, Leba_publication_date, Programme
) %>% slice(1:20)

knitr::kable(enterprises_latest, "simple")


tooltip_latest_enterprise <- enterprises %>% 
  arrange(desc(Leba_publication_date)) %>% slice(1)


individuals_latest <- tb_fs %>% arrange(desc(Leba_publication_date)) %>% select(
  Naal_wholename, type, Leba_publication_date, Programme, citizenship.countryDescription, Subject_type
) %>% filter(Subject_type == "P") %>% slice(1:20) %>% select(-Subject_type)

knitr::kable(individuals_latest, "simple")

tooltip_latest_individuals <- tb_fs %>% filter(Subject_type == "P") %>% 
  arrange(desc(Leba_publication_date)) %>% slice(1)




################################################################################
#                 CUMULATIVE and ABSOLUTE CHART
################################################################################

tb_fs %>% 
  mutate(year = year(Leba_publication_date)) %>%  
  group_by(Programme, year) %>% 
  count() %>% 
  ungroup() %>% 
  complete(Programme, year = 2006:2022, 
           fill = list(n = 0)) %>% 
  na.omit() %>% 
  group_by(Programme) %>% 
  mutate(cumsum=cumsum(n)) %>% 
  ggplot(aes(x = year, y = cumsum, fill = Programme)) +
  geom_bar(stat = "identity",
           position = "stack",
           width = 1) +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  xlab("")+
  ylab("")+
  scale_x_continuous(breaks = seq(2006, 2022, 2))

tb_fs %>% 
  mutate(year = year(Leba_publication_date)) %>% 
  ggplot(aes(x = year, fill = Programme)) +
  geom_bar(stat = "count",
           position = "stack",
           width = 1) +
  theme_minimal() +
  xlab("")+
  ylab("")+
  scale_fill_viridis(discrete = TRUE) 

################################################################################
#                 AREA CHART
################################################################################

growth <- tb_fs %>% 
  count(Programme, Leba_publication_date) %>% 
  add_count(Programme, name = "Programme_n")

library(ggridges)
library(scales)

growth$Leba_publication_date <- as.Date(growth$Leba_publication_date)


growth %>% 
  filter(!is.na(Programme)) %>% 
  ggplot(aes(Leba_publication_date, Programme)) +
  geom_density_ridges(rel_min_height = 0.01, scale = 3, alpha = 0.7)+
  geom_point(data = filter(growth, Programme_n <= 2)) +
  xlab("")+
  ylab("") +
  scale_x_date(breaks = date_breaks("4 years"), labels = date_format("%Y")) +
  ggtitle("Density plots of sanctioned afghanians for the regimes afghanians have been sanctioned for") +
  theme_minimal() 
