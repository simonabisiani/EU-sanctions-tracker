# Libraries
library(viridis)
#library(hrbrthemes)
library(plotly)
library(readr)
library(tidyverse)
library(lubridate)

################################################################################
# TRAVEL BANS
################################################################################

#library(XML)
#travel_bans <- xmlParse("TravelBansExport_20220913_151647.xml")
#travel_bans <- xmlToDataFrame("TravelBansExport_20220913_151647.xml")

library(jsonlite)

tb <- fromJSON("travel_bans_massi.json") %>% as.data.frame()
travel_bans <- read_csv("travel_bans_massi.csv")


################################################################################
# FINANCIAL SANCTIONS
################################################################################

File1_1 <- read_delim(
  "20220913-FULL-1_1.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

File1_0 <- read_delim(
  "20220913-FULL-1_0.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

################################################################################
# MERGED
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

merged_tb_fs <-
  full_join(File1_0_filtered,
            tb_filtered,
            by = c("Entity_logical_id...2" = "logicalId"), keep = TRUE)

unique_id <-
  merged_tb_fs %>% distinct(Entity_logical_id...2, .keep_all = TRUE)

unique_id_away_E <- unique_id %>% filter(Subject_type != "E")


################################################################################
#                          ENTERPRISES
################################################################################

enterprises <-
  File1_0_filtered %>% filter(Subject_type == "E") %>% distinct(Entity_logical_id...2, .keep_all = TRUE)

################################################################################
#                          INDIVIDUALS
################################################################################

individuals <-
  File1_0_filtered %>% 
  filter(Subject_type == "P") %>% 
  distinct(Entity_logical_id...2, .keep_all = TRUE)

individuals_tb <-
  tb_filtered %>% 
  filter(subjectType.classificationCode == "P") %>% 
  distinct(logicalId, .keep_all = TRUE)

all_individuals <-
  full_join(individuals,
            individuals_tb,
            by = c("Entity_logical_id...2" = "logicalId"), keep = TRUE)


################################################################################
#                          TIMELINE ENTITIES
################################################################################

File1_0_unique <- File1_0_filtered %>%
  distinct(Entity_logical_id...2, .keep_all = TRUE)

tb_unique <- tb_filtered %>% 
  distinct(logicalId, .keep_all = TRUE)

merged_alread_uniqued_tb_fs <-
  full_join(File1_0_unique,
            tb_unique,
            by = c("Entity_logical_id...2" = "logicalId"), keep = TRUE) 

merged_alread_uniqued_tb_fs %>% 
  ggplot(aes(x = Leba_publication_date, fill = Subject_type)) +
  geom_bar(stat = "count",
           position = "stack",
           width = 55) +
  theme_minimal() +
  ggtitle("Number of sanctioned (both tb and fs) enterprises and individuals on any given day")


merged_alread_uniqued_tb_fs %>% 
  mutate(year = year(Leba_publication_date)) %>% 
ggplot(aes(x = year, fill = Subject_type)) +
  geom_bar(stat = "count",
           position = "stack",
           width = 1) +
  theme_minimal() +
  ggtitle("Number of sanctioned (both tb and fs) enterprises and individuals on any given year")


merged_alread_uniqued_tb_fs %>% 
  mutate(semester = semester(Leba_publication_date, with_year = TRUE)) %>% 
  ggplot(aes(x = semester, fill = Subject_type)) +
  geom_bar(stat = "count",
           position = "stack",
           width = 0.5) +
  theme_minimal() +
  ggtitle("Number of sanctioned (both tb and fs) enterprises and individuals on any given semester")


tooltip_entity_year <- merged_alread_uniqued_tb_fs %>% 
  mutate(year = year(Leba_publication_date)) %>% 
  filter(year == 2004) %>% 
  count(Subject_type)

tooltip_type_year <- merged_alread_uniqued_tb_fs %>% 
  mutate(year = year(Leba_publication_date)) %>% 
  filter(year == 2004) %>% 
  mutate(type = case_when(
    is.na(Entity_logical_id...2) & !is.na(logicalId) ~ "travel ban",
    !is.na(Entity_logical_id...2) & is.na(logicalId) ~ "financial sanction",
    !is.na(Entity_logical_id...2) & !is.na(logicalId) ~ "both travel and financial sanction")) %>% 
  count(type)


################################################################################
#             SINGLE SANCTION COUNT (double count of individual)
################################################################################

tb_fs <- merged_alread_uniqued_tb_fs %>% 
  mutate(type = case_when(
    is.na(Entity_logical_id...2) & !is.na(logicalId) ~ "travel ban",
    !is.na(Entity_logical_id...2) & is.na(logicalId) ~ "financial sanction",
    !is.na(Entity_logical_id...2) & !is.na(logicalId) ~ "both travel and financial sanction")) 


tb_fs %>% 
  mutate(year = year(Leba_publication_date)) %>% 
  ggplot(aes(x = year, fill = type)) +
  geom_bar(stat = "count",
           position = "stack",
           width = 1) +
  theme_minimal() +
  ggtitle("Number of sanctioned enterprises and individuals on any given year", subtitle = 
            "Grouped by whether they have received one or both types of sanctions")



travel_filled <- individuals_tb %>% left_join(individuals, by = c("logicalId" = "Entity_logical_id...2"), keep = TRUE)


travel_plus_financial <-
  bind_rows(
    "financial_sanction" = individuals,
    "travel_ban" = travel_filled,
    "financial_sanction" = enterprises,
    .id = "id"
  )


#mutate(a = coalesce(count, value))   # replace na values with values from another column

travel_plus_financial %>% 
  mutate(year = year(Leba_publication_date)) %>% 
  ggplot(aes(x = year, fill = id)) +
  geom_bar(stat = "count",
           position = "stack",
           width = 1) +
  theme_minimal() +
  ggtitle("Number of sanctions on any given year", subtitle = "individuals who have received both a fs and a tb are counted twice")




################################################################################
#                          REGIMES CHART 
################################################################################

# NON CUMULATIVE
tb_fs %>% 
  mutate(year = year(Leba_publication_date)) %>% 
  ggplot(aes(x = year, fill = Programme)) +
  geom_bar(stat = "count",
           position = "stack",
           width = 1) +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Yearly number of sanctioned individuals/enterprises for each regime")


# CUMULATIVE
tb_fs %>% 
  mutate(year = year(Leba_publication_date)) %>%  
  group_by(Programme, year) %>% 
  count() %>% 
  ungroup() %>% 
  complete(Programme, year = 2002:2022, 
  fill = list(n = 0)) %>% 
  na.omit() %>% 
  group_by(Programme) %>% 
  mutate(cumsum=cumsum(n)) %>% 
  ggplot(aes(x=year, y=cumsum, fill=Programme, text=Programme)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Cumulative number of individuals/enterprises sanctioned in each regime") +
  #theme_ipsum() +
  scale_x_continuous(breaks = seq(2002, 2022, 2))


# CUMULATIVE BARCHART
tb_fs %>% 
  mutate(year = year(Leba_publication_date)) %>%  
  group_by(Programme, year) %>% 
  count() %>% 
  ungroup() %>% 
  complete(Programme, year = 2002:2022, 
           fill = list(n = 0)) %>% 
  na.omit() %>% 
  group_by(Programme) %>% 
  mutate(cumsum=cumsum(n)) %>% 
  ggplot(aes(x=year, y=cumsum, fill=Programme, text=Programme)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  #ggtitle("Cumulative number of individuals/enterprises sanctioned in each regime") +
  #theme_ipsum() +
  scale_x_continuous(breaks = seq(2002, 2022, 2))




################################################################################
#                          MONITOR REGIMES CHART 
################################################################################

tb_fs %>% 
  mutate(year = year(Leba_publication_date)) %>% 
  filter(year %in% (2006:2012)) %>% 
  count(Programme) %>% 
  ggplot(aes(x = reorder(Programme, -n), y = n)) +
  geom_bar(stat = "identity",
           width = 1) +
  theme_minimal() +
  xlab("") +
  ylab("numero di nuove sanzioni") +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Ranking numero di sanzioni per regime per range di anni specifico (qua dal 2006 al 2012)")

tb_fs %>% 
  mutate(year = year(Leba_publication_date)) %>% 
  filter(year == 2022) %>% 
  count(Programme) %>% 
  ggplot(aes(x = reorder(Programme, -n), y = n)) +
  geom_bar(stat = "identity",
           width = 1) +
  theme_minimal() +
  xlab("") +
  ylab("numero di nuove sanzioni") +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Ranking numero di sanzioni per regime per anni specifici (qua 2022)")

tooltip_ukr_2022 <- tb_fs %>% 
  mutate(year = year(Leba_publication_date)) %>% 
  filter(year == 2022 & Programme == "UKR") %>% 
  count(Subject_type)


# monitor regimes chart relative % increase and timeline
regime_change <- tb_fs %>% 
  group_by(Programme) %>% 
  mutate(tot_sanctions = n()) %>% 
  group_by(Leba_publication_date, .add = TRUE) %>% 
  mutate(date_count = n()) %>% 
  group_by(Programme) %>% 
  filter(Leba_publication_date == max(Leba_publication_date)) %>% 
  distinct(Programme, .keep_all = TRUE) %>% 
  mutate(perc_increase = ((tot_sanctions - (tot_sanctions - date_count)) / (tot_sanctions - date_count)) * 100) %>% 
  filter(perc_increase != Inf)

library(scales)
  
ggplot(regime_change, aes(x = Leba_publication_date, y = perc_increase)) +
  geom_text(aes(label=Programme)) +
  scale_x_date(limits = as.Date(c("2020-01-01", "2022-09-20"))) +
  scale_y_continuous(limits = c(0, 101)) +
  theme_minimal() +
  xlab("")+
  ylab("Percent increase") +
  ggtitle("% increase of regime sizes at the latest release of new sanctions")


ggplot(regime_change, aes(x = Leba_publication_date, y = perc_increase)) +
  geom_text(aes(label=Programme)) +
  scale_x_date(limits = as.Date(c("2021-07-01", "2022-09-20"))) +
  scale_y_continuous(limits = c(0, 101)) +
  theme_minimal() +
  xlab("")+
  ylab("Percent increase") +
  ggtitle("% increase of regime sizes at the latest release of new sanctions (2021 S2 - today)")


################################################################################
#                       TREEMAP ENTITY TYPE
################################################################################


# TREEMAP ENTITY TYPE
sum_e <- nrow(enterprises)
sum_i <- nrow(all_individuals)
Subject_type <- c("enterprises", "individuals")
n <- c(sum_e, sum_i)
treemap_entities <- data.frame(Subject_type, n)
treemap_entities$perc <- round(n / sum(n), 2)

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
#                       TREEMAP PROGRAMME
################################################################################

#install.packages("treemapify")
library(treemapify)


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


tooltip_treemap_country <- tb_fs %>% 
  filter(citizenship.countryDescription == "RUSSIAN FEDERATION") %>% 
  count(Programme)


################################################################################
#                 LATEST SANCTIONED INDIVIDUALS / ENTERPRISES
################################################################################


enterprises_latest <- enterprises %>% arrange(desc(Leba_publication_date)) %>% select(
  Naal_wholename, Leba_publication_date, Programme
) %>% slice(1:10)

knitr::kable(enterprises_latest, "simple")


tooltip_latest_enterprise <- enterprises %>% 
  arrange(desc(Leba_publication_date)) %>% slice(1)


individuals_latest <- tb_fs %>% arrange(desc(Leba_publication_date)) %>% select(
  Naal_wholename, type, Leba_publication_date, Programme, citizenship.countryDescription, Subject_type
) %>% filter(Subject_type == "P") %>% slice(1:10) %>% select(-Subject_type)

knitr::kable(individuals_latest, "simple")

tooltip_latest_individuals <- tb_fs %>% filter(Subject_type == "P") %>% 
  arrange(desc(Leba_publication_date)) %>% slice(1)


################################################################################
#                 SANKEY DIAGRAM PROGRAMME COMPOSITION
################################################################################

sankey_data <- tb_fs %>% 
  count(Programme, Subject_type, type) 

sankey <- data.frame (entity  = c("individuals", "enterprise"),
                  tot = c(2933, 648),
                  tb = c(2722, 0),
                  fs = c(2883,648),
                  ind_tb_only = c(50, 0),
                  ind_fs_only = c(211, 0),
                  ind_both = c(2672, 0)
)

write.csv(sankey, "sankey.csv")


################################################################################
#                 AREA CHART
################################################################################

growth <- tb_fs %>% 
  count(Programme, Leba_publication_date) %>% 
  add_count(Programme, name = "Programme_n")

growth_rate <- growth %>%
  group_by(Programme) %>% 
  arrange(Leba_publication_date) %>%
  mutate(Diff_date = as.numeric(Leba_publication_date - lag(Leba_publication_date)),
         Diff_date = replace_na(Diff_date, 0),
#,  # Difference in time (just in case there are gaps)
         Diff_growth = n - lag(n), # Difference in route between years
         Rate_percent = (Diff_growth / Diff_date)/lag(n * 100),
         Diff_growth = replace_na(Diff_growth, 0),
         Rate_percent = replace_na(Rate_percent, 0)) %>% 
  filter(!is.na(Programme)) %>% 
  group_by(Programme) %>% 
  mutate(tot_days = n()) %>% 
  filter(tot_days != 1) %>% 
  filter(Programme != "UKR")


library(ggridges)
library(scales)

growth$Leba_publication_date <- as.Date(growth$Leba_publication_date)

ggplot(growth_rate, aes(Diff_date, Programme, height = Rate_percent, group = Programme, fill = Programme)) + 
  geom_density_ridges(stat = "identity", scale = 5) +
  theme_minimal() + 
  xlab("number of days since first sanction")+
  ylab("")+
  ggtitle("Growth rate of regime programme over time")


growth %>% 
  filter(!is.na(Programme)) %>% 
ggplot(aes(Leba_publication_date, Programme)) +
  geom_density_ridges(rel_min_height = 0.01, scale = 3, alpha = 0.7)+
  geom_point(data = filter(growth, Programme_n <= 2)) +
  xlab("")+
  ylab("") +
  scale_x_date(breaks = date_breaks("4 years"), labels = date_format("%Y")) +
  ggtitle("Density plots of regimes: the area under the curve is 100% of the regime") +
  theme_minimal() 

# growth %>% 
#   filter(!is.na(Programme)) %>% 
#   ggplot(aes(Leba_publication_date, Programme)) +
#   geom_density_ridges(rel_min_height = 0.001, jittered_points = TRUE,
#                       position = position_points_jitter(width = 0.5, height = 0),
#                       point_shape = "|", point_size = 2,
#                       alpha = 0.7) +
#   xlab("")+
#   ylab("") +
#   scale_x_date(breaks = date_breaks("4 years"), labels = date_format("%Y")) +
#   ggtitle("Density plots of regimes: the area under the curve is 100% of the regime") +
#   theme_minimal() 
# 
#   
# growth %>% 
#   filter(!is.na(Programme)) %>% 
#   ggplot(aes(Leba_publication_date, Programme, height = ..density..)) +
#   geom_density_ridges(stat = "density", trim = TRUE, scale = 10) +
#   # geom_density_ridges(stat = "binline", binwidth=90,
#   #                     draw_baseline = F) +
#   xlab("")+
#   ylab("") +
#   scale_x_date(breaks = date_breaks("4 years"), labels = date_format("%Y")) +
#   ggtitle("The area under the sum of the bins in each regime is 100% of the regime sanctions") +
#   theme_minimal() 
