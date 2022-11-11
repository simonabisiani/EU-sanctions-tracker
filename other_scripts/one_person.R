> one_person <- all_individuals[360,]
> one_person_and_related <- left_join(one_person, all_individuals, by = "Leba_numtitle")
> View(one_person_and_related)
> one_person_and_related <- all_individuals %>% filter(Leba_numtitle == one_person$Leba_numtitle)
> View(one_person_and_related)
> write.csv(one_person_and_related, "one_person_and_related.csv")