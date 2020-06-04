library("tidyverse")

taxaT  <- read_csv("families_massaged.csv")
## Put the subject number in another column.
taxaT$subj <- substring(taxaT$sampleName, first=1, last=2)


## Find the most populous taxa, aggregated over individual subjects and days.
mostCommonTaxa  <- taxaT %>%
  group_by(taxon, distance) %>%
  summarize(avgFracByDay=mean(fracBySample)) %>%
  group_by(distance) %>%
  arrange(desc(avgFracByDay)) %>%
  top_n(n=5) %>%
  ungroup() %>%
  distinct(taxon) %>%
  pull(taxon)

ggplot(taxaT %>% filter(taxon %in% mostCommonTaxa),
       aes(x=degdays, y=fracBySample, color=distance)) +
  geom_point() +
  facet_wrap(~taxon, scale="free_y")
