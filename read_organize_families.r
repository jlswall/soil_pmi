library("tidyverse")
library("readxl")
library("stringr")


## ##################################################
## Read data from level 5 (family) taxonomic level.
fileNm <- "orig_data_files/amanda_preferred_format.xlsx"
rawAllT <- read_excel(path=fileNm, sheet="Level 5")
rm(fileNm)

## Check that the "total" column is the sum of all the following columns.
all.equal(rawAllT$total, apply(rawAllT[,6:ncol(rawAllT)], 1, sum))
## So, we remove the total column.
rawAllT <- rawAllT %>% select(-total)

## Remove the "Negative column" (last column).
rawAllT <- rawAllT %>% select(-Negative)

## For all observations, the value of "taxlevel" is 5 (denoting
## family-level), so we remove this column.  We also remove "rankID"
## and "daughterlevels", since we aren't using them.
rawAllT <- rawAllT %>% select(-taxlevel, -rankID, -daughterlevels)
## ##################################################



## ##################################################
## Concentrate on the taxa counts for the individual cadavers on the
## various collection days.  Transfer from wide format to long format.

## Go from wide to long format.
rawIndivT <- rawAllT %>%
  gather(sampleName, counts, -taxon)
## ##################################################



## ##################################################
## Join with sampling information to get information about accumulated
## degree days.

## Read in sampling information which I've already processed.
samplingT <- read_csv("sampling_info.csv")

## Merge the taxa counts with the sampling information. Note:
## Collection day was removed, since I don't think we'll use it.
indivT <- rawIndivT %>%
  left_join(samplingT) %>%
  select(-collectionDay)
rm(rawIndivT, rawAllT, samplingT)

## Separate out the subject identifier from the sampleName.
indivT$subj <- substring(indivT$sampleName, first=1, last=2)
## ##################################################



## ##################################################
## Count the number of unique family-level taxa observed for the
## 2 distances (0m, 3m).
indivT %>% filter(counts>0) %>% group_by(distance) %>% distinct(taxon) %>% summarize(n=n())
##   distance     n
##   <chr>    <int>
## 1 0m         198
## 2 3m         229
## ##################################################




## ##################################################
## The eukaryote data includes many taxa that couldn't be classified
## down to the family level.  So, we consider the percentage of each
## sample that is made up of each taxon (no matter the classification
## level).  Then, all taxa that do not meet a certain criteria will be
## grouped together into a "Rare" category.  The taxa which do meet
## the criteria will be considered in the random forest model.

## To calculate percentages, we need total counts for each sample.
## We find that the total for each sample is the same (5287).
ctBySampleT <- indivT %>%
  group_by(sampleName) %>%
  summarize(totals=sum(counts))


## #########################
## Some taxa don't occur frequently.  It's hard to make a hard cutoff
## for what constitutes "frequently", but many of the taxa make up
## less than 1% of the counts for any sample.

## I'm going to set the frequency cutoff at 1% (0.01).  This means
## that in order to meet the cutoff, a specific family-level taxa must
## make up at least 1% of the total family-level counts for that
## sample.
freqCutoff <- 0.01

## ## Get list of maximum taxa percentages for the two distances (0m,
## ## 3m) sorted in descending order:
## indivT %>%
##   left_join(ctBySampleT) %>%
##   mutate(fracBySampleName = counts/totals) %>%
##   group_by(distance, taxon) %>%
##   summarize(maxFracBySampleName = max(fracBySampleName)) %>%
##   ## filter(maxFracBySampleName >= freqCutoff) %>%
##   arrange(distance, desc(maxFracBySampleName)) %>%
##   print(n = Inf)


## Count how many times each taxa beats the cutoff (freqCutoff) for
## each distance.  Then, keep only those which meet cutoff for more
## than 1 sample.  Save those taxa names for each distance.
freqTaxaByDistanceT <- indivT %>%
  left_join(ctBySampleT) %>%
  mutate(fracBySampleName = counts/totals,
         isExceed=(fracBySampleName>=freqCutoff)) %>%
  group_by(taxon, distance) %>%
  summarize(numExceed = sum(isExceed)) %>%
  filter(numExceed > 1) %>%
  ## arrange(distance, desc(numExceed)) %>%
  select(taxon, distance)


## For each distance, how many taxa meet the cutoff?
freqTaxaByDistanceT %>% group_by(distance) %>% summarize(n=n())

## Save list of frequent taxa by distance to a CSV file.
write.csv(freqTaxaByDistanceT, file="family_freq_taxa_by_distance.csv",
          row.names=F)
## #########################
## ##################################################



## ##################################################
## Rename taxa that occur less than the frequency cutoff allows as
## "rare".  Then, sum all these "rare" taxa into one row for each
## sample.
commontaxaT <- indivT
## Find rows which contain taxa which do not meet the frequency cutoff
## for that type.
indicRare <- !( with(commontaxaT, paste(taxon, type, sep=":")) %in% with(freqTaxaByDistanceT, paste(taxon, type, sep=":")) )
## For these "rare" taxa, we change taxon name to rare.
commontaxaT[indicRare, "taxon"] <- "Rare"
## Now, we add up the rare counts so that "Rare" appears only one for
## each sample.
commontaxaT <- commontaxaT %>%
  group_by(sampleName, taxon, type, degdays) %>%
  summarize(counts = sum(counts))


## Use the table of total counts by subj/day to find the fraction
## represented by each taxa for each subj/day.
commontaxaT <- commontaxaT %>%
  left_join(ctBySampleT) %>%
  mutate(fracBySample=counts/totals) %>%
  select(-totals)


## Check that the fractions add up to 1, appropriately.
unique(
    commontaxaT %>%
    group_by(sampleName) %>%
    summarize(sumFracBySample = sum(fracBySample)) %>%
    pull(sumFracBySample)
)


## Remove the list of taxa names that satisfied the frequency cutoff.
rm(indicRare, freqCutoff, freqTaxaByTypeT)
## ##################################################



## ##################################################
## Save the tibble to a file for use in separate code
## for graphing and analysis.

write.csv(commontaxaT, file="families_massaged.csv", row.names=FALSE)
## ##################################################
