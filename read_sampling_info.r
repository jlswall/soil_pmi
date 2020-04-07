library("tidyverse")
library("readxl")
library("stringr")


## ##################################################
## Read information about collections, ADD, dates, etc. from
## spreadsheet in "orig_data_files/SampleInformation.xlsx".

## Skip the header in row 1 and use our own column names.
fileNm <- "orig_data_files/SampleInformation.xlsx"
samplingT <- read_excel(fileNm, skip=1,
                        col_names=c("sampleName", "collectionDay",
                                    "degdays", "distance") 
                                    )
rm(fileNm)

## The last row just contains the word "Negative".  We remove that row.
samplingT <- samplingT %>% filter(sampleName!="Negative")
## ##################################################


## ##################################################
## Ideally, we would have 6 samples at each distance (0m, 3m) for each
## of 8 collection day, resulting in 6*2*8=96 samples.

## Count the number of such samples for each collection day.
samplingT %>% group_by(collectionDay) %>% summarize(nObs=n())
## Count the number of such samples for each collection day and distance.
samplingT %>% group_by(collectionDay) %>% summarize(nObs=n())

## If we had the full number of samples for each collection day and
## distance, the counts would look like this table.
fullObsT <- as_tibble( expand.grid(
    collectionDay=unique(samplingT$collectionDay),
    distance=c("0m", "3m"),
    stringsAsFactors=FALSE)
    )
fullObsT$fullObsn <- 6

## Identify count how many samples we have for each distance and each
## collection day.  In the next step, we join this with fullObsT and
## compare to see which samples are missing.
samplingT %>%
  group_by(collectionDay, distance) %>%
  summarize(nObs=n())


fullObsT %>%
  left_join(samplingT %>%
            group_by(collectionDay, distance) %>%
            summarize(nObs=n())
            ) %>%
  filter((nObs < fullObsn) | is.na(nObs)) %>%
  select(-fullObsn)
## This table shows which collections have missing scapula or rib, along with how many observations there are.
## T1            0m           5
## ##################################################


## ##################################################
## Write out the sample information into a CSV file.
write.csv(samplingT, file="sampling_info.csv", row.names=F)
## ##################################################
