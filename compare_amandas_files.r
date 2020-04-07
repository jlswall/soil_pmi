library("tidyverse")
library("readxl")

## Read in raw file from Amanda. It contains taxa at all levels (order, class,
## family, etc.)
allDataFileNm <- "orig_data_files/original_all_taxa.csv"
rawAllT <- read_tsv(file=allDataFileNm)
subsetT <- rawAllT %>% filter(taxlevel==5)
rm(allDataFileNm)


## Now, compare with the file which Amanda says she has been using.
preferFileNm <- "orig_data_files/amanda_preferred_format.xlsx"
preferT <- read_excel(preferFileNm)
rm(preferFileNm)


## It looks like Amanda's preferred file contains the same info as the
## original file (just in an Excel format and with different order of
## rows/columns).
all.equal(rawAllT, preferT)
all_equal(rawAllT, preferT, ignore_row_order=FALSE)
all_equal(rawAllT, preferT, ignore_col_order=FALSE)
