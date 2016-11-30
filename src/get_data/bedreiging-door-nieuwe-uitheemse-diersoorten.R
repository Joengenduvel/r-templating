#
# INBO Natuurindicatoren
# collect and tidy data for number of invasive species in Flanders
#
# author Van Hoey S.
#

library(zoo)
library(plyr)
library(dplyr)
library(tidyr)

###
# The aim is to start from the final data product described and provided by
# the species checklist: https://github.com/inbo/alien-species-checklist

# + vertaling ! bij pathways
###

# Get the aggregated checklist from github: master data file in alien-species-checklist repository
checklist_location <- "https://raw.githubusercontent.com/inbo/alien-species-checklist/master/data/processed/aggregated-checklist.tsv"
checklist <- read.csv(checklist_location, sep = "\t")

## PATHWAY FIGURES
## -----------------

# The aggregated checklist combines different pathways to a single species. In
# order to count species for each pathway, these need to be extracted to multiple
# rows (each |)
checklist_splitted <- separate_rows(checklist, introductionPathway, sep = "\\|")
# trim the trailing and starting white spaces
checklist_splitted$introductionPathway <- sapply(checklist_splitted$introductionPathway,
                                                 stringr::str_trim)

# create a category and subcategory column based on the split
checklist_pathway <- checklist_splitted %>%
                        separate(introductionPathway, c("category",
                                                        "subcategory"),
                        sep = ">", remove = FALSE, fill = "right") %>%
                        mutate_at(c("category", "subcategory"),
                                  stringr::str_trim) %>%
                        replace_na(list(subcategory = "unknown"))

# We could actually save as such and make table plots based this data (geom_bar). However,
# currently is opted to process up to the level close to the figure itself
checklist_pathway$kingdom <- as.character(checklist_pathway$kingdom)
pathway_count <- checklist_pathway  %>%
                    filter(kingdom %in% c('Plantae', 'Animalia')) %>%
                    count(vars = c("introductionPathway", "category",
                                   "subcategory", "kingdom"))

# hardcoding the translation  of categories as additional column
pathway_count$category_nl <- revalue(pathway_count$category,
                                    c("contaminant" = "TRANSPORTBESMETTING",
                                      "corridor" = "CORRIDOR",
                                      "escape" = "ONTSNAPPING",
                                      "release" = "UITZETTEN IN DE NATUUR",
                                      "stowaway" = "TRANSPORT VERSTEKELING",
                                      "to be determined by experts" = "TE BEPALEN DOOR EXPERTEN",
                                      "unaided" = "AUTONOME UITBREIDING",
                                      "unknown" = "ONBEKEND"))

## DUTCH TRANSLATION OF PATHWAY -> if subcategories needed as well
#pathway_en_nl <- read.csv("../data/vocabularies/pathways_CBD_en_nl.csv",
#                          sep = "\t")

write.csv(pathway_count,
          file = "../data/processed/bedreiging-door-nieuwe-uitheemse-diersoorten-1.csv",
          row.names = FALSE)

## CUMULATIVE COUNT OVER YEARS
## ----------------------------
# startend from aggregated, code overnemen

# ATTENTION! we consider the years provided in the "firstObservationYearBE"
# column to be validated, correct and in proper format

# some temporary fixes could eventually be introduced here as showcased on
# https://github.com/inbo/alien-species-checklist/blob/master/notebooks/0-stijnvanhoey-viz-pathways.ipynb
# however, this is should be tackled by Adriaens T.
# for now, coercing whatever can be interpreted well:
checklist$firstObservationYearBE <- as.numeric(as.character(checklist$firstObservationYearBE))
# only use values with year
checklist <- checklist[!is.na(checklist$firstObservationYearBE),]


habitats = c("freshwater", "marine", "estuarine", "terrestrial")

# Filter the habitat descriptions on the different habitat short names
for (habitat in habitats) {
    checklist[habitat] <- stringr::str_extract(checklist$habitat,
                                               habitat)
}

# drop the rows without habitat info (for at least any one there is habitat defined)
checklist <- checklist[rowSums(is.na(checklist[habitats])) < 4,]

# Create the period sections
bin_years <- 15
checklist$period_sections <- cut(checklist$firstObservationYearBE,
                                 seq(1800, 2030, bin_years),
                                 include.lowest = TRUE, ordered_result = TRUE)

# years earlier to given
min_year <- 1800
checklist[checklist$firstObservationYearBE < min_year,
          "firstObservationYearBE"] <- min_year

# Combine the habitat columns to a single variable column
checklist <- checklist %>% gather(habitat_short, habitat_name,
                                  c(freshwater, marine,
                                    estuarine, terrestrial)) %>%
    select(-habitat_short) %>%
    drop_na()

# Sort on the year values
checklist <- checklist[order(checklist$firstObservationYearBE), ]

# combine the period section for every 15 years HIEERDOOR TEVEEL!!
checklist$period_count <- ave(rep(1, nrow(checklist)),
                              checklist$period_sections,
                              checklist$habitat_name,
                              FUN = sum)

# count number of species in each habitat-period combination
checklist$habitat_name <- factor(checklist$habitat_name)
checklist_count <- plyr::count(checklist, c("habitat_name", "period_sections"))

# create cumulative counts per factor
checklist_count$cumcount <- ave(checklist_count$freq,
                                checklist_count$habitat_name,
                                FUN = cumsum)

# Fill in the remaining habitat-period combination
# strategy: combine with a dataframe with all combinations and fill in
# empty spots with continuous value from last known
levels(checklist_count$period_sections) <- seq(min_year, 2020, bin_years)
checklist_count$habitat_name <- factor(checklist_count$habitat_name)

all_options <- with(checklist_count,
                    expand.grid(period_sections = levels(checklist_count$period_sections),
                                habitat_name = levels(habitat_name)))
all_options <- all_options[order(all_options$period_sections),]

checklist_count_all <- merge(all_options, checklist_count,
                             by = c("habitat_name", "period_sections"),
                             all.x = TRUE)

checklist_count_all <- ddply(checklist_count_all, .(habitat_name), na.locf)
checklist_count_all$cumcount <- as.numeric(checklist_count_all$cumcount)
checklist_count_all$period_sections <- as.numeric(checklist_count_all$period_sections)

checklist_count_all$habitat_name_nl <- revalue(checklist_count_all$habitat_name,
                                              c("estuarine" = "estuarien",
                                                "freshwater" = "zoetwater",
                                                "marine" = "marien",
                                                "terrestrial" = "terrestrisch"))
write.csv(checklist_count_all,
          file = "../data/processed/bedreiging-door-nieuwe-uitheemse-diersoorten-2.csv",
          row.names = FALSE)
