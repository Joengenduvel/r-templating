#
# INBO Natuurindicatoren
# collect and tidy data for vlinder index
#
# author Van Hoey S.
#

library(readxl)
library(dplyr)
library(curl)
library(reshape2)

data.url <- "https://inbo-website-prd-532750756126.s3-eu-west-1.amazonaws.com/s3fs-public/bestanden/natuurindicatoren/Graslandvlinderindex_data2016_web.xlsx"

# download the excel file as temporary file and store as df
df <- data.url %>%
        curl_download(tempfile(fileext = ".xlsx")) %>%
        read_excel()

# remove rows with only NA values (or just a single value)
df <- df[rowSums(is.na(df)) < dim(df)[2] - 1,]
# remove redundant year and soort/year rows
# Hooibeestje and Argusvlinder are duplicated in the rows
df <- filter(df, !Soort %in% c('Soort', 'Jaar',
                               'Hooibeestje', 'Argusvlinder'))
# make tidy
tidy.df <- melt(df, id.vars = "Soort",
           value.name = "grasland_index",
           variable.name = "year")
# remove NA values
tidy.df <- na.omit(tidy.df)
# make years numerical values
tidy.df$year <- as.numeric(as.character(tidy.df$year))

# save as CSV in processed folder
write.csv(tidy.df,
          file = "../data/processed/europese-vlinderindex-graslanden.csv",
          row.names = FALSE)

