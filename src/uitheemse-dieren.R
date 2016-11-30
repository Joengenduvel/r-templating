library(ggplot2)
library(dplyr)

script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

df_pathway <- read.csv("../data/processed/bedreiging-door-nieuwe-uitheemse-diersoorten-1.csv")
df_count <- read.csv("../data/processed/bedreiging-door-nieuwe-uitheemse-diersoorten-2.csv")

# function to automatically create multiliners for x-axis,
# splitting on the first space after the center
label.splitter <- function(label, maxlength){
  label <- as.character(label)
  if (nchar(label) > maxlength) {
    # find space closest to center of string
    split.space <- nchar(label) %/% 2
    regex.statement <- paste("(^.{", split.space, "})(\\S*)([ ])", sep = "")
    label <- gsub(regex.statement, "\\1\\2\n", label)
  }
  return(label)
}


# preprocessing

df_pathway <- mutate_at(df_pathway, .cols = c("category"), .funs = toupper)

# subcategory add line splits
max.length.allowed <- 30
df_pathway$subcategory <- as.factor(sapply(df_pathway$subcategory,
                                           FUN = label.splitter,
                                           max.length.allowed))
# category add line splits
max.length.allowed <- 20
df_pathway$category <- as.factor(sapply(df_pathway$category,
                                        FUN = label.splitter,
                                        max.length.allowed))

df_pathway$category_nl <- as.factor(sapply(df_pathway$category_nl,
                                           FUN = label.splitter,
                                           max.length.allowed))


#subcategories

subcategoriesIntegrated <- ggplot(df_pathway, aes(x = reorder(subcategory, -freq),
                                                  y = freq, fill = kingdom)) +
  geom_bar(stat = "identity", width = .65) +
  ylab("Number of species") +
  facet_grid(. ~ category, scales = "free_x",
             space = "free_x") +
  #theme_inbo2015(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,
                               vjust = 0.2, size = 10),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12, angle = 90,
                               hjust = 0.5),
    axis.title.y = element_text(size = 14),
    strip.text.x = element_text(angle = 90, size = 12),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.text = element_text(angle = 90, size = 14),
    legend.title = element_blank(),
    legend.text.align = -.6
  )

subcategoriesIntegratedEnglish <- ggplot(df_pathway,  aes(x = reorder(category, freq, function(x){sum(x)}),
                                                          y = freq, fill = kingdom)) +
  geom_bar(stat = "summary", fun.y = sum,
           width = .65) + 
  xlab("Pathway") +
  ylab("Number of species") + 
  coord_flip() +
  theme(legend.position = "top",
        legend.title = element_blank()
  )

getCorrectLanguageList <- function(list, value) {
  for (i in 1:length(list)) {
    if(list[[i]]$name == value)
       list[[i]]
  }
}


tranlation <- function(source, language) {
  test <- c(2,"hop")
  english <- list(name="english", words=  c("estuarine", "freshwater","marine","terrestrial"))
  dutch <- list(name="dutch", words = c("estuarinen", "proper water", "mineralen", "hopsa"))
  languages <- list(english, dutch)
  result <- lapply(source,function(el) { getCorrectLanguageList(languages, language)$words[[which(english == el)]]  })
  result
}

cumulativeSpeciesTranslated <- function(translate, language) {
  ggplot(data = df_count, aes(x = period_sections, 
                              y = cumcount)) +
    geom_bar(stat = "identity") +
    #theme_inbo2015(base_size = 14) +
    facet_grid(translate(habitat_name, language) ~ .) +
    scale_x_discrete(labels = seq(1800, 2030, 15)) +
    xlab("") +
    ylab("Number of alien species") +
    theme(strip.text.y = element_text(size = 16)) +
    scale_x_continuous(breaks = seq(1800, 2010, 50))
}

cumulativeSpeciesEnglish <- ggplot(data = df_count, aes(x = period_sections, 
                              y = cumcount)) +
    geom_bar(stat = "identity") +
    #theme_inbo2015(base_size = 14) +
    facet_grid(habitat_name ~ .) +
    scale_x_discrete(labels = seq(1800, 2030, 15)) +
    xlab("") +
    ylab("Number of alien species") +
    theme(strip.text.y = element_text(size = 16)) +
    scale_x_continuous(breaks = seq(1800, 2010, 50))


cumulativeSpecies <- ggplot(data = df_count, aes(x = period_sections, 
                                                 y = cumcount)) +
  geom_bar(stat = "identity") +
  #theme_inbo2015(base_size = 14) +
  facet_grid(habitat_name_nl ~ .) +
  scale_x_discrete(labels = seq(1800, 2030, 15)) +
  xlab("") +
  ylab("Aantal uitheemse diersoorten") +
  theme(strip.text.y = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(1800, 2010, 50))

list1 <- list(name = "Subcategories integrated", language.dutch = function() { subcategoriesIntegrated }, language.english = function() { subcategoriesIntegratedEnglish})
list2 <- list(name = "Cumulative Species", language.dutch = function() { cumulativeSpecies }, language.english = function() { cumulativeSpeciesEnglish})
#list2 <- list(name = "Cumulative Species", language.dutch = function() { cumulativeSpeciesTranslated(tranlation, "english") }, language.english = function() {})
data_information <- list(list1, list2)
rmarkdown::render("template.Rmd", output_file = "../html-files/bedreiging-door-nieuwe-uitheemse-diersoorten.html")
