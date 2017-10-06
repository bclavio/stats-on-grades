## Packages
library(knitr)
library(rmarkdown)

setwd('C:/Users/BiancaClavio/Documents/stats-on-grades/examples')


## Data
personalized_info <- read.csv(file = "meeting_times.csv")

## Loop
for (i in 1:nrow(personalized_info)){
  rmarkdown::render(input = "mailMerge-test.Rmd",
                    output_format = "pdf_document",
                    output_file = paste("handout_", i, ".pdf", sep=''),
                    output_dir = "handouts/")
}