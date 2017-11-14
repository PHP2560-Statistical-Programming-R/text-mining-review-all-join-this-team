unlink("data", recursive = TRUE) # where data is stored after clean
unlink("olivia_results",recursive=TRUE)
unlink("graph", recursive = TRUE) # where graphs are stores
unlink("linde_graphs", recursive = TRUE) # where graphs are stores
unlink("anna_graphs", recursive = TRUE) # where graphs are stores

dir.create(file.path("data"), showWarnings = FALSE)
dir.create(file.path("olivia_results"), showWarnings = FALSE)
dir.create(file.path("graph"), showWarnings = FALSE)
dir.create(file.path("linde_graphs"), showWarnings = FALSE)
dir.create(file.path("anna_graphs"), showWarnings = FALSE)

## run all scripts
source("get_data.R")
source("clean_data.R")
source("olivia_analysis.R")
source("linde-analysis.R")
source("olivia_vis.R")
source("anna-analyzeData.R")
source("brian_graph.R")
rmarkdown::render("olivia_paper.Rmd", output_format = "html_document")
rmarkdown::render("linde_paper.Rmd", output_format = "html_document")
rmarkdown::render("anna_paper.Rmd", output_format = "html_document")
rmarkdown::render("brian_paper.Rmd", output_format = "html_document")

