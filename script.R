source("input_questions.R")
source("custom_functions.R")

library(rmarkdown)
library(knitr)
library(flextable) 
library(dplyr)
library(kableExtra) 

#performed_by = run_by_question()

#run_date = run_date_question()

#run_no = run_no_question()

runs <- data.frame(matrix(nrow = 0, ncol = 1))

colnames(runs) <- c("Run Type")

analysis_type = analysis_question()

#run_type = run_type_question()

file_path = folder_chooser()

render_template1(file_path)

#render("./main.Rmd", params = list(), output_file = "1.pdf") 
