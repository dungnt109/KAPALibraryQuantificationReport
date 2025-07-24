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

summary <- data.frame(matrix(nrow = 0, ncol = 8))



analysis_type = analysis_question()

#run_type = run_type_question()

file_path1 = "/home/dungnt/Documents/Repository/QuantificationReport/Sample results folder/2024-07-25; Lot 0000649535_20250701 - ALB MNC 0624 Std Curve, 4mM - Run Information.csv"
file_path3 = folder_chooser()


render_template1(file_path1)

#render("./main.Rmd", params = list(), output_file = "1.pdf") 
