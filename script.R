source("input_questions.R")

library(rmarkdown)
library(knitr)
library(flextable) 
library(dplyr)
library(kableExtra) 

#performed_by = run_by_question()

#run_date = run_date_question()

#run_no = run_no_question()

analysis_type = analysis_question()

#run_type = run_type_question()

file_path = folder_chooser()

path = sub("Run Information.*", "", file_path)

figure_path <- paste(path, "Amplification.png", sep="")


standard_curve_path = paste(path, " Standard Curve Results.csv", sep="")

standard_curve_result = read.csv(standard_curve_path, sep=",", header=TRUE)




render("./main.Rmd", params = list(), output_file = "1.pdf") 
