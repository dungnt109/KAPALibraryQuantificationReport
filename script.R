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

file_path1 = "/home/dungnt/Documents/Repository/QuantificationReport/Std Curve and Samples/2024-07-25; Lot 0000649535_20250701 - KAPA - Run_01-04 - Set_3 - Sample - Run Information.csv"
file_path3 = "/home/dungnt/Documents/Repository/QuantificationReport/Std Curve and Samples/2024-07-25; Lot 0000649535_20250701 - KAPA - Run_01-04 - Set_3 - Sample - Run Information.csv"
 


#render_template3(file_path3)

render("./main.Rmd", params = list(), output_file = "1.pdf") 
