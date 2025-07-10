source("input_questions.R")

library(rmarkdown)
library(knitr)


#performed_by = run_by_question()

#run_date = run_date_question()

#run_no = run_no_question()

analysis_type = analysis_question()

#run_type = run_type_question()

file_path = folder_chooser()

path = sub("Run Information.*", "", file_path)

figure_path <- paste(path, "Amplification.png", sep="")
quantification_result_path = paste(path, " Quantification Cq Results.csv", sep="")



quantification_result = read.csv(quantification_result_path, sep=",", header=TRUE)

quantification_result = quantification_result[ , -c(1, 3, 8, 9, 10, 11, 12, 13, 14, 15, 16)]

print(quantification_result)

render("./main.Rmd", params = list(), output_file = "1.pdf") 
