source("input_questions.R")

library(rmarkdown)


#performed_by = run_by_question()

#run_date = run_date_question()

#run_no = run_no_question()

analysis_type = analysis_question()

#run_type = run_type_question()

file_path = folder_chooser()

path = sub("Run Information.*", "", file_path) 

print(path)

#cat(folder) 



render("./main.Rmd", params = list(), output_file = "1.pdf") 
