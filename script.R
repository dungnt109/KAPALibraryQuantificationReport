source("input_questions.R")
source("custom_functions.R")

library(rmarkdown)
library(knitr)
library(flextable) 
library(dplyr)
library(kableExtra) 

performed_by = run_by_question()

run_date = run_date_question()

run_no = run_no_question()

summary <- data.frame(matrix(nrow = 0, ncol = 8))
runs <- data.frame(matrix(nrow = 0, ncol = 6)) 
index <- 0 

import_run_index <- 0 

analysis_type <- analysis_question()

while (TRUE) {

	run_type <- run_type_question()


	if (run_type == "Standard Curve (standards only)"){
		file_path  <- file.choose(new = FALSE)
		#file_path = "/home/dungnt/Documents/Repository/QuantificationReport/Std Curve and Samples/2024-07-25; Lot 0000649535_20250701 - KAPA - Run_01-04 - Set_3 - Sample - Run Information.csv"
		threshold <- threshold_question()

		index = index + 1

		runs <- rbind(runs, data.frame(Run = run_type, Threshold = threshold, Folder = file_path, Reference = NA, Index = index, ImportIndex = NA ))

	} else if (run_type == "Standard Curve (with samples)"){

		#file_path = "/home/dungnt/Documents/Repository/QuantificationReport/Std Curve and Samples/2024-07-25; Lot 0000649535_20250701 - KAPA - Run_01-04 - Set_3 - Sample - Run Information.csv"
		#threshold <- threshold_question()

		#index = index + 1

		#runs <- rbind(runs, data.frame(Run = run_type, Threshold = threshold, Folder = file_path, Reference = NA, Index = index, ImportIndex = NA))
	} else {
		file_path <- file.choose(new = FALSE)
		#file_path = "/home/dungnt/Documents/Repository/QuantificationReport/Std Curve and Samples/2024-07-25; Lot 0000649535_20250701 - KAPA - Run_01-04 - Set_3 - Sample - Run Information.csv"

		threshold <- threshold_question()

		reference  = reference_question(runs) 

		import_run_index = import_run_index + 1

		runs <- rbind(runs, data.frame(Run = run_type, Threshold = threshold, Folder = file_path, Reference = reference, Index = NA, ImportIndex = import_run_index))


	}


	has_more_run <- more_run_question()

	if (has_more_run == "No") {
		break 
	}

}


for (i in 1:nrow(runs)) {

        run_type <- runs[i, "Run"]

        run <- runs[i,]

#	print(run$Folder) 

#	print(runs[i, "Folder"])

	#render_template1(run, summary)

}


#result1 <- render_template1(file_path1, summary)

#summary <- result1$summary

#result3 <- render_template3(file_path3, summary)

#print(result3$summary) 

render("./main.Rmd", params = list(), output_file = "1.pdf") 
