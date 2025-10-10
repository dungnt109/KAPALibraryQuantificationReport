source("input_questions.R")
source("custom_functions.R")

generated_date <- format(Sys.time(), format="%Y-%m-%d %H:%M:%S")

version_number <- "V1.0"

pipeline_version <- "Version 1.0, 10 Oct 2025"

performed_by = run_by_question()

run_date = run_date_question()

run_no = run_no_question()

summary <- data.frame(matrix(nrow = 0, ncol = 8))
runs <- data.frame(matrix(nrow = 0, ncol = 6)) 
index <- 0 

import_run_index <- 0 

analysis_type <- analysis_question()

while (TRUE) {

	input_type <- input_type_question()


	if (input_type == "Standard Curve"){
		file_path  <- file.choose(new = FALSE)
		#file_path = "/home/dungnt/Documents/Repository/QuantificationReport/Std Curve and Samples/2024-07-25; Lot 0000649535_20250701 - KAPA - Run_01-04 - Set_3 - Sample - Run Information.csv"
		threshold <- threshold_question()

		index = index + 1

		runs <- rbind(runs, data.frame(InputType = input_type, Threshold = threshold, Folder = file_path, Reference = NA, Index = index, ImportIndex = NA ))

	} else if (input_type == "Standard Curve (with samples)"){

		#file_path = "/home/dungnt/Documents/Repository/QuantificationReport/Std Curve and Samples/2024-07-25; Lot 0000649535_20250701 - KAPA - Run_01-04 - Set_3 - Sample - Run Information.csv"
		#threshold <- threshold_question()

		#index = index + 1

		#runs <- rbind(runs, data.frame(InputType = input_type, Threshold = threshold, Folder = file_path, Reference = NA, Index = index, ImportIndex = NA))
	} else if (input_type == "Import") {
		file_path <- file.choose(new = FALSE)
		#file_path = "/home/dungnt/Documents/Repository/QuantificationReport/Std Curve and Samples/2024-07-25; Lot 0000649535_20250701 - KAPA - Run_01-04 - Set_3 - Sample - Run Information.csv"

		threshold <- threshold_question()

		reference  = reference_question(runs) 

		import_run_index = import_run_index + 1

		runs <- rbind(runs, data.frame(InputType = input_type, Threshold = threshold, Folder = file_path, Reference = reference, Index = NA, ImportIndex = import_run_index))


	}


	has_more_run <- more_run_question()

	if (has_more_run == "No") {
		break 
	}

}


for (i in 1:nrow(runs)) {

        input_type <- runs[i, "InputType"]

        run <- runs[i,]

#	print(run$Folder) 

#	print(runs[i, "Folder"])

	#render_KAPA_template1(run, summary, analysis_type)

}


     

globalVar <- list(
   analysis_type = analysis_type, 
   generated_date = generated_date, 
   version_number = version_number, 
   excel_file_name = paste(analysis_type, "_Result_", gsub(":", "_", generated_date), "_", version_number, ".xlsx", sep="")
)


library(rmarkdown)
library(knitr)
library(flextable)
library(dplyr)
library(kableExtra)
library(openxlsx)

render("./main.Rmd", params = list(), output_file = paste( analysis_type, "_Report_", gsub(":", "_", generated_date), "_", version_number, ".pdf", sep="")) 
