run_by_question <- function(){

	options <- c("Florence Lim", "Huan Pei Tee", "Nurhilya", "Others", "Blank")


	result <- paste("\t", seq_along(options), ". ", options, sep = "")

	final_result <- paste(result, collapse = "\n")
        cat("\nPerformed by?\n")
	cat (final_result)
	cat("\n")

        answer <- as.numeric(trimws(readLines("stdin",n=1)))

        if (answer == length(options) - 1){
                cat("Please specify:")
                run_by <- readLines("stdin",n=1)
        } else if (answer < length(options)) {
		run_by  <- options[answer]
	} else {
                run_by <- ""
        }

        return(run_by)
}

run_date_question <- function(){

	options <- c(format(Sys.Date(), "%d/%m/%Y"), "Blank")

	result <- paste("\t", seq_along(options), ". ", options, sep = "")

	final_result <- paste(result, collapse = "\n")

	cat("\nDate?\n")
	cat(final_result) 
	cat("\n")

        answer <- as.numeric(trimws(readLines("stdin",n=1)))

	if (answer < length(options)){
 		run_date <- options[answer]
	} else {
                run_date <- ""
	}
        return(run_date)

}

run_no_question <- function() {

	cat("\nPlease key in the Run No.?\n")
	run_no <- as.numeric(trimws(readLines("stdin",n=1)))
	return(run_no)
}

analysis_question <- function() {

	options <- c("Albumin Quantification", "KAPA Library Quantification")

	final_result = paste(paste("\t", seq_along(options), ". ", options, sep = ""), collapse = "\n")

	cat("\nAnalysis:\n")
	cat(final_result)
	cat("\n")

	answer <- as.numeric(trimws(readLines("stdin",n=1)))

	return(options[answer])
}


input_type_question <- function() {
	options <- c("Standard Curve", "Import")

	final_result = paste(paste("\t", seq_along(options), ". ", options, sep = ""), collapse = "\n")

	cat("\nInput type:\n")
        cat(final_result)
        cat("\n")

        answer <- as.numeric(trimws(readLines("stdin",n=1)))

        return(options[answer])

}

folder_chooser <- function(){

	cat("\nChoose a folder:\n")
	#folder <- file.choose(new = FALSE)
	#folder <- "/home/dungnt/Documents/Repository/QuantificationReport/Sample results folder/2024-07-25; Lot 0000649535_20250701 - ALB MNC 0624 Std Curve, 4mM - Run Information.csv"
	folder <- "/home/dungnt/Documents/Repository/QuantificationReport/Import Run Data/2024-11-13; Lot 0000649535_20250701 - ALB Import for RGQ vs CFX Validation, 4mM - Run Information.csv"
	return(folder)

}

reference_question <- function(runs){
	cat("\nWhich standard curve as reference?\n")

	for (i in 1:nrow(runs)) {
		
		index <- runs[i, "Index"]
		if (!is.na(index)) {
			cat(paste("\t", index, ". Standard Curve Set ", index, "\n", sep=""))
		}
	}

	answer <- as.numeric(trimws(readLines("stdin",n=1)))
	return (answer) 
	
}

threshold_question <- function(){

	cat("Please key in the baseline threshold?\n")

	threshold <- trimws(readLines("stdin",n=1))
	threshold <- as.numeric(threshold)

	return(threshold) 
}

more_run_question <- function(){

	options <- c("No", "Yes")

        final_result = paste(paste("\t", seq_along(options), ". ", options, sep = ""), collapse = "\n")

        cat("\nDo you have another run file to analyze?:\n")
        cat(final_result)
        cat("\n")

        answer <- as.numeric(trimws(readLines("stdin",n=1)))

        return(options[answer])

}
