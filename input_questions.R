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
        } else if (answer == length(options)) {
		run_by = ""
	} else {
                run_by <- options[answer] 
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

	if (answer == length(options)){
 		run_date <- ""
	} else {
                run_date <- options[answer]
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


run_type_question <- function() {
	options <- c("Standard Curve (standards only)", "Standard Curve (with samples)", "Import")

	final_result = paste(paste("\t", seq_along(options), ". ", options, sep = ""), collapse = "\n")

	cat("\nRun type:\n")
        cat(final_result)
        cat("\n")

        answer <- as.numeric(trimws(readLines("stdin",n=1)))

        return(options[answer])

}
