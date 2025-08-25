render_template1 <- function(run, summary){

	file_path <- run$Folder

        threshold <- run$Threshold

	run_type <- run$Run

	index <- run$Index
 


	#Experiment Information
	
	experiment_information <- get_experiment_information(file_path)
	file_name <- experiment_information$file_name
	run_started <- experiment_information$run_started
	run_ended <- experiment_information$run_ended
	machine <- experiment_information$machine



	#Quantification Information

	path = sub("Run Information.*", "", file_path)

	standard_curve_path = paste(path, " Standard Curve Results.csv", sep="")

	standard_curve_result = read.csv(standard_curve_path, sep=",", header=TRUE)

	slope <-   sprintf("%#.1f", standard_curve_result$Slope)
	reaction_efficiency <- round(standard_curve_result$Efficiency..)
	r_2_value <- sprintf("%#.2f", standard_curve_result$R.2)   

	if (round(standard_curve_result$Slope, digits=1) >= -3.6 && round(standard_curve_result$Slope, digits=1) <= -3.1){
		slope_status = color_text("PASS")
	} else {
		slope_status = color_text("ALERT")
	}

	if (reaction_efficiency >= 90 && reaction_efficiency <= 110){
		reaction_efficiency_status = color_text("PASS")
	} else {
		reaction_efficiency_status = color_text("ALERT")
	}

	if (round(standard_curve_result$R.2, digits=2) >= 0.99){
		r_2_status = color_text("PASS")
	} else {
		r_2_status = color_text("ALERT")
	}

	#Quantification Results 
	## figure 

	figure_path <- paste(path, "Amplification.png", sep="") 

	## Table 

	df <- get_quantification_result_df(file_path)


	df <- df[df$Type != "Unkn", ]


	B <- standard_curve_result$Y.Intercept 
	M <- standard_curve_result$Slope

	df <- df[order(df$Type), ]

	df <- df[order(df$Type == "NTC"), ]

	df <- add_calculated_concentration_column(df, B, M)

	df <- add_replicate_column(df, "Type")

	df <- add_average_column(df, "Type")

	df <- add_delta_average_column(df)

	df <- df[, c(1, 2, 3, 4, 6, 5, 7, 8, 9)]


	display = get_display_df(df)

	#display <- display %>% select(-average)


	#Quality Control Checklist


	ntc = df$`Ct Value`[df$Type=="NTC"]
	std6 = df$`Ct Value`[df$Type=="Std-6"]


	ntc_has_numeric = any(!is.na(suppressWarnings(as.numeric(unlist(ntc)))))

	std6_has_numeric = any(!is.na(suppressWarnings(as.numeric(unlist(std6)))))

	if (!ntc_has_numeric) {
		status6 <- color_text("PASS")
	} else {

		if (!std6_has_numeric){
			status6 <- color_text("ALERT")
		} else {
			ntc_min = min(suppressWarnings(as.numeric(ntc)), na.rm =TRUE)
			std6_max = max(suppressWarnings(as.numeric(std6)), na.rm =TRUE)

			if (ntc_min - std6_max >= 3){
					status6 <- color_text("PASS")
				} else {
					status6 <- color_text("ALERT")
				}
		}


	}


	numeric_replicates <- suppressWarnings(as.numeric(df$replicate))


	valid_replicates <- numeric_replicates[!is.na(numeric_replicates)]

	if (all(valid_replicates >= -1.5 & valid_replicates <= 1.5)){
		
		replicate_status <- color_text("PASS")

	} else {
		
		replicate_status <- color_text("ALERT")

	}

	numeric_averages <- suppressWarnings(as.numeric(df$delta_average))

	valid_averages <- numeric_averages[!is.na(numeric_averages)]


	if (all(valid_averages >= 3.1 & valid_averages <= 3.6 )){
		
		average_status <- color_text("PASS")

	} else {
		
		average_status <- color_text("ALERT")

	}

	overall_status = all(grepl("PASS", c(slope_status, 
		                                 reaction_efficiency_status, 
		                                 r_2_status, 
		                                 replicate_status, 
		                                 average_status, 
		                                 status6)))
	if (overall_status){
		overall_qc_status <- color_text("PASS")
	} else {
		overall_qc_status <- color_text("ALERT")
	}

	run_date <- strsplit(file_name, ";")[[1]][1]

	lot_no <- strsplit(strsplit(file_name, "_")[[1]][1], "Lot ")[[1]][2]

	opening_date <- strsplit(strsplit(file_name, " -")[[1]][1], "_")[[1]][2]

	summary <- rbind(summary, list(Index <- nrow(summary)+1, 
				       Date  <- run_date, 
				       Type  <- paste0("Standard Curve (Set ", index ,")"), 
				       Threshold <- threshold, 
				       Lot <- lot_no, 
				       Opening <- opening_date, 
				       TotalSamples <- "N.A.", 
				       Status <- overall_qc_status))


	return(list(latex=knitr::knit_child("template1.Rmd", quiet = TRUE, envir = environment()), summary = summary))


}


render_template3 <- function(run, summary){

	file_path <- run$Folder

        threshold <- run$Threshold

        run_type <- run$Run

	reference <- run$Reference

	#Experiment Information
	experiment_information <- get_experiment_information(file_path)
	file_name <- experiment_information$file_name
	run_started <- experiment_information$run_started
	run_ended <- experiment_information$run_ended
	machine <- experiment_information$machine

	#Quantification Results 

	path = sub("Run Information.*", "", file_path)

	figure_path <- paste(path, "Amplification.png", sep="") 

	df <- get_quantification_result_df(file_path)

	df <- df[df$Type %in% c("Std-3", "Std-6", "NTC", "Unkn" ), ]

	df <- df[order(df$Type), ]

	df <- df[order(df$Type == "NTC"), ]

	df <- df[order(df$Type == "Unkn"), ]

	subset <- df[df$Type == "Unkn", ]

	subset$Sample_order <- match(subset$Sample, unique(subset$Sample))

	subset_sorted <- subset[order(subset$Sample_order), ]

	subset_sorted$Sample_order <- NULL

	#subset_sorted <- subset[order(subset$Sample), ]

	subset_other <- df[df$Type != "Unkn", ]

	df <- rbind(subset_other, subset_sorted)

	reference_file_path <- runs[runs$Index == reference, "Folder"][1]


        standard_curve_path = paste( sub("Run Information.*", "", reference_file_path) , " Standard Curve Results.csv", sep="")

	standard_curve_result = read.csv(standard_curve_path, sep=",", header=TRUE)

	
	M <- standard_curve_result$Slope

	B <- mean(df$`Ct Value`[df$Type == "Std-3"], na.rm = TRUE) - (M * log10(df$`Given Concentration`[which(df$Type == "Std-3")[1]]))



	df <- add_calculated_concentration_column(df, B, M)

	df <- add_replicate_column(df, "Sample")

	df <- add_average_column(df, "Sample")

	df <- add_delta_average_column(df)

	df <- df[, c(1, 2, 3, 4, 6, 5, 7, 8, 9)]


	display = get_display_df(df)


	display <- display[, -c(9)]

	average_standard_3 = mean(df$`Ct Value`[df$Type == "Std-3"], na.rm = TRUE)

	average_standard_6 = mean(df$`Ct Value`[df$Type == "Std-6"], na.rm = TRUE)

	file_path_reference = sub(" Standard Curve Results.*", "", standard_curve_path ) 
	file_path_reference = paste(file_path_reference, "Run Information.csv", sep="")

	df_ref = get_quantification_result_df(file_path_reference)

	average_standard_3_ref = mean(df_ref$`Ct Value`[df_ref$Type == "Std-3"], na.rm = TRUE)

	average_standard_6_ref = mean(df_ref$`Ct Value`[df_ref$Type == "Std-6"], na.rm = TRUE)



	if (abs(average_standard_3 - average_standard_3_ref) <= 1.5){
		status1 <- color_text("PASS")
	} else {
		status1 <- color_text("ALERT")
	}

	if(abs(average_standard_6 - average_standard_6_ref) <= 1.5){
 		status2 <- color_text("PASS")
	} else {
		status2 <- color_text("ALERT")
	}

	ntc = df$`Ct Value`[df$Type=="NTC"]
	std6 = df$`Ct Value`[df$Type=="Std-6"]


	ntc_has_numeric = any(!is.na(suppressWarnings(as.numeric(unlist(ntc)))))

	std6_has_numeric = any(!is.na(suppressWarnings(as.numeric(unlist(std6)))))

	if (!ntc_has_numeric) {
        	status3 <- color_text("PASS")
	} else {

        	if (!std6_has_numeric){
                	status3 <- color_text("ALERT")
        	} else {
                	ntc_min = min(suppressWarnings(as.numeric(ntc)), na.rm =TRUE)
                	std6_max = max(suppressWarnings(as.numeric(std6)), na.rm =TRUE)

                	if (ntc_min - std6_max >= 3){
                                status3 <- color_text("PASS")
                        } else {
                                status3 <- color_text("ALERT")
                        }
       		 }


	}

	numeric_replicates <- suppressWarnings(as.numeric(df$replicate))

	valid_replicates <- numeric_replicates[!is.na(numeric_replicates)]

	if (all(valid_replicates >= -1.5 & valid_replicates <= 1.5)){

        	replicate_status <- color_text("PASS")

	} else {

        	replicate_status <- color_text("ALERT")

	}


	overall_status = all(grepl("PASS", c(status1, status2, status3, replicate_status)))

	if (overall_status){
        	overall_qc_status <- color_text("PASS")
	} else {
        	overall_qc_status <- color_text("ALERT")
	}






	run_date <- strsplit(file_name, ";")[[1]][1]

	lot_no <- strsplit(strsplit(file_name, "_")[[1]][1], "Lot ")[[1]][2]

	opening_date <- strsplit(strsplit(file_name, " -")[[1]][1], "_")[[1]][2]

	summary <- rbind(summary, list(Index <- nrow(summary)+1, 
				       Date <- run_date, 
				       Type <- "Import", 
				       Threshold <- threshold, 
				       Lot <- lot_no, 
				       Opening <- opening_date, 
				       TotalSamples <- "", 
				       Status <- overall_qc_status))



	return(list(latex=knitr::knit_child("template3.Rmd", quiet = TRUE, envir = environment()), summary = summary))

}

get_experiment_information <- function(file_path){
	data <- read.csv(file_path, sep=",", header=FALSE)
	file_name = paste(data[1, 2], data[1, 3], sep="")
	file_name = gsub("_", "\\\\_", file_name)
	run_started = data[5, 2]
	run_ended = data[6, 2]
	machine = data[11, 2]
	if (machine == "CT019934"){
		machine = "CFX1"
	} else if (machine == "CT020097"){
		machine = "CFX2"
	}

	list(file_name = file_name, run_started = run_started, run_ended = run_ended, machine = machine)
}

get_quantification_result_df <- function(file_path){
	path = sub("Run Information.*", "", file_path)
	quantification_result_path = paste(path, " Quantification Cq Results.csv", sep="")

	df = read.csv(quantification_result_path, sep=",", header=TRUE)

	df = df[ , -c(1, 3, 4, 7, 9, 10, 12, 13, 14, 15, 16)]

	df <- df[, c(1, 3, 2, 5, 4)]


	colnames(df) <- c("Well", "Sample", "Type", "Given Concentration", "Ct Value")

	return(df)
}

add_calculated_concentration_column <- function(df, B, M){
	df$concentration <- ifelse(is.na(as.numeric(df$`Ct Value`)), "", round( 10^((df$`Ct Value` - B) / M)))
	return(df)
}

add_replicate_column <- function(df, column){

	column_name <- sym(column)

	df <- df %>%
	  group_by({{column_name}}) %>%
	  mutate(replicate = abs(max(`Ct Value`) - min(`Ct Value`))) %>%
	  ungroup()


	df <- df %>%
	  group_by({{column_name}}) %>%
	  mutate(replicate = ifelse(row_number() == 1, replicate, NaN)) %>%
	  ungroup()

	return(df)

}

add_average_column <- function(df, column){

	column_name <- sym(column)

	df <- df %>%
	  group_by({{column_name}}) %>%
	  mutate(average = mean(`Ct Value`)) %>%
	  ungroup()

	df <- df %>%
	  group_by({{column_name}}) %>%
	  mutate(average = ifelse(row_number() == 1, average, NaN)) %>%
	  ungroup()

	return(df)
}

add_delta_average_column <- function(df){
	df$val_num <- suppressWarnings(as.numeric(df$average))

	# Create result column
	df$delta_average <- NA  

	for (i in 1:(nrow(df))) {
	  # Only process non-NA (non-empty) values
	  if (!is.na(df$val_num[i])) {
	    
	    # Search for the next non-NA value
	    j <- i + 1
	    while (j <= nrow(df) && is.na(df$val_num[j])) {
	      j <- j + 1
	    }

	    # If we found a valid next value, subtract
	    if (j <= nrow(df)) {
	      df$delta_average[i] <- df$val_num[j] - df$val_num[i]
	    } else {
	      df$delta_average[i] <- NA  
	    }
	  } else {
	    df$delta_average[i] <- NA
	  }
	}

	df <- df %>% select(-val_num)

	return(df)
}

get_display_df <- function(df){

	display <- df 

	display$concentration <- ifelse(is.na(as.numeric(df$`Ct Value`)), "", df$concentration) 

	display$`Ct Value` = ifelse(is.na(as.numeric(df$`Ct Value`)), "", sprintf("%#.2f",df$`Ct Value`))


	display$replicate = ifelse(is.na(as.numeric(df$replicate)),  "", sprintf("%#.2f", df$replicate) )

	display$average = ifelse(is.na(as.numeric(df$average)),  "", sprintf("%#.2f", df$average) )

	display$delta_average = ifelse(is.na(as.numeric(df$delta_average)),  "", sprintf("%#.2f", df$delta_average) )

	display$`Given Concentration` = ifelse(is.na(as.numeric(df$`Given Concentration`)),  "", format(df$`Given Concentration`, scientific = FALSE))



	colnames(display) <- c("\\makecell[l]{Well}", 
		          "\\makecell[l]{Sample}", 
		          "\\makecell[l]{Type}", 
		          "\\makecell[l]{Given\\\\Concentration\\\\(copies)}", 
		          "\\makecell[l]{Calculated\\\\Concentration\\\\(copies)}", 
		          "\\makecell[l]{Ct\\\\Value}", 
		          "\\makecell[l]{$\\Delta Ct$ of\\\\Replicates}", 
		          "average",
		          "\\makecell[l]{$\\Delta Ct$ of\\\\Average Ct}")

	return (display)

}

color_text <- function(text){
	if (text == "PASS"){
		return ("\\textcolor{darkgreen}{PASS}")
	} else if (text == "ALERT"){
		return ("\\textcolor{orange}{ALERT}")
	}
}

