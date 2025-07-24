render_template1 <- function(file_path){

data <- read.csv(file_path, sep=",", header=FALSE)
file_name = paste(data[1, 2], data[1, 3], sep="")
file_name = gsub("_", "\\\\_", file_name)
run_started = data[5, 2]
run_ended = data[6, 2]
machine = data[11, 2]

if (machine == "CT019934"){
	machine = "CFX96 (1)"
} else if (machine == "CT020097"){
	machine = "CFX96 (2)"
}

path = sub("Run Information.*", "", file_path)

figure_path <- paste(path, "Amplification.png", sep="") 

standard_curve_path = paste(path, " Standard Curve Results.csv", sep="")

standard_curve_result = read.csv(standard_curve_path, sep=",", header=TRUE)

slope <-   sprintf("%#.1f", standard_curve_result$Slope)
reaction_efficiency <- round(standard_curve_result$Efficiency..)
r_2_value <- sprintf("%#.2f", standard_curve_result$R.2)   

if (round(standard_curve_result$Slope, digits=1) >= -3.6 && round(standard_curve_result$Slope, digits=1) <= -3.1){
	slope_status = "\\textcolor{darkgreen}{PASS}"
} else {
	slope_status = "\\textcolor{orange}{ALERT}"
}

if (reaction_efficiency >= 90 && reaction_efficiency <= 110){
	reaction_efficiency_status = "\\textcolor{darkgreen}{PASS}"
} else {
	reaction_efficiency_status = "\\textcolor{orange}{ALERT}"
}

if (round(standard_curve_result$R.2, digits=2) >= 0.99){
	r_2_status = "\\textcolor{darkgreen}{PASS}"
} else {
	r_2_status = "\\textcolor{orange}{ALERT}"
}


quantification_result_path = paste(path, " Quantification Cq Results.csv", sep="")

df = read.csv(quantification_result_path, sep=",", header=TRUE)

df = df[ , -c(1, 3, 4, 7, 9, 10, 12, 13, 14, 15, 16)]

df <- df[, c(1, 3, 2, 5, 4)]


colnames(df) <- c("Well", "Sample", "Type", "Given Concentration", "Ct Value")

B <- standard_curve_result$Y.Intercept 
M <- standard_curve_result$Slope

df <- df[order(df$Type), ]

df <- df[order(df$Type == "NTC"), ]

df <- df %>%
  group_by(Type) %>%
  mutate(replicate = abs(max(`Ct Value`) - min(`Ct Value`))) %>%
  ungroup()

df <- df %>%
  group_by(Type) %>%
  mutate(replicate = ifelse(row_number() == 1, sprintf("%#.2f", replicate), "")) %>%
  ungroup()

df <- df %>%
  group_by(Type) %>%
  mutate(average = mean(`Ct Value`)) %>%
  ungroup()

df <- df %>%
  group_by(Type) %>%
  mutate(average = ifelse(row_number() == 1, average, "")) %>%
  ungroup()


df$val_num <- suppressWarnings(as.numeric(df$average))

# Create result column
df$delta_average <- df$val_num

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
      df$delta_average[i] <- sprintf("%#.2f", df$val_num[j] - df$val_num[i])
    } else {
      df$delta_average[i] <- ""
    }
  } else {
    df$delta_average[i] <- ""
  }
}

df <- df %>% select(-val_num)

df <- df %>% select(-average)

ntc = df$`Ct Value`[df$Type=="NTC"]
std6 = df$`Ct Value`[df$Type=="Std-6"]

#ntc_min = min(suppressWarnings(as.numeric(ntc)), na.rm =TRUE)
#std6_max = max(suppressWarnings(as.numeric(std6)), na.rm =TRUE)

ntc_has_numeric = any(!is.na(suppressWarnings(as.numeric(unlist(ntc)))))

std6_has_numeric = any(!is.na(suppressWarnings(as.numeric(unlist(std6)))))

print(ntc)
print(ntc_has_numeric)
print(std6)
print(std6_has_numeric)


df$concentration <- ifelse(df$`Ct Value` == "NaN", "", round( 10^((df$`Ct Value` - B) / M))) 

df$`Ct Value` = ifelse(df$`Ct Value` == "NaN", "", sprintf("%#.2f",df$`Ct Value`))

df$replicate = ifelse(df$replicate == "NaN",  "", df$replicate)

df$`Given Concentration` = ifelse(df$`Given Concentration` == "NaN",  "", format(df$`Given Concentration`, scientific = FALSE))

df <- df[, c(1, 2, 3, 4, 8, 5, 6, 7)]

colnames <- c("\\makecell[l]{Well}", 
	          "\\makecell[l]{Sample}", 
	          "\\makecell[l]{Type}", 
	          "\\makecell[l]{Given\\\\Concentration\\\\(copies)}", 
	          "\\makecell[l]{Calculated\\\\Concentration\\\\(copies)}", 
	          "\\makecell[l]{Ct\\\\Value}", 
	          "\\makecell[l]{$\\Delta Ct$ of\\\\Replicates}", 
	          "\\makecell[l]{$\\Delta Ct$ of\\\\Average Ct}")

numeric_replicates <- suppressWarnings(as.numeric(df$replicate))


valid_replicates <- numeric_replicates[!is.na(numeric_replicates)]

if (all(valid_replicates >= -1.5 & valid_replicates <= 1.5)){
	
	replicate_status <- "\\textcolor{darkgreen}{PASS}"

} else {
	
	replicate_status <- "\\textcolor{orange}{ALERT}"

}

numeric_averages <- suppressWarnings(as.numeric(df$delta_average))

valid_averages <- numeric_averages[!is.na(numeric_averages)]


if (all(valid_averages >= 3.1 & valid_averages <= 3.6 )){
	
	average_status <- "\\textcolor{darkgreen}{PASS}"

} else {
	
	average_status <- "\\textcolor{orange}{ALERT}"

}


summary <- rbind(summary, list(nrow(summary)+1, "", "", "", "", "", "", ""))


return(list(latex=knitr::knit_child("template1.Rmd", quiet = TRUE, envir = environment()), summary = summary))
#return ("hello")

}
