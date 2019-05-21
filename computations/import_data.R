options(width=190)
#for references
os_sep = "/"
dataset_path = paste("../","datasets","cleaned_data",sep=os_sep)
#read in all the datas and only get value col
ENSO_data 	<- read.csv(paste(dataset_path, "ENSO_Clean.csv", sep=os_sep))
NAO_data 	<- read.csv(paste(dataset_path, "NAO_Clean.csv", sep=os_sep))
NPI_data 	<- read.csv(paste(dataset_path, "NPI_Clean.csv", sep=os_sep))
PDO_data 	<- read.csv(paste(dataset_path, "PDO_Clean.csv", sep=os_sep))

#for easier reference
indices_list = list(ENSO_data, NAO_data, NPI_data, PDO_data)

#get number of shared date index rows 
first_year_date <- "1900-03-01"
num_shared_rows <- .Machine$integer.max
for (dataset in indices_list) {
	min_row 	<- which(first_year_date == dataset$Date)[[1]]
	total_row 	<- nrow(dataset) - min_row
	if (total_row < num_shared_rows) num_shared_rows = total_row
}

#construct our dataset format with only data that overlaps in date
modes_df = data.frame(matrix(nrow=num_shared_rows, ncol=length(indices_list)))
for (idx in seq_along(indices_list)) {
	dataset		<- indices_list[[idx]]
	idx_name 	<- toString(dataset$Index[[1]])
	#set col data
	colnames(modes_df)[[idx]] <- idx_name
	min_row 	<- which(first_year_date == dataset$Date)[[1]]
	top_row 	<- min_row + num_shared_rows-1
	shared_rows <- dataset[ min_row:top_row, "Value" ]
	modes_df[idx_name] <- shared_rows
}
#add time col
modes_df$time <- 1:nrow(modes_df)
modes_df <- modes_df[,c(ncol(modes_df),1:(ncol(modes_df)-1))]

#handpicked best E from simplex output
best_E <- function(mode) {
	switch(	mode,
			"ENSO" 	= 5,
			"NAO"	= 8,
			"NPI"	= 7,
			"PDO"	= 3,
			)
}
