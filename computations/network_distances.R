source ("import_data.R")
library(qgraph)
library(rEDM)

num_pairs <- length(modes_df)*(length(modes_df)-1)
weight_pairs <- data.frame(to=character(num_pairs),
		from=character(num_pairs),thickness=numeric(num_pairs),
		stringsAsFactors=FALSE)

GEN_CCM_TEMPORAL_GRAPH <- TRUE
CALC_SYNCHRONIZATION <- FALSE
NUM_TIME_WINDOWS = 20

#handpicked best E from simplex output
best_E <- function(mode) {
	switch(	mode,
			"ENSO" 	= 5,
			"NAO"	= 8,
			"NPI"	= 7,
			"PDO"	= 3,
			)
}

interval_size <- ceiling( nrow( modes_df ) /  NUM_TIME_WINDOWS )


#generate connections between each pair
if ( GEN_CCM_TEMPORAL_GRAPH ) {
	#generate constant coordinate positions. 
	coords <- c(0, 0, 
				1, 0,
				0.5, 1, 
				0.3, 0.2)
	coords <- matrix(coords, 4,2,byrow=T)

	#run for num interval windows generating weights for each window
	for ( idx in 1:NUM_TIME_WINDOWS ) {
		start_row 	<- 1 + (idx-1)*interval_size
		end_row		<- idx*interval_size
		windowed_df <- 	modes_df[start_row:end_row,]	

		#where to write this plot to
		png( paste("plots",idx,sep='/') )

		#to keep track of which row to write to 
		curr_row <- 1
		for ( idx in 1:ncol( windowed_df )) {
			mode_name <- colnames(windowed_df)[idx]
			mode_vec <- windowed_df[,idx]
			#iterate every pair between this mode and other modes
			other_modes <- subset( windowed_df, select = -idx )
			for ( col_idx in 1:ncol( other_modes )) {
				#get the weight of this connection
				ccm_df <- ccm ( windowed_df, E = best_E(mode_name), 
					lib_column = colnames(other_modes)[col_idx],
					target_column = mode_name, 
					lib_sizes = nrow(windowed_df), num_samples=1)
				pair_weight <- ccm_df$rho
				#set the pair data in the df
				weight_pairs[curr_row,][1] <- mode_name
				weight_pairs[curr_row,][2] <- colnames(other_modes)[col_idx]
				weight_pairs[curr_row,][3] <- pair_weight
				curr_row <- curr_row+1
			}
		}
		qgraph( weight_pairs, layout=coords, esize=5, theme='gray')
	}	

	#make the gif
	list_of_images = ""
	for ( idx in 1:NUM_TIME_WINDOWS ) {
		curr_img <- paste("plots",idx,sep='/')
		list_of_images <- paste( list_of_images, curr_img, sep=' ')
	}
	gif_cmd <- paste('convert -delay 40 ',list_of_images, ' output_gif.gif',sep='')
	print(gif_cmd)
	#system(gif_cmd)	
	#system(paste("cd",getwd(), "&& ",gif_cmd,sep=" "))
}
if ( CALC_SYNCHRONIZATION ) {
	#iterate through every row to compute network distace
	for(row_idx in 1:nrow( modes_df )) {
		row <- dataFrame[row_idx,]
		for ( col_idx in 1:ncol( modes_df )) {
				
		}
	}
}
