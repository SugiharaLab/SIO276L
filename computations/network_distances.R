source ("import_data.R")
library(qgraph)
library(rEDM)
library(ggplot2)

num_pairs <- length(modes_df)*(length(modes_df)-1)
weight_pairs <- data.frame(to=character(num_pairs),
		from=character(num_pairs),thickness=numeric(num_pairs),
		stringsAsFactors=FALSE)

GEN_CCM_TEMPORAL_GRAPH <- FALSE
SLIDING_WINDOW_NETWORK <- FALSE
CALC_SYNCHRONIZATION <- FALSE
NUM_TIME_WINDOWS = 20

args <- commandArgs(trailingOnly = TRUE)

#handpicked best E from simplex output
best_E <- function(mode) {
	switch(	mode,
			"ENSO" 	= 5,
			"NAO"	= 8,
			"NPI"	= 7,
			"PDO"	= 3,
			)
}
#generate constant coordinate positions. 
coords <- c(0, 0, 
		1, 0,
		0.5, 1, 
		0.3, 0.2)
coords <- matrix(coords, 4,2,byrow=T)

#generate connections between each pair
if ( GEN_CCM_TEMPORAL_GRAPH ) {
	#break interval size to be in 11 month windows
	interval_size <- ceiling( nrow( modes_df ) /  NUM_TIME_WINDOWS )
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

	#make the gif using shell script
}
#try the same but for sliding window instead of blocks
if ( SLIDING_WINDOW_NETWORK || args[1]=="slidingwindow") {
	print("calculating syncrhonization")
	window_size <- 120
	delta=1 #space between windows
	start_row <- 0
	end_row <- window_size
	plot_idx <- 0
	png("plots/%d.png")
	while ( end_row < nrow(modes_df) ) {
		windowed_df <- 	modes_df[start_row:end_row,]	
		#to keep track of which row to write to 
		curr_row <- 1
		#go thru every col to get pairwise ccm
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
		qgraph( weight_pairs, title=paste(start_row,end_row,sep=":"),
				edge.labels=TRUE, layout=coords, esize=5, theme='gray')
		#update bounds 
		start_row 	<- start_row + delta 
		end_row 	<- end_row + delta 
		plot_idx <- plot_idx+1
	}	
	#make gif with shell script in dir
	system( paste("./make_gif.sh ", plot_idx) )
}
if ( CALC_SYNCHRONIZATION || args[1]=="synchronization") {
	print("calculating syncrhonization")
	total_network_distances <- list()
	#iterate through every row to compute network distance
	window_size <- 120
	delta=1 #space between windows
	start_row <- 0
	end_row <- window_size
	plot_idx <- 0
	png("plots/%d.png")
	while ( end_row < nrow(modes_df) ) {
		windowed_df <- 	modes_df[start_row:end_row,]	
		#to keep track of which row to write to 
		curr_row <- 1
		#holds the sum of current correlation distances sqrt
		sumDists <- 0
		#go thru every col to get pairwise ccm - actually forgot order doesn't matter 
		for ( idx in 1:ncol( windowed_df )) {
			mode_name <- colnames(windowed_df)[idx]
			mode_vec <- windowed_df[,idx]
			#iterate every pair between this mode and other modes
			other_modes <- subset( windowed_df, select = -idx )
			for ( col_idx in 1:ncol( other_modes )) {
				#get the weight of this connection
				pair_weight <- cor( windowed_df[idx], other_modes[col_idx], method="pearson",
					use="complete.obs") 
				sqrt_cor <- sqrt( 2*(1-abs(pair_weight)) )
				sumDists <- sumDists + sqrt_cor	
				#set the pair data in the df
				weight_pairs[curr_row,][1] <- mode_name
				weight_pairs[curr_row,][2] <- colnames(other_modes)[col_idx]
				weight_pairs[curr_row,][3] <- pair_weight
				curr_row <- curr_row+1
			}
		}
		#calc total network synchronization at this point
		totalNetwokDist <- sumDists*2/(ncol(modes_df)*(ncol(modes_df)-1))
		totalNetwokDist <- format(round(totalNetwokDist, 2), nsmall = 2)
		total_network_distances <- c(total_network_distances, totalNetwokDist)
		networkDistStr <- paste( "total network dist is ",totalNetwokDist)
		title <- paste("synchronization/correation graph ",start_row,":",end_row,"\n",networkDistStr)
		if ( FALSE ) { #change to actually graph
			qgraph( weight_pairs[1:3], title=title,
					edge.labels=TRUE, directed=FALSE, layout=coords, esize=5, theme='gray')
		}
		#update bounds 
		start_row 	<- start_row + delta 
		end_row 	<- end_row + delta 
		plot_idx <- plot_idx+1
	}	
	#make gif with shell script in dir
	#system( paste("./make_gif.sh ", plot_idx) )
	#pdf("synchronizationPlot.pdf")
	#qplot(seq_along(total_network_distances), total_network_distances)
	save(total_network_distances, file="synchronization_series.RData")
	print("done calculating syncrhonization")
}
