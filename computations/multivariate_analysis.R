#file to try doing multivariate analysis
source("import_data.R")
library(rEDM)

normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}
#method to construct the embedding we need
makeEmbedding <- function( vecToEmbed, E, colName="x" ) {
	embeddingMatrix <- matrix ( NA, nrow=length(vecToEmbed), ncol=E+1 )
	colnames(embeddingMatrix) <- paste(colName, 0:E, sep="_lag_")
	embeddingMatrix[,1] <- vecToEmbed
	for ( idx in 1:E+1 ) {
		newEmbedding <- head(c(NA,embeddingMatrix[,idx-1]),-1)
		embeddingMatrix[,idx] <- newEmbedding
	}	
	embeddingMatrix <- embeddingMatrix[,-1]
	return(data.frame(embeddingMatrix))
}
#combine the dataframes
modes_df$ENSO <- normalize(modes_df$ENSO)
embedding <- makeEmbedding( modes_df$ENSO, 14, "ENSO")
for ( idx in 1:ncol(embedding) ) {
	currCol <- colnames(embedding)[ idx ]
	modes_df[ currCol ] <- embedding[ currCol ]
}

lib = c(1,800)
pred = c(801, 1300)

#running the simplex on both versions to make sure embeddings correctly
columns = c(1,5:10)
print(colnames(modes_df))
#1] "time"        "ENSO"        "NAO"         "NPI"         "PDO"         "ENSO_lag_1"  "ENSO_lag_2"  "ENSO_lag_3"  "ENSO_lag_4"  "ENSO_lag_5"  "ENSO_lag_6"  "ENSO_lag_7"  "ENSO_lag_8" 
#[14] "ENSO_lag_9"  "ENSO_lag_10" "ENSO_lag_11" "ENSO_lag_12" "ENSO_lag_13" "ENSO_lag_14" total_network_distances

#wow! that's a good result! .8 rho!
block_lnlp_output <- block_lnlp(modes_df, columns = columns, 
		target_column = 1, lib=lib, pred=pred,first_column_time = TRUE)
print("block output:")
print(block_lnlp_output)
print("smap output:")
#s_map(modes_df$ENSO, lib=lib,pred=pred )

#print("simplex univariate output:")
#simplex_out <- simplex(modes_df$ENSO,E=5,num_neighbors=6)
#print(simplex_out)

