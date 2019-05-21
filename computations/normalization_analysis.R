#file to test analysis on mormalized data
library( rEDM)
source("import_data.R")

modes_df <- na.omit(modes_df)

normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}
dfNorm <- as.data.frame(lapply(modes_df, normalize))
#note: normalized data did better!
print("normalized:")
block_lnlp( dfNorm, target=2, columns=2:4, theta=4 )
s_map(dfNorm$PDO, E = 4, theta=4 )
s_map(modes_df$PDO, E = 4, theta=4 )

#try including the synchronization data
load("synchronization_series.RData")
total_network_distances <- as.numeric(unlist(total_network_distances))
with_sync <- modes_df
with_sync <- with_sync[1:length(total_network_distances),]
with_sync$total_network_distances <- total_network_distances
dfNorm <- as.data.frame(lapply(with_sync, normalize))

print("normalized with synchronize data:")
block_lnlp( dfNorm, target=2, columns=2:5, theta=4 )
print("nonnormalized:")
block_lnlp( modes_df, target=2, columns=2:4, theta=4 )
