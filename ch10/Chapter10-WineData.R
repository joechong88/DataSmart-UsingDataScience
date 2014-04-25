##################################################################
#
# Wine Data k-means clustering
#
##################################################################

# Use Spherical K-means, provided by package skmeans
library(skmeans)

# read in the wine data from CSV (WineKMC.csv)
winedata <- read.csv("WineKMC.csv")

# display winedata (all rows, column 1:9)
winedata[, 1:9]

# cleanup the blank spaces that is displayed as NA into 0
winedata[is.na(winedata)] <- 0

# show the winedata again after clean-up particularly col 8:17
winedata[1:10, 8:17]

# at this point, your dataset is good to start data science analysis

##################################################################
#
# Now to k-means. 
# skmeans() function accepts a matrix where each row corresponds to
# an object to cluser
# Our current data, winedata, is column-orientated with a bunch of 
# deal descriptors at the beginning that the algorithm isn't gonna
# want to see. Hence need to transpose.
#
# Using ncol(winedata), you'll find that there are 107 columns
# To isolate the purchase vectors as rows for each customer by
# tranposing the data from column 8 to 107 and putting them into 
# a new variable, winedata.transposed
##################################################################
ncol(winedata)
winedata.transposed <- t(winedata[,8:107])

# display the new variable so that it is ready for skmeans
winedata.transposed[1:10, 1:10]

# use skmeans, specifying 5 as number of classes and use of a genetic algorithm
# and assign the result winedata.clusters
winedata.clusters <- skmeans(winedata.transposed, 5, method="genetic")
winedata.clusters
str(winedata.clusters)

# now if you want to pull back the cluster assignment for row 4, use
# the matrix notation on the cluster vector, or by name
winedata.clusters$cluster[4]
winedata.clusters$cluster[which(row.names(winedata.transposed)=="Wright")]

# now to understand the cluster, we look at the descriptors of the deals
# that defined them. Use the aggregate() function and group by cluster
aggregate(winedata.transposed, by=list(winedata.clusters$cluster), sum)

# write them into a variable and transpose them
winedata.clustercounts <- t(aggregate(winedata.transposed, by=list(winedata.clusters$cluster), sum)[, 2:33])
winedata.clustercounts

# now, write back the 7 columns of descriptive data back on to the deals 
# using the column bind function cbind()
winedata.desc.plus.counts <- cbind(winedata[,1:7], winedata.clustercounts)

# sort them using the order(), here's an example to sort descending for Cluster 1
winedata.desc.plus.counts[order(-winedata.desc.plus.counts[,8]),]
winedata.desc.plus.counts[order(-winedata.desc.plus.counts[,9]),]
winedata.desc.plus.counts[order(-winedata.desc.plus.counts[,10]),]
winedata.desc.plus.counts[order(-winedata.desc.plus.counts[,11]),]
winedata.desc.plus.counts[order(-winedata.desc.plus.counts[,12]),]

write.csv(winedata.desc.plus.counts[order(-winedata.desc.plus.counts[,8]),], "WineCluster.csv")
