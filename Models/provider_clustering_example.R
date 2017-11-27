############################################
#
# This program uses a K-Means Clustering algorithm to cluster providers based on HCPCS code utilization.
# It uses publicly available Medicare data as an example for how it would be run once this application
# has access to L.A. Care's data through the Data Science Platform.
#
# Additional transformation will be required since the actual data will be line-level data that needs
# to be aggregated and transformed for analysis.
#
# This program assumes that the user has already established an appropriate number of clusters to create.
#
############################################

# dictate the number of clusters to be formed
k1 <- 5

#specify directory to import the data
wd <- "/Users/username/import_path" 
#Download the Medicare_Provider_Util_Payment_PUF_CY2015.txt" dataset out of the .zip archive found at the following path: 
#http://www.cms.gov/apps/ama/license.asp?file=http://download.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Medicare_Provider_Util_Payment_PUF_CY2015.zip

#specify directory to write the outputs
wd2 <- "/Users/username/write_path"

#load required packages
packages <- c("data.table",
              "stats",
              "ggplot2",
              "dplyr",
              "tidyr")

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(data.table)
library(stats)
library(ggplot2)
library(dplyr)
library(tidyr)

#set working directory
setwd(wd)
set.seed(32541)


mcr_file <- "Medicare_Provider_Util_Payment_PUF_CY2015.txt"
mcr <- fread(mcr_file)

#filter data to include only Pain Management providers in California
mcr <- mcr[2:nrow(mcr)]
ca <- mcr[nppes_provider_state == "CA" & provider_type == "Pain Management"]
rm(mcr)

ca1 <- ca[, c("npi", 
             "provider_type", 
             "hcpcs_code",
             "hcpcs_description",
             "line_srvc_cnt",
             "average_Medicare_payment_amt")]

#transform (cast wide) focal variables for evaluation by the k-means clustering algorithm
final<- dcast(ca1, npi ~ hcpcs_code, value.var = c("line_srvc_cnt"), sum)

npi <- final$npi
final$npi <-  NULL
final <- scale(final) #normalize the variables
dim(final)


#run k-means analysis
km1 <- kmeans(final, centers = k1, nstart = 10)
clusters <- data.table(npi = npi, cluster = as.factor(km1$cluster))

centers <- data.frame(cluster = factor(1:k1), km1$centers)
centers <- data.frame(t(centers ))
for(i in 1:ncol(centers)) {
  centers[, i] <- as.numeric(as.character(centers[, i]))
}
colnames(centers) <- paste("Cluster", c(1:k1))
centers$Symbol <- row.names(centers)
centers <- gather(centers, "Cluster", "Mean", -Symbol)
centers$Color = centers$Mean > 0
centers <- subset(centers, Symbol != "cluster")

cluster_sizes <- data.frame(cluster = c(1:length(km1$size)),
                            provider_count = km1$size)


#write necessary tables for Tableau
########################################
setwd(wd2)
write.csv(cluster_sizes, "cluster_sizes.csv", row.names = FALSE)


details <- dplyr::left_join(ca1, clusters, by = "npi")

final2 <- data.table(npi, final)
final2 <- melt(final2, id.vars = c("npi"))
colnames(final2) <- c("npi", "hcpcs_code", "normalized_line_srvc_cnt")
final3 <- dplyr::full_join(ca1, final2, by = c("npi", "hcpcs_code"))
final4 <- dplyr::left_join(final3, clusters, by = "npi")
final4$line_srvc_cnt <- ifelse(is.na(final4$line_srvc_cnt), 0, final4$line_srvc_cnt)
write.csv(final4, "provider_details.csv", row.names = FALSE)

descriptions <- final3[c("hcpcs_code", "hcpcs_description")]
descriptions <- descriptions[!is.na(descriptions$hcpcs_description), ]
descriptions <- descriptions[which(!duplicated(descriptions$hcpcs_code)), ]
write.csv(descriptions, "hcpcs_desc.csv", row.names = FALSE)


