library(dummies)
library(dplyr)

raw_data = read.csv("Member Dataset.csv",stringsAsFactors = F)
length(unique(raw_data$Member_ID))

# avg across 3 months
raw_data[,7:36][is.na(raw_data[,7:36])]<- 0
raw_data_new = (raw_data[,7:16] + raw_data[,17:26] + raw_data[,27:36])/3

# slice features for k means, factorize categ vars
feature_idx = c(2,3,4,37,38,39,41)
inp_data = cbind(raw_data[,feature_idx],raw_data_new)
colnames(inp_data)

# Handle NAs in data
unlist(lapply(inp_data,function(x) sum(is.na(x))))
inp_data[,4:6][is.na(inp_data[,4:6])] = 0
inp_data[,7][is.na(inp_data[,7])] = "unknown"
inp_data[,1][is.na(inp_data[,1])] = mean(inp_data[,1],na.rm = T)

for(i in c(3,7)){
  inp_data[,i] = as.factor(inp_data[,i])
}


# determine best k for kmeans
# create dummy vars for 1 categorical vars
inp_data_knn  = df1 <- cbind(inp_data[,c(1,2,4:6)], dummy(inp_data$Cust_type, sep = "_"))
wss = c()
for(i in 2:15){
  wss = c(wss,kmeans(inp_data_knn,i)$tot.withinss)
}
plot(2:15,wss)

# we choose 10 clusters
inp_data$cluster = kmeans(inp_data_knn,10)$cluster


# produce avg norm product recomendation order for each cluster
aggr_cluster = aggregate(. ~ cluster,inp_data[,8:18],mean)
aggr_cluster_norm = as.data.frame(lapply(aggr_cluster[,-1], function(x)(x-min(x))/(max(x)-min(x))))
aggr_cluster_norm = as.data.frame(t(aggr_cluster_norm))
aggr_cluster_norm$product = row.names(aggr_cluster_norm)


# provide recommendation for CLUSTER 1
out_list = list()
ordr = arrange(aggr_cluster_norm[,c(1,11)],desc(V1))$product
tempdata = inp_data[inp_data$cluster==1,]
tempdata = cbind(tempdata[,1:7],tempdata[,ordr])
prod_reco = c()

for(j in 1:nrow(tempdata)){
  
  prod_reco = c(prod_reco,ordr[which(tempdata[j,8:17]==0)[1]])
  
}
  

# final data with product reccomendation
tempdata$prod_reco = prod_reco
table(prod_reco)


