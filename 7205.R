dataset = read.csv("7205dataset.csv",sep = " ",header = FALSE)

#why there is 100001 obs?

head(dataset)

#remove the first value "100000"

dataset = data.frame(key = dataset$V1[2:100001],x1 = dataset$V2[2:100001],x2 = dataset$V3[2:100001])

#generally look into dataset
summary(dataset)

length(which(dataset$x1 == dataset$x2))
length(which(dataset$x1 > dataset$x2))
length(which(dataset$x1 < dataset$x2))

#looks most likely that x1,x2 are random number between 0-100000

# hypothesis 1: use mean of x1,x2 to order dataset is the most effecient R-tree:


for (i in 1:nrow(dataset)) {
  dataset$mean[i] = (dataset$x1[i]+dataset$x2[i])/2
}

dataset_mean = dataset[order(dataset$mean), ]

# if there is m branches in parent nodes
#  we put b_max data points in each child node
# for simplity, only consider no underflow case
# Create a table display how many branches and sub-braches in the r-tree as a "guide" to query
generate_branch_index = function(m,dataset_table){
  dataset_mean_tree = dataset_table
  branch_number = ceiling(nrow(dataset_mean_tree)/m)
  while (branch_number[length(branch_number)] > m ){
    branch_number = c(branch_number,ceiling(branch_number[length(branch_number)]/m))
  }
  branch_number = c(branch_number,1)
  return(branch_number)
}

# Function create R-tree
generate_r_tree = function(m,dataset_table) {
  b_min = integer(0.4*m)
  b_max = m
  dataset_mean_tree = dataset_table
  data_layer = 1
  while(b_max^data_layer < nrow(dataset_mean_tree)){
    dataset_mean_tree = data.frame(dataset_mean_tree, parent_layer = NA)
      for(i in c(seq(1,nrow(dataset_mean_tree),b_max^(data_layer)),nrow(dataset_mean_tree))){
        if(i+b_max^data_layer-1<= nrow(dataset_mean_tree)){
            child_nodes_min = dataset_mean_tree$mean[i]
            child_nodes_max = dataset_mean_tree$mean[i+b_max^data_layer-1]
            for(n in seq(i,i+b_max^data_layer-1,1)){
              dataset_mean_tree[n,ncol(dataset_mean_tree)] = paste(child_nodes_min,child_nodes_max)
            }
        }else{
            child_nodes_min = dataset_mean_tree$mean[i]
            child_nodes_max = dataset_mean_tree$mean[nrow(dataset_mean_tree)]
            for(n in seq(i,nrow(dataset_mean_tree),1)){
              dataset_mean_tree[n,ncol(dataset_mean_tree)] = paste(child_nodes_min,child_nodes_max)
            }
        }
      }
    data_layer = data_layer + 1
  }
  dataset_mean_tree = data.frame(dataset_mean_tree, parent_layer1 = NA)
  child_nodes_min = dataset_mean_tree$mean[1]
  child_nodes_max = dataset_mean_tree$mean[nrow(dataset_mean_tree)]
  for(n in seq(1,nrow(dataset_mean_tree),1)){
    dataset_mean_tree[n,ncol(dataset_mean_tree)] = paste(child_nodes_min,child_nodes_max)
  }
return(dataset_mean_tree)
}


# list of m values that will not cause underflow
m_list = NULL
for(m_val in 1:nrow(dataset)){
  b_min_val = as.integer(0.4*m_val)
  b_max_val = m_val
  if ((nrow(dataset) - as.integer(nrow(dataset)/b_max_val)*b_max_val) >= b_min_val) {
    m_list = c(m_list,m_val)
  }
}

#choose random five m value(m<1000) from v_list

m_list_s = sample(m_list[which(m_list<1000)],5)

#=====================================================================================

# [QUERY WITHOUT R-TREE]

# A normal query with x1 and x2 in R scan all the rows in dataset.

# How many queries do you want to execute?
query_time = 1


query_list = data.frame(key= 1:query_time,x1= sample(1:100000,query_time),x2 =  sample(1:100000,query_time))

query_result = data.frame(NA)

time_of_query = system.time(
  for (i in 1:nrow(query_list)){
    for(n in 1:nrow(dataset)){
      if(max(query_list$x1[i],query_list$x2[i]) >= max(dataset$x1[n],dataset$x2[n])){
        query_result[n,i]= paste(dataset$key[n],dataset$x1[n],dataset$x2[n])
      }
    }
  }
)

cat("The query time for" , query_time, "query/queries is", time_of_query[3],"seconds")

#=====================================================================================

# [QUERY WITH R-TREE]

#generate a r_tree for testing using mean dataset

rtree_test = generate_r_tree(18,dataset_mean)

branch_index_test = generate_branch_index(18,dataset_mean)

# There is only one root in rtree, so we start it from branches after root.

for (i in 1:length(branch_index_test)) {
  for (n in 1:(branch_index_test[length(branch_index_test)-i])){
    as.numeric(unlist(rtree_test[(nrow(rtree_test)/branch_index_test[length(branch_index_test)-i]),(ncol(rtree_test)-i)]))[1]
    as.numeric(unlist(rtree_test[(nrow(rtree_test)/branch_index_test[length(branch_index_test)-i]),(ncol(rtree_test)-i)]))[2]
  }
}
