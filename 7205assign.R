#========Load dataset and some general analysis===============

dataset = read.csv("7205dataset.csv",sep = " ",header = FALSE)

#why there is 100001 obs?

head(dataset)

#remove the first value "100000"

dataset = data.frame(key = dataset$V1[2:100001],x1 = dataset$V2[2:100001],y1 = dataset$V3[2:100001])

#generally look into dataset
summary(dataset)

#============How many times of query do you want to run?==============
query_time = 10   #<======input value here
query_list = data.frame(key= 1:query_time,x1= sample(1:100000,query_time),x2 =  sample(1:100000,query_time),y1= sample(1:100000,query_time),y2 =  sample(1:100000,query_time))

query_list =data.frame(c(17840,18840,13971,14971),
                       c(33451,34451,29693,30693),
                       c(791,1791,2515,3515),
                       c(81921,82921,94973,95973),
                       c(75678,76678,53545,54545),
                       c(90965,91965,11078,12078),
                       c(69904,70904,67308,68308),
                       c(7187,8187,56997,57997),
                       c(34375,35375,60942,61942),
                       c(57144,58144,24954,25954))


colnames(query_list)= c("x1","x2","y1","y2")

###################FUNCTION-1##Function creates index for R-tree###############

# if there is m branches in parent nodes
#  we put b_max data points in each child node
# for simplity, only consider no underflow case
# Create a table display how many branches and sub-braches in the r-tree as a "guide" to query
generate_branch_index = function(m,dataset_table){
  branch_number = ceiling(nrow(dataset_table)/m)
  while (branch_number[length(branch_number)] > m ){
    branch_number = c(branch_number,ceiling(branch_number[length(branch_number)]/m))
  }
  branch_number = c(nrow(dataset_table),branch_number)
  return(branch_number)
}


###################FUNCTION-2##Function creates R-tree###########################

generate_r_tree = function(m,dataset_table) {
  b_max = m
  data_layer = 1
  while(b_max^data_layer < nrow(dataset_table)){
    dataset_table = data.frame(dataset_table, parent_layer = NA)
    for(i in seq(1,nrow(dataset_table),b_max^(data_layer))){
      child_nodes_x_min = min(dataset_table[i:min((i+(b_max^data_layer)),nrow(dataset_table)),2])
      child_nodes_x_max = max(dataset_table[i:min((i+(b_max^data_layer)-1),nrow(dataset_table)),2])
      child_nodes_y_min = min(dataset_table[i:min((i+(b_max^data_layer)),nrow(dataset_table)),3])
      child_nodes_y_max = max(dataset_table[i:min((i+(b_max^data_layer)-1),nrow(dataset_table)),3])
      for(n in seq(i,min((i+b_max^data_layer),nrow(dataset_table)),1)){
        dataset_table[n,ncol(dataset_table)] = paste(child_nodes_x_min,child_nodes_x_max,child_nodes_y_min,child_nodes_y_max)
      }
    }
    data_layer = data_layer + 1
  }
  dataset_table = data.frame(dataset_table, parent_layer1 = NA)
  child_nodes_x_min = dataset_table[1,1]
  child_nodes_x_max = dataset_table[nrow(dataset_table),1]
  child_nodes_y_min = dataset_table[1,2]
  child_nodes_y_max = dataset_table[nrow(dataset_table),2]
  for(n in seq(1,nrow(dataset_table),1)){
    dataset_table[n,ncol(dataset_table)] = paste(child_nodes_x_min,child_nodes_x_max,child_nodes_y_min,child_nodes_y_max)
  }
  return(dataset_table)
}




###################FUNCTION-3##A sub-function used in r-tree query function##################
break_node = function(node){
  return(c(as.numeric(unlist(strsplit(unlist(node),split=" "))[1]),as.numeric(unlist(strsplit(unlist(node),split=" "))[2]),as.numeric(unlist(strsplit(unlist(node),split=" "))[3]),as.numeric(unlist(strsplit(unlist(node),split=" "))[4])))
}

###################FUNCTION-4##Function query Rtree###################
rtree_query = function(mvalue,dataset_table1){
  # There is only one root in rtree, so we start it from branches after root.
  node_access = paste(c(1,nrow(dataset_table1)),collapse = " ")
  
  # root layer and first layer under root
  for (j in nrow(query_list)) {
    for (i in (ncol(dataset_table1)-1)){
      for (n in 1:branch_index_test[length(branch_index_test)]){#every parent branch
        if (max(query_list$x1[j],query_list$x2[j])>=max(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[1],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[2])
            && min(query_list$x1[j],query_list$x2[j])<=min(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[1],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[2])
            && max(query_list$y1[j],query_list$y2[j])>=max(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[3],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[4])
            && min(query_list$y1[j],query_list$y2[j])<=min(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[3],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[4])
            ){
          node_access = rbind(node_access,paste(c(mvalue^(length(branch_index_test)-1)*(n-1)+1,min(nrow(dataset_table1),mvalue^(length(branch_index_test)-1)*n)),collapse = " "))
        }
      }
    }
    temp =2
    temp1 = 1
    # layer above child nodes
    if (ncol(dataset_table1)>6){
      for (i in (ncol(dataset_table1)-2):5){
        for (n in temp:nrow(node_access)){
          for (k in seq(break_node(node_access[n])[1],break_node(node_access[n])[2],mvalue^(i-4))){
            if (max(query_list$x1[j],query_list$x2[j])>=max(break_node(dataset_table1[k,i])[1],break_node(dataset_table1[k,i])[2])
                && min(query_list$x1[j],query_list$x2[j])<=min(break_node(dataset_table1[k,i])[1],break_node(dataset_table1[k,i])[2])){
              node_access = rbind(node_access,paste(c(k,min(nrow(dataset_table1),k+mvalue^(i-4)-1)),collapse = " "))
            }
          }
        }
        temp = n+1
      } 
    }
    # child nodes
    for (n in temp:nrow(node_access)){
      for (k in seq(break_node(node_access[n])[1],break_node(node_access[n])[2],1)){
        if (max(query_list$x1[j],query_list$x2[j])<=max(dataset_table1[k,2],dataset_table1[k,3]) && min(query_list$x1[j],query_list$x2[j])>=min(dataset_table1[k,2],dataset_table1[k,3])){
          node_access = rbind(node_access,paste(k,collapse = " "))
        }
        else if (max(query_list$x1[j],query_list$x2[j])>=max(dataset_table1[k,2],dataset_table1[k,3]) && min(query_list$x1[j],query_list$x2[j])<=max(dataset_table1[k,2],dataset_table1[k,3])){
          node_access = rbind(node_access,paste(k,collapse = " "))
        }
        else if (max(query_list$x1[j],query_list$x2[j])>=min(dataset_table1[k,2],dataset_table1[k,3]) && min(query_list$x1[j],query_list$x2[j])<=min(dataset_table1[k,2],dataset_table1[k,3])){
          node_access = rbind(node_access,paste(k,collapse = " "))
        }
        else if (max(query_list$x1[j],query_list$x2[j])>=max(dataset_table1[k,2],dataset_table1[k,3]) && min(query_list$x1[j],query_list$x2[j])<=min(dataset_table1[k,2],dataset_table1[k,3])){
          node_access = rbind(node_access,paste(k,collapse = " "))
        }
      }
      temp = n+1
    }
    rtree_result = as.numeric(node_access[temp:length(node_access)])
    
    return(rtree_result)
  }
}


###################Create Rtree with m = 50 #######################

library(NbClust)

test = kmeans(dataset,centers= 2000, iter.max = 50 )   

dataset_c = data.frame(dataset, order = test[["cluster"]])
dataset_c = dataset_c[order(dataset_c$order), ]

rtree = generate_r_tree(50,dataset_c)

branch_index_test = generate_branch_index(50,dataset_c)


#======================================[QUERY WITHOUT R-TREE]=============================================
# [QUERY WITHOUT R-TREE]

# A normal query with x1 and x2 in R scan all the rows in dataset.






query_result = NULL
time_of_query = system.time(
  for (i in 1:nrow(query_list)){
    tem = 0
    for(n in 1:nrow(dataset)){
      if(max(query_list$x1[i],query_list$x2[i]) >= dataset$x1[n] 
         && min(query_list$x1[i],query_list$x2[i]) <= dataset$x1[n]
         &&max(query_list$y1[i],query_list$y2[i]) >= dataset$y1[n]
         && min(query_list$y1[i],query_list$y2[i]) <= dataset$y1[n]){
        tem = tem +1
      }
    }
    query_result= c(query_result,tem)
  }
)
query_result = query_result[which(complete.cases(query_result)),]
cat("The query time for" , "query/queries is", time_of_query[3],"seconds")

#======================================[QUERY WITH R-TREE]================================================



# [QUERY WITH R-TREE]



#Let's time the rtree query:



time_of_rtree_query = system.time( for(i in 1){rtree_query_result = rtree_query(m_list_s[i],rtree_test)})

cat("The query time for" , query_time, "query/queries is", time_of_rtree_query[3],"seconds")

temp_set = rtree_test[rtree_query_result,(1:3)]
rtree_result = temp_set[order(temp_set$key),]

