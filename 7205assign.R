#========Load dataset and some general analysis===============

dataset = read.csv("7205dataset.csv",sep = " ",header = FALSE)

#why there is 100001 obs?

head(dataset)

#remove the first value "100000"

dataset = data.frame(key = dataset$V1[2:100001],x1 = dataset$V2[2:100001],y1 = dataset$V3[2:100001])

#generally look into dataset
summary(dataset)
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

  rtree_result = NULL
  # root layer and first layer under root
  for (j in 1:nrow(query_list)) {
    df = 0
    node_access = paste(c(1,nrow(dataset_table1)),collapse = " ")

    for (i in (ncol(dataset_table1)-1)){
      for (n in 1:branch_index_test[length(branch_index_test)]){#every parent branch
        if ((min(query_list$x1[j],query_list$x2[j])>=max(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[1],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[2])
              &&  max(query_list$x1[j],query_list$x2[j])>=max(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[1],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[2]))
            || (min(query_list$x1[j],query_list$x2[j])<=min(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[1],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[2])
              &&  max(query_list$x1[j],query_list$x2[j])<=min(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[1],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[2]))
            || (min(query_list$y1[j],query_list$y2[j])>=max(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[3],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[4])
              &&  max(query_list$y1[j],query_list$y2[j])>=max(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[3],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[4]))
            || (min(query_list$y1[j],query_list$y2[j])<=min(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[3],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[4])
              && max(query_list$y1[j],query_list$y2[j])<=min(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[3],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[4]))
                ){
        }else{node_access = rbind(node_access,paste(c(mvalue^(length(branch_index_test)-1)*(n-1)+1,min(nrow(dataset_table1),mvalue^(length(branch_index_test)-1)*n)),collapse = " "))
        }
      }
    }
    temp =2
    # layer above child nodes
    if (ncol(dataset_table1)>6){
      for (i in (ncol(dataset_table1)-2):5){
        for (n in temp:nrow(node_access)){
          for (k in seq(break_node(node_access[n])[1],break_node(node_access[n])[2],mvalue^(i-4))){
            if ((min(query_list$x1[j],query_list$x2[j])>=max(break_node(dataset_table1[k,i])[1],break_node(dataset_table1[k,i])[2])
                    && max(query_list$x1[j],query_list$x2[j])>=max(break_node(dataset_table1[k,i])[1],break_node(dataset_table1[k,i])[2]))
                || (max(query_list$x1[j],query_list$x2[j])<=min(break_node(dataset_table1[k,i])[1],break_node(dataset_table1[k,i])[2])
                    && min(query_list$x1[j],query_list$x2[j])<=min(break_node(dataset_table1[k,i])[1],break_node(dataset_table1[k,i])[2]))
                || (min(query_list$y1[j],query_list$y2[j])>=max(break_node(dataset_table1[k,i])[3],break_node(dataset_table1[k,i])[4])
                    && max(query_list$y1[j],query_list$y2[j])>=max(break_node(dataset_table1[k,i])[3],break_node(dataset_table1[k,i])[4]))
                || (max(query_list$y1[j],query_list$y2[j])<=min(break_node(dataset_table1[k,i])[3],break_node(dataset_table1[k,i])[4])
                    && min(query_list$y1[j],query_list$y2[j])<=min(break_node(dataset_table1[k,i])[3],break_node(dataset_table1[k,i])[4]))
            ){
            }
            else{node_access = rbind(node_access,paste(c(k,min(nrow(dataset_table1),k+mvalue^(i-4)-1)),collapse = " "))
            }
          }
        }
        temp = n+1
      } 
    }
    # child nodes


    for (n in temp:nrow(node_access)){
      for (k in seq(break_node(node_access[n])[1],break_node(node_access[n])[2],1)){
        if (max(query_list$x1[j],query_list$x2[j])>=dataset_table1[k,2]
            && min(query_list$x1[j],query_list$x2[j])<=dataset_table1[k,2]
            && max(query_list$y1[j],query_list$y2[j])>=dataset_table1[k,3]
            && min(query_list$y1[j],query_list$y2[j])<=dataset_table1[k,3]){
          node_access = rbind(node_access,paste(k,collapse = " "))
          df= df+1
        }
      }
    }
    rtree_result = c(rtree_result,df)
    
  }

  return(rtree_result)
}
  

#============Input the query do you want to run==============



query_list =data.frame( x1 = c(17840,33451,791,81921,75678,90965,69904,7187,34375,57144), 
                        x2 = c(18840,34451,1791,82921,76678,91965,70904,8187,35375,58144),
                        y1 = c(13971,29693,2515,94973,53545,11078,67308,56997,60942,24954),
                        y2 = c(14971,30693,3515,95973,54545,12078,68308,57997,61942,25954))



###################Create Rtree with m = 10 #######################


test = kmeans(dataset,centers= 10000, iter.max = 10 )   

dataset_c = data.frame(dataset, order = test[["cluster"]])
dataset_c = dataset_c[order(dataset_c$order), ]

rtree = generate_r_tree(10,dataset_c)

branch_index_test = generate_branch_index(10,dataset_c)


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

cat("The query time for" , "query/queries is", time_of_query[3],"seconds")

#======================================[QUERY WITH R-TREE]================================================



# [QUERY WITH R-TREE]



#Let's time the rtree query:



time_of_rtree_query = system.time( for(i in 1){rtree_query_result = rtree_query(10,rtree)})

cat("The query time for query/queries is", time_of_rtree_query[3],"seconds")
