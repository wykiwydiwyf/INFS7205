#========Load dataset and some general analysis===============

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
  b_min = integer(0.4*m)
  b_max = m
  data_layer = 1
  while(b_max^data_layer < nrow(dataset_table)){
    dataset_table = data.frame(dataset_table, parent_layer = NA)
    for(i in seq(1,nrow(dataset_table),b_max^(data_layer))){
      child_nodes_min = min(dataset_table[i:min((i+(b_max^data_layer)),nrow(dataset_table)),2],dataset_table[i:min((i+(b_max^data_layer)),nrow(dataset_table)),3])
      child_nodes_max = max(dataset_table[i:min((i+(b_max^data_layer)-1),nrow(dataset_table)),2],dataset_table[i:min((i+(b_max^data_layer)-1),nrow(dataset_table)),3])
      for(n in seq(i,min((i+b_max^data_layer),nrow(dataset_table)),1)){
        dataset_table[n,ncol(dataset_table)] = paste(child_nodes_min,child_nodes_max)
      }
    }
    data_layer = data_layer + 1
  }
  dataset_table = data.frame(dataset_table, parent_layer1 = NA)
  child_nodes_min = min(dataset_table[1,1],dataset_table[1,2])
  child_nodes_max = max(dataset_table[nrow(dataset_table),1],dataset_table[nrow(dataset_table),2])
  for(n in seq(1,nrow(dataset_table),1)){
    dataset_table[n,ncol(dataset_table)] = paste(child_nodes_min,child_nodes_max)
  }
  return(dataset_table)
}

###################FUNCTION-3##A sub-function used in r-tree query function##################
break_node = function(node){
  return(c(as.numeric(unlist(strsplit(unlist(node),split=" "))[1]),as.numeric(unlist(strsplit(unlist(node),split=" "))[2])))
}

###################FUNCTION-4##Function query Rtree###################
rtree_query = function(mvalue,dataset_table1){
  # There is only one root in rtree, so we start it from branches after root.
  node_access = paste(c(1,nrow(dataset_table1)),collapse = " ")
  
  # root layer and first layer under root
  for (j in nrow(query_list)) {
    for (i in (ncol(dataset_table1)-1)){
      for (n in 1:branch_index_test[length(branch_index_test)]){#every parent branch
        if (max(query_list$x1[j],query_list$x2[j])<=max(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[1],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[2]) && min(query_list$x1[j],query_list$x2[j])>=min(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[1],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[2])){
          node_access = rbind(node_access,paste(c(mvalue^(length(branch_index_test)-1)*(n-1)+1,min(nrow(dataset_table1),mvalue^(length(branch_index_test)-1)*n)),collapse = " "))
        }
        else if (max(query_list$x1[j],query_list$x2[j])>=max(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[1],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[2]) && min(query_list$x1[j],query_list$x2[j])<=max(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[1],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[2])){
          node_access = rbind(node_access,paste(c(mvalue^(length(branch_index_test)-1)*(n-1)+1,min(nrow(dataset_table1),mvalue^(length(branch_index_test)-1)*n)),collapse = " "))
        }
        else if (max(query_list$x1[j],query_list$x2[j])>=min(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[1],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[2]) && min(query_list$x1[j],query_list$x2[j])<=min(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[1],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[2])){
          node_access = rbind(node_access,paste(c(mvalue^(length(branch_index_test)-1)*(n-1)+1,min(nrow(dataset_table1),mvalue^(length(branch_index_test)-1)*n)),collapse = " "))
        }
        else if (max(query_list$x1[j],query_list$x2[j])>=max(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[1],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[2]) && min(query_list$x1[j],query_list$x2[j])<=min(break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[1],break_node(dataset_table1[mvalue^(length(branch_index_test)-1)*(n-1)+1,i])[2])){
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
            if (max(query_list$x1[j],query_list$x2[j])<=max(break_node(dataset_table1[k,i])[1],break_node(dataset_table1[k,i])[2]) && min(query_list$x1[j],query_list$x2[j])>=min(break_node(dataset_table1[k,i])[1],break_node(dataset_table1[k,i])[2])){
              node_access = rbind(node_access,paste(c(k,min(nrow(dataset_table1),k+mvalue^(i-4)-1)),collapse = " "))
            }
            else if (max(query_list$x1[j],query_list$x2[j])>=max(break_node(dataset_table1[k,i])[1],break_node(dataset_table1[k,i])[2]) && min(query_list$x1[j],query_list$x2[j])<=max(break_node(dataset_table1[k,i])[1],break_node(dataset_table1[k,i])[2])){
              node_access = rbind(node_access,paste(c(k,min(nrow(dataset_table1),k+mvalue^(i-4)-1)),collapse = " "))
            }
            else if (max(query_list$x1[j],query_list$x2[j])>=min(break_node(dataset_table1[k,i])[1],break_node(dataset_table1[k,i])[2]) && min(query_list$x1[j],query_list$x2[j])<=min(break_node(dataset_table1[k,i])[1],break_node(dataset_table1[k,i])[2])){
              node_access = rbind(node_access,paste(c(k,min(nrow(dataset_table1),k+mvalue^(i-4)-1)),collapse = " "))
            }
            else if (max(query_list$x1[j],query_list$x2[j])>=max(break_node(dataset_table1[k,i])[1],break_node(dataset_table1[k,i])[2]) && min(query_list$x1[j],query_list$x2[j])<=min(break_node(dataset_table1[k,i])[1],break_node(dataset_table1[k,i])[2])){
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


#============How many times of query do you want to run?==============
query_time = 1   #<======input value here
query_list = data.frame(key= 1:query_time,x1= sample(1:100000,query_time),x2 =  sample(1:100000,query_time))
#=====================================RUN THIS TO GET A RANDOM NUMBER OF M========================================


# list of m values that will not cause underflow
m_list = NULL
for(m_val in 1:nrow(dataset)){
  b_min_val = as.integer(0.4*m_val)
  b_max_val = m_val
  if ((nrow(dataset) - as.integer(nrow(dataset)/b_max_val)*b_max_val) >= b_min_val) {
    m_list = c(m_list,m_val)
  }
}

#choose random five m value(m<100) from v_list

m_list_s = sample(m_list[which(m_list<100 & m_list>5)],1)


#generate a r_tree using different hypothesis


#=================#hypothesis 2#RUN THIS TO GENERATE A RTREE BY ORDER OF MIN=============================
##hypothesis 2##: use minimun of x1,x2 to order dataset is the most effecient R-tree:


for (i in 1:nrow(dataset)) {
  dataset$min[i] = min(dataset$x1[i],dataset$x2[i])
}

dataset_min = dataset[order(dataset$min), ]

rtree_test = generate_r_tree(m_list_s[1],dataset_min)

branch_index_test = generate_branch_index(m_list_s[1],dataset_min)


#=================#hypothesis 3#RUN THIS TO GENERATE A RTREE BY ORDER OF MAX=============================
##hypothesis 3##: use minimun of x1,x2 to order dataset is the most effecient R-tree:


for (i in 1:nrow(dataset)) {
  dataset$max[i] = max(dataset$x1[i],dataset$x2[i])
}

dataset_max = dataset[order(dataset$max), ]

rtree_test = generate_r_tree(m_list_s[1],dataset_max)

branch_index_test = generate_branch_index(m_list_s[1],dataset_max)


#=================#hypothesis 1#RUN THIS TO GENERATE A RTREE BY ORDER OF MEAN=============================
##hypothesis 1##: use mean of x1,x2 to order dataset is the most effecient R-tree:


for (i in 1:nrow(dataset)) {
  dataset$mean[i] = (dataset$x1[i]+dataset$x2[i])/2
}

dataset_mean = dataset[order(dataset$mean), ]

rtree_test = generate_r_tree(m_list_s[1],dataset_mean)

branch_index_test = generate_branch_index(m_list_s[1],dataset_mean)

#======================================[QUERY WITHOUT R-TREE]=============================================
# [QUERY WITHOUT R-TREE]

# A normal query with x1 and x2 in R scan all the rows in dataset.



query_result = data.frame(NA)

time_of_query = system.time(
  for (i in 1:nrow(query_list)){
    for(n in 1:nrow(dataset)){
      if(max(query_list$x1[i],query_list$x2[i]) <= max(dataset$x1[n],dataset$x2[n]) && min(query_list$x1[i],query_list$x2[i]) >= min(dataset$x1[n],dataset$x2[n])){
        query_result[n,i]= paste(dataset$key[n],dataset$x1[n],dataset$x2[n])
      }
      else if(min(query_list$x1[i],query_list$x2[i]) <= max(dataset$x1[n],dataset$x2[n]) && max(query_list$x1[i],query_list$x2[i]) >= max(dataset$x1[n],dataset$x2[n])){
        query_result[n,i]= paste(dataset$key[n],dataset$x1[n],dataset$x2[n])
      }
      else if(min(query_list$x1[i],query_list$x2[i]) <= min(dataset$x1[n],dataset$x2[n]) && max(query_list$x1[i],query_list$x2[i]) >= min(dataset$x1[n],dataset$x2[n])){
        query_result[n,i]= paste(dataset$key[n],dataset$x1[n],dataset$x2[n])
      }
      else if(max(query_list$x1[i],query_list$x2[i]) >= max(dataset$x1[n],dataset$x2[n]) && min(query_list$x1[i],query_list$x2[i]) <= min(dataset$x1[n],dataset$x2[n])){
        query_result[n,i]= paste(dataset$key[n],dataset$x1[n],dataset$x2[n])
      }
    }
  }
)
query_result = query_result[which(complete.cases(query_result)),]
cat("The query time for" , query_time, "query/queries is", time_of_query[3],"seconds")

#======================================[QUERY WITH R-TREE]================================================



# [QUERY WITH R-TREE]



#Let's time the rtree query:



time_of_rtree_query = system.time( for(i in 1){rtree_query_result = rtree_query(m_list_s[i],rtree_test)})

cat("The query time for" , query_time, "query/queries is", time_of_rtree_query[3],"seconds")

temp_set = rtree_test[rtree_query_result,(1:3)]
rtree_result = temp_set[order(temp_set$key),]


#=======================THE END=========================