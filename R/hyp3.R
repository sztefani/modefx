################################################

#HYPOTHESIS 3
#MODE EFFECT EXISTS AND IS NOT THE SAME
#FOR EVERY ITEM

################################################
#hyp3() conducts Tukey's test for post-hoc, multiple comparisons
#Code to conduct Tukey's test was adapted from
#https://lifewithdata.com/2023/08/23/how-to-perform-tukeys-test-in-r/
#It requires the package ggplot2
#cgraphs>> an n by n matrix (global) storing cluster-membership
#' A wrapper function for Tukey's test (TukeyHSD) for post-hoc, multiple comparisons of item-level mode effect differences
#'
#' @return A dataframe showing item-by-item comparisons of mode effect difference
#' @export hyp3
#' @importFrom crayon blue
#' @importFrom stats as.formula
#'
#' @examples hyp3()

hyp3 <- function()
{

  if(!e$init==3)
  {
    message(blue("Must first call hyp2 before hyp3"))
    return ()
  }
  modelTUK<- TukeyHSD(e$modelAOV, conf.level= 0.95)
  e$modelTUK_df <- as.data.frame(modelTUK$IDs)
  names(e$modelTUK_df)[4] <- "pval"
  names(e$modelTUK_df)
  e$modelTUK_df$pval <- round(e$modelTUK_df$pval, 2)
  e$modelTUK_df <- e$modelTUK_df[order(-e$modelTUK_df$pval), ]
  View(e$modelTUK_df)
  e$cgraphs <- .setClusters()
  View(e$cgraphs)
  e$init<- 4
  return (e$modelTUK_df)

}


#Terminology
# A cluster refers to all items that show non-significant
# difference in their mode effect
# Neighbour refers to the second item paired to the
# first item in Tukey's test
# ex. For MC1-MC2, MC2 is a neighbour of MC1
.setClusters <- function(s= "")
{
  .set_diffME_P(e$modelTUK_df)
  n_by_n <- .create_nBYn()
  n <- length(n_by_n)
  #compgroupNUM: placeholder for numeric label of complete graphs (positive)
  #noncgroupNUM: placeholder for numeric label of non-complete graphs (negative)
  compgroupNUM<- 0
  noncgroupNUM<- 0

  # label all the neighbourhoods within diffME_P
  # as a complete graph or not a complete graph
  completed<- c()
  for (i in 1: nrow(e$diffME_P))
  {
    if(e$diffME_P[i, 1] %in% completed)
    {
      next
    }
    cluster <- c()
    #get the cluster of item IDS of the element at position i
    cluster <- .get_cluster(clustR= c(e$diffME_P[i, 1]))
    completed<- append(completed, cluster)
    if(length(cluster) <= 2)
    {
      next
    }
    #check if this cluster forms a complete graph
    #Clusters with only two elements do not count
    #as complete graphs
    complgraph<- .complGRAPH(cluster)

    if(complgraph)
    {
      compgroupNUM <- compgroupNUM + 1
      groupNUM <- compgroupNUM
    }
    else
    {
      noncgroupNUM <- noncgroupNUM - 1
      groupNUM <- noncgroupNUM
    }

    n_by_n<- .update_nBYn(cluster, groupNUM, n_by_n)

  }


  e$cgraphs<- as.data.frame(n_by_n)

}


.get_diffME_P <- function()
{
  return ( e$diffME_P )
}


#get_diffME_P() creates two columns of itemIDS containing the
#first and second entry respectively of each of the items paired
#in the Tukey test with p > 0.10
#modelTUK_df>> a global variable representing the results of
#Tukey's test as a dataframe
.set_diffME_P<- function(s= "")
{

  #item_1_2: a vector of the item names with non-significant
  #mode effect difference (p > 0.10)
  item_1_2 <- c()
  itemID_1 <- c()
  itemID_2 <- c()

  rowNames<- row.names(e$modelTUK_df)
  rowNames;
  for (i in 1: nrow(e$modelTUK_df))
  {
    #if p-val is greater than 0.10 then add
    #the name of the two items to the list
    if(e$modelTUK_df[i, 4] > 0.99)
    {
      item_1_2 <- rowNames[i]

      #get the string that occurs before the minus
      #sign
      itemID1 <- sub("-.*", "", item_1_2)
      itemID_1 <- append(itemID_1, itemID1)

      #get the string that occurs after the minus
      #sign
      itemID2 <- sub(".*-", "", item_1_2)
      itemID_2 <- append(itemID_2, itemID2)
    }
  }

  e$diffME_P <- as.data.frame(cbind(itemID_1, itemID_2))
  return (1)

}



#create_nBYn: creates an n by n matrix of 0s with column names
#each representing the unique items in pairs that were reported as having
#non-significantly different mode effect by Tukey's test in hypothesis 3
.create_nBYn <- function()
{
  #union returns all entries that are in either
  #of two objects. It includes each entry once
  allentries <- union(e$diffME_P[, 1], e$diffME_P[, 2])
  n<- length(allentries)

  #make an n by n matrix with all zeros
  n_by_n <- do.call("rbind", replicate(n, rep(0, n), simplify= FALSE))
  colnames(n_by_n) <- allentries
  rownames(n_by_n) <- allentries

  return (n_by_n)

}


.get_cluster <- function(clustR= c(), neighbs= c())
{

  neighbs<- c()
  #get a list of those neighbours of elements in diffME_P
  #that are not already in clustR (see get_neighbours)
  for(i in 1: length(clustR))
  {
    neighbs <- append(neighbs, setdiff(.get_neighbours(clustR[i]), clustR))
  }

  #base case
  if(length(neighbs) == 0)
  {
    return (clustR)
  }
  else
  {
    clustR<- append(clustR, setdiff(neighbs, clustR))
    clustR<- .get_cluster(clustR, neighbs)
  }

  return (clustR)

}


#get neighbours(itm2check) returns all neighbours of the item passed
#including the item passed
#itm2check: a single item name (string)
.get_neighbours <- function(itm2check= "")
{

  neighbours <- c(itm2check)
  for(i in 1: nrow(e$diffME_P))
  {
    addnew<- 0
    #check if itm2check is in either the first or second entry of diffME_P
    if(itm2check == e$diffME_P[i, 1] & !e$diffME_P[i, 2] %in% neighbours)
    {
      addnew <- 2
    }
    else if(itm2check== e$diffME_P[i, 2] & !e$diffME_P[i, 1] %in% neighbours)
    {
      addnew <- 1
    }
    if(addnew >0)
    {
      newneighb <- e$diffME_P[i, addnew]
      neighbours <- append(neighbours, newneighb)
    }

  }
  return (neighbours)

}


#update_nBYn goes through each pair of itemIDs from possible_INV
#and changes their entry in nBYn to group_NUM
#possible_INV: a vector of item IDs (strings) that
#belong to the same cluster
#nBYn: a variable that helps to build cgraphs
.update_nBYn <- function(possible_INV= c(), group_NUM= 0, nBYn= matrix())
{

  if(length(possible_INV) <= 2)
  {
    return (NULL)
  }
  #Change all pair entries in possible_inv to groupNUM
  for (m in 1: (length(possible_INV)))
  {
    for(n in 1: (length(possible_INV)))
    {
      nBYn[possible_INV[m] ,possible_INV[n]] <- group_NUM
    }
  }
  return (nBYn)

}


# complGRAPH(itemLIST) investigates if the
# cluster forms a 'complete graph'
# (i.e. a 'graph' of vertices and edges where each vertex
# connects to every other vertex)
# It returns 1 if every possible pair
# in the itemLIST is also in diffME_p (p > 0.10)

# itemLIST: a vector of items names (strings) representing a given cluster
# diffME_p>> a global dataframe that stores two columns of item IDs.
# Each item belongs to a pair of items from Tukey's test
# that do not show significantly different
# mode effect  (p > 0.10)
.complGRAPH <- function(itemLIST= c())
{
  #next check for completeness within possible_inv
  if(length(itemLIST) <=2)
  {
    return (0)
  }
  for(i in 1: (length(itemLIST)-1))
  {
    pairIN<- FALSE
    for(j in (i+1): length(itemLIST))
    {
      counter<- 1
      #check if the i, j pair of possible_inv
      #is in diffME_P
      for(m in 1: nrow(e$diffME_P))
      {
        counter<- counter + 1
        if((e$diffME_P[m, 1]== itemLIST[i] & e$diffME_P[m, 2]== itemLIST[j])|
           (e$diffME_P[m, 1]== itemLIST[j] & e$diffME_P[m, 2]== itemLIST[i]))
        {
          pairIN<- TRUE
          break
        }
      }
      if(!pairIN)
      {
        return (0)
      }
    }

  }

  return (1)

}



