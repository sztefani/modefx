#getInvGroup() looks through mode effect differences of items
#that do not show statistically
#significant difference (p > 0.10) to check
#if they might be invariant. The following process
#notes that if ME= 0 for certain items then we should be able
#to represent the difference in mode effect of
#invariant items as a complete
#graph, meaning that every item-vertex
#of the graph is connected to every other
#item-vertex
#See https://en.wikipedia.org/wiki/Complete_graph
#' Finds the numeric label of the cluster of items that has range of mode effect inclusive of zero
#'

#' @return The numeric label of the cluster of items that has range of mode effect inclusive of zero
#' @export
#' @importFrom crayon blue
#'
#' @examples getInvGroup()
getInvGroup <- function()
{
  if(!e$init==4)
  {
    message(blue("Must first call hyp3"))
    return()
  }
  if(nrow(e$cgraphs) > 0)
  {
    #get the min and max value in cgraphs
    mingroup<- min(e$cgraphs)
    maxgroup<- max(e$cgraphs)
    #find the invariant items
    #For each groupnum between minval and maxval
    #get every unique item with that groupnum
    #Using the list of unique items, find the range
    #of mode effect for those items in MEvals= df_ME
    for(group_num in mingroup:maxgroup)
    {
      if(!group_num ==0)
      {
          rangeME<- getRange(group_num)
          if(.containsZero(rangeME))
          {
            return (group_num)
          }

      }

    }

    message("")
    message(cat(blue("No invariant group found")))
    return (-10000)
  }
  else
  {
    message(cat(blue("Run hyp3() before calling this method.")))
    return(-10001)
  }

}


#containsZero(rangeMM) checks if the range includes zero. If so, then
#return the range
#rangeMM: a two-length vector (numeric) containing the lower bound
#of the range of mode effect in a cluster and the upper bound
.containsZero <- function(rangeMM= c())
{

  if(rangeMM[1] <= rangeMM[2] & rangeMM[1] <= 0 & rangeMM[2] >=0)
  {
    return (1)
  }

  return (0)

}
