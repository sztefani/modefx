#getRange(unqITEMS) returns the range of mode effect in df_ME
#of items listed in unqITEMS
#unqITEMS: a vector of item names (strings) representing items in
#a given cluster
#df_ME>> global dataframe storing mode effect of all items
#' getRange
#'
#' @param clusterN The number of the cluster from getClusters()
#'
#' @return Range of mode effect of the given cluster
#' @export getRange
#' @importFrom crayon blue
#'
#' @examples getRange(1)
getRange<- function(clusterN= -100000)
{
  if(!e$init==4)
  {
    message(blue("Must first call hyp3"))
    return ()
  }
  if(clusterN == -100000 | clusterN == 0)
  {
    message(blue("Please enter a valid cluster number. See getClusters()"))
    return (NULL)
  }
  else
  {
    itemsin<- getClustItems(clusterN)
    return (.getRangeItems(itemsin))
  }

}


.getRangeItems <- function(unqITEMS= c())
{
  #get the rows that are in unqITEMS
  df_ME_temp<- e$df_ME
  df_ME_temp$itemID<- gsub("[^0-9]", "", df_ME_temp$itemID)
  rowsin<- e$df_ME[df_ME_temp$itemID %in% unqITEMS,  ]
  rowsin<- as.vector(rowsin$ME)
  minME<- min(as.numeric(rowsin))
  maxME<- max(as.numeric(rowsin))
  return (c(minME, maxME))
}
