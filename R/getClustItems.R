#getCLUSTIEMS: gets the name of the items that have
#the cluster number that is passed as an argument
#groupN: a numerical value from the global variable cgraphs
#' Gets the ID of the items that have the cluster number that is passed as an argument
#'
#' @param groupN One of the cluster numbers. See getClusters()
#'
#' @return Returns a vector of itemIDs as characters
#' @export getClustItems
#' @importFrom crayon blue
#'
#' @examples getClustItems(2)
getClustItems <- function(groupN= 0)
{
  clustITEMS<- c()
  if(!e$init==4)
  {
    message(blue("Must first call hyp3"))
    return()
  }
  if(groupN==0)
  {
    message(blue("Please enter a valid group number"))
    return ()
  }
  if(!groupN == 0)
  {
    #get every item belonging to groupN
    for (rowname in rownames(e$cgraphs))
    {
      for(colname in colnames(e$cgraphs))
      {
        if(e$cgraphs[rowname, colname] == groupN & !colname %in% clustITEMS)
        {
          clustITEMS <- append(clustITEMS, colname)
          break
        }
      }
    }

  }
  else
  {
    message("0 is not a valid cluster number")
  }


  return (sort(as.character(factor(clustITEMS))))

}
