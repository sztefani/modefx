# cgraphs>> an n by n dataframe (global) storing cluster-membership
#' Returns clustered-items in a dataframe of crossed itemIDs where paired-items with the same label do not show different mode effect
#'
#' @return Returns a dataframe that indicates membership to clusters. A negative label indicates that the cluster does not form a complete graph, positive indicates a complete graph, and 0 indicates non-membership to a cluster of more than two items
#' @export
#' @importFrom crayon blue
#' @examples getClusters()
getClusters <- function()
{

  if(!e$init==4)
  {
    message(blue(("Must first call hyp3")))
    return()
  }
  View(e$cgraphs)
  return (e$cgraphs)
}
