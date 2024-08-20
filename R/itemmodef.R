
e<- new.env()
e$cgraphs<- data.frame()
e$modelTUK_df<- data.frame()
#df_ME cols: itemID, mode effect
e$df_ME<- data.frame()
e$diffME_P<- data.frame()
e$df_BM<- data.frame()
e$modelAOV<- NULL
e$init<- 0
e$SD<- c()

#' Initializing function that accepts a dataframe with paper and computer based measures for either item-difficulty,
#' -discrimination, or -guessing.

#' @param pbcbDF A dataframe with three columns. The first column is item ID (string). The second is the paper-based measure (numeric) and the third is the computer-based measure (numeric).
#' The measures should arrive to this package adjusted by the pooled standard deviation of the PBA 'and CBA IRT measure per item.
#' @return Returns 1 if initialization is successful.
#' @importFrom fastDummies dummy_cols
#' @export itemmodef
#'
#' @examples mydata= data.frame(itemID= c("MC1", "MC2", "MC3"),
#' PBAvals= c(-0.25, 0.00, 0.25), CBAvals= c(-0.30, 0.00, 0.30))
#' itemmodef (pbcbDF= mydata)
#'
itemmodef <- function( pbcbDF= data.frame())
{
  e$init<- 0
  e$df_ME <- pbcbDF[, 1]
  if(!(is.numeric(pbcbDF[, 1]) & is.numeric(pbcbDF[, 2]) & is.numeric(pbcbDF[, 3]) & is.numeric(pbcbDF[, 4])))
  {
    message(blue("Columns entered must all be vectors of class 'numeric'"))
    return()
  }
  #multi-group IRT approach: get mode effect per item by subtracting
  #the PBA from the CBA measure
  e$df_ME <- as.data.frame(cbind(e$df_ME, as.numeric((as.numeric(pbcbDF[, 3]) - as.numeric(pbcbDF[, 2])))))

  colnames(e$df_ME) <- c("itemID", "ME")
  #df_BMP/C cols: measure, mode, itemID, [name of all items]
  df_BMP <- as.data.frame(as.numeric(pbcbDF[, 3]))
  df_BMP <- cbind(df_BMP, mode= c(rep(0, nrow(df_BMP))))
  df_BMP <- cbind(df_BMP, itemID= pbcbDF[, 1])
  colnames(df_BMP) <- c("measure", "MODE", "itemID")

  df_BMC <- as.data.frame(as.numeric(pbcbDF[, 2]))
  df_BMC <- cbind(df_BMC, mode= c(rep(1, nrow(df_BMC))))
  df_BMC <- cbind(df_BMC, itemID= pbcbDF[, 1])
  colnames(df_BMC) <- c("measure", "MODE", "itemID")

  #df_BM: holds item measure, mode, and ID
  e$df_BM <-rbind(df_BMP, df_BMC)
  e$init<- 1
  e$SD<- pbcbDF[, 4]
  #Dummy encode the itemID using the fastDummies package
  #Kaplan, J. & Schlegel, B. (2023). fastDummies: Fast Creation of Dummy (Binary) Columns and Rows from Categorical Variables. Version 1.7.1. URL: https://github.com/jacobkap/fastDummies, https://jacobkap.github.io/fastDummies/
  e$df_BM <- dummy_cols(e$df_BM, select_columns= c('itemID'), remove_selected_columns= FALSE)
  e$df_BM<- as.data.frame(e$df_BM)


}
