#get_featureME() conducts multiple linear regression using
#item factors and ME per item

#filename: stores the name of a flat file (csv) that has the
#item features as column names. Each row represents an item
#If the item has the feature then the entry is 1, otherwise 0
#It returns a summary of the results of the linear regression
#as a dataframe. ex. "features_SCI6EN.csv"
#' Conducts multiple linear regression using item factors and mode effect per item
#'
#' @param filename A file path to a binary csv file that stores features of items as columns. The first column stores item ID as a string.
#'
#' @return Returns the results of the multiple linear regression as a dataframe
#' @export
#' @importFrom crayon blue
#' @importFrom utils View
#' @importFrom utils read.csv
#' @importFrom stats lm
#' @importFrom stats interaction.plot
#'
#'
#' @examples get_featureME("featurefile.csv")
get_featureME <- function(filename= "")
{

  if(!file.exists(filename))
  {
    message(blue("Invalid filename"))
    return ()
  }
  else
  {
    #A. import the factors file to a data frame
    #itemFEATS holds the binary dataframe with col headers
    itemFEATS <- read.csv(filename, stringsAsFactors=T)
    colnames(itemFEATS)[1]<- "itemID"
    itemFEATS$itemID<- gsub("[^0-9]", "", itemFEATS$itemID)
    colNAMES<- colnames(itemFEATS)
    colNUM<- length(colNAMES)
    invGROUP<- getInvGroup()
    #check format of itemFEATS
    #The entries other than the first column or row should all be either 0 or 1
    for(i in 2: nrow(itemFEATS))
    {
      for(j in 2: ncol(itemFEATS))
      {
        if(!(itemFEATS[i, j]==0 | itemFEATS[i, j]==1))
        {
          message(blue("Entries of file must be either 0 or 1. The first column should list the item names. The first row should list the item features"))
          return()
        }
      }
    }
    #Leave out those items marked as possibly invariant
    if(invGROUP > -10000)
    {
      possinv<- as.numeric(as.character(getClustItems(invGROUP)))
      df_ME_temp<- e$df_ME
      df_ME_temp$itemID<- gsub("[^0-9]", "", df_ME_temp$itemID)
      df_ME1 <- subset(df_ME_temp, !(df_ME_temp$itemID %in% possinv))
      itemFEATS1 <- subset(itemFEATS, !(df_ME_temp$itemID %in% possinv))

    }
    else
    {
      df_ME_temp<- e$df_ME
      df_ME_temp$itemID<- gsub("[^0-9]", "", df_ME_temp$itemID)
      df_ME1<- df_ME_temp
      itemFEATS1<- itemFEATS
    }



    #B. append the ME of each item to itemFEATS
    itemFEATS_ME<- as.data.frame(cbind(itemFEATS1, df_ME1$ME))

    colnames(itemFEATS_ME)[ncol(itemFEATS_ME)]<- "itemME"



    #C.
    #run the multiple linear regression
    #first check for interaction fx

    colNAMES<- colnames(itemFEATS_ME)

    colNUM<- length(colNAMES)

    if(colNUM > 4)
    {
      for(i in 2: (colNUM-2))
      {

        for(j in (i+1): (colNUM-1))
        {

          interaction.plot(x.factor = itemFEATS_ME[, i], #x-axis variable
                           trace.factor = itemFEATS_ME[, j], #variable for lines
                           response = itemFEATS_ME[, colNUM], #y-axis variable
                           fun = mean, #metric to plot
                           ylab = "Mode Effect",
                           xlab = colNAMES[i],
                           col = c("pink", "blue"),
                           lty = 1, #line type
                           lwd = 2, #line width
                           trace.label = colNAMES[j])
        }
      }
    }

    formulaTERMS<- ""
    for(i in 2: (colNUM-1))
    {
      formulaTERMS<- paste(formulaTERMS, colNAMES[i])
      if(!i==colNUM-1)
      {
        formulaTERMS<- paste(formulaTERMS, "+")
      }
    }
    #append interaction terms to linear regression formula
    for(i in 2: (colNUM-2))
    {
      formulaTERMS<- paste(formulaTERMS, "+", collapse=NULL)
      for(j in (i+1): (colNUM-1))
      {

        formulaTERMS<- paste0(formulaTERMS, " ", colNAMES[i], ":", colNAMES[j])
        if(!(j==(colNUM-1)))
        {
          formulaTERMS<- paste(formulaTERMS, "+", collapse= NULL)
        }
      }
    }
    #message(blue(paste("formula: itemME ~", formulaTERMS)))
    message(blue(formulaTERMS))
    mainformula<- as.formula(paste("itemME ~", formulaTERMS))
    linearmodelME <- lm(mainformula, data= itemFEATS_ME)
    print(summary(linearmodelME))
    return ( summary(linearmodelME) )
  }


}
