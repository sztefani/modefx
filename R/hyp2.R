################################################

#HYPOTHESIS 2 MODE EFFECT
#IS THE SAME FOR EVERY ITEM

################################################

#Method: Generate a data set with the
#given distribution for the measure of mode effect per item being tested
#(measure is either difficulty, discrimination, or guessing)
#For each item, generate a data set of N= 5000
#One vector represents the item ID
#The other vector is one value from the normal distribution
#that has measurement's ME as mean and SD= 1
#The function returns the results of the ANOVA as a dataframe
###The following code for conducting one-way ANOVA was adapted from
###https://www.statology.org/one-way-anova-r/

#' A wrapper function for one-way ANOVA on mode effect (aov) with item ID as the predictor
#'
#' @return Return value of 1 indicates items contribute differently to mode effect (p <= 0.10)
#' @export hyp2
#' @importFrom crayon blue
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom stats TukeyHSD
#' @importFrom stats aov
#' @importFrom stats rnorm
#' @importFrom stats sd
#' @importFrom stats bartlett.test
#' @importFrom graphics boxplot
#'
#' @examples hyp2()
hyp2 <- function()
{
  IDs <- c()
  MEs <- c()
  if(nrow(e$df_ME) == 0)
  {
    message(blue("Must first call itemmodef to initialize values."))
    return ()
  }
  if(!e$init==2)
  {
    message(blue("Must first call hyp1"))
    return()
  }

  #print(df_ME)
  #simulate data representing 5000 students for each item's mode effect
  for (i in 1: nrow(e$df_ME))
  {
    dataID <- rep(e$df_ME$itemID[i], 5000)
    dataNorm <- rnorm(n= 5000, mean= as.numeric(e$df_ME$ME[i]), sd= e$SD[i])
    IDs <- append(IDs, dataID)
    MEs <- append(MEs, dataNorm)
  }

  #format IDs so that they are numbers
  IDs<- gsub("[^0-9]", "", IDs)
  IDs<- as.numeric(IDs)

  #id_ME: stores the mode effect data generated for each item
  id_ME <- as.data.frame(cbind(IDs, MEs))
  id_ME$MEs <- as.numeric(id_ME$MEs)

  ###Look at mean and SD of each item
  id_ME %>% group_by(IDs) %>% summarise(mean = mean(MEs), sd = sd(MEs))
  #####################################
  # Create several boxplots so that
  # the item labels can be seen clearly
  #####################################
  uqIDs<- c(unique(id_ME$IDs))
  rem<- 0
  if(length(uqIDs) > 10)
  {
    iters<- floor(length(uqIDs)/10)
    rem<- length(uqIDs)%%10
  }
  else
  {
    iters<- length(uqIDs)
  }

  for(i in 1:iters)
  {
    start<- (i-1)*10 + 1
    end<- (i-1)*10 + 10
    ###Look at boxplot for MEs per item
    boxplot(MEs ~ IDs, data= subset(id_ME, IDs %in% c(start:end)),
            main= "Mode Effect By Item",
            xlab= "Item ID",
            ylab= "Mode Effect",
            col= "steelblue",
            border= "black")
  }
  if(rem > 0)
  {
    boxplot(MEs ~ IDs, data= subset(id_ME, IDs %in% c(end+1:end+1+rem)),
            main= "Mode Effect By Item",
            xlab= "Item ID",
            ylab= "Mode Effect",
            col= "steelblue",
            border= "black")
  }
  ###
  id_ME$IDs<- as.character(id_ME$IDs)
  colnames(id_ME[1])<- "IDs"
  bartlettRES<- bartlett.test(MEs~IDs, id_ME)
  print(bartlettRES)

  e$modelAOV<- aov(MEs ~ IDs, data= id_ME)
  #summary(modelAOV)[[1]]: is a dataframe with the 5th column the
  #p value of the item IDs
  print(summary(e$modelAOV))

  e$init<- 3
  if(summary(e$modelAOV)[[1]][1,5] > 0.1)
  {
    message("/n")
    message(cat(blue("According to the one-way ANOVA, item ID is not statistically significant at predicting mode effect at the 0.10 significance level, p= ", summary(e$modelAOV)[[1]][1,5])))
    message(cat(blue("According to the one-way ANOVA with itemID as a predictor, the items do not individually show a difference in mode effect")))
    return (0)
  }
  else
  {
    message("")
    message(cat(blue("According to the one-way ANOVA, item ID is statistically significant at predicting mode effect at the 0.10 significance level, p= ", summary(e$modelAOV)[[1]][1,5])))
    message(cat(blue("The items individually show a difference in mode effect")))
    return (1)
  }

}

