################################################

#HYPOTHESIS 1 MODE EFFECT IS 0

################################################
#' A wrapper function for lfe::waldtest to test the null hypothesis
#' that the overall mode effect equals 0
#'
#' @return 1 if overall mode effect is statistically different from zero
#' @export hyp1
#' @importFrom crayon blue
#' @importFrom lfe felm
#' @importFrom lfe waldtest
#'
#' @examples hyp1()

hyp1 <- function()
{

  if(!e$init ==1)
  {
    message(blue(("Must first call itemmodef to initialize values.")))
    return()
  }
  #Create Linear Model for difficulty
  linearmodel <- lfe::felm(measure~ MODE, data= e$df_BM)
  #Use the Wald test to see if the parameter for mode effect equals 0
  waldtesti<- lfe::waldtest(linearmodel, R= 1)

  #If chi-squared test p value is significant then the mode effect does not show difference from zero"
  #Return 0
  chiP<- waldtesti['p']
  e$init<- 2
  if(chiP > 0.1)
  {
    message("")
    message(cat(blue("The chi-squared p-value of the Wald-test is: ", chiP)))
    message(cat(blue("The items do not show overall mode effect")))
    return (0)
  }
  else
  {
    message("")
    message(cat(blue("The chi-squared p-value of the Wald-test is: ", chiP)))
    message(cat(blue("The items show overall mode effect")))
    return (1)
  }

}


