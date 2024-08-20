#' Title initenv
#'
#' @return No return value. Initializes the global environment.
#' @export initenv
#'
#' @examples initenv()
initenv<- function()
{
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
}
