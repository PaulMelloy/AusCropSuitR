#' Set API key
#'
#' @param email character string, users email
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' set_API_key("john.smith at email.com")
#' }
#'
set_API_key<- function(email){

  if(isFALSE(file.exists(".Renviron"))){
    cat("SILO_API_KEY='", email,"'","\n",sep = "",file = ".Renviron",append = TRUE)
  }else{
    renvir <- file(".Renviron",open = "r")
    renv_contents <- readLines(renvir)

    if(any(grepl("SILO_API_KEY=",renv_contents))) return(NULL)

    # if empty file pretend there is 1 empty line so no error on logical evaluation
    if(length(renv_contents)==0) renv_contents <- ""

    # if the last row is not an empty line add new line before key
    if(renv_contents[length(renv_contents)] == ""){
      cat("SILO_API_KEY='", email,"'","\n",sep = "",file = ".Renviron",append = TRUE)
      close(renvir)
    }else{
      cat("\nSILO_API_KEY='", email,"'","\n",sep = "",file = ".Renviron",append = TRUE)
      close(renvir)
    }
  }
  return(NULL)
}
