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
  # set SILO api key as a system variable.
  Sys.setenv(SILO_API_KEY = email)

  if(isFALSE(file.exists(".Renviron"))){
    cat("SILO_API_KEY='", email,"'","\n",sep = "",file = ".Renviron",append = TRUE)
    warning("R session may need to be restarted to take effect")

  }else{
    # Read .Renviron file if it already exists
    renvir <- file(".Renviron",open = "r")
    renv_contents <- readLines(renvir)

    # if API key already exists exit
    if(any(grepl("SILO_API_KEY=",renv_contents))) return(NULL)

    # if empty file pretend there is 1 empty line so no error on logical evaluation
    if(length(renv_contents)==0) renv_contents <- ""

    # if the last row is not an empty line add new line before key
    if(renv_contents[length(renv_contents)] == ""){
      cat("SILO_API_KEY='", email,"'","\n",sep = "",file = ".Renviron",append = TRUE)
    }else{
      cat("\nSILO_API_KEY='", email,"'","\n",sep = "",file = ".Renviron",append = TRUE)
    }
    close(renvir)
    warning("R session may need to be restarted to take effect")
  }
  return(NULL)
}
