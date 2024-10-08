#' Get TERN soil data providers
#'
#' @return data.frame of soil data providers
#' @export
#'
#' @examples
#' soil_data_providers <- get_soil_providers()
get_soil_providers <- function(){

  # Get the Providers data as a CSV file
  csvf <- tempfile()
  download.file('https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets?format=csv', csvf, mode="wb", quiet = T)
  provider_df <- read.csv(csvf)
  unlink(csvf)

  class(provider_df) <- c("SoilFedProviders",class(provider_df))

  return(provider_df)
}

#' Print soil providers data.frame
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#' @method print SoilFedProviders
#' @noRd
print.SoilFedProviders <- function(x, ...){
  x2 <- x[,colnames(x) %in% "Description" == FALSE]
  x2
}


#' Provide dataset infomation
#'
#' @param dataset character of length one giving the dataset to query information.
#'  Should be one of the DataSets in `get_soil_providers()`
#'
#' @return Formatted description of the dataset
#' @export
#'
#' @examples
#' dataset_info("TERNSurveillance")
dataset_info <- function(dataset){
  providers <- get_soil_providers()

  if(isFALSE(dataset %in% providers$DataSet)) stop("dataset not found.\n
                                                   use one of the datasets in get_soil_providers()")

  ds_info <- providers[providers$DataSet == dataset,]
  cat("DataSet:", ds_info$DataSet, "    Licence:", ds_info$LicenceType,"\n")
  cat("Organisation:", ds_info$OrgFullName,"\n")
  cat("Website:", ds_info$OrgURL,"\n")
  cat("   Contact person - ", ds_info$ContactPerson,"\n")
  cat("   Contact email  - ", ds_info$ContactEmail,"\n\n")

  cat("Metadata information available at: ", ds_info$MetaDataURI,"\n")
  cat("Longitude extent: ", ds_info$MinX," - ", ds_info$MaxX,"\n")
  cat("Latitude extent: ", ds_info$MaxY," - ", ds_info$MinY, "\n\n")

  cat("Dataset description\n",ds_info$Description,"\n")

}


#' Get Soil Properties
#'
#' @return data.frame of soil properties
#' @export
#'
#' @examples
#' get_soil_properties()
get_soil_properties <- function(){
  csvf <- tempfile()
  download.file('https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties?format=csv', csvf, mode="wb", quiet = T)
  prop_df <- read.csv(csvf)
  unlink(csvf)
  return(prop_df)
}

#' Get Soil Property groups
#'
#' @return data.frame of soil property groups
#' @export
#'
#' @examples
#' get_soil_properties()
get_soil_properties <- function(){
  csvf <- tempfile()
  download.file('https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties?format=csv', csvf, mode="wb", quiet = T)
  prop_df <- read.csv(csvf)
  unlink(csvf)
  return(prop_df)
}

