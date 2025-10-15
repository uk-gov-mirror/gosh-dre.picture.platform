#' Provide R and OS version
#'
#' Description
#'
#' @family utils_logger
#' @export
get_system_info <- function(){
  system_info <- list(
    r_version = R.version$version.string,
    os_version = utils::osVersion
  )
  system_info
}

#' Wrapper for Sys.getenv
#'
#' Description
#'
#' @family utils_logger
#' @export
get_env_info <- function(){
  Sys.getenv()
}

#' Wrapper for Sys.getenv
#'
#' Description
#'
#' @family utils_logger
#' @export
get_packages_info <- function(){
  packages <- data.table::as.data.table(utils::installed.packages())
  filtered_packages <- packages[,c("Package","Version","Depends","Imports")]
  filtered_packages
}
