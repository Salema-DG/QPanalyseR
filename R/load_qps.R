
#' @title Load QP data
#'
#' @description
#' Easily load cleaned QP data.
#'
#' @param name Name of the dataset to load. A strting.
#' @param in_linux Logical value indicating if the package is being ran on PROSPER's linux server.
#'
#' @return A dataset or a list of available datasets, if "name" is left in blank.
#'
#' @export
#'
#' @examples
#' df <- load_qps("qp_simple")
#'
#'
load_qps <- function(name,
                     in_linux = T) {

  stopifnot(in_linux == T)

  if (in_linux == T) {
    path <- "../2_data/030_final_versions"
  }

  if (is.null(name)) {
    return(list.files(path))
  } else{
    return(readRDS(paste0(path, "/", name, ".rds")))
  }
}



