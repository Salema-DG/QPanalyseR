
#' @title Calculate the Mode
#' 
#' @description
#' Calculate the mode of a vector.
#' The function returns multiple modes, if they exist.
#' The user can also, in case of a tie, keep the biggest or the smallest. 
#' 
#' @param x A numeric/integer vector to calculate the mode for.
#' @param untie optional. In case there is a tie, the function unties to the "max" or the "min".
#' 
#' @return A vector with the modes.
#' 
#' @export
#' 
#' @examples
#' vec <- c(1, 2, 3, 3, 4, 4, 5)
#' 
#' vec |> modes()
#' vec |> modes(untie = "max")
#' vec |> modes(untie = "min")
#' 
modes <- function(x,
                  untie = "none") {
  
  # calculate a vector of modes
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  vec_mode <- ux[tab == max(tab)]
  
  if (untie == "max") {
    vec_mode <- max(vec_mode)
  }
  
  if (untie == "min") {
    vec_mode <- min(vec_mode)
  }
  
  return(vec_mode)
  
}


