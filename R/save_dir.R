
#' @title Easy save of object with the same name as it's object
#'
#' @description facilitate the saving of objects, to perform the sample construction
#' and the graphic building else where
#'
#' @param x the object to save
#' @param save_ext path
#' @param latex logical value. Should it be save as a .tex file?
#'
#' @examples
#' # save_dir(vec1, path_ext)
#' # save_dir(table1, path_ext, latex = T)

save_dir <- function(x,
                     save_ext = "/mnt/vmJoanaShare/QP_stand/5_data_other_proj/data_peer_proj_main/descriptives/",
                     latex = F) {
  var_name <- deparse(substitute(x))

  if (latex == F) {
    x %>% write_rds(paste0(save_ext, var_name, ".rds"))
  } else{
    x %>% write(paste0(save_ext, var_name, ".tex"))
  }

}



