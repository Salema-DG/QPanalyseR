% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_dir.R
\name{save_dir}
\alias{save_dir}
\title{Easy save of object with the same name as its object}
\usage{
save_dir(
  x,
  save_ext =
    "/mnt/vmJoanaShare/QP_stand/5_data_other_proj/data_peer_proj_main/descriptives/",
  latex = F
)
}
\arguments{
\item{x}{the object to save}

\item{save_ext}{path}

\item{latex}{logical value. Should it be save as a .tex file?}
}
\description{
facilitate the saving of objects, to perform the sample construction
and the graphic building else where
}
\examples{
# save_dir(vec1, path_ext)
# save_dir(table1, path_ext, latex = T)
}
