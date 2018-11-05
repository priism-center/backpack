#' Start Lesson
#'
#' Function to start a lesson. 
#' @usage start_lesson(name)
#' @param name Name of the lesson to learn.
#' @details This can also be done using the learnr package as follows 
#' 'learnr::run_tutorial("2004-MatrixAlgebra",package="backpack")'
#' @examples start_lesson(name="2004-MatrixAlgebra")
#' 
#' start_lesson()

start_lesson <- function(name="2004-MatrixAlgebra"){
  rmarkdown::run(paste0("./inst/tutorials/",name,"/",name,".Rmd"))
}

