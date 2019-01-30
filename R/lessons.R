#' Start Lesson
#'
#' Function to start a lesson. 
#' @usage start_lesson(lesson_name)
#' @param lesson_name Name of the lesson to learn.
#' @details This can also be done using the learnr package as follows 
#' 'learnr::run_tutorial("2004-MatrixAlgebra",package="backpack")'. It must be noted that when using this, 
#' the packages needed for the tutorial aren't automatically loaded. It is recommended to use start_lesson.
#' @examples start_lesson(name="2004-MatrixAlgebra")
#' 
#' start_lesson()

start_lesson <- function(lesson_name="2004-MatrixAlgebra"){
  avl_lessons <- backpack::list_lessons()
  
  if(length(lesson_name) > 1){
    stop("Please enter only 1 lesson name")
  }
  
  if(!(lesson_name %in% avl_lessons)){
    stop(paste0("Lesson ",lesson_name," not found! Use list_lessons to look for availables ones."))
  }
  
  if(!is.null(backpack::view_binders(search=lesson_name))){
    load_binders(lesson_name)
  }
  rmarkdown::run(paste0(".tutorials/",lesson_name,"/",lesson_name,".Rmd"))
}

#' List Lessons
#'
#' Function to list available lessons. 
#' @usage list_lessons()
#' @examples list_lesson()
#' 

list_lessons <- function(){
  lessons = list.files('./tutorials')
  lessons = lessons[ ! lessons %in% c('readme.md', 'README.md')]
  return(lessons)
}
