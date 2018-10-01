#' @import dplyr
#' @import tidyr
#' @import methods
#' @importFrom stats median quantile
NULL

#' Class for LongitudinalData
#'
#' This is the class for LongitudinalData
setClass("LongitudinalData",
         slots = list(data = "data.frame",
                      subject.num = "numeric",
                      visit.num = "numeric",
                      room.num = "numeric",
                      is.subject = "logical",
                      is.visit = "logical",
                      is.room = "logical",
                      room.ID = "character",
                      visit.ID = "numeric",
                      subject.ID = "numeric",
                      is.summary = "logical"
         )
)




#' Generic Function for subject
#'
#' This is the generic function for subject method
#'
#' @param x It's the Longitudinal object that user wants to investigate
#'
#' @param ... It's the query id that will be used
setGeneric("subject", function(x, ...){
  standardGeneric("subject")
})

#' Generic Function for visit
#'
#'This is the generic function for visit method
#'
#' @param x It's the Longitudinal object that user wants to investigate
#'
#' @param ... It's the query id that will be used
setGeneric("visit", function(x,...){
  standardGeneric("visit")
})

#' Generic Function for visit
#'
#'This is the generic function for room method
#'
#' @param x It's the Longitudinal object that user wants to investigate
#'
#' @param ... It's the query id that will be used
setGeneric("room",function(x,...){
  standardGeneric("room")
})


#' Construct a LongitudinalData object
#'
#' This is a function that take a dataframe as a parameter, and then use the
#' default setting to return a LongitudinalData object.
#'
#' @param x A dataframe that users want to convert it into a LongitudinalData object
#'
#' @return This function returns a LongitudinalData object
#'
#' @export
make_LD <- function(x){
  subject.tmp = x %>% select(id) %>% table %>%length
  visit.tmp = x %>% select(visit) %>% table %>%length
  room.tmp = x %>% select(room) %>% table %>%length
  new("LongitudinalData",
      data = x,
      subject.num = subject.tmp,
      visit.num = visit.tmp,
      room.num = room.tmp,
      is.subject = FALSE,
      is.visit = FALSE,
      is.room = FALSE,
      is.summary= FALSE
      )
}
#' Extract a subject from the whole LongitudinalData object
#'
#' This is a function that take a LongitudinalData object as a parameter, and then use the
#' query parameter to find the correspongding subject
#'
#' @param x A LongitudinalData object that users want to extract specific samples from
#' @param query The specific subject that the user wants to know
#'
#' @return This function returns a LongitudinalData object for the specific subject
#'
#' @export
setMethod("subject",
          c(x = "LongitudinalData"),
          function(x, query){
            new("LongitudinalData",
                data= x@data %>% filter(id == query),
                subject.ID = query,
                room.ID = x@room.ID,
                visit.ID = x@visit.ID,
                is.subject = TRUE,
                is.room = x@is.room,
                is.visit = x@is.visit,
                is.summary = x@is.summary
                )
          }
)

#' Extract a visit from the whole LongitudinalData object
#'
#' This is a function that take a LongitudinalData object as a parameter, and then use the
#' query parameter to find the correspongding visit
#'
#' @param x A LongitudinalData object that users want to extract specific samples from
#' @param query The specific visit that the user wants to know
#'
#' @return This function returns a LongitudinalData object for the specific visit
#'
#' @export
setMethod("visit",
          c(x = "LongitudinalData"),
          function(x, query){
            new("LongitudinalData",
                data= x@data %>% filter(visit == query),
                visit.ID = query,
                room.ID = x@room.ID,
                subject.ID = x@subject.ID,
                is.visit = TRUE,
                is.room = x@is.room,
                is.subject = x@is.subject,
                is.summary = x@is.summary
                )
          }
)

#' Extract a room from the whole LongitudinalData object
#'
#' This is a function that take a LongitudinalData object as a parameter, and then use the
#' query parameter to find the correspongding room
#'
#' @param x A LongitudinalData object that users want to extract specific samples from
#' @param query The specific room that the user wants to know
#'
#' @return This function returns a LongitudinalData object for the specific room
#'
#' @export
setMethod("room",
          c(x = "LongitudinalData"),
          function(x, query){
            new("LongitudinalData",
                data= x@data %>% filter(room == query),
                room.ID = query,
                subject.ID = x@subject.ID,
                visit.ID = x@visit.ID,
                is.room = TRUE,
                is.visit = x@is.visit,
                is.subject = x@is.subject,
                is.summary = x@is.summary
                )
          }
)

#' Generic Function for summary
#'
#' This is the generic function for summary method
#'
#' @param x It's the LongitudinalData that will be used
setGeneric("summary",function(x){
    standardGeneric("summary")
})


#' Obtain the summary result for a LongitudinalData object
#'
#' If the LongitudinalData object is the result of one query, such as a certain subject, a certain visit or a certain room,
#' then the summary result will be a spread table of the values of the variable not included. For instance, if the object
#' is the result of querying a subject, then the summary result will be the spread table of visit and room. If the Longitud
#' inalData object is not the result of a single query, then the result of the summary will be the quantile of the values,
#' which includes 'Min.', '1st Qu.', "Median", "Mean", "3rd Qu.", 'Max.'.
#'
#' @param x a LongitudinalData object that the user wants to know its summary information
#'
#' @return This function returns a summary dataframe
#'
#' @export
setMethod("summary",
          c(x = "LongitudinalData"),
          function(x){
            x@is.summary = TRUE
            value = NULL
            value.mean = NULL
            if(x@is.subject == TRUE & x@is.room == FALSE & x@is.visit == FALSE){
              x@data %>% select(visit, room, value) %>% group_by(room, visit) %>% summarize(value.mean = mean(value)) %>% spread(room, value.mean)
            }else if(x@is.visit == TRUE & x@is.room == FALSE & x@is.subject ==FALSE){
              x@data %>% select(id, room, value) %>% group_by(id, room) %>% summarize(value.mean = mean(value)) %>% spread(room, value.mean)
            }else if(x@is.room == TRUE & x@is.visit == FALSE & x@is.subject ==FALSE){
              x@data %>% select(id, visit, value) %>% group_by(id, visit) %>% summarize(value.mean = mean(value)) %>% spread(visit, value.mean)
            }else{
              x@data %>% summarize("Min." = min(value), "1st QU."= quantile(value, 0.25), "Median" = median(value), "3rd QU." = quantile(value, 0.75), "Max." = max(value) )
            }

          }
)


#'Generic function for print
#'
#'This is the generic function for print
#'
#' @param x It's the LongitudinalData that will be used
#'
#' @param ... the content that the user wants to print
setGeneric("print")


#' Print the LongitudinalData object
#'
#' If the LongitudinalData object is the result of one query, such as a certain subject, a certain visit or a certain room,
#' then the summary result will be a spread table of the values of the variable not included. For instance, if the object
#' is the result of querying a subject, then the summary result will be the spread table of visit and room. If the Longitud
#' inalData object is not the result of a single query, then the result of the summary will be the quantile of the values,
#' which includes 'Min.', '1st Qu.', "Median", "Mean", "3rd Qu.", 'Max.'.
#'
#' @param x a LongitudinalData object that the user wants to print
#'
#' @return This function has no return
#'
#' @export
setMethod("print",
          c(x = "LongitudinalData"),
          function(x){
            if(x@is.room!= TRUE & x@is.subject!= TRUE & x@is.visit!= TRUE & x@is.summary != TRUE){
              print(paste("Longitudinal dataset with", x@subject.num, "subjects"))
            }
            if(x@is.subject == TRUE & x@is.summary != TRUE){
              if(nrow(x@data) == 0){
                print(NULL)
              }else{
                print(paste("subject ID:", x@subject.ID))
              }
            }
            if(x@is.visit == TRUE & x@is.summary != TRUE){
              if(nrow(x@data) == 0){
                print(NULL)
              }else{
                print(paste("visit ID:", x@visit.ID))
              }
            }
            if(x@is.room == TRUE & x@is.summary != TRUE){
              if(nrow(x@data) == 0){
                print(NULL)
              }else{
                print(paste("room ID:", x@room.ID))
              }
            }
            if(x@is.summary == TRUE){
              print(paste("ID", (x@data %>% select(id))[1]))
              print(x %>% summary)
            }
          })






