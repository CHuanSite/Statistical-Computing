library(methods)
library(readr)
library(dplyr)
library(tidyr)
#Set class of LongitudinalData
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
#Set class for subject, visit and room
setClass("subject",
         slots = list(data = "data.frame", subject.ID = "numeric"),
         contains = "LongitudinalData"
)
setClass("visit",
         slots = list(data = "data.frame", visit.ID = "numeric" ),
         contains = "LongitudinalData"
)
setClass("room",
         slots = list(data = "data.frame", room.ID = "character"),
         contains = "LongitudinalData"
)

#Set generic functions
setGeneric("subject", function(x, ...){
  standardGeneric("subject")
})
setGeneric("visit", function(x,...){
  standardGeneric("visit")
})
setGeneric("room",function(x,...){
  standardGeneric("room")
})

make_LD <- function(dataframe){
  subject.tmp = dataframe %>% select(id) %>% table %>%length
  visit.tmp = dataframe %>% select(visit) %>% table %>%length
  room.tmp = dataframe %>% select(room) %>% table %>%length
  new("LongitudinalData",
      data = dataframe, 
      subject.num = subject.tmp, 
      visit.num = visit.tmp, 
      room.num = room.tmp,
      is.subject = FALSE, 
      is.visit = FALSE, 
      is.room = FALSE,
      is.summary= FALSE
      )
}


#subject function
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
#visit function
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
#room function
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

setGeneric("summary",function(x){
  standardGeneric("summary")
})





setMethod("summary",
          c(x = "LongitudinalData"),
          function(x){
            x@is.summary = TRUE
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




setGeneric("print")
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






