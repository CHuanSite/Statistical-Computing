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
                      is.room = "logical"
                      )
         )
#Set class for subject, visit and room
setClass("subject",
         slots = list(data = "data.frame", Id = "numeric"),
         contains = "LongitudinalData"
         )
setClass("visit",
         slots = list(data = "data.frame", Id = "numeric" ),
         contains = "LongitudinalData"
         )
setClass("room",
         slots = list(data = "data.frame", Id = "character"),
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

setGeneric("print")

setMethod("print",
          c(x = "LongitudinalData"),
          function(x){
            print(paste("Longitudinal dataset with", x@subject.num, "subjects"))
          })
#print method for subject class
setMethod("print",
          c(x = "subject"),
          function(x){
            if(nrow(x@data) == 0){
              print(NULL)
            }else{
              print(paste("subject ID:", x@Id))
            }
          }
          )
#print method for visit class
setMethod("print",
          c(x = "visit"),
          function(x){
            if(nrow(x@data) == 0){
              print(NULL)
            }else{
              print(paste("visit ID:", x@Id))
            }
          }
)
#print method for room class
setMethod("print",
          c(x = "room"),
          function(x){
            if(nrow(x@data) == 0){
              print(NULL)
            }else{
              print(paste("room ID:", x@Id))
            }
          }
)
#Set the generics function for summary
setGeneric("summary",function(x){
  standardGeneric("summary")
})
#summary function for subject class
setMethod("summary",
          c(x = "subject"),
          function(x){
            x@data %>% select(visit, room, value) %>% group_by(room, visit) %>% summarize(value.mean = mean(value)) %>% spread(room, value.mean)
          }
)
#summary function for room class
setMethod("summary",
          c(x = "room"),
          function(x){
            x@data %>% select(id, visit, value) %>% group_by(id, visit) %>% summarize(value.mean = mean(value)) %>% spread(visit, value.mean)
          }
)
#summary function for visit class 
setMethod("summary",
          c(x = "visit"),
          function(x){
            x@data %>% select(id, room, value) %>% group_by(id, room) %>% summarize(value.mean = mean(value)) %>% spread(room, value.mean)
          }
)
#subject function
setMethod("subject",
          c(x = "LongitudinalData"),
          function(x, query){
            new("subject", data= x@data %>% filter(id == query), Id = query)
          }
          )
#visit function
setMethod("visit",
          c(x = "LongitudinalData"),
          function(x, query){
            new("visit", data= x@data %>% filter(visit == query), Id = query)
          }
)
#room function
setMethod("room",
          c(x = "LongitudinalData"),
          function(x, query){
            new("room", data= x@data %>% filter(room == query), Id = query)
          }
)

make_LD <- function(dataframe){
  subject = dataframe %>% select(id) %>% table %>%length
  visit = dataframe %>% select(visit) %>% table %>%length
  room = dataframe %>% select(room) %>% table %>%length
  new("LongitudinalData",data = dataframe, subject.num = subject, visit.num = visit, room.num = room)
}

x <- make_LD(MIE)
table(MIE$id)




