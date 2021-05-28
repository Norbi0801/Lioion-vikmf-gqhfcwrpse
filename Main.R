#######################################
source("LoadData.R")
#source("Packages.R")

library(data.table)
library(dplyr)
library(tidyr)
library(stringi)

#######################################
#TODO: Projekt
#TODO: Zaimportowanie danych

Posts_xml <- read.csv("C:\\Users/User/Documents/GitHub/NOETSI/arqade.stackexchange/Posts.xml.csv")

zapytanie_1 <- function(Posts_xml){
  postsDT_arqade <- data.table(Posts_xml)
  postsDT_arqade_1 <- setDT(postsDT_arqade)[, CreationDate := format(as.Date(CreationDate), "%Y-%m") ]
  postsDT_arqade_1 <- postsDT_arqade_1[PostTypeId == 1]
  
  postsDT_arqade_1$Tags <- substr(postsDT_arqade_1$Tags, 
                                  2, 
                                  nchar(postsDT_arqade_1$Tags) - 1)
  
  s <- strsplit(postsDT_arqade_1$Tags, split = "><")
  s1 <- data.frame(Id = rep(postsDT_arqade_1$Id, sapply(s, length)), Tags = unlist(s))
  s1 <- data.table(s1)
  typeof(s1)
  
  postsDT_arqade_1 <- postsDT_arqade_1[ ,Tags := NULL]
  
  postsDT_arqade_2 <- merge(postsDT_arqade_1, s1, by = "Id")
  
  ans <- postsDT_arqade_2[,
                          .(Tags_number = .N),
                          by = c("CreationDate", "Tags")][order(CreationDate, decreasing = TRUE)][CreationDate>="2012-02"]
  ans1 <- split(ans, by=c("CreationDate"))
  
  ans2 <- lapply(ans1, function(x){
    sort <- x[order(Tags_number, decreasing = TRUE)]
    sort[1:5]
    
  })
  
  y <- do.call(rbind, ans2)
  
  y <- split(y, by = c("Tags"))
  #ans1 <- ans[,
  #            .(best_tag = max(Tags_number)),
  #            by = c("CreationDate")]
  #TODO Czy skupiamy si? na wyselekcjonowaniu miesi?cy
}
x <- zapytanie_1(arqade.Posts)


plot(seq(1,length(x$CreationDate)), x$Tags_number, xlabel = x$CreationDate)

# 25 userów z największą ilością użytych tagów. 
tags_users_comparision <- function(Posts_xml, Users_xml){
  PostsDT1 <- data.table(Posts_xml)
  PostsDT <- PostsDT1[, .(Id,
                          PostTypeId,
                          CreationDate,
                          OwnerUserId,
                          Tags)]
  PostsDT <- PostsDT[PostTypeId == 1, ]
  
  PostsDT$Tags <- substr(PostsDT$Tags,
                         2,
                         nchar(PostsDT$Tags) - 1)
  split <- strsplit(PostsDT$Tags, split = "><")
  split1 <- data.frame(Id = rep(PostsDT$Id, sapply(split, length)), Tags = unlist(split))
  PostsDT <- PostsDT[, Tags := NULL]
  PostsDT <- merge(PostsDT, split1, by = "Id")
  
  UsersDT1 <- data.table(Users_xml)
  UsersDT <- UsersDT1[, .(Id,
                          DisplayName,
                          Location,
                          Difference = (UpVotes - DownVotes))]
  UsersDT <- UsersDT[Id != -1, ]
  
  DT <- merge(PostsDT, UsersDT, by.x = "OwnerUserId", by.y = "Id")
  
  DT1 <- count(DT, OwnerUserId, wt = n_distinct(Tags), name = "Number_of_Tags")
  
  ans <- merge(UsersDT, DT1, by.x = "Id", by.y = "OwnerUserId")
  ans <- ans[, Id := NULL]
  ans <- ans[Difference >= 1000, ]
  
  ans <- ans[order(Number_of_Tags, decreasing = TRUE)]
  ans <- ans[1:25, ]
  return(ans)
}

### QUEENS GAMBIT ###

PostsDT <- data.table(Posts_xml)

Questions <- PostsDT[PostTypeId == 1, ]
Questions_before <- Questions["2020-06-01" <= CreationDate & CreationDate <= "2020-10-31", ]
Questions_after <- Questions[CreationDate >= "2020-11-01", ]
Answers <- PostsDT[PostTypeId == 2, ]
Answers_before <- Answers["2020-06-01" <= CreationDate & CreationDate <= "2020-10-31", ]
Answers_after <- Answers[CreationDate >= "2020-11-01", ]

numbers <- function(DT){
  DT <- setDT(DT)[, CreationDate := format(as.Date(CreationDate), "%Y-%m") ]
  DT <- DT[, .(Number = .N),
           by = "CreationDate"]
  return(DT)
}

Questions_before <- numbers(Questions_before)
Questions_after <- numbers(Questions_after)
Answers_before <- numbers(Answers_before)
Answers_after <- numbers(Answers_after)

###################
