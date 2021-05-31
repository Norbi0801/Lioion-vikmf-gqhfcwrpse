#######################################

source("Packages.R")
source("LoadData.R")

#######################################

tags_trends <- function(Posts){
  postsDT_arqade <- setDT(Posts)[, CreationDate := format(as.Date(CreationDate), "%Y-%m")][PostTypeId == 1]  
  postsDT_arqade$Tags <- substr(postsDT_arqade$Tags, 
                                2, 
                                nchar(postsDT_arqade$Tags) - 1)
  
  s <- strsplit(postsDT_arqade$Tags, split = "><")
  s1 <- data.table(Id = rep(postsDT_arqade$Id, sapply(s, length)), Tags = unlist(s))
  postsDT_arqade <- postsDT_arqade[ ,Tags := NULL]
  ans <- merge(postsDT_arqade, s1, by = "Id")[,
                                              .(Tags_number = .N),
                                              by = c("CreationDate", "Tags")][order(CreationDate, decreasing = TRUE)][CreationDate>="2017-02"]
  ans <- split(ans, by=c("CreationDate"))
  ans <- lapply(ans, function(x){
    x[order(Tags_number, decreasing = TRUE)][1:3]
  })
  #split(do.call(rbind, ans), by = c("Tags"))
  do.call(rbind, ans)
  
}

#######################################

# 25 userów z największą ilością użytych tagów. 
tags_users_comparision <- function(PostsDT, UsersDT){
  PostsDT <- PostsDT[, .(Id,
                         PostTypeId,
                         CreationDate,
                         OwnerUserId,
                         Tags)][PostTypeId == 1, ]
  
  PostsDT$Tags <- substr(PostsDT$Tags,
                         2,
                         nchar(PostsDT$Tags) - 1)
  split <- strsplit(PostsDT$Tags, split = "><")
  split1 <- data.table(Id = rep(PostsDT$Id, sapply(split, length)), Tags = unlist(split))
  PostsDT <- PostsDT[, Tags := NULL]
  PostsDT <- merge(PostsDT, split1, by = "Id")
  
  UsersDT <- UsersDT[, .(Id,
                         DisplayName,
                         Location,
                         Difference = (UpVotes - DownVotes))][Id != -1, ]
  
  DT <- merge(PostsDT, UsersDT, by.x = "OwnerUserId", by.y = "Id")
  
  DT <- count(DT, OwnerUserId, wt = n_distinct(Tags), name = "Number_of_Tags")
  
  ans <- merge(UsersDT, DT, by.x = "Id", by.y = "OwnerUserId")
  ans[, Id := NULL][Difference >= 1000, ][order(Number_of_Tags, decreasing = TRUE)][1:15]
}

#######################################

### QUEENS GAMBIT ###

queens_gambit <- function(PostsDT){
  Questions <- PostsDT[PostTypeId == 1, ]
  Questions_before <- Questions["2020-06-01" <= CreationDate & CreationDate <= "2020-10-31", ]
  Questions_after <- Questions[CreationDate >= "2020-11-01", ]
  Answers <- PostsDT[PostTypeId == 2, ]
  Answers_before <- Answers["2020-06-01" <= CreationDate & CreationDate <= "2020-10-31", ]
  Answers_after <- Answers[CreationDate >= "2020-11-01", ]
  
  numbers <- function(DT){
    DT[, CreationDate := format(as.Date(CreationDate), "%Y-%m")][, .(Number = .N), by = "CreationDate"]
  }
  list(
    Questions_before = numbers(Questions_before),
    Questions_after = numbers(Questions_after),
    Answers_before = numbers(Answers_before),
    Answers_after = numbers(Answers_after)
  )
}

#######################################

trends <- function(x){
  x[, .(Date = substr(CreationDate,1,7), Points = ifelse(PostTypeId == 1, ViewCount/ifelse(Score==0,Score+1,Score), Score))][, .(Points = sum(Points)), by = Date][order(Date, decreasing = TRUE)][1:12]
}

