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

queens_gambit1 <- function(PostsDT){
  Questions <- PostsDT[PostTypeId == 1, ]
  Questions_q <- Questions["2020-07-01" <= CreationDate & CreationDate <= "2021-03-01", ]
  Answers <- PostsDT[PostTypeId == 2, ]
  Answers_q <- Answers["2020-07-01" <= CreationDate & CreationDate <= "2021-03-01", ]

  
  numbers <- function(DT){
    DT[, CreationDate := format(as.Date(CreationDate), "%Y-%m")][, .(Number = .N), by = "CreationDate"]
  }
  Questions_q <- numbers(Questions_q)
  colnames(Questions_q)[2] <- "Number of Questions"
  Answers_q <- numbers(Answers_q)
  colnames(Answers_q)[2] <- "Number of Answers"
  ans <- merge(Questions_q, Answers_q, by = "CreationDate")
  V <- c(ans[[2]], ans[[3]])
  data.table(Data = ans[[1]],V = V, Color = ifelse(ans[[1]]<"2020-10",1,2), G = ifelse(V == ans[[2]],1,2))
}

#######################################

trends <- function(x, c){
  ans <- x[, .(Date = substr(CreationDate,1,7), Points = ifelse(PostTypeId == 1, ViewCount/ifelse(Score==0,Score+1,Score), Score))][, .(Points = sum(Points)), by = Date][order(Date, decreasing = TRUE)][1:12]
  data.table(Date = ans[,Date], Points = ans[,Points], Group = c)
}


zapis <- function(DT, nazwa){
  write.csv(DT, paste("Documents/GitHub/Lioion-vikmf-gqhfcwrpse//" ,nazwa, '.csv', sep = ""))
}


