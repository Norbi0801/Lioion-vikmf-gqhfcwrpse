### TAGS AND USERS COMPARISION ###
### ONLY VALUE USERS - DIFF OVER 1000 ###
library(data.table)
library(dplyr)

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
ans1 <- tags_users_comparision(Posts_xml, Users_xml)
