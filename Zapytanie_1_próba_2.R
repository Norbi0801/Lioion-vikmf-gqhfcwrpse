#install.packages("tidyr")
#install.packages("stringi")
library(data.table)
library(dplyr)
library(tidyr)
library(stringi)
?stri_extract_all
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
                         by = c("CreationDate", "Tags")]

ans1 <- ans[,
           .(best_tag = max(Tags_number)),
           by = c("CreationDate")]

