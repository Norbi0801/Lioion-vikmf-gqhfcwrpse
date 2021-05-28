library(lubridate)
library(data.table)
library(dplyr)
Posts_chess <- data.table(Posts_xml)

value_tags_semester <- function(Posts_xml){

Posts1 <- Posts_xml %>%
  mutate(Semester = paste0(substring(year(CreationDate),1,4),"/0",semester(CreationDate)))

y <- split(Posts1, by = c("Semester"))
y <- lapply(y, FUN = function(x){
  nrow(x)
})

}