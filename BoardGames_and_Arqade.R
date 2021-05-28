install.packages(lubridate)
library(lubridate)
Posts <- data.table(Posts_xml)

Posts1 <- Posts %>%
  mutate(Semester = paste0(substring(year(CreationDate),1,4),"/0",semester(CreationDate)))

