### GAMBIT KROLOWEJ ###
library(data.table)
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