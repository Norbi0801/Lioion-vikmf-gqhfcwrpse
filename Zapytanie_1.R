install.packages("data.table")
install.packages("XML")
install.packages("methods")
install.packages("flatxml")
install.packages("xmlconvert", dependencies = TRUE)
library(data.table)



library("XML")
library("methods")
library(flatxml)
library(xmlconvert)
?xml_to_df
?xmlToDataFrame
badges_arqade <- readXML("Badges.xml")
comments_arqade <- xmlToDataFrame("Comments.xml")
posthistory_arqade <- xmlToDataFrame("PostHistory.xml")
postlinks_arqade <- xmlToDataFrame("PostLinks.xml")
posts_arqade <- xmlToDataFrame("Posts.xml")
tags_arqade <- readXML("Tags.xml")
users_arqade <- xmlToDataFrame("Users.xml")
votes_arqade <- xmlToDataFrame("Votes.xml")

readXML <- function(source){
  xmlfile <- xmlTreeParse(source)
  topxml <- xmlRoot(xmlfile)
  data.frame(t(topxml), row.names=NULL)
}

doc<-xmlParse("Badges.xml")
xmldf <- xmlToDataFrame(nodes = getNodeSet(doc, ))

doc <- fxml_importXMLFlat("Tags.xml")
tags_arcade <- fxml_toDataFrame(doc)

badges_arqade <- xml_to_df("Badges.xml")
install.packages("plyr")
library("XML")
library("plyr")
###
readXML <- function(source){
  doc <- xmlParse(source,useInternalNodes = TRUE)
  xL <- xmlToList(doc) ###is to convert xml doc into List
  data <- ldply(xL, data.frame)
  head(data)
}
?read_xml

example.tags <- system.file("Tags.xml", package="xmlconvert")
tags <- xml_to_df(file = "tags.xml")
worldpop.tags <- xml_to_df("Users.xml", records.tags = "Id")

?xmlToDataFrame

library(XML)
library(dplyr)

xml_doc <-"Tags.xml"

myXML = xmlParse("Tags.xml")
tags_arqade = xmlToDataFrame(myXML, stringsAsFactors = FALSE,) %>% 
  mutate_all(~type.convert(., as.is = T))
