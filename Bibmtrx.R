#SEE website for instuction how to save search results from Web of Science:
#WEB: https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html

#LOAD LIBRARIES:
#install.packages("bibliometrix", dependencies=TRUE)
library(bibliometrix)
library(tidyverse)
setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/BCL_R/BCL")#Set working directory


#Web of Science TABLE 4 HYBRID PAPER:========
WOS <- readFiles("Web_OfScience_TotalSearch168_savedrecs.bib")
WOS_DATA <- convert2df(WOS , dbsource = "isi", format = "bibtex") #convert to data.frame = 168  41
#write.csv(WOS_DATA, file = "WebOfScienceSearch_19june2019.csv")

WOS_Tbl <- select(WOS_DATA, AU,PY, TI,ID, DI, WC )
#write.csv(WOS_Tbl, file = "WebOfScienceSearch_TABLE_01july2019.csv")

NetMatrix <- biblioNetwork(WOS_DATA, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30,
                Title = "Keywords associated with hybrid infrastructure (WoS)",
                type = "fruchterman",
                size=T,edgesize = 5,labelsize=0.9)



#Peter MAcreadie's bibliometrix:======
P <- readFiles("PM.bib") #Read in records
PM <- convert2df(P, dbsource = "isi", format = "bibtex") #convert to data.frame

results <- biblioAnalysis(PM, sep = ";")
results$nAuthors #621 co-authors
length(PM$UT)#104 articles extracted

#Draw a network analysis on Peter's keywords:
NetMatrix <- biblioNetwork(PM, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30,
                Title = "Peter's keywords and their co-occurrences",
                type = "fruchterman",
                size=T,edgesize = 5,labelsize=0.9)

#Most important authors:
DF <- dominance(results, k = 100)
DF

#Blue Carbon bibliometrix:======
B <- readFiles("BC.txt") #Read in records
BC <- convert2df(B, dbsource = "isi") #convert to data.frame

length(BC$UT)#500 = Maxium allowed to export off Web of Science at one time

#Draw a network analysis on Blue Carbon keywords:
NetMatrix <- biblioNetwork(BC, analysis = "co-occurrences",
                           network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30,
                Title = "Keywords associated with Blue Carbon",
                type = "fruchterman",
                size=T,edgesize = 5,labelsize=0.9)

