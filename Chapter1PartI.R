# R and Web Semantic
#
#  A basic methodology in R for querying Pubmed in order to obtain correlations
#  between the search terms and for the organization of  resources by topic.
#
#  Author: Francesco Bardozzo -\\- Supervisor: Prof.ssa Sabrina Senatore
#
#  Chapter 1 - Part I


library(rentrez)
library(XML)
library(qdap)
library(tm)

if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")

library(rJava) 
library(devtools)
library(rrdf)
library(rrdflibs)

querypub <- c("diabetes", "cancer")
ndoc <- 10000
wscorpus <- "D:/CorpusPubMed/texts"


ld <- QueryToCorpusAbsPubMed(querypub, wscorpus, ndoc)

QueryToCorpusAbsPubMed <- function(querypub, wscorpus, ndoc)
{
  require(rentrez)
  require(XML)
  
  pubmed_search <- entrez_search( db = "pubmed", 
                                  term = paste(querypub, collapse = " AND "), 
                                  retmax = ndoc )
  pubmed_search$ids
  
  pubmed_search$file
  
  termini<- xmlToList(pubmed_search$file)
  
  ld<- vector("list",length(termini$TranslationSet))
  for(i in 1:length(termini$TranslationSet))
  {    
    gsub("\"",'',paste())
    d<-strsplit(termini$TranslationSet[i]$Translation$To,'\"')
    tt<-names(table(d))
    tt<-tt[tt!=""]
    tt<-tt[-grep('All|AND|OR',tt)]
    ld[[i]] <- tt
  }
  
  
  names(ld)<-querypub
  
  
  setwd(wscorpus)
  
  
  for(j in 0:as.numeric(pubmed_search$retmax))
  {
    
    iddoc <- entrez_fetch(db = "pubmed", id=pubmed_search$ids[j], rettype="xml")
    
    data <- xmlParse(iddoc)
    
    lista<- xmlToList(data)
    
    ll<-lista$PubmedArticle$MedlineCitation$Article$Abstract$AbstractText
    
    ll<-paste(lista$PubmedArticle$MedlineCitation$Article$Abstract, sep="", collapse=", ")
    
    
    print(pubmed_search$ids[j])
    sink(paste(pubmed_search$ids[j],".txt", sep=""))
    cat(ll)
    sink()
    
    
  }
  
  
  return (ld)
  
}



AdjacencyTermMatrix <- function()
{
require(tm) #load text mining library
#setwd('D:/CorpusPubMed/texts') #sets R's working directory to near where my files are
a  <-Corpus(DirSource("D://CorpusPubMed/texts"), readerControl = list(language="en")) 
#specifies the exact folder where my text file(s) is for analysis with tm.
summary(a)  #check what went in
a <- tm_map(a, removeNumbers)
a <- tm_map(a, removePunctuation)
#a <- tm_map(a , stripWhitespace)
a <- tm_map(a, tolower)
a <- tm_map(a, removeWords, stopwords("english")) # #C:\Program Files\R\R-3.1.2\library\tm\stopwords
#secondo me si dovrebbe fare ancora un pò di pulizia.
a <- tm_map(a, removeWords, c("cresults", "attrs", "listtext"))
a <- tm_map(a, removeWords, c("aboard","about","above","across", "after", "against", "along","amid","among",
                              "around", "anti", "as","at","before","behind","below", "beneath", "beside",
                              "besides", "between", "beyond", "but","by","concerning", "considering", "despide",
                              "down", "during", "except", "excepting","excluding", "following", "for", "from",
                              "in", "inside", "into", "like", "minus", "near", "of", "off", "on","onto", "opposite",
                              "outside", "over", "past", "per", "plus", "regarding", "round", "save", "since", "than",
                              "through", "to", "toward", "towards", "under", "underneath", "unlike", "untill", 
                              "up", "upon", "versus", "via", "with", "within", "without")

)

a <- tm_map(a, removeWords, stop1)
a <- tm_map(a, removeWords, stop2)

a <- tm_map(a, stemDocument, language = "english")
corpus_clean <- tm_map(a, PlainTextDocument)

adtm <-DocumentTermMatrix(corpus_clean, control = list(bounds = list(local = c(1,Inf)))) 

return (adtm)

}


