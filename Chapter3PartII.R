# R and Web Semantic
#
#  A basic methodology in R for querying Pubmed in order to obtain correlations
#  between the search terms and for the organization of  resources by topic.
#
#  Author: Francesco Bardozzo -\\- Supervisor: Prof.ssa Sabrina Senatore
#
#  Chapter 3 - Part II

install.packages("rJava") # if not present already

if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")

library(rJava) #works

install.packages("devtools") # if not present already

library(devtools)

install_github("rrdf", "egonw", subdir="rrdflibs") # if not present already
install_github("rrdf", "egonw", subdir="rrdf", build_vignettes = FALSE) # if not present already

library(rrdf) #works
library(rrdflibs) #works

#Secondo me basta un RDF. Jena Ext.


storet = new.rdf(ontology=FALSE)

addRDFPrefixTopic <- function(qcode, store)
{
  add.prefix(store, "qe", paste("http://query.org/query/", qcode ,"/edge/",sep=""))
  add.prefix(store, "q", "http://query.org/")
  add.prefix(store, "qq", "http://query.org/query/")
  add.prefix(store, "qn", "http://query.org/node/")
  add.prefix(store, "qt", paste("http://query.org/query/", qcode ,"/topic/",sep=""))
  add.prefix(store, "abs", paste("http://www.ncbi.nlm.nih.gov/pubmed/"))
  return(store)
  
  
}


decideid0 <- function(code, ass)
{
  return(paste("http://query.org/",code,"/",ass, sep=""))
}



addTopicsTerms <- function(storet, qcode, Terms, Topic)
{
  
  #Query Class
  add.triple(storet,
             subject = paste("http://query.org/query",sep=""),
             predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
             obj="http://www.w3.org/2000/01/rdf-schema#Class")
  
  #Query 0001 Instance
  add.triple(storet,
             subject = paste("http://query.org/query/0001/",sep=""),
             predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
             obj=paste("http://query.org/query",sep=""))
  #Node Class
  add.triple(storet,
             subject = paste("http://query.org/node" ,sep=""),
             predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
             obj="http://www.w3.org/2000/01/rdf-schema#Class")
  #Edge Class
  add.triple(storet,
             subject = paste("http://query.org/edge" ,sep=""),
             predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
             obj="http://www.w3.org/2000/01/rdf-schema#Class")
  
  add.triple(storet,
             subject = paste("http://query.org/query/0001/edge",sep=""),
             predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
             obj=paste("http://query.org/edge",sep=""))
  #Topic
  add.triple(storet,
             subject = paste("http://query.org/topic" ,sep=""),
             predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
             obj="http://www.w3.org/2000/01/rdf-schema#Class")
  
  add.triple(storet,
             subject = paste("http://query.org/query/0001/topic",sep=""),
             predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
             obj=paste("http://query.org/topic",sep=""))
  
  
  add.data.triple(storet,
                  subject = "http://query.org/topic",
                  predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#comment",
                  data="Resource that describes the Topic Models, executed with
                  LDA - VEM. The class for each instance of Topic presents 15 terms most important for a specific Topic
                  and the abstracts linked.")
  
  

  add.triple(storet,
             subject = "http://www.ncbi.nlm.nih.gov/pubmed/",
             predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
             obj="http://www.w3.org/2000/01/rdf-schema#Class")
  

  
  add.data.triple(storet,
                  subject  = "http://www.ncbi.nlm.nih.gov/pubmed/",
                  predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#comment",
                  data="This resource is an external resource that 
                        refers to the service of pubmed. We use this resource for link the abstracts
                        related to a particular topic by their unique identifier.")
  

  
  add.data.triple(storet,
                  subject = paste("http://query.org/query",sep=""),
                  predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#comment",
                  data="The class Query, its instances are the executed queries.
                  For these reasons this class is cohesive with the class Edge and Nodes. ")
  
  
  add.data.triple(storet,
                  subject = paste("http://query.org/node" ,sep=""),
                  predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#comment",
                  data="The class Node. Its instances are the terms used by the instances of the class Query,
                  for the study of the Topics both for the study of the terms correlations (class Edge).")
  
  
  add.data.triple(storet,
                  subject = paste("http://query.org/edge",sep=""),
                  predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#comment",
                  data="The class Edge, its istances represents the correlations between two Nodes. 
                  This class is cohesive with the RDF Bag container class. ")
  
  add.triple(storet,
             subject = "http://query.org/query/0001/topic/bag",
             predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
             obj="http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag")
  
  
  
  
  
  
  
  
  for(i in 1:length(Terms[1,]))
  {
    add.triple(storet,
               subject = "http://query.org/query/0001/topic/bag",
               predicate =paste("http://www.w3.org/1999/02/22-rdf-syntax-ns#", i, sep="_"),
               obj=paste("http://query.org/query/0001/topic/topic", i, sep="#"))
    add.triple(storet,
               subject = paste("http://query.org/query/0001/topic/topic", i, sep="#"),
               predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
               obj=paste("http://query.org/topic", sep=""))
    
    for (j in 1:length(Terms[,1]))
    {
      
      
      

      add.triple(storet,
                 subject = paste("http://query.org/query/0001/topic/topic", i, sep="#"),
                 predicate ="http://query.org/node/node",
                 obj=paste("http://query.org/node/node", gsub(" ", "_", Terms[[j,i]]), sep = "#"))

      add.triple(storet,
                 subject = paste("http://query.org/node/node",  gsub(" ", "_", Terms[[j,i]]), sep = "#"),
                 predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
                 obj=paste("http://query.org/node" ,sep=""))
      
      
      
      
      
      
    }}
  
  return(storet)
}




addTopicsSourcesX <- function(storet, qcode, Terms, Topic)
{
  
  for(i in 1:length(Terms[1,]))
  {
    pubidst<-names(Topic[Topic==i])
    for (j in 1:length(Topic[Topic==i]))
    {
      
      if(j < 2){

      add.triple(storet,
                 subject = paste("http://query.org/query/0001/topic/topic", i, sep="#"),
                 predicate ="http://query.org/query/0001/topic/abspm",
                 obj=paste("http://www.ncbi.nlm.nih.gov/pubmed/",pubidst[j], sep=""))
    
      add.triple(storet,
                   subject = paste("http://www.ncbi.nlm.nih.gov/pubmed/",pubidst[j], sep=""),
                   predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
                   obj="http://www.ncbi.nlm.nih.gov/pubmed/")
        
        

        
        
        
        }
      

      
    }}
  
  return(storet)
}






storet = new.rdf(ontology=TRUE)

qcode = "0001"
storet = addRDFPrefixTopic(qcode, storet)
storet = addTopicsTerms(storet, qcode, Terms)
storet = addTopicsSourcesX(storet, qcode, Terms, Topic)


summarize.rdf(storet)
save.rdf(storet, "finalStoreTURTLEv.xml", "TURTLE")
save.rdf(storet, "finalStoreRDFv.xml", "RDF/XML")


#Merge degli Store.


finalstore = combine.rdf(store[[2]], storet)
save.rdf(finalstore, "finalStoreTURTLEAbs.xml", "TURTLE")
save.rdf(finalstore, "finalStoreRDFAbs.xml", "RDF/XML")



