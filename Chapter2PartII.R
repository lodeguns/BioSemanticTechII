# R and Web Semantic
#
#  A basic methodology in R for querying Pubmed in order to obtain correlations
#  between the search terms and for the organization of  resources by topic.
#
#  Author: Francesco Bardozzo -\\- Supervisor: Prof.ssa Sabrina Senatore
#
#  Chapter 2 - Part II

  
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




store = new.rdf(ontology=FALSE)

#querylog <- paste("query-", Sys.time(), sep="")





decideid0 <- function(code, ass)
{
  return(paste("http://query.org/",code,"/",ass, sep=""))
}





addRDFPrefix <- function(qcode, store)
{
  add.prefix(store, "qe", paste("http://query.org/query/",qcode ,"/edge/",sep=""))
  add.prefix(store, "q", "http://query.org/")
  add.prefix(store, "qq", "http://query.org/query/")
  add.prefix(store, "qn", "http://query.org/node/")
  return(store)
  
  
}


addRDFEdges <- function(store, qcode, ass, count)
{
  count = count
  
  
  #Query Class
   add.triple(store,
             subject = paste("http://query.org/query",sep=""),
             predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
             obj="http://www.w3.org/2000/01/rdf-schema#Class")
   
   #Query 0001 Instance
   add.triple(store,
              subject = paste("http://query.org/query/0001/",sep=""),
              predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
              obj=paste("http://query.org/query",sep=""))
   #Node Class
   add.triple(store,
               subject = paste("http://query.org/node" ,sep=""),
               predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
               obj="http://www.w3.org/2000/01/rdf-schema#Class")
   #Edge Class
   add.triple(store,
              subject = paste("http://query.org/edge" ,sep=""),
              predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
              obj="http://www.w3.org/2000/01/rdf-schema#Class")
   
   add.triple(store,
              subject = paste("http://query.org/query/0001/edge",sep=""),
              predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
              obj=paste("http://query.org/edge",sep=""))
  
  add.data.triple(store,
             subject = paste("http://query.org/query",sep=""),
             predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#comment",
             data="The class Query, its instances are the executed queries.
                   For these reasons this class is cohesive with the class Edge and Nodes. ")
  

  
  add.data.triple(store,
                  subject = paste("http://query.org/node" ,sep=""),
                  predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#comment",
                  data="The class Node. Its instances are the terms used by the instances of the class Query,
                        for the study of the Topics both for the study of the terms correlations (class Edge).")
  

  
  add.data.triple(store,
                  subject = paste("http://query.org/edge",sep=""),
                  predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#comment",
                  data="The class Edge, its istances represents the correlations between two Nodes. 
                        This class is cohesive with the RDF Bag container class. ")

  add.triple(store,
             subject = "http://query.org/query/0001/edge/bag",
             predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
             obj="http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag")
  
  
  for(i in 1:length(ass))
  {
    for (j in 1:length(ass[[i]]))
    {
      if(length(ass[[i]])!=0)
        {
        count = count+1
        
        add.triple(store,
                   subject = "http://query.org/query/0001/edge/bag",
                   predicate =paste("http://www.w3.org/1999/02/22-rdf-syntax-ns#", count, sep="_"),
                   obj=paste("http://query.org/query/0001/edge/edge", count, sep="#"))

        # Tipologia 
        add.triple(store,
                   subject = paste("http://query.org/query/0001/edge/edge", count, sep="#"),
                   predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
                   obj=paste("http://query.org/edge", sep=""))

        # Tipologia 
        add.triple(store,
                   subject = paste("http://query.org/query/0001/edge/edge", count, sep="#"),
                   predicate ="http://query.org/node/node",
                   obj=paste("http://query.org/node/node", gsub(" ", "_", names(ass[[i]][j])), sep = "#"))
        #Node Class Instance
        add.triple(store,
                   subject = paste("http://query.org/node/node", gsub(" ", "_", names(ass[[i]][j])), sep = "#"),
                   predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
                   obj=paste("http://query.org/node" ,sep=""))
        
        
         # Tipologia 
        add.triple(store,
                   subject = paste("http://query.org/query/0001/edge/edge", count, sep="#"),
                   predicate ="http://query.org/node/node",
                   obj=paste("http://query.org/node/node", gsub(" ", "_", names(ass)[i]), sep = "#"))
        #Node Class
        add.triple(store,
                   subject =paste("http://query.org/node/node", gsub(" ", "_", names(ass)[i]), sep = "#"),
                   predicate ="http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
                   obj=paste("http://query.org/node" ,sep=""))

        
        add.data.triple(store,
                        subject = paste("http://query.org/query/0001/edge/edge", count, sep="#"),
                        predicate = "http://query.org/query/0001/edge/correlation",
                        data =gsub(" ", "_", ass[[i]][[j]]),
                        type ="float")



    
    }
    
    }}
  
  return(list(count,store))
  }
  

store = new.rdf(ontology=FALSE)
qcode = "0001"

store = addRDFPrefix(qcode, store)
#in input gli si da uno store e un contatatore.
store = addRDFEdges(store, qcode, ass, 0)
store = addRDFEdges(store[[2]], qcode, ass2, store[[1]])

summarize.rdf(store[[2]])
save.rdf(store[[2]], "storeAssRules1310.xml", "TURTLE")
save.rdf(store[[2]], "storeAssRules1310rdf.xml", "RDF/XML")


## API validator
#http://rhizomik.net/html/redefer/rdf2svg-form/