# R and Web Semantic
#
#  A basic methodology in R for querying Pubmed in order to obtain correlations
#  between the search terms and for the organization of  resources by topic.
#
#  Author: Francesco Bardozzo -\\- Supervisor: Prof.ssa Sabrina Senatore
#
#  Chapter 3 - Part III


queryMeSH15 <- function(vect){
#nota se wd2="" funziona anche con una sola parola.
#si possono ipotizzare versioni più avanzate con più parole.
  #Sys.sleep(15)
  
require(SPARQL)
  
  rex = paste( "REGEX(?dName,'" ,vect[1], "','i') || REGEX(?cName,'", vect[1] , "','i')", sep="")
  
  if(length(vect)> 1){
  for(i in 2:length(vect))
    rex = paste(rex, "||REGEX(?dName,'" ,vect[i], "','i') || REGEX(?cName,'", vect[i] , "','i')", sep="")
  }
  
  
  
  qr= paste("
          PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
          PREFIX owl: <http://www.w3.org/2002/07/owl#>
          PREFIX meshv: <http://id.nlm.nih.gov/mesh/vocab#>
          PREFIX mesh: <http://id.nlm.nih.gov/mesh/>
          PREFIX mesh2015: <http://id.nlm.nih.gov/mesh/2015/>

          SELECT ?d ?dName ?c ?cName 
          FROM <http://id.nlm.nih.gov/mesh>
          WHERE {
                  ?d a meshv:Descriptor .
                  ?d meshv:concept ?c .
                  ?d rdfs:label ?dName .
                  ?c rdfs:label ?cName
                  FILTER(",rex,") 
                }
        ", sep = "")
  

d <- SPARQL(
  
  url="http://id.nlm.nih.gov/mesh/sparql/",
  
  query=qr,
  
  # I parametri che si prende MeSH.
  extra =  list(
             format="HTML", 
             inference="TRUE",
             year="current", 
             limit="20000",
             offset="0"
             ) 
)
 
return(d)
            
}



d$results

meshResources <- function(ass, i){
    d<-queryMeSH15("cancer")
    
    dName<-gsub("@en", '', gsub('"','',d$results$dName))
    cName<-gsub("@en", '', gsub('"','',d$results$cName))
    
    ag<-agrep(names(ass[i]), cName)
    
      #if(length(ag)!=0)
      print("associazioni dirette:  ")
      result<-d$results[ag,]
      return(list(ass[[i]], result, d))
      }
      #Sys.sleep(15)