# BioSemanticTechII
Multi-topic analysis leveraging PubMed abstracts and MeSH terms. 
Please refer to the full documentation [here](https://github.com/lodeguns/BioSemanticTechII/blob/master/Topic%20Analysis%20on%20PubMed%20abstracts%20and%20MeSH%20terms%20with%20R%20and%20SPRQL.pdf).

** An interesting function **
For those impatient here a query to MeSH SPARQL EndPoint of PubMed terms. These terms are extracted from the PubMed abstracts due a topic analysis. Through these types of functions and future developments it is possible to infer the causality between deseases, moreover, their comorbidities.


```

queryMeSH15 <- function(vect){

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
      print("direct correlations (causal) :  ")
      result<-d$results[ag,]
      return(list(ass[[i]], result, d))
      }
      #Sys.sleep(15)
      
```
