
# R and Web Semantic
#
#  A basic methodology in R for querying Pubmed in order to obtain correlations
#  between the search terms and for the organization of  resources by topic.
#
#  Author: Francesco Bardozzo -\\- Supervisor: Prof.ssa Sabrina Senatore
#
#  Chapter 2 - Part I



adtm <- removeSparseTerms(adtm, 0.01)


ass<-findAssocs(adtm, ld[[2]], 0.2)
ass<-removedups(ass)
ass2<-findAssocs(adtm, ld[[1]], 0.2)
ass2<-removedups(ass2)





removedups <- function(ass2){
    for(j in 1:length(querypub))
    {
      if(length(ass2[grep(querypub[j], names(ass2))])!=0)
        {
         
        nampb = names(ass2[-grep(querypub[j], names(ass2))])
         
        for(i in 1:length(nampb))
        {
          
          if(grep(nampb[i], names(ass2)) !=0)
          { 
            
            vr <- names(ass2[grep(nampb[i], names(ass2))])
            
              index<- which( nchar(vr) 
                             >=
                             max(nchar(vr))
                             )
              
              
              ass2[grep(vr[index], names(ass2))] <- ass2[which(nampb[i] == names(ass2))]
              
              #ass2<-ass2[-which(nampb[i] == names(ass2))]
            
          }
          
        }
      
      
    }
    
    
    }
  
  
  return(ass2)
  
  }