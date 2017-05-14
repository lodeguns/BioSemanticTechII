# R and Web Semantic
#
#  A basic methodology in R for querying Pubmed in order to obtain correlations
#  between the search terms and for the organization of  resources by topic.
#
#  Author: Francesco Bardozzo -\\- Supervisor: Prof.ssa Sabrina Senatore
#
#  Chapter 3 - Part I



adtm_backup<-adtm
adtm<-adtm_backup
adtm <-DocumentTermMatrix(corpus_clean, control = list(stopwords = TRUE, minWordLength = 3,
                                                       removeNumbers = TRUE, removePunctuation = TRUE, 
                                                       bounds = list(local = c(4,Inf))))

adtm$dimnames$Docs<-pubmed_search$ids

library(topicmodels)
library(slam)


summary(col_sums(adtm))
term_tfidf <-
  tapply(adtm$v/row_sums(adtm)[adtm$i], adtm$j, mean) * log2(nDocs(adtm)/col_sums(adtm > 0))
summary(term_tfidf)



adtm <- adtm[,term_tfidf >= 0.2]
adtm <- adtm[row_sums(adtm) > 0,]
summary(col_sums(adtm))


k <- 10
SEED <- 10000
jss_TM <-
  list(VEM = LDA(adtm, k = k, control = list(seed = SEED)),
       VEM_fixed = LDA(adtm, k = k,
                       control = list(estimate.alpha = FALSE, seed = SEED)),
       Gibbs = LDA(adtm, k = k, method = "Gibbs",
                   control = list(seed = SEED, burnin = 2000,
                                  thin = 100, iter = 2000))
      , CTM = CTM(adtm, k = k,
                 control = list(seed = SEED,
                                var = list(tol = 10^-4), em = list(tol = 10^-3))))

sapply(jss_TM[1:2], slot, "alpha")



sapply(jss_TM, function(x)
  mean(apply(posterior(x)$topics,
             1, function(z) - sum(z * log(z)))))


Topic <- topics(jss_TM[["VEM"]], 1)
Terms <- terms(jss_TM[["VEM"]], 15)
sapply(jss_TM[1:2], slot, "alpha")


Topic <- topics(jss_TM[["Gibbs"]], 1)
Terms <- terms(jss_TM[["Gibbs"]], 10)

View(terms(jss_TM[["Gibbs"]], 30))










