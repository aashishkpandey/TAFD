#----------------------------------------------#
#           Text Analytics for dummies         #
#----------------------------------------------#

# contact - aashishkpandey@gmail.com

#________________________________________________#

require(text2vec) || install.packages("text2vec")
require(data.table) || install.packages("data.table")
require(stringr) || install.packages("stringr")
require(tm) || install.packages("tm")
require(RWeka) || install.packages("RWeka")
require(tokenizers) || install.packages("tokenizers")
require(slam) || install.packages("slam")
require(wordcloud) || install.packages("wordcloud")
require(ggplot2) || install.packages("ggplot2")
require(maptpx) || install.packages("maptpx")

library(maptpx)
library(text2vec)
library(data.table)
library(stringr)
library(tm)
library(RWeka)
library(tokenizers)
library(slam)
library(wordcloud)
library(ggplot2)

#________________________________________________#

# text = 'aashish_pandey'
stpw1 = readLines('https://raw.githubusercontent.com/sudhir-voleti/basic-text-analysis-shinyapp/master/data/stopwords.txt')# stopwords list from git
stpw2 = tm::stopwords('english')      # tm package stop word list; tokenizer package has the same name function, hence 'tm::'
stpw3  = unique(gsub("'"," ",c(stpw1,stpw2)))

text.clean = function(x)                    # text data
{ require("tm")
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}

dtm.tcm.creator <- function(text,id = "",
                            std.clean = TRUE,
                            std.stop.words = TRUE,
                            stop.words.additional = c('a','b'),
                            bigram.encoding = TRUE,
                            bigram.min.freq = 2,
                            min.dtm.freq = 2,
                            skip.grams.window = 5) {
  
  if (class(text) != "character" | length(text) < 3){
    stop("data format Not correct. Make sure it's a character verctor of length above 3")
  }
  
  if ((id == "")[1]){
    id = 1:length(text)
  }
  
  if (std.clean == TRUE) {
    print("Performing Standard Text Cleaning")
    
    text = text.clean(text)
  }
  
  if (std.stop.words == TRUE){
    print("Removing Stop Words")
    
    
    stop.words.f = unique(c(stpw3,stop.words.additional))
    text = removeWords(text,stop.words.f)            # removing stopwords created above
    text = stripWhitespace(text)                  # removing white spacestop.words.additional
  }

  tok_fun = word_tokenizer  # using word & not space tokenizers
  
  if (bigram.encoding == TRUE){
    
    # data = data.frame(id = 1:length(text),text = text, stringsAsFactors = F)

    print("finding bi-grams for encoding with selected criteria")
    
    it_0 = itoken( text,
                   tokenizer = tok_fun,
                   ids = id,
                   progressbar = T)
    
    vocab = create_vocabulary(it_0, ngram = c(2L, 2L))
    pruned_vocab = prune_vocabulary(vocab, term_count_min = bigram.min.freq)
    replace_list = pruned_vocab$vocab$terms[order(pruned_vocab$vocab$terms_counts, decreasing = T)]
    
    if (length(replace_list) > 0){
      text = paste("",text,"")
      
      pb <- txtProgressBar(min = 1, max = (length(replace_list)), style = 3) ; i = 0
      
      print(paste("Encoding",length(replace_list),"bi-grams as unigram"))
      for (term in replace_list){
        i = i + 1
        focal.term = gsub("_", " ",term)        # in case dot was word-separator
        replacement.term = term
        text = gsub(paste("",focal.term,""),paste("",replacement.term,""), text)
        setTxtProgressBar(pb, i)
      }                  
    } else {
      print("No bigram to encode with selected criteria")}
  }
  
  print("Creating Document Term Matrix")
  # Create DTM
  it_m = itoken(text,
                tokenizer = tok_fun,
                ids = id,
                progressbar = T)
  
  vocab = create_vocabulary(it_m)
  pruned_vocab = prune_vocabulary(vocab,
                                  term_count_min = min.dtm.freq)
  
  vectorizer = vocab_vectorizer(pruned_vocab)
  
  dtm_m  = create_dtm(it_m, vectorizer)
  dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)
  
  print("Creating Term Co-occurrence Matrix")

  vectorizer = vocab_vectorizer(pruned_vocab,
                                grow_dtm = FALSE,
                                skip_grams_window = skip.grams.window)

  tcm = create_tcm(it_m, vectorizer) # func to build a TCM

  print("Done!!")
  out = list(dtm = dtm, tcm = tcm)

  return(out)
}

#_____________________________________________________#
#_____________________________________________________#

dtm.word.count <- function(dtm) {
  
  if (ncol(dtm) > 1000) {
    tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = b[-which(b >= ncol(dtm))]
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
    }
  } else {
    ss.col = colSums(as.matrix(dtm))
  }
  
  tsum = ss.col
  tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
  return(tsum)
}

#_____________________________________________________#
#_____________________________________________________#

dtm.word.cloud <- function(count = count, title = "Title"){
  
  if (class(count)[1] == "DocumentTermMatrix"|class(count)[1] == "simple_triplet_matrix")
  {
    tsum = dtm.word.count(count)
  } else {
    tsum = count
  }
  
  if (class(tsum) != "numeric") stop("Give input as wordcount or DocumentTermMatrix")
  
  wordcloud(names(tsum), tsum,     # words, their freqs 
            scale = c(4, 0.5),     # range of word sizes
            1,                     # min.freq of words to consider
            max.words = 200,       # max #words
            colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
  title(sub = title)     # title for the wordcloud display
}   

#________________________________________________________#
#________________________________________________________#

distill.cog.tcm = function(mat1, # input TCM or DTM MAT
                           mattype = "DTM", # "DTM" or TCM
                           title, # title for the graph
                           s,    # no. of central nodes
                           k1){  # max no. of connections  
  require(igraph)
  
  mat1 = as.matrix(mat1)
  
  if (mattype == "DTM"){
    mat1 = tcrossprod(t(mat1))
  }
  
  if (ncol(mat1) > 1000) {
    tst = round(ncol(mat1)/100)  # divide mat1's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = b[-which(b >= ncol(mat1))]
    b = c(0,b,ncol(mat1))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempmat1 = mat1[,(b[i]+1):(b[i+1])]
      su = colSums(as.matrix(tempmat1))
      ss.col = c(ss.col,su);rm(su)
    }
  } else {
    ss.col = colSums(as.matrix(mat1))
  }
  
  # a = colSums(mat1) # collect colsums into a vector obj a
  a = ss.col
  b = order(-a)     # nice syntax for ordering vector in decr order  
  
  mat2 = mat1[b, b]     # order both rows and columns along vector b
  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##
  
  wc = NULL
  
  for (i1 in 1:s){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
    mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc,word)
  } # i1 loop ends
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  
  # mat4 = mat2[1:40,1:40]
  if (mattype == "DTM"){
    graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  } else {
    graph <- graph.adjacency(mat4, mode = "directed", weighted=T)    # Create Network object
  }
  
  graph = simplify(graph) 
  V(graph)$color[1:s] = "green"
  V(graph)$color[(s+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singletons?
  
  plot(graph, 
       layout = layout.kamada.kawai, 
       main = title)
} 

#________________________________________________________#
#________________________________________________________#

fit.lda.topics  <- function(dtm,K=2 ){
  
  simfit <- topics(dtm,  K=K, verb=3)
  theta = simfit$theta
  omega = simfit$omega
  
  if (ncol(dtm) > 1000) {
    tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = b[-which(b >= ncol(dtm))]
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
    }
  } else {
    ss.col = colSums(as.matrix(dtm))
  }
  
  lift = theta*0;       # lift will have same dimn as the theta matrix
  
  sum1 = sum(dtm)
  pterms = ss.col/sum1     # each column's marginal occurrence probability
  
  for (i in 1:nrow(theta)){  
    for (j in 1:ncol(theta)){
      ptermtopic = 0; pterm = 0;
      ptermtopic = theta[i, j]
      pterm = pterms[i]
      lift[i, j] = ptermtopic/pterm     # divide each cell by the column's marg. occurr. proby.
    }
  }

censored.lift = lift

  for (i in 1:nrow(lift)){
    censored.lift[i,][censored.lift[i,] < max(censored.lift[i,])] = 0   # hard assigning tokens to topics
  }


if(nrow(dtm) < 1000) {k1 = 10} else {k1= 100}   # to avoid machine choking up in v small datasets

tst = ceiling(nrow(dtm)/k1)  # now using 1% of the rows at a time
a = rep(tst, (k1 - 1))
b = cumsum(a);rm(a)    # cumsum() is cumulative sum.
b = c(0, b, nrow(dtm))  # broke the supermassive dtm into chunks of 1% ncol each
a0 = which(b > nrow(dtm));    # sometimes, rounding errors cause out of bound errors
if (length(a0) > 0) {b = b[-a0]}

eta.new = NULL
for (i1 in 1:K){
  
  a2 = c(NULL)
  for (i in 1:(length(b)-1)) {
      tempdtm = dtm[(b[i]+1):(b[i+1]),]
      a = matrix(rep(lift[, i1], nrow(tempdtm)), nrow(tempdtm), ncol(tempdtm), byrow = TRUE)
      a1 = rowSums(as.matrix(tempdtm * a))
      a2 = c(a2, a1); rm(a, a1, tempdtm)
      } # i ends
  
  eta.new = cbind(eta.new, a2); rm(a2)
  } # i1 ends
  
# rownames(eta.new) = rownames(simfit$omega)
colnames(eta.new) = colnames(simfit$theta)

kappa = eta.new / rowSums(eta.new)   # calc topic proportions for each document
output = list(theta = theta,omega = omega, lift = lift, censored.lift = censored.lift,
              eta = eta.new, kappa = kappa )
return(output)
  }

analyze.lda <- function(file = "lda_interpretation.pdf",dtm = dtm,tcm = tcm, censored.lift = censored.lift,
                        theta = theta, cent = cent, conn = conn ) {
  
  K = ncol(theta)
  pdf(file = file)
  
  wordcounts = dtm.word.count(dtm)
  dtm.word.cloud(count = wordcounts,title = 'Term Frequency Wordcloud')
  
  distill.cog.tcm(mat1=tcm, # input TCM MAT,
                  mattype = "TCM",
                  title = "TCM from glove algorithm - Graph ", # title for the graph
                  s=cent,    # no. of central nodes
                  k1 = conn)  # No. of Connection with central Nodes
  
  distill.cog.tcm(mat1=dtm, # input TCM MAT
                  mattype = "DTM",
                  title = "TCM from DTM Adjacency - Graph", # title for the graph
                  s=cent,    # no. of central nodes
                  k1 = conn)  # No. of Connection with central Nodes
  
        for (i in 1:K){       # For each topic 
          
          a0 = which(censored.lift[,i] > 1) # terms with lift greator than 1 for topic i
          freq = theta[a0,i] # Theta for terms lift greator than 1
          freq = sort(freq, decreasing = T) # Terms with higher probilities for topic i
          
          # Auto Correction -  Sometime terms in topic with lift above 1 are less than 100. So auto correction
          n = ifelse(length(freq) >= 100, 100, length(freq))
          top_word = as.matrix(freq[1:n])
          
          # SUB tcm
          sub.tcm = tcm[colnames(tcm) %in% names(a0),colnames(tcm) %in% names(a0)]
          
          # SUB dtm
          sub.dtm = dtm[,colnames(dtm) %in% names(a0)]
          
          #   Plot wordcloud
          wordcloud(rownames(top_word), top_word,  scale=c(4,.2), 1,
                    random.order=FALSE, random.color=FALSE, 
                    colors=brewer.pal(8, "Dark2"))
          mtext(paste("Latent Topic",i), side = 3, line = 2, cex=2)
          
          # PLot TCM
          distill.cog.tcm(mat1=sub.tcm, # input TCM MAT,
                          mattype = "TCM",
                          title = paste0("TCM from glove algorithm - Topic ",i), # title for the graph
                          s=cent,    # no. of central nodes
                          k1 = conn)  # No. of Connection with central Nodes
          # mtext(paste("Term co-occurrence - Topic",i), side = 3, line = 2, cex=2)
        
          distill.cog.tcm(mat1=sub.dtm, # input TCM MAT
                          mattype = "DTM",
                          title = paste0("TCM from DTM Adjacency - Topic ",i), # title for the graph
                          s=cent,    # no. of central nodes
                          k1 = conn)  # No. of Connection with central Nodes
          
          
        } # i loop ends
 dev.off() 
}


#_________________________________________________________#
#_____________TIDYTEXT____________________________________#

tidy.sentiment = function(text) {

  require(tidytext)
  require(tidyr)
  require(dplyr)
  
  if (length(text[text !=""]) == 0 ) stop (print("Null Vector :( "))
  
  if (length(text) == 1) {
    textdf = data_frame(text0 = text) %>% 
      unnest_tokens(text, text0, token = "sentences")
  } else {
    textdf = data_frame(text = text)  
  }
  
  # if (lexicon == "nrc") {
      sent.nrc = textdf %>%
      mutate(linenumber = row_number()) %>%
      ungroup() %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments("nrc")) %>%
      count(sentiment, index = linenumber %/% 1, sort = TRUE) %>%
      mutate(method = "nrc")
  # }
  
  # if (lexicon == "bing") {
      sent.bing = textdf %>%
      mutate(linenumber = row_number()) %>%
      ungroup() %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments("bing")) %>%
      count(sentiment, index = linenumber %/% 1, sort = TRUE) %>%
      mutate(method = "bing")
  # }
  
  # if (lexicon == "afinn") {
      sent.afinn = textdf %>%
      mutate(linenumber = row_number()) %>%
      ungroup() %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments("afinn")) %>%
      group_by(index = linenumber %/% 1) %>% 
      summarise(sentiment = sum(score)) %>% 
      mutate(method = "afinn")
  # }
  
  # if (lexicon == "loughran") {
      sent.loughran = textdf %>%
      mutate(linenumber = row_number()) %>%
      ungroup() %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments("loughran")) %>%
      count(sentiment, index = linenumber %/% 1, sort = TRUE) %>%
      mutate(method = "loughran")
  # }
 
      # all = rbind(sent.nrc,sent.bing,sent.loughran)
       
      a = data.frame(sent.nrc %>% spread(sentiment, n, fill = 0))
      b = data.frame(sent.bing %>% spread(sentiment, n, fill = 0))
      c = data.frame(sent.afinn)
      d = data.frame(sent.loughran %>% spread(sentiment, n, fill = 0))

      a1 = c(setdiff(setdiff(unique(tidytext::sentiments$sentiment),NA),names(a)),"sentiment")
      a11 = data.frame(matrix(0,nrow(a),length(a1))); colnames(a11) = a1
      a = cbind(a,a11)

      b1 = c(setdiff(setdiff(unique(tidytext::sentiments$sentiment),NA),names(b)),"sentiment")
      b11 = data.frame(matrix(0,nrow(b),length(b1))); colnames(b11) = b1
      b = cbind(b,b11)

      c1 = c(setdiff(setdiff(unique(tidytext::sentiments$sentiment),NA),names(c)))
      c11 = data.frame(matrix(0,nrow(c),length(c1))); colnames(c11) = c1
      c = cbind(c,c11)

      d1 = c(setdiff(setdiff(unique(tidytext::sentiments$sentiment),NA),names(d)),"sentiment")
      d11 = data.frame(matrix(0,nrow(d),length(d1))); colnames(d11) = d1
      d = cbind(d,d11)

      all.sentiments = rbind(a,b,c,d)

      out = list(sent.nrc = sent.nrc, sent.bing = sent.bing, sent.afinn = sent.afinn, sent.loughran = sent.loughran, all.sentiments = all.sentiments)
    return(out)
  }

#_________________________________________________________#
#_________________________________________________________#
