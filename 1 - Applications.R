rm(list=ls())
source("https://raw.githubusercontent.com/aashishkpandey/TAFD/master/Functions.R")

#--------------------------------------------------------#
#--------------------------------------------------------#

files = dir("E:\\Dropbox\\patents data - Bkp\\")
length(files)
grep("~",files)
files = files[2:length(files)]
length(files)

df = data.frame(NULL)
for (file in files){
  uri <- paste0("E:\\Dropbox\\patents data - Bkp\\",file)
  pdf <- readDOC(AntiwordOptions = "")(elem = list(uri = uri),
                                       language = "en",
                                       id = "id1")
  text = content(pdf)
  text = text[text!=""]
  text = paste(text,collapse = " ")
  dft = data.frame(file = file, text = text, stringsAsFactors = F)
  df = rbind(df,dft)
  
  }

#------------------------------------------#
textb = df$text
ids = df$file

#-------------------------#
dtm.tcm = dtm.tcm.creator(text = textb,
                      id = ids,
                      std.clean = TRUE,
                      std.stop.words = TRUE,
                      stop.words.additional = c('na'),
                      bigram.encoding = TRUE,
                      bigram.min.freq = 20,
                      min.dtm.freq = 5,
                      skip.grams.window = 10)
dtm = dtm.tcm$dtm
tcm = dtm.tcm$tcm
#--------------------------------------------------------#
#--------------------------------------------------------#

wordcounts = dtm.word.count(dtm)
windows()  
dtm.word.cloud(count = wordcounts,title = 'Term Frequency Wordcloud')

# #or
# windows()  
# dtm.word.cloud(count = dtm1,title = 'Title2')

#--------------------------------------------------------#
#--------------------------------------------------------#
windows()
distill.cog.tcm(mat1=tcm, # input TCM MAT,
                mattype = "TCM",
                title = "TCM from glove algorithm - Graph ", # title for the graph
                s=10,    # no. of central nodes
                k1 = 5)  # No. of Connection with central Nodes

windows()
distill.cog.tcm(mat1=dtm, # input TCM MAT
                mattype = "DTM",
                title = "TCM from DTM Adjacency - Graph", # title for the graph
                s=10,    # no. of central nodes
                k1 = 3)  # No. of Connection with central Nodes

#--------------------------------------------------------#
#--------------------------------------------------------#

sna = tidy.sentiment(textb)

ggplot(sna$sent.nrc, 
       aes(index, n, fill = sentiment)) +     # index is x col, n is y col. fill=?
        geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +     # stat=?
          facet_wrap(~sentiment, ncol = 2, scales = "free_x")     # so cool.

ggplot(sna$sent.bing, 
       aes(index, n, fill = sentiment)) +     # index is x col, n is y col. fill=?
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +     # stat=?
  facet_wrap(~sentiment, ncol = 2, scales = "free_x")     # so cool.

ggplot(sna$sent.loughran, 
       aes(index, n, fill = sentiment)) +     # index is x col, n is y col. fill=?
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +     # stat=?
  facet_wrap(~sentiment, ncol = 2, scales = "free_x")     # so cool.

ggplot(sna$sent.afinn, 
       aes(index, sentiment)) +     # index is x col, n is y col. fill=?
        geom_bar(alpha = 1, stat = "identity", position = "identity", show.legend = FALSE)      # stat=?

#--------------------------------------------------------#
#--------------------------------------------------------#

tdm.text.wordlistfilter <- function()
  text.sentence.split <- function()
    text.nrc.sentiment <- function()
      text.polarity <- function()
        
        