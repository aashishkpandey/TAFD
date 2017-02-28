rm(list=ls())
source("https://raw.githubusercontent.com/aashishkpandey/TAFD/master/Text%20Analysis%20for%20Dummies.R")

#--------------------------------------------------------#
#--------------------------------------------------------#

textb = readLines('clipboard')

dtm_sparse = dtm.create.text(text = textb,
                             id = 1:length(textb),
                             std.clean = TRUE,
                             std.stop.words = TRUE,
                             stop.words.additional = c('na'),
                             bigram.encoding = TRUE,
                             bigram.min.freq = 10,
                             min.dtm.freq = 5)

dtm = as.DocumentTermMatrix(dtm_sparse, weighting = weightTf)

#-------------------------#
tcm = tcm.create.text(text = textb,
                      id = 1:length(textb),
                      std.clean = TRUE,
                      std.stop.words = TRUE,
                      stop.words.additional = c('na'),
                      bigram.encoding = TRUE,
                      bigram.min.freq = 10,
                      min.dtm.freq = 5,
                      skip.grams.window = 10)
#--------------------------------------------------------#
#--------------------------------------------------------#

wordcounts = dtm.word.count(dtm)
windows()  
dtm.word.cloud(count = wordcounts,title = 'Title1')

# #or
# windows()  
# dtm.word.cloud(count = dtm1,title = 'Title2')

#--------------------------------------------------------#
#--------------------------------------------------------#
windows()
distill.cog.tcm(mat1=tcm, # input TCM MAT,
                mattype = "TCM",
                title = "TCM Graph", # title for the graph
                s=10,    # no. of central nodes
                k1 = 5)  # No. of Connection with central Nodes

windows()
distill.cog.tcm(mat1=dtm, # input TCM MAT
                mattype = "DTM",
                title = "TCM from DTM Graph", # title for the graph
                s=10,    # no. of central nodes
                k1 = 3)  # No. of Connection with central Nodes

#--------------------------------------------------------#
#--------------------------------------------------------#

tdm.text.wordlistfilter <- function()
  text.sentence.split <- function()
    text.nrc.sentiment <- function()
      text.polarity <- function()