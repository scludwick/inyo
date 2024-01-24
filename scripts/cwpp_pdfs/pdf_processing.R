library(pdftools)
library(stringr)
library(quanteda)

#vector of pdf names
pdfs <- list.files("large_data/cwpp_pdfs_CA", pattern = "pdf$", full.names = T)
#apply pdf_text to each
plans <- lapply(files, pdf_text)
#did not like, errors but seemed to read it ok

#alternatively using tm to make a corpus
library(tm)
corp <- Corpus(URISource(pdfs),
               readerControl = list(reader = readPDF))
plans.tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(3, Inf)))) 
findFreqTerms(plans.tdm, lowfreq = 100, highfreq = Inf)
