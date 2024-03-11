library(pdftools)
library(stringr)
library(quanteda)
library(tokenizers)
library(tm)
library(stm)

#vector of pdf names
pdfs <- list.files("large_data/cwpp_pdfs_CA", pattern = "pdf$", full.names = T)
# #apply pdf_text to each
plans <- lapply(pdfs, pdf_text)
#did not like, lots of errors but seemed to read it ok?

#tokenize sentences
tokens <- lapply(plans, tokenize_sentences)
text_preprocessing<- function(x) {
  gsub('http\\S+\\s*','',x) # remove URLs
  gsub('#\\S+','',x) # remove hashtags
  gsub('[[:cntrl:]]','',x) # remove controls and special characters
  gsub("^[[:space:]]*","",x) # remove leading whitespaces
  gsub("[[:space:]]*$","",x) # remove trailing whitespaces
  gsub(' +', ' ', x) # remove extra whitespaces
}
clean_toks <- lapply(tokens, text_preprocessing)
#name items
names(clean_toks) <- lapply(pdfs, basename)

df <- do.call(rbind, clean_toks)


# 
# tokens[[1]][[6]]
# tokens_q <- quanteda::tokens(
#   tokens,
#   what = "sentence",
#   remove_punct = TRUE,
#   remove_symbols = TRUE,
#   remove_numbers = TRUE,
#   remove_url = TRUE,
#   remove_separators = TRUE,
#   split_hyphens = TRUE,
#   include_docvars = TRUE,
#   padding = FALSE,
#   verbose = quanteda_options("verbose")
# )



# text cleaning from tyler code
text_locs  = 'input/eis_text/'
tlist = list.files('input/eis_text',"*.txt$")
comment_locs = 'input/eis_comment_feis_text/'
doc_sentences = lapply(seq_along(tlist),function(tl){
  x = readLines(paste0(text_locs,tlist[tl]))
  sents = tokenizers::tokenize_sentences(x)[[1]]
  regex_line_number_string = "([0-9]{1,2}\\s){3,}"
  regex_table_dump = "(Alternative\\s[A-Z0-9]\\s){2,}"
  regex_largenumber_dump = "[0-9]{5,}|[0-9]{3,}-[0-9]{3,}"
  regex_internet_dump = "@|\\.com|cc:"
  regex_funkychar_dump = "\\*|_"
  sents = sents[!grepl(regex_line_number_string,sents)]
  sents = sents[!grepl(regex_table_dump,sents)]
  sents = sents[!grepl(regex_largenumber_dump,sents)]
  sents = sents[!grepl(regex_funkychar_dump,sents)]
  sents = sents[nchar(sents)>50 & nchar(sents) < 400]
  #drop weird spacing
  sents = sents[!grepl('\\s[a-z]\\s[a-z]\\s|\\s[A-Za-z]\\s[A-Za-z]\\s[A-Za-z]\\s',sents)]
  sents = sents[!grepl('\\s{2,}',sents)]
  #end in a period
  sents = sents[grepl('\\.$',sents)]
  sents = sents[!duplicated(sents)]
  sents = sents[!grepl("\\([^\\)]+$",sents)]
  sents = sents[!grepl("[A-Z]{8,}",sents)]
  data.frame(text = sents,file = tlist[tl])})




text_preprocessing<- function(x) {
  gsub('http\\S+\\s*','',x) # remove URLs
  gsub('#\\S+','',x) # remove hashtags
  gsub('[[:cntrl:]]','',x) # remove controls and special characters
  gsub("^[[:space:]]*","",x) # remove leading whitespaces
  gsub("[[:space:]]*$","",x) # remove trailing whitespaces
  gsub(' +', ' ', x) # remove extra whitespaces
}

clean <- lapply(plans, text_preprocessing)
head(clean)

#get filename without extension and file path
# names <- tools::file_path_sans_ext(basename(pdfs))
#alternatively using regex: extract everything after the last / and before the last . 
# names(plans) <- sub('.*/(.*)\\..*', '\\1', pdfs)





corp <- VCorpus(DirSource("large_data/cwpp_pdfs_CA"), readerControl = list(reader = readPDF)) 
corp_q <- quanteda::corpus(corp)
head(docvars(corp_q))


tokens_q <- quanteda::tokens(
  corp_q,
  what = "sentence",
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = TRUE,
  remove_separators = TRUE,
  split_hyphens = TRUE,
  include_docvars = TRUE,
  padding = FALSE,
  verbose = quanteda_options("verbose")
)


tokens <- tokens_select(tokens, selection="remove", min_nchar = 3, pattern = c(stopwords("en"),"community", "wildfire", "protection","plan", "CWPP", "fire", "area", "counties", "county", "city", "cities", "map"))

dfm <- quanteda::dfm(tokens)
dfm <- dfm_wordstem(dfm, language = quanteda_options("language_stemmer"))

plans_slam <- readCorpus(dfm, type="dtm")
plans_out <- list(documents = plans_slam$documents, vocab = as.character(plans_slam$vocab))
plans_stm <- stm(documents=plans_out$documents, vocab=plans_out$vocab,
                 K=50, max.em.its = 10, init.type='Spectral')
