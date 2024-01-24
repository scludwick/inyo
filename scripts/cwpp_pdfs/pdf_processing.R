library(pdftools)
library(stringr)
library(quanteda)
library(tokenizers)

#vector of pdf names
pdfs <- list.files("large_data/cwpp_pdfs_CA", pattern = "pdf$", full.names = T)
#apply pdf_text to each
plans <- lapply(files, pdf_text)
#did not like, errors but seemed to read it ok?
#get filename without extension and file path
names(plans) <- tools::file_path_sans_ext(basename(pdfs))
#alternatively using regex: extract everything after the last / and before the last . 
# names(plans) <- sub('.*/(.*)\\..*', '\\1', pdfs)

#tokenize sentences, needs cleaning
tokens <- lapply(plans, tokenize_sentences)


