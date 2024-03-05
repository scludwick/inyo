library(pdftools)
library(stringr)
library(quanteda)
library(tokenizers)
library(tm)
library(stm)

#pdf names
pdfs <- list.files("large_data/cwpp_pdfs_CA", pattern = "pdf$", full.names = T)
#read pdfs to text and save as .txt, 
text <- lapply(pdfs, function(pdfs) {
    totext <- pdf_text(pdfs)
    txtfiles <- paste0("data/int_data/", tools::file_path_sans_ext(basename(pdfs)), ".txt")
    writeLines(totext, txtfiles)
})

#tokenize

txtfiles <- list.files("data/int_data/txt", pattern = ".txt$", full.names=T)
#from tyler code
doc_sentences = lapply(seq_along(txtfiles),function(tl){
x = readLines(txtfiles[tl])
sents <- tokenize_sentences(x)[[1]] #why [[1]]?
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
data.frame(text = sents,file = txtfiles[tl])})


#trying for one to see workflow
t1 <- txtfiles[[1]]
x <- readLines(t1)
#returns a list of sentences
sents <- tokenize_sentences(x)
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

df <- data.frame(text = sents)
