### vector of packages needed for this script
packs = c('stringr','data.table')
### check if package already installed
need <- packs[!packs %in% installed.packages()[,'Package']]
### install things that arent' installed yet
install.packages(need)
### load packages
sapply(packs,library,character.only = T)


### if plan already has clean txt version, skip or redo?
OVERWRITE <- F # e.g., for if we update cleaning

### where the raw text files are
raw_txt_loc <- 'large_data/raw_cwpp_to_text/'
raw_file_list <- list.files(raw_txt_loc)

### where to store plain text files
cleantext_loc <- 'large_data/filtered_cwpp_texts/'
if(!dir.exists(cleantext_loc)){dir.create(cleantext_loc)}


# for each pdf file
for(file in raw_file_list){
  clean_text_file <- paste0(cleantext_loc,file)
  if(file.exists(clean_text_file)&!OVERWRITE){next}else{
    text_file_loc <- paste0(raw_txt_loc,file)
    tx <- readLines(text_file_loc)
    # remove lines with no characters
    tx <- tx[tx!='']
    # remove lines that start with 2 or more spaces
    tx <- tx[!str_detect(tx,'^\\s{2,}')]
    # remove lines that with the word "Page"
    tx <- tx[!str_detect(tx,'^Page\\s')]
    # remove lines that with the word "Table"
    tx <- tx[!str_detect(tx,'^Table\\s')]
    # remove lines that with the word "Figure"
    tx <- tx[!str_detect(tx,'^Figure\\s')]
    # remove lines with no letters
    tx <- tx[str_detect(tx,'[A-Za-z]')]
    # remove lines with no lower case letters
    tx <- tx[str_detect(tx,'[a-z]')]
    # lines with really high spaces ratio
    tx <- tx[str_count(tx,'\\s')/nchar(tx) < 0.2]
    # remove lines with one word that don't end in punctuation
    tx <- tx[!str_detect(tx,'^[A-Za-z]{1,}$')]
    # remove lines with way too many periods
    tx <- tx[!str_count(tx,'\\.{8,}')]
    collapse_text <- paste(tx,collapse = ' ')  
    # specify location and name of text file
    fileConn<-file(clean_text_file)
    writeLines(collapse_text,fileConn) 
    close(fileConn)
  }
}

  
  