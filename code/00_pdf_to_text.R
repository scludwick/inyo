### vector of packages needed for this script
packs = c('pdftools','stringr','tesseract')
### check if package already installed
need <- packs[!packs %in% installed.packages()[,'Package']]
### install things that arent' installed yet
install.packages(need)
### load packages
sapply(packs,library,character.only = T)


### if plan already has txt conversion, skip or redo?
### (e.g., if there's some font issue or want to run ocr...)
OVERWRITE <- F

### where the plans are
pdf_loc <- 'large_data/cwpp_pdfs_CA/'
file_list <- list.files(pdf_loc)

### where to store plain text files
plaintext_loc <- 'large_data/raw_cwpp_to_text/'
if(!dir.exists(plaintext_loc)){dir.create(plaintext_loc)}


# for each pdf file
for(file in file_list){
  # name converstion for plain text file (pdf-->txt)
  text_file_name <- str_replace(file,'pdf$','txt')
  text_file_loc <- paste0(plaintext_loc,text_file_name)
  if(file.exists(text_file_loc) & !OVERWRITE){next}else{
    print(paste0('converting ',file, ' to plain text'))
    # read into plain text, vector that is n pages long
    pdf_file_name <- paste0(pdf_loc,file)
    text = pdftools::pdf_text(pdf_file_name)
    ### if all items are blank, see if ocr via tesseract works...
    if(all(text=='')){text = pdftools::pdf_ocr_text(pdf_file_name)}
    ###### NOTE THIS IS A PLACEHOLDER THAT ASSUMES NO FILE ERRORS, JUST TEXT VS. IMAGE ISSUE
    ## if still bad, just ignore for now ###
      ## drop blank pages
      text <- text[text!='']
      #only write if text exists 
      if(length(text)>0){
      # specify location and name of text file
      fileConn<-file(text_file_loc)
      # write by line, each line is a line from doc
      writeLines(text, fileConn)
      close(fileConn)
      }
    }
  }






