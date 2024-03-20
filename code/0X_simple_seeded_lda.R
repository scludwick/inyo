### vector of packages needed for this script
packs = c('seededlda','stringr','quanteda','tokenizers')
### check if package already installed
need <- packs[!packs %in% installed.packages()[,'Package']]
### install things that arent' installed yet
install.packages(need)
### load packages
sapply(packs,library,character.only = T)

# read in files to make corpus
tx_file_dir <-'large_data/filtered_cwpp_texts/'
tx_files <- list.files(tx_file_dir)
tx_vector <- sapply(tx_files,function(x) readLines(paste0(tx_file_dir,x)))

corp <- quanteda::corpus(tx_vector)
sent_toks <- tokenize_sentence(corp)

#### RIGHT HERE I WOULD LIKELY TO SOME FILTERING TO KEEP THINGS THAT LOOK LIKE REAL SENTENCES. WE GOT RID OF A LOT OF PROBLEMS IN THE RAW TEXT TO CLEANED LINE CONVERSIONS
### BUT KEYING ON THIGNS THAT SMELL LIKE SENTENCES COULD BE HELPFUL EHRE (E.G., SHOULD END IN PUNCTUATION, SHOULD HAVE TO TOO FEW OR TOO MANY CHARACTERS)

#if line ends in punctuation, proportion of numbers to alphabetic characters, should always have fewer spaces and numbers than characters (probably fewer than 25%)
#drop weird spacing

#got this from tyler tuolomne code
sents_func <- function(sents) {
  #weird spacing
  sents = sents[!grepl('\\s[a-z]\\s[a-z]\\s|\\s[A-Za-z]\\s[A-Za-z]\\s[A-Za-z]\\s',sents)]
  sents = sents[!grepl('\\s{2,}',sents)]
  #end in a period
  sents = sents[grepl('\\.$',sents)]
  sents = sents[!duplicated(sents)]
  sents = sents[!grepl("\\([^\\)]+$",sents)]
  sents = sents[!grepl("[A-Z]{8,}",sents)]
}
#trying 
sents <- lapply(sent_toks, sents_func)
sents <- lapply(sents, function(sents) sents[lengths(sents) > 0])

recorp <- sapply(sent_toks,paste,collapse =' ')
recorp <- quanteda::corpus(recorp)
toks <- tokens(recorp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE)

#### COMPOUND SOME IMPORTANT NGRAMS (E.G., DEFENSIBLE SPACE)
comp_words <- c('defensible space*', 'fuel break*', 'building standard*', 'residential development*', 'emergency response*', 'emergency service*', 'cal fire', 'fire safe council*', 'water supply')
toks <- quanteda::tokens_compound(toks, pattern = phrase(comp_words), case_insensitive = T)
# kw_comp <- kwic(toks, pattern = phrase(c(str_replace(comp_words, " ", "_"))))
# head(kw_comp, 25)

### want to remove place names, get map dictionary? starting with removing community name
names <- str_extract(tx_files, pattern = "(?<=_)[^_]+(?=_)") %>%
  str_split(pattern = "(?<=[a-z])(?=[A-Z])") %>%
  unlist() %>%
  unique() %>%
  tolower()
# this also removes names that might be important words like "lake", "trail", "salmon", "creek" etc. so figure that out, keeping some important ones
names <- names[!names %in% c("lake", "creek", "slope")]
# I checked terms across places and only 2 terms were less than 11 places somehow, so think for now keep it like this?

#made min char 3
dfmt <- dfm(toks) %>%
  dfm_remove(c(stopwords("en"), names), min_nchar = 3) %>%
  ## needs to be in at least 10 docs 
  dfm_trim(min_docfreq = 10, max_docfreq = 230, docfreq_type = "count") 

#temporary dictionaries, need to discuss how many terms per topic and refine topics
mgmt_dict <- dictionary(list(defensible_space = c("defensible_space"),
                                   fuelbreak = c("fuelbreak", "fuel_break"),
                                   thinning = c("thin*",'brushing'),
                                   prescribed = c("prescribed", "beneficial"),
                                   emergency = c("emergenc*"),
                                   landscaping = c('landscap*'),
                             #not sure water is a management strategy?
                                   water = c("water*", "water_supply"),
                                   code = c('code','building_standard*'),
                                   develop = c("residential*", "structur*")))

implmnt_dict <- dictionary(list(recruit = c("retain","recruit","volunteer"),
                                      educate = c("educat*","outreach"),
                                      evac = c("evacuat*"),
                                      firewise = c("firewise"),
                                      programs = c("program*"),
                                      comms = c("communicat*")))

#### we need to play with the residual and use that to gauge model GOF
### e.g., how does altering the number of "other" topics shape semantic coherence, modularity, etc.

#check results of different Ks
set.seed(1234)
lda_seed1 <- textmodel_seededlda(dfmt, c(mgmt_dict, implmnt_dict), min_termfreq = 15, max_iter = 500, residual = 20)
terms1 <- terms(lda_seed1, n = 15)
terms1

set.seed(2345)
lda_seed2 <- textmodel_seededlda(dfmt, c(mgmt_dict, implmnt_dict), min_termfreq = 15,max_iter = 500, residual = 40)
terms2 <- terms(lda_seed1, n = 15)
terms2

set.seed(3456)
lda_seed3 <- textmodel_seededlda(dfmt, c(mgmt_dict, implmnt_dict), min_termfreq = 15,max_iter = 500, residual = 30)
terms3 <- terms(lda_seed1, n = 15)
terms3

set.seed(8907)
lda_seed4 <- textmodel_seededlda(dfmt, c(mgmt_dict, implmnt_dict), min_termfreq = 15, max_iter = 500, residual = 10)
terms4 <- terms(lda_seed4, n = 15)
terms4

set.seed(4567)
lda1 <- textmodel_lda(dfmt, k = 30, max_iter = 500)
termslda1 <- terms(lda1)
termslda1

set.seed(5678)
lda2 <- textmodel_lda(dfmt, k = 50, max_iter = 500)
termslda2 <- terms(lda2)
termslda2
#i dont know if any of these are very good or not

#proportions of doc per topic really wild dist.
docstheta <- as.data.frame(lda_seed1$theta[,1:15])
boxplot(docstheta)


library(dplyr)
#metadata
cwpp_vars <- sf::st_read("data/int_data/cwpp_vars/cwpp_phys.shp")
meta <- as.data.frame(tx_files) 
meta$base <- str_replace(meta$tx_files, "\\.txt", "")
meta$base <- str_replace(meta$base, ".{5}$", "")
meta <- merge(meta, cwpp_vars, by.x = 'base', by.y = 'Name') %>%
  select(-c("NA_"))

#distribution of topic props and meta vars is very wonky




