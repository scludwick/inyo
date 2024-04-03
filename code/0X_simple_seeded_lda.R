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
#removing empty sentences
sents <- lapply(sents, function(sents) sents[lengths(sents) > 0])
#removing very short or empty docs with fewer than 20 sentences until I can fix those docs
sents <- sents[!lengths(sents) < 20]


recorp <- sapply(sents,paste,collapse =' ')
recorp <- quanteda::corpus(recorp)
toks <- tokens(recorp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE)

#### COMPOUND SOME IMPORTANT NGRAMS (E.G., DEFENSIBLE SPACE)
comp_words <- c('defensible space*', 'fuel break*', 'building standard*', 'building code*', 'residential development*', 'emergency response*', 'emergency service*', 'cal fire', 'fire safe council*', 'water supply', 'home harden*')
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
  dfm_trim(min_docfreq = 5, max_docfreq = 210, docfreq_type = "count") 

#temporary dictionaries, need to discuss how many terms per topic and refine topics
mgmt_dict <- dictionary(list(defensible_space = c("defensible_space"),
                             home_hardening =c("home_harden*"),
                                   fuelbreak = c("fuelbreak", "fuel_break"),
                                   thinning = c("thin*",'brushing'),
                                   prescribed = c("prescribed", "beneficial"),
                                   emergency = c("emergenc*"),
                                   landscaping = c('landscap*'),
                             #not sure water is a management strategy?
                                   water = c("water*", "water_supply"),
                                   code = c('code*','building_standard*'),
                                   develop = c("residential*", "structur*")))

implmnt_dict <- dictionary(list(recruit = c("retain","recruit","volunteer"),
                                      educate = c("educat*","outreach"),
                                      evac = c("evacuat*"),
                                      firewise = c("firewise"),
                                      programs = c("program*"),
                                      comms = c("communicat*"),
                                      ej = c("income", "disadvantage*")
                                ))


#### we need to play with the residual and use that to gauge model GOF
### e.g., how does altering the number of "other" topics shape semantic coherence, modularity, etc.

#get model results for different Ks
f <- function(k, dfmt, dict) {
  seed_model <- textmodel_seededlda(dfmt, dict, min_termfreq = 15, max_iter = 500, residual = k)
  return(seed_model)
}
kvals <- seq(5, 40, 5)
models <- setNames(lapply(kvals, f, dfmt = dfmt, dict = c(mgmt_dict, implmnt_dict)), as.character(kvals))
models <- saveRDS(models, file = "data/int_data/seededModelsKs.RDS")
models <- readRDS("data/int_data/seededModelsKs.RDS")

termsK5 <- as.data.frame(terms(models[[1]], n = 20), ) # top 20 words per topic
thetaK5 <- as.data.frame(models[[1]]$theta) # give topic-document probabilities
phiK5 <- as.data.frame(models[[1]]$phi) # gives word-topic probabilities

terms(models[[4]])


## stm?????
#assign metadata to dfm
meta <- read.csv("data/int_data/docmeta.csv")
meta <- meta[meta$tx_files %in% docnames(dfmt),]
meta_theta[is.na(meta_theta)] <- 0 # i think this tracks
docvars(dfmt) <- meta
# my_dfm <- dfm_subset(dfmt, ntoken(dfmt) >= 2000) 
library(stm)
#convert to stm
dfmstm <- convert(dfmt, to = "stm")
kvals <- seq(5, 40, 5)
findk <- searchK(dfmstm$documents, dfmstm$vocab, K = kvals, verbose = TRUE)
findk$results
compare <- data.frame("K" = kvals, 
                   "Coherence" = unlist(findk$results$semcoh),
                   "Exclusivity" = unlist(findk$results$exclus))


stm15 <- stm(dfmstm$documents, dfmstm$vocab, K = 15, prevalence = ~year)
# mean_exp + Conifer + Shrubland + all_public + RPL_theme1 + RPL_theme2 + RPL_theme3 + RPL_theme4
stm20 <- stm(dfmstm$documents, dfmstm$vocab, K = 20, prevalence = ~Conifer)
plot(stm15)
plot(stm20)
labelTopics(stm15,topics = c(1:15), n=5)
#remove URLs did not work on all



## notes from datalab Carl
## export seededlda to csv (topic-word distributions and document-topic distributions) and then get into ldaviz
## run mathematical analyses for potential topics to justify hyperparameters 
## one hyperparsameter controls shape of distribution of theta (words to topics), defend the decision on that one -- probably want a smoother curve rather than giant long tail bc vocab is likely less varied than standard english literature library
## compare dtm to standard 
## other distribution is topic - document 

## review stopwords list 
## named entity recognition R -- 

## text mining datalab workshop series


