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

recorp <- sapply(sent_toks,paste,collapse =' ')
recorp <- quanteda::corpus(recorp)
toks <- tokens(recorp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE)


#### NOTE HERE WE PROBS WANT TO COMPOUNDS SOME IMPORTANT NGRAMS (E.G., DEFENSIBLE SPACE)
comp_words <- c('defensible space*', 'fuel break*', 'building standard*', 'residential development*', 'emergency response*', 'emergency service*', 'cal fire', 'fire safe council*', 'water supply')
toks <- quanteda::tokens_compound(toks, pattern = phrase(comp_words), case_insensitive = T)
# kw_comp <- kwic(toks_comp, pattern = phrase(c(str_replace(comp_words, " ", "_"))))
# head(kw_comp, 25)

### want to remove place names, get map dictionary? starting with removing community name
names <- str_extract(tx_files, pattern = "(?<=_)[^_]+(?=_)") %>%
  str_split(pattern = "(?<=[a-z])(?=[A-Z])") %>%
  unlist() %>%
  unique() %>%
  tolower()
# this also removes names that might be important words like "lake", "trail", "salmon", "creek" etc. so figure that out, but for now manually remove obvious ones
names <- names[!names %in% c("lake", "trail", "salmon", "creek", "slope")]


dfmt <- dfm(toks) %>%
  dfm_remove(c(stopwords("en"), names), min_nchar = 2) %>%
  ## needs to be in at 5 docs
  dfm_trim(min_docfreq = 5, docfreq_type = "count")


#temporary dictionaries, need to discuss how many terms per topic and refine topics
mgmt_dict <- dictionary(list(defensible_space = c("defensible_space"),
                                   fuelbreak = c("fuelbreak", "fuel_break"),
                                   thinning = c("thinning",'brushing'),
                                   prescribed = c("prescribed","intentional", "beneficial"),
                                   emergency = c("emergency", "emergency_response", "emergency_service*"),
                                   landscaping = c('landscaping'),
                                   water = c("water", "water_supply"),
                                   code = c('code','building_standard*'),
                                   develop = c("residential*")))

implmnt_dict <- dictionary(list(recruit = c("retain","recruit","volunteer"),
                                      educate = c("educat*","outreach"),
                                      evac = c("evacuat*"),
                                      firewise = c("firewise"),
                                      comms = c("communicat*")))

#### we need to play with the residual and use that to gauge model GOF
### e.g., how does altering the number of "other" topics shape semantic coherence, modularity, etc.

lda_seed <- textmodel_seededlda(dfmt, c(mgmt_dict, implmnt_dict), min_termfreq = 10,
                                max_iter = 500,residual = 10)

terms(lda_seed, n = 10)
topics <- topics(lda_seed)
