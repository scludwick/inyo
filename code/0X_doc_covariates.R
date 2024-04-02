# attaching metadata to docs
cwpp_vars <- sf::st_read("data/int_data/cwpp_vars/all_vars.shp")
tx_file_dir <-'large_data/filtered_cwpp_texts/'
tx_files <- list.files(tx_file_dir)
meta <- as.data.frame(tx_files) 
meta$base <- str_extract(meta$tx_files, ".*(?=.{9}$)") 
meta$year <- factor(str_extract(meta$tx_files, "(\\w{4})(?=\\.txt)"))
meta$jur <- factor(str_extract(meta$tx_files, "(.{4})(?=.{9}$)"))
#merge community info with plan info
meta <- merge(cwpp_vars, meta, by.x = 'Name', by.y = 'base')
#select only docs in the dfm?
# docs <- docnames(dfmt)
# meta <- meta[meta$tx_files %in% docs,]
meta <- sf::st_drop_geometry(meta)
meta <- meta %>% 
  dplyr::relocate(c(tx_files,year, jur), .after=Name)
meta <- write_csv(meta, "data/int_data/docmeta.csv")


# adding thetas ex if using ldaseed2 
terms <- as.data.frame(terms2)
thetas <- as.data.frame(lda_seed2$theta)
thetas <- log(thetas)
thetas$tx_files <- row.names(thetas)
row.names(thetas) <- NULL
thetas <- thetas[, c(ncol(thetas), 1:(ncol(thetas)-1))]
meta_theta <- meta[meta$tx_files %in% thetas$tx_files,] #only keeping metadata for the files we analyzed
meta_theta <- merge(meta_theta, thetas)
meta_theta[is.na(meta_theta)] <- 0
