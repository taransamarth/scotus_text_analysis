require(tidyverse)
require(ggplot2)
require(haven)
require(stringr)
require(httr)
require(jsonlite)
require(quanteda)
require(LSX)
require(robustbase)

# Functions to identify which party holds executive branch when case argued before Court
fed_date_check <- function(x, v) { 
  for (row in 1:nrow(presidents)) {
    if (between(x,presidents[row,"start"], presidents[row,"end"])) { 
      return(presidents[row,v])
    }
  }
}

fed_checker <- function(x,v) { 
  return(unlist(lapply(x, fed_date_check,v)))
}

# Get case text from case.law API for 1 case with cite + justice name
get_case_text <- function(x,y) {
  link <- paste("https://api.case.law/v1/cases/?cite=",x,"&full_case=true",sep="")
  
  justice <- paste("Justice", str_split(y,",")[[1]][1])
  
  # API key removed
  text <- rawToChar(GET(link,add_headers(Authorization="Token XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))$content)
  
  Encoding(text) <- "UTF-8"
  
  opinion_df <- fromJSON(text)$results$casebody$data$opinions[[1]]
  
  if (is.null(opinion_df)) { return(NA)}
  
  if (length((opinion_df %>% filter(grepl(justice,author))))==0) { return(NA)}
  
  return((opinion_df %>% filter(grepl(justice,author)))$text)
}

# Helper to get case texts using vector of cites + justice names
get_texts <- function(u,v) {
  return(unlist(lapply(mapply(get_case_text,u,v,USE.NAMES = F), function (x) ifelse(length (x) > 0, x, NA))))
}

# Same preprocessing pipeline as Watanabe (2020) -- split into sentences, tokenize, identify and compound capitalized collocations, remove stopwords,
# trim (the doc/term frequencies are generally arbitrary here)
preprocess <- function(x) {
  
  # Retrieve texts
  df <- x
  df <- df %>% mutate(op_text = get_texts(usCite,name))
  print(paste0("Missingness: ", (sum(is.na(df$op_text))/nrow(df))*100, "% of ", nrow(df), " cases."))
  
  # Filter out unretrievable observations
  df <- df %>% filter(!is.na(op_text))
  # saveRDS(df, file="sample.RDS")
  
  # Convert to sentence-level corpus
  df_corp <- corpus(df,text_field = "op_text",docid_field = "voteId",unique_docnames = TRUE)
  df_corp <- corpus_subset(df_corp, texts(df_corp) != "")
  df_corp_reshape <- corpus_reshape(df_corp, to="sentences")
  df_toks <- tokens(df_corp_reshape, remove_punct=T,remove_numbers=T)
  
  # Tokenize multiword pronouns
  cap_toks <- df_toks %>% tokens_select(pattern="^([A-Z][a-z\\-]{2,})", valuetype = "regex", case_insensitive = FALSE, padding = TRUE) 
  tstat <- textstat_collocations(cap_toks, tolower=FALSE, size=2:3, mincount=30) 
  df_toks <- tokens_compound(df_toks, tstat, case_insensitive = T) %>% tokens_remove(stopwords("en"), padding = TRUE)
  
  # Remove single-letter terms + trim dfm to only reflect terms that appear with non-trivial frequencies
  df_dfm <- tokens_select(df_toks, pattern = "\\b(\\w)\\b", selection = "remove", valuetype = "regex")
  df_dfm <- dfm_trim(dfm(df_toks), min_termfreq = 10, min_docfreq = 5)
  
  return(list(df_toks, df_dfm, df))
}

# Model-fitting function
## Preprocess data frame, identify tokens surrounding given term with statistical significance, and run model with given dictionary.
fit_model <- function(x, dict, term, k) {
  df <- x
  p <- preprocess(df)
  dfm <- p[[2]]
  toks <- p[[1]]
  term_context <- char_context(toks, pattern=term, p=0.05)
  term_lss <- textmodel_lss(dfm, seeds=dict,terms=term_context,k=k, cache=T)
  return(term_lss)
}

# Model prediction function
## Given dataframe and model, fit model onto corpus of opinion texts.
## Returns original dataframe with fits and LOESS smoothed time series.
predict_model <- function(x, m) {
  df <- x
  p <- preprocess(df)
  df_dfm <- p[[2]]
  df <- p[[3]] %>% filter(!is.na(op_text))
  df_dfm <- dfm_group(df_dfm)
  predicted <- predict(m,newdata=df_dfm, se.fit = T)
  df$fit <- predicted$fit
  df$se <- predicted$se.fit
  df <- df %>% filter(!is.na(fit), !is.na(se))
  df$var.fit <- df$se^2
  smoothed <- smooth_lss(df, lss_var="fit", engine="loess",date="dateArgument")
  return(list(df, smoothed))
}
