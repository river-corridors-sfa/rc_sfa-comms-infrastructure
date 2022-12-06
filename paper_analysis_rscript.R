################################################################################
# Text analysis for commmunication infrastructure papers
###############################################################################

#Uploading libraries
require(librarian)
librarian::shelf(tm,dplyr, tidytext, tidyverse,stringr,stringi,
                 widyr,igraph, ggraph,
                 wordcloud, reshape2, graphlayouts,
                 pluralize, quanteda, qgraph, cowplot, readr, pdftools)

#working with files in r: https://www.r-bloggers.com/2021/05/working-with-files-and-folders-in-r-ultimate-guide/
#reading pdf files for text analysis: https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/
#pdftools package in r - error: Invalid Font Weight: https://stackoverflow.com/questions/39630212/pdftools-package-in-r-error-invalid-font-weight 
#text mining with r: https://www.tidytextmining.com/tidytext.html


files <- list.files(pattern ="pdf$")

filelength <- length(files)

set.seed(61204)
clean <- list()
p_info <- list()

for(i in 1:filelength){
    ppr <- pdftools::pdf_text(pdf = files[i]) %>% 
    str_to_lower() %>% 
    str_replace_all("\\t"," ") %>% 
    str_replace_all("\n"," ") %>% 
    str_replace_all("     "," ") %>% 
    str_replace_all("    "," ") %>% 
    str_replace_all("   "," ") %>% 
    str_replace_all("  "," ") %>% 
    str_replace_all("[:digit:]"," ") %>% 
    str_replace_all("[:punct:]"," ") %>% 
    str_trim()
    pnf <- pdftools::pdf_info(pdf = files[i])
    p_info[[i]] <- pnf
    clean[[i]] <- ppr
}

pubs <- as.data.frame(unlist(clean))
colnames(pubs) <- c("text")


wstats <- list()
ppr_length <- length(clean)

for(i in 1:ppr_length){
  ppr_st <- clean[i] %>%  
    unnest_tokens(output = word, input = text)%>%
    anti_join(stop_words, by = "word")%>%
    filter(str_detect(word,"[:alpha:]"))%>%
    distinct() %>% 
    count(word, sort = TRUE) %>% 
    select(word, n) %>% 
    mutate(rank = row_number(),
           total=sum(n),
           t_freq = n/total)
  wstats[[i]] <- ppr_st
}











clean_words <-pubs%>% 
  unnest_tokens(output = word, input = text)%>%
  anti_join(stop_words, by = "word")%>%
  filter(str_detect(word,"[:alpha:]"))%>%
  distinct() %>% 
  count(word, sort = TRUE) %>% 
  select(word, n) %>% 
  mutate(rank = row_number(),
         total=sum(n),
         t_freq = n/total)

#Distribution of frequency values
clean_words %>% filter(rank<26) %>% 
  ggplot(aes(t_freq, fct_reorder(word, t_freq), fill = t_freq)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Frequency", y = NULL)

#Zipf's law for survey answers
clean_words %>% 
  ggplot(aes(rank,t_freq)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2) +
  scale_x_log10() +
  scale_y_log10()

pbs_dat <- data.frame(P1)




pubs <- lapply(files,pdf_text)

for()

corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF))

pubs.tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(3, Inf)))) 
inspect(pubs.tdm[1:10,])

ft <- findFreqTerms(pubs.tdm, lowfreq = 100, highfreq = Inf)
as.matrix(pubs.tdm[ft,]) 
