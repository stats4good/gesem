##-- Pacotes ----
library(gtrendsR)
library(highcharter)
library(wordcloud2)
library(ggthemes)
library(widgetframe)
library(dplyr)
library(rvest)
library(stringr)
library(tm)
library(SnowballC)
##-- Google Trends ----
dados_gtrends <- gtrends(keyword = c("estupro", "assédio sexual", "assalto", "assassinato"), time = "today 12-m")
dados_gtrends <- dados_gtrends$interest_over_time
dados_gtrends$hits <- as.numeric(dados_gtrends$hits)
dados_gtrends$date <- as.Date(dados_gtrends$date)

write.table(x = dados_gtrends, file = "data/dados_gtrends.csv", sep = ",", row.names = F)
##-- Webscrapping ----
get_pages <- function(key, start){
  url = URLencode(sprintf("https://www.google.com/search?q=%s&start=%s", key, start))
  
  page <- read_html(url)
  
  results <- page %>% 
    html_nodes("h3.r > a") %>% 
    html_attr("href") %>%
    str_replace_all(pattern = "/url\\?q=", replacement = "") %>%
    str_split(pattern = "&", simplify = T)
  
  results <- data.frame(links = as.character(results[,1]),
                        posicao = start:(start + length(results[,1]) - 1), stringsAsFactors = F)
  return(results)
}
get_pages_content <- function(link){
  content <- try(read_html(link))
  
  if(!( "try-error" %in% class(content))){
    content <- content %>%
      html_nodes('p') %>% 
      html_text() %>% 
      str_trim() %>% 
      unlist()  
    return(content)
  } else{
    return(NULL)
  }
  
} 

starts <- seq(1, 170, by = 10)
key <- "estupro"

links <- lapply(X = starts, FUN = get_pages, key = key)
links <- do.call(links, what = "rbind")

conteudo <- sapply(links$link, FUN = get_pages_content)
conteudo <- unlist(conteudo)

palavras_conteudo <- unlist(str_match_all(conteudo, '\\w+\\b'))
palavras_conteudo <- removePunctuation(x = palavras_conteudo)
palavras_conteudo <- removeNumbers(x = palavras_conteudo)
palavras_conteudo <- str_to_lower(string = palavras_conteudo)

stopwords1 <- stopwords('pt')
stopwords2 <- read.table(file = "https://gist.github.com/alopes/5358189.txt", stringsAsFactors = F)
stopwords <- c(stopwords1, stopwords2$V1, "estupro", "anos", "caso", "contra", letters)

palavras_conteudo <- palavras_conteudo[!(palavras_conteudo %in% stopwords)]
palavras_conteudo <- str_replace_all(string = palavras_conteudo, pattern = "mulheres", replacement = "mulher")
palavras_conteudo <- str_replace_all(string = palavras_conteudo, pattern = "homens", replacement = "homem")
palavras_conteudo <- data.frame(word = palavras_conteudo)

write.table(x = palavras_conteudo, file = "data/palavras_conteudo.csv", sep = ",", row.names = F)
