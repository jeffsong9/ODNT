source("R/utility_function.R")
library(tidyverse)
library(rvest)

#' Obtain definitions and its POS of an English word.
#'
#' @param voc Original English word. The definition of \code{voc} will be scraped 
#' @export
#' @return Definitions and its POS of an English word \code{voc} will be generated.
#' @examples
#' get_def("tuna")

# get_html=function(x) {
#   tryCatch(read_html(x),
#            warning = function(w) {1},
#            error = function(e) {1})}

# definition=function(i){
#   defs=strsplit(raw_data[i], "\r\n\r\n \r\n\r\n")[[1]]   # Use \r\n\r\n \r\n\r\n to identify definitions
#   POS=sub("(.*?) .*", "\\1", defs)                       # scrape POS
#   def=if(grepl("\r\n\r\n", defs)==T){                     # Scrape definitions.  \r\n\r\n used as delimiter
#     sub("(.*?) (.*)", "\\2", defs) %>%                 
#       strsplit("\r\n\r\n") %>% do.call("cbind",.) %>% 
#       gsub("\r\n", "", .) %>% 
#       gsub("^ *| *$", "", .)
#   } else {sub("(.*?) (.*)", "\\2", defs) %>%
#       gsub("\r\n", "", .) %>% 
#       gsub("^ *| *$", "", .)}
#   return(cbind(POS, def))
# }

get_def=function(voc){
  html<-sub("VOCAB", voc, "http://www.dictionary.com/browse/VOCAB?s=t") %>% get_html()
  path3=c(pbk_spot='//div [@class="def-list"] //section [@class="def-pbk ce-spot"]') # pbk='//div [@class="def-list"] //section [@class="def-pbk]')
  raw_data<-lapply(path3, function(x) html_nodes(html, xpath=x) %>% html_text)
  raw_data<-do.call("cbind",raw_data) %>%
    gsub(" {2,}", " ", .) %>%
    gsub("\r\n ", "",.)
  def=lapply(raw_data, function(i) definition(i)) %>% do.call("rbind",.)
  def=cbind(voc,def)
  return(def)
}

# gen_syn=function(i){
#   def_syn=syn[[i]] %>% gsub(".*>(.*?)<.*", "\\1",.)
#   m=length(def_syn)
#   def_syn2=cbind(matrix(rep(c(POS[i], TTL[i]), each=m), ncol=2), def_syn)
#   return(def_syn2)
# }

#' Obtain synonyms and its POS of an English word.
#'
#' @param voc Original English word. The synonyms of \code{voc} will be scraped 
#' @export
#' @return Synonyms and its POS of an English word \code{voc} will be generated.
#' @examples
#' get_syn("great")

get_syn=function(voc){
  html<-sub("VOCAB", voc, "http://www.thesaurus.com/browse/VOCAB?s=t") %>% get_html()
  path=c(xml='//div [@class="synonyms"]',POS='//div [@class="synonyms"]//div [@class="synonym-description"]', TTL='//div [@class="synonyms"]//div [@class="synonym-description"]//strong [@class="ttl"]')
  if(length(html)!=1){
    get_xml=lapply(path, function(x) html_nodes(html, xpath = x))
    POS=get_xml$POS %>% html_text %>% sub("\r\n {2,}(.*?)\r\n.*", "\\1", .) 
    TTL=get_xml$TTL %>% html_text
    # check$POS%>% as.character()%>% gsub("<div id=\"(.*?)\".*", "\\1", .)
    check_filter=get_xml$xml%>% as.character()%>% gsub(".*filters\" id=\"(.*?)\".*", "\\1", .)
    text_node=sapply(check_filter, function(x) gsub("TARGET",x,"#TARGET .text"))
    syn=lapply(text_node, function(x) html_nodes(html, x))
    if (length(syn)==0){
      Syn=cbind.data.frame(voc, NA, NA, NA)
      names(Syn)<-c("original_word", "POS", "TTL", "synonym")  
    } else{
      # n is the number of different definitions
      n=length(POS)
      all_syn=lapply(1:n, function(i) {def_syn=syn[[i]] %>% gsub(".*>(.*?)<.*", "\\1",.);
                     m=length(def_syn);
                     def_syn2=cbind(matrix(rep(c(POS[i], TTL[i]), each=m), ncol=2), def_syn);
                     return(def_syn2)}) %>% do.call("rbind",.)
      m=nrow(all_syn)
      origin_data=rep(voc, each=m)
      cbind(origin_data, all_syn) %>% data.frame(stringsAsFactors=F) -> Syn
      names(Syn)<-c("original_word", "POS", "TTL", "synonym")}
  } else {
    Syn=cbind.data.frame(voc,NA, NA, NA)
    names(Syn)<-c("original_word", "POS", "TTL", "synonym")
  }
  return(Syn)
}
