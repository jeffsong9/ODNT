get_html=function(x) {
  tryCatch(read_html(x),
           warning = function(w) {1},
           error = function(e) {1})}

definition=function(i){
  defs=strsplit(i, "\r\n\r\n \r\n\r\n")[[1]]   # Use \r\n\r\n \r\n\r\n to identify definitions
  POS=sub("(.*?) .*", "\\1", defs)                       # scrape POS
  def=if(grepl("\r\n\r\n", defs)==T){                     # Scrape definitions.  \r\n\r\n used as delimiter
    sub("(.*?) (.*)", "\\2", defs) %>%                 
      strsplit("\r\n\r\n") %>% do.call("cbind",.) %>% 
      gsub("\r\n", "", .) %>% 
      gsub("^ *| *$", "", .)
  } else {sub("(.*?) (.*)", "\\2", defs) %>%
      gsub("\r\n", "", .) %>% 
      gsub("^ *| *$", "", .)}
  return(cbind(POS, def))
}

gen_syn=function(i){
  def_syn=syn[[i]] %>% gsub(".*>(.*?)<.*", "\\1",.)
  m=length(def_syn)
  def_syn2=cbind(matrix(rep(c(POS[i], TTL[i]), each=m), ncol=2), def_syn)
  return(def_syn2)
}
