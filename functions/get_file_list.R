get_file_list<-function(url){
  require(rvest,magrittr)
  start_time<-Sys.time()
  tryCatch(
    read_html(url) %>%
      html_node(xpath="/html/body/table") %>%
      html_table() %>%
      select(name="Name",last_modified="Last modified",size="Size") %>%
      filter(name!="Parent Directory", name!="") ,
    
    error=function(c) data.frame(error=as.character(c))
  ) %>%
  add_column(elapsed_time=Sys.time()-start_time)
}