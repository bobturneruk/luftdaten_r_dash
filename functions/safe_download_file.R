safe_download_file<-function(url,name,destination_folder){
  require(curl)
  tryCatch(
    data.frame(download_code=curl_download(url,paste0(destination_folder,name))),
    error=function(c) data.frame(error=as.character(c))
  )
}