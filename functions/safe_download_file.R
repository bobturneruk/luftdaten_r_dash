safe_download_file<-function(url,name,destination_folder){
  tryCatch(
    data.frame(download_code=download.file(url,paste0(destination_folder,name))),
    error=function(c) data.frame(error=as.character(c))
  )
}