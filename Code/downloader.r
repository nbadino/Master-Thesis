library(httr)
library(jsonlite)
library(utils)
library(XML)
library(RCurl)
library(stringr)
library(R.utils)
# Retrieve access token
mail <- readline(prompt="Enter mail: ")
pass <- readline(prompt="Enter password: ")
credentials <- c(mail,pass)
time_span <- c(2012,2021)
time_unit <- "year"
start=2012
end=2021
out_folder <- "E:/sats/eog_downloader"
options(timeout=300)

eog_downloadr <- function(credentials, start, end, data_type, out_folder){
  params <- list(
    client_id = 'eogdata_oidc',
    client_secret = '2677ad81-521b-4869-8480-6d05b9e57d48',
    username = credentials[1],
    password = credentials[2],
    grant_type = 'password'
  )
  token_url <- 'https://eogauth.mines.edu/auth/realms/master/protocol/openid-connect/token'
  response <- POST(token_url, body = params, encode = "form")
  access_token_list <- fromJSON(content(response,as="text",encoding="UTF-8"))
  access_token <- access_token_list$access_token
  # Submit request with token bearer and write to output file
  ## Change data_url variable to the file you want to download
  data_url <- 'https://eogdata.mines.edu/eog/EOG_sensitive_contents'
  auth <- paste('Bearer', access_token)
  print(auth)
  ## You can either define the output file name directly
  # output_file <- 'EOG_sensitive_contents.txt'
  ## Or get the filename from the data_url variable
  #output_file <- basename(data_url)
  #download.file(data_url,output_file,mode = "wb", headers = list(Authorization = auth))
  #credentials
  
  #month
  link <- "https://eogdata.mines.edu/nighttime_light/annual/v21/"
  xData <- getURL(link)
  doc <- htmlParse(xData)
  readHTMLTable(doc)[[1]]$Name -> file.list
  file.list
  for (i in file.list){
    link2 <- paste(link,i,sep="")
    print(link2)
    xData <- getURL(link2)
    doc <- htmlParse(xData)
    readHTMLTable(doc)[[1]]$Name -> file.list2
    print(file.list2)
    filename <- file.list2[grep(data_type, file.list2)]
    download_link <- paste(link2,filename,sep="")
    print(download_link)
    filename2 <- paste(out_folder,"/",gsub('/', '', i),".gz",sep="")
    download.file(download_link,filename2,mode = "wb", headers = list(Authorization = auth))
    gunzip(filename2, destname = paste(dirname(filename2),"/",i,".tif",sep=""), overwrite = FALSE, remove = FALSE, BFR.SIZE = 1e+07)                       
    
  }
}
eog_downloadr(credentials = credentials, 2012, 2021, "median_masked", out_folder = "F:/tmp/eog_downloader")
