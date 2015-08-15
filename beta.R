library(XML)
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(stringr)
library(shiny)
library(bitops)
library(RCurl)
library(httr)

# Function to parse the url from the html

i = 1
parseIt <- function(x){
  x <- GET(x)
  cat("Status: ",x$status_code,'\n')
  htmlTreeParse(x, useInternalNodes = TRUE,isURL = TRUE)
}

  
murl = "http://coimbatore.quikr.com/Real-Estate/w561"

ts <- tryCatch(parseIt(murl),
               error = function(e) NULL)
masterdata = list()
while(!is.null(ts) && i <=1000){
  ids1 = xpathApply(ts,'//div/div/div/div/div/div//*[@class="snb_entire_ad a1d1top "]',fun = function(x) xmlAttrs(x)[["id"]])
  ids2 = xpathApply(ts,'//div/div/div/div/div/div//*[@class="snb_entire_ad ad "]',fun = function(x) xmlAttrs(x)[["id"]])
  ids = c(ids1,ids2)
  dataset = lapply(ids,function(x){
    id = paste0('//div/div/div/div/div/div//*[@id="',x,'"]')
    title = xpathApply(ts,paste0(id,'/div/div/div/h3/a'),fun = xmlValue)
    hyper = xpathApply(ts,paste0(id,'/div/div/div/h3/a'),fun = function(x) c(xmlAttrs(x)[["href"]]))
    price = xpathApply(ts,paste0(id,'/div/div//*[@class="snb_price_tag"]'),fun = xmlValue)
    chic = xpathApply(ts,paste0(id,'/div/div//*[@class="snb_ad_chicklet"]'),fun = xmlValue)
    pics = xpathApply(ts,paste0(id,'/div/div//*[@class="multi_img_icon"]'),fun = xmlValue)
    type = xpathApply(ts,paste0(id,'/div'),fun = function(x) xmlAttrs(x)[["class"]])
    snippet = xpathApply(ts,paste0(id,'/div/div//*[@class="snb_ad_detail translate"]'),fun = xmlValue)
    date =  xpathApply(ts,paste0(id,'/div//*[@class="snb-price snb_date_wapp"]'),fun = xmlValue)
    c(title = title,hyper = hyper,
      price=price,chic=chic,pics=pics,
      type=type,snippet = snippet,
      date = date,url=murl, id = x)
  })
  masterdata = append(masterdata,dataset)
  i = i+1
  newlink = paste0(murl,"?page=",i)
  cat(paste0("Working on ",newlink),'\t')
  ts <- tryCatch(parseIt(newlink),
                 error = function(e) NULL)
}

ttt = (plyr::ldply(masterdata, data.frame))
View(plyr::ldply(dataset, data.frame))
saveRDS(ttt,"extracted.rds")
