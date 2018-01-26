library(ZillowR)
library(rvest)
library(dplyr)
library(DT)


# data1 = read.csv('Addresses.csv', header = F, colClasses = 'character')$V1
# data2 = read.csv('Addresses.csv', header = F, colClasses = 'character')$V2
# data = data.frame(street = data1, city.state = as.character(data2))

data = data.frame(
  street = c('25 Otsego Drive',
             '2 Utica Drive',
             '5 Seneca Drive'),
  city.state = c(rep('01749', 3)))

get.zillowdata = function(df, address, city.state){
  require(ZillowR)
  set_zillow_web_service_id('X1-ZWz18t7c2nm5mz_4vl2o')
  results = do.call(rbind, lapply(1:nrow(df), function(i){
    z = tryCatch({
      zdata = GetDeepSearchResults(address = df$street[i],
                                   citystatezip = df$city.state[i],
                                   zws_id = getOption("ZillowR-zws_id"),
                                   url = "http://www.zillow.com/webservice/GetDeepSearchResults.htm")
      return(zdata)
    },
    
    error = function(cond) {
      message(paste("No Data Available:", df$street[i], df$city.state[i]))
      return(NA) # Choose a return value in case of error
    },
    
    warning = function(cond) {
      message(paste("Zdata caused a warning:", df$street[i], df$city.state[i]))
      return(NA) # Choose a return value in case of warning
    },
    # print processing message to screen
    finally = {
      message(paste("Processed Address:", df$street[i], df$city.state[i]))
      message(paste(i, "of", nrow(df), 'processed'))
    }
    )
  }))
  
  if(nrow(results)==nrow(df)){
    results = cbind(df, results)
    
    print(paste('Original data had', nrow(df), 'rows. Returning a dataframe with', nrow(results),
                'rows. Returned dataframe has', sum(is.na(results$amount)), 'missing zdata values.'))
    
    return(results)
  }
  else(print("Error: nrows(df) do not match nrows(zdata)"))
}

get.zillowdata(data)


library(ZillowR)
library(xml2)
library(RCurl)

set_zillow_web_service_id('X1-ZWz18t7c2nm5mz_4vl2o')
output123 = GetDeepSearchResults(address = '25 Otsego Drive', citystatezip = '01749', zws_id = getOption("ZillowR-zws_id"), url = "http://www.zillow.com/webservice/GetSearchResults.htm")

results <- xmlToList(output123$response[["results"]])

getValRange <- function(x, hilo) {
  ifelse(hilo %in% unlist(dimnames(x)), x["text",hilo][[1]], NA)
}

out <- apply(results, MAR=2, function(property) {
  zpid <- property$zpid
  links <- unlist(property$links)
  address <- unlist(property$address)
  z <- property$zestimate
  zestdf <- list(
    amount=ifelse("text" %in% names(z$amount), z$amount$text, NA),
    lastupdated=z$"last-updated",
    valueChange=ifelse(length(z$valueChange)==0, NA, z$valueChange),
    valueLow=getValRange(z$valuationRange, "low"),
    valueHigh=getValRange(z$valuationRange, "high"),
    percentile=z$percentile)
  list(id=zpid, links, address, zestdf)
})

data <- as.data.frame(do.call(rbind, lapply(out, unlist)),
                      row.names=seq_len(length(out)))
...