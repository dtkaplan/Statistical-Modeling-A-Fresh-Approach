read.path <- "/Users/kaplan/Downloads/Text_all/"
write.path <- "/Users/kaplan/temp/DVB/"

convertOneFile <- function(name){
  d <- read.delim( paste(read.path,name,sep=""),na.strings=c(".", "NA") )
  name <- sub( "(TXT|txt)$", "csv", name)
  name <- sub( " ", "_", name)
  write.csv(d, paste(write.path,name,sep=""), row.names=FALSE )
}
convertToCSV <- function(names) {
  for (name in names) {
    tryCatch(convertOneFile(name), 
             error = function(e){print(paste("Error:",name))}, 
             finally=print(name))
  }
}

fnames <- dir( path=read.path, pattern="(txt|TXT)$")