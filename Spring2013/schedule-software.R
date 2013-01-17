# Software for putting together the schedule


acroChapter <- function(chap,access="ISM13s",probnum=NULL,label=chap){
  if (!is.null(probnum)) probnum <- paste("#",probnum,sep="")
  
  paste("<a href='",
        paste("https://dl.dropbox.com/u/5098197/Exercises/HTML/",
              "SM2-Chapter-",chap,"-Problems/SM2-Chapter-",chap,
              "-Problems-A.html?access=",access,probnum,sep=""),
        "'>",label,"</a>",sep="")
}
# A short form for individual problems.
PN <- function(probs){
  res <- ""
  for (probnum in probs){
    chap <- substr(probnum,1,nchar(probnum)-2)
    prob <- substr(probnum,nchar(chap)+1,100)
    label <- paste(chap,".",prob,sep="")
    foo <- acroChapter(chap=chap, probnum=probnum, label=label )
    res <- paste(res,", ",foo,sep="")
  }
  return(substr(res,3,10000)) # But get rid of the trailing comma.
}

# Setting up the calendar material
makeDayLinkName <- function(month,day,year=2013){
  paste("Day-", year, "-", month, "-", day, sep="")
}
dd <- function(month,day,year=2013) {
  paste("<a target='self' href='#",makeDayLinkName(month,day,year),"'>",day,"</a>",sep="")
}
classDayContents <- function(weekday="", month, day, year=2013) {
  # put in the header and a bookmark
  paste("<hr>\n### ", 
        paste('<a name="',makeDayLinkName(month,day,year),'">',weekday," ", day," ", month,"</a>",sep=""))
}
todaysNotes <- function(orig,fname,marker="notes") {
  paste("<a href='../Notes/", fname, "'>",marker,"</a>","<a href='../Notes/",orig,"'>*</a>",sep="")
}