computeFileInfo = getLineLengths =
    #
    # Computes the line length for each file.
    # This was the initial information we computed for each file.
    # The name should change to reflect what it measures.
    #
function(files)    
{
    vals = lapply(files, function(f) structure(nchar(readLines(f)), class = "FileLineLength"))
    names(vals) = basename(files)
    class(vals) = "FileLineLengthInfo"
    vals
}

##########

toplevelTypes =
    # why hoist code - in case we parse(text = "....")
function(file, names = TRUE, code = parse(file))
{
    # do we want the names of the objects
    ans = unlist(sapply(code, getToplevelCodeType))
    if(names)
        names(ans) = sapply(code, getToplevelName)

    ans
}

###
TypeColorMap =
    c(call = "black",
      data = "red",
      "function" = "green",
      S4method = "lightseagreen",
      S3method = "darkseagreen1",      
      S4As = "green4",
      S4generic = "mediumseagreen",      
      S3generic = "darkolivegreen",
      S4Class = "purple",
      S3OldClass = "purple4",      
      "if" = "yellow",
      symbol = "gold",
      ifTRUE = "grey",
      ifFALSE = "red",
      NULL = "red3"
      )

#getToplevelTypeDF =
getToplevelTypes =
function(file, asDataFrame = FALSE, colorMap = TypeColorMap)
{
    if(file.info(file)$isdir)
        file = getRFiles(file)

    if(length(file) > 1) {
        ans = lapply(file, getToplevelTypes)
        if(asDataFrame) {
            ans = as(ans, "data.frame") # MultiFileToplevelTypeInfo")
#            ans = do.call(rbind, ans)
#            class(ans) = c("MultiFileToplevelTypeInfo", "ToplevelTypeInfo", "data.frame")
        } else {
            names(ans) = file
            class(ans) = "ToplevelTypeInfoList"
        }
        return(ans)
    }
    
    tmp = toplevelTypes(file)
    structure(data.frame(len = rep(1, length(tmp)),
                         color = colorMap[tmp],
                         type = tmp, file = rep(file, length(tmp)),
                         name = names(tmp)),
              class = c("ToplevelTypeInfo", "data.frame"))
}    

setOldClass(c("MultiFileToplevelTypeInfo", "ToplevelTypeInfo", "data.frame", "list"))
setOldClass(c("ToplevelTypeInfoList", "list"))

setAs("ToplevelTypeInfoList", "data.frame", # "MultiFileToplevelTypeInfo",
      function(from) {
            ans = do.call(rbind, from)
            class(ans) = if(length(from) > 1)
                            c("MultiFileToplevelTypeInfo", "ToplevelTypeInfo", "data.frame")
                         else
                            c("ToplevelTypeInfo", "data.frame")
            ans
        })

setAs("MultiFileToplevelTypeInfo", "list", # "ToplevelTypeInfoList",
      function(from) {
            ans = split(from, from$file)
            ans = lapply(ans, function(x){
                                  class(x) = c("ToplevelTypeInfo", "data.frame")
                                  rownames(x) = NULL
                                  x
                              })          
            class(ans) =  "ToplevelTypeInfoList"
            ans
      })

######################


simpleLineType =
function(file, lines = readLines(file))    
{}





