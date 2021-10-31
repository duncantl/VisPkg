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
    # why hoist code - in case we parse(text = "....").
    # Then we can pass it via code argument.
function(file, S3Methods = NULL, names = TRUE, code = parse(file))
{
    # do we want the names of the objects
    ans = unlist(sapply(code, getToplevelCodeType, S3Methods))
    if(names && length(code) > 0)
        names(ans) = sapply(code, getToplevelName)

    ans
}

###
TypeColorMap =
    c(call = "orange",
      data = "lightblue", # cyan
      "function" = "green",
      "if" = "yellow",
      "if(TRUE)" = "grey",
      "if(FALSE)" = "red",
      NULL = "red3",   
      S4Class = "purple",
      S4generic = "mediumseagreen",      
      S4method = "lightseagreen",
      S4As = "green4",
      S3OldClass = "purple4",
      S3generic = "darkolivegreen",
      S3method = "darkseagreen1",
      symbol = "gold"
     )

getS3Methods =
    # x could be
    #  1) a namespace object/environment
    #  2) the name of a package
    #  3) a directory name
    #  4) a file name in the R directory
    # 
function(x)
{
    if(is.environment(x)) {
        if(isNamespace(x))
            return(getS3Methods(x$.__NAMESPACE__.))
        
        # Check if S3Methods exists. If none registered, maybe no entry.
        return(x$S3methods)
    } else if(is.character(x)) {
        if(!file.exists(x))
            return(getS3Methods(getNamespace(x)))

        if(!file.info(x)$isdir)
            return(getS3Methods(dirname(x)))

        if(basename(x) %in% c("R", "src", "man"))
            x = dirname(x)

        getS3Methods(basename(x))
    }

    

}

#getToplevelTypeDF =
getToplevelTypes =
    #
    # ns = getNamespace("tools")
    # ty = getToplevelTypes("~/R/R-new/src/library/tools/R", S3Methods = ns$.__NAMESPACE__.$S3methods)
    # This identifies the S3 methods.
    #
    # Now finds the S3 methods from file.
    #
function(file, S3Methods = getS3Methods(file[1]), asDataFrame = FALSE, colorMap = TypeColorMap)
{
    if(file.info(file)$isdir)
        file = getRFiles(file)

    if(length(file) > 1) {
        ans = lapply(file, getToplevelTypes, S3Methods, asDataFrame, colorMap)
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
    
    tmp = toplevelTypes(file, S3Methods)
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






########################

orderFiles =
function(x, asIndices = FALSE)
{
    w = sapply(x, is.data.frame)

    v = if(all(w))
           sapply(x, nrow)
        else
            sapply(x, length)

    o = order(v)    
    if(asIndices)
        o
    else
        x[o]        
}
