computeFileInfo =
    #
    # Computes the line length for each file.
    # This was the initial information we computed for each file.
    # The name should change to reflect what it measures.
    #
function(files)    
{
    vals = lapply(files, function(f) structure(nchar(readLines(f)), class = "FileLineLength"))
    names(vals) = basename(files)
    vals
}

##########

toplevelTypes =
    # why hoist code - in case we parse(text = "....")
function(file, code = parse(file))
{
   unlist(sapply(code, getToplevelCodeType))
}


getToplevelCodeType =
function(e, followIf = TRUE)
{
 #   browser()
    if(is.expression(e)) {
        if(length(e) == 1)
            return( getToplevelCodeType(e[[1]]) )
        else
            return( sapply(as.list(e), getToplevelCodeType))
    }

    if(class(e) == "{")
        return(sapply(as.list(e), getToplevelCodeType))
    
    if(is.call(e)) {
        if(is.name(e[[1]])) {
            op = as.character(e[[1]])
            switch(op,
                   "=" =,
                   "<-" = getToplevelCodeType(e[[3]]),
                   "setMethod" = "S4method",
                   "setGeneric" = "S4generic",
                   "setAs" = "S4As",
                   "setClass" = "S4Class",                   
                   "setOldClass" = "S3OldClass",
                   "function" = "function",
                   "if" = switch(class(e[[2]]),
                                 "logical" = if(e[[2]]) "ifTRUE" else "ifFALSE",
                                  if(followIf) 
                                      list('if', sapply(e[[3]], getToplevelCodeType), if(length(e) > 3) sapply(e[[4]], getToplevelCodeType))
                                  else "if"),
                   "structure" = "data", # check class of first element. Check for class
                   c =,
                   "matrix" = ,
                   "data.frame" =,
                   array = "data",
                   if(isConstantOrCall(e)) "data" else "call")
#                    stop("check"))
        }
    } else {
        switch(class(e),
                   "numeric" =,
                   "integer" = ,
                   "logical" =,
                   "character" =,
                   "complex" = "data",
                   "{" = "{",
                   name = "symbol",
                    NULL = "NULL",
                   stop("check2"))
    }
}


isConstantOrCall =
    #
    # TRUE
    #  -1L
    #  -x
    # FALSE
    #  - foo(x) 
function(e)    
{
  is.call(e) && length(e) == 2 && (isLiteral(e[[2]]) || is.name(e[[2]]))
}


###
TypeColorMap =
    c(call = "black",
      data = "red",
      "function" = "green",
      S4method = "lightseagreen",
      S4As = "green4",
      S4generic = "darkolivegreen",
      S4Class = "purple",
      S3OldClass = "purple4",      
      "if" = "yellow",
      symbol = "gold",
      ifTRUE = "grey",
      ifFALSE = "red",
      NULL = "red3"
      )

getToplevelTypeDF =
function(file)
{
    tmp = toplevelTypes(file)
    data.frame(len = rep(1, length(tmp)),
               color = TypeColorMap[tmp],
               type = tmp)
}    



######################


simpleLineType =
function(file, lines = readLines(file))    
{

}





