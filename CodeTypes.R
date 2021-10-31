getToplevelCodeType =
    #
    #
    #
function(e, S3Methods = NULL, followIf = FALSE) # was TRUE
{
    if(is.expression(e)) {
        if(length(e) == 1)
            return( getToplevelCodeType(e[[1]], S3Methods, followIf) )
        else
            return( sapply(as.list(e), getToplevelCodeType, S3Methods, followIf))
    }

    if(class(e) == "{")
        return(sapply(as.list(e), getToplevelCodeType, S3Methods, followIf))
    
    if(is.call(e)) {
        if(is.name(e[[1]])) {
            op = as.character(e[[1]])
            switch(op,
                   "=" =,
                   "<-" = if(isS3Method(e[[2]], S3Methods))
                             "S3method"
                          else
                             getToplevelCodeType(e[[3]], S3Methods, followIf),
                   "setMethod" = "S4method",
                   "setGeneric" = "S4generic",
                   "setAs" = "S4As",
                   "setClass" = "S4Class",                   
                   "setOldClass" = "S3OldClass",
                   "function" = getFunctionType(e),
                   "if" = switch(class(e[[2]]),
                                 "logical" = if(e[[2]]) "if(TRUE)" else "if(FALSE)",
                                  if(followIf) 
                                     list('if',
                                          sapply(e[[3]], getToplevelCodeType, S3Methods, followIf),
                                          if(length(e) > 3)
                                             sapply(e[[4]], getToplevelCodeType, S3Methods, followIf))
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

isS3Method =
function(name, S3Methods = NULL)
{
    if(!is.null(S3Methods)) {
        if(is.call(name))
            return(FALSE)
        
        if(is.name(name))
            name = as.character(name)
        return(name %in% S3Methods[,3])
   }

    FALSE
}

getFunctionType =
function(e)
{
    k = findCallsTo(e, c("UseMethod", "NextMethod"))
    if(length(k) == 0)
        return("function")

    k = sapply(k, function(x) deparse(x[[1]]))
    if("NextMethod" %in% k)
        "S3method"
    else
        "S3generic"
#    browser()    
}


getToplevelName =
function(e)
{
    if(is.expression(e))
        return(NA)
    if(class(e)  == "{")
        return("{")
    
    if(is.call(e)) {
        if(is.name(e[[1]])) {
            op = as.character(e[[1]])
            two = NA
            if(length(e) > 1)
                two = rmQuote(deparse(e[[2]]))
            
            switch(op,
                   "=" =,
                   "<-" = two,
                   setMethod = paste(two), # get signature
                   setGeneric = two,
                   setAs = sprintf("%s -> %s", rmQuote(two), rmQuote(deparse(e[[3]]))),
                   setClass = two, 
                   setOldClass = { if(isLiteral(e[[2]])) two else rmQuote(deparse(e[[2]][[2]]))},  # c()
                   "if" = if(isLiteral(e[[2]])) sprintf("if(%s)", e[[2]]),
                   op
#                   browser()
                   )
        }
    } else if(is.name(e))
        as.character(e)
    else
        browser()
}

rmQuote =
function(x)
    gsub("(^[\"']|[\"']$)", "", x)

isConstantOrCall =
    #
    # TRUE
    #  -1L
    #  -x
    # FALSE
    #  - foo(x) 
function(e)    
  is.call(e) && length(e) == 2 && (isLiteral(e[[2]]) || is.name(e[[2]]))
