plot.ToplevelTypeInfoList =
function(x, ...)
    showFiles(x, ...)    
#could have been
#   origShowFiles(vals = x, ...)
# but this is better. (Why?)

plot.MultiFileToplevelTypeInfo =
function(x, ...)
    plot(as(x, "list"), ...)

plot.FileLineLengthInfo =
function(x, ...)
    showFiles(vals = x, ...)

showFiles =
function(info, labelsAtTop = TRUE, labels = stripCommonPrefix(names(vals)),
         legend = TRUE,             
         ..., drawLines = TRUE, main = dir)
    UseMethod("showFiles")

showFiles =
function(info, labelsAtTop = TRUE, labels = stripCommonPrefix(names(info)),
         legend = TRUE,             
         ..., drawLines = TRUE, main = character())
    UseMethod("showFiles")


showFiles.ToplevelTypeInfoList = showFiles.FileLineLengthInfo =
function(info, labelsAtTop = TRUE, labels = stripCommonPrefix(names(info)),
         legend = TRUE,             
         ..., drawLines = TRUE, main = character())
    origShowFiles(vals = info, labelsAtTop = labelsAtTop, labels = labels, legend = legend, ..., drawLines = drawLines, main = main)


showFiles.MultiFileToplevelTypeInfo =
function(info, labelsAtTop = TRUE, labels = stripCommonPrefix(names(info)),
         legend = TRUE,             
         ..., drawLines = TRUE, main = character())
{
    info = as(info, "list")
    showFiles(info, labelsAtTop = labelsAtTop, labels = labels, legend = legend, ..., drawLines = drawLines, main = main)
}


showFiles.character =
function(info, labelsAtTop = TRUE, labels = stripCommonPrefix(names(info)),
         legend = TRUE, FUN = getLineLengths,  pattern = "\\.[RrSsQq]$",
         ..., drawLines = TRUE, main = character())
{
    info = xpdFileNames(info, pattern)
    info = FUN(info)
    showFiles(info, labelsAtTop = labelsAtTop, labels = labels, legend = legend, ..., drawLines = drawLines, main = main)
}


`[.ToplevelTypeInfoList` = `[.FileLineLengthInfo` =
function(x, i, j, ...)
   structure(NextMethod(), class = class(x))


xpdFileNames =
    #
    # e.g. xpdFileNames(c("VisPkg/S3methods.R", dir, "VisPkg/fileInfo.R"), order = TRUE)
    #
function(info, pattern = '\\.[RrSsQq]$', order = FALSE) # add recursive = TRUE/FALSE
{
    i = file.info(info)
    if(any(i$isdir)) {
        tmp = lapply(info[i$isdir], getRFiles, pattern = pattern)
        if(!order)
            info = c(info[!i$isdir], unlist(tmp))
        else {
            info = as.list(info)
            info[i$isdir] = tmp
            info = unlist(info)
        }
    }

    info
}
