plot.ToplevelTypeInfoList =
function(x, ...)
    showFiles(vals = x, ...)

plot.MultiFileToplevelTypeInfo =
function(x, ...)
    plot(as(x, "list"), ...)

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
         legend = TRUE, FUN = computeFileInfo,  pattern = "\\.[RrSsQq]$",
         ..., drawLines = TRUE, main = character())
{
    i = file.info(info)
    if(any(i$isdir))
        info = c(info[!i$isdir], unlist(lapply(info[i$isdir], getRFiles, pattern = pattern)))

    info = FUN(info)
    showFiles(info, labelsAtTop = labelsAtTop, labels = labels, legend = legend, ..., drawLines = drawLines, main = main)
}


`[.ToplevelTypeInfoList` = `[.FileLineLengthInfo` =
function(x, i, j, ...)
   structure(NextMethod(), class = class(x))

