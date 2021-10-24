showFiles =
    #
    #
    #
function(dir, vals = computeFileInfo(files), files = getRFiles(dir, pattern), pattern = "\\.[RrSsQq]$", ..., drawLines = TRUE)
{
    showFileOutlines(vals, main = dir, drawLines = drawLines, ...)
}

computeFileInfo =
function(files)    
{
    vals = lapply(files, function(f) structure(nchar(readLines(f)), class = "FileLineLength"))
    names(vals) = basename(files)
    vals
}

showFileOutlines =
function(vals, main = "", drawLines = TRUE, ...)
{
    opar = par(no.readonly = TRUE)
    on.exit(par(mar = opar$mar))

    mkEmptyPlot(names(vals), main)
    
    x0 = seq(.5, by = 1, length = length(vals))
#    nlines = sapply(vals, length)
    nlines = sapply(vals, function(x) if(is.data.frame(x)) nrow(x) else length(x))
    bottom = 1 - nlines/max(nlines)
    rect(x0, bottom, x0 + 1, rep(1, length(vals)))

    if(drawLines) {
        ans = mapply(showFileElements, vals, x0, x0 + 1, bottom, MoreArgs = list(top = 1, ...))
        #ans = do.call(rbind, ans)
        #lines(ans)
    }
}

mkEmptyPlot =
function(fileNames, main = "")
{
    nc = max(nchar(fileNames))
    par(mar = c(floor(nc/3), 1, 2, 1))
    plot(0, xlim = c(1, length(fileNames)), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", main = main)
    box()
    text(labels = fileNames, x = 1:length(fileNames), y = par("usr")[3]*1.2, xpd = NA, srt = 45, adj = 1)
}


showFileElements =
function(elInfo, left, right, bottom, top = 1,
         color = if(is.data.frame(elInfo)) elInfo[,2] else "black", ...)
{
    coords = computeFileLineCoords(elInfo, left, right, bottom, top = top)
    
    if(length(color) != nrow(coords))
        color = rep(color, each = 3)

    # lines(coords, col = color)
    drawLines(coords, col = color, ...)
}

computeFileLineCoords =
function(elInfo, left, right, bottom, top = 1)
{

    if(is.data.frame(elInfo))
        elInfo = elInfo[,1]
    
    # elInfo will initially be the number of characters per line
    # so we want to draw each line between left and right proportionally
    # to that length.

    # lines(c(10, 10.5, NA, 10, 10.6) + 1, c(.9, .9, NA, .8, .8), col = "red")
    len = length(elInfo)
    yv = seq(top, bottom, length = len)
    y = x = rep(NA, 3 * len)
    i = seq(1, by = 1, length = 3*len)
    y[ (i %% 3 != 0) ] = rep(yv, each = 2)

    i = seq(1, by = 3, length = len)
    x[i] = left
    x[i+1] = left + (right - left)*elInfo/max(elInfo)
      
    cbind(x, y)
}



drawLines =
function(coords, col = coords[, 3], ...)    
{
    groups =  split(1:length(col), col)
    # Could do this with by(), tapply(), aggregate()
    mapply(function(idx, col) {
             lines(coords[idx, 1:2], col = col, ...)
           }, groups, names(groups))
   
}
    
