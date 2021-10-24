origShowFiles =
    #
    #
    #
function(dir = "", vals = getLineLengths(files), files = getRFiles(dir, pattern), pattern = "\\.[RrSsQq]$",
         labelsAtTop = TRUE, labels = stripCommonPrefix(names(vals)),
         legend = TRUE,             
         ..., drawLines = TRUE, main = dir)
{
    opar = par(no.readonly = TRUE)
    on.exit(par(mar = opar$mar))

    mkEmptyPlot(labels, main, labelsAtTop)
    
    showFileOutlines(vals, main = dir, drawLines = drawLines, labelsAtTop = labelsAtTop, ...)
#    if(!missing(legend) || isTRUE(legend))
#        mkLegend(legend, vals)
}



showFileOutlines =
function(vals, drawLines = TRUE, ...)
{
    x0 = seq(.5, by = 1, length = length(vals))

    nlines = sapply(vals, function(x) if(is.data.frame(x)) nrow(x) else length(x))
    bottom = 1 - nlines/max(nlines)
    rect(x0, bottom, x0 + 1, rep(1, length(vals)))

    if(drawLines) {
        ans = mapply(showFileElements, vals, x0, x0 + 1, bottom, MoreArgs = list(top = 1, ...), SIMPLIFY = FALSE)
        tmp = do.call(rbind, ans)
        drawLines(tmp[, 1:2], tmp[[3]])
        return(invisible(ans))
    }
}

stripCommonPrefix =
function(vals)    
{
      # See if they are all in the same directory before requiring Rlibstree be available.
    pre = dirname(vals)
    if(length(unique(pre)) == 1)
        return(basename(vals))

    pre = Rlibstree::getCommonPrefix(vals)
    if(length(pre))
        substring(vals, nchar(pre) + 1L)
    else
        vals
}

mkEmptyPlot =
function(fileNames, main = "", labelsAtTop = TRUE)
{
    nc = max(nchar(fileNames))
    mar = c(floor(nc/3), 1, 2, 1)
    if(labelsAtTop) {
        mar[3] = mar[1] + 2
        mar[1] = 1
        y = par("usr")[4]*1.01
    } else
        par("usr")[3]*1.2
    
    par(mar = mar)
    plot(0, xlim = c(1, length(fileNames)), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "", main = main)
    box()

    text(labels = fileNames, x = 1:length(fileNames), y = y, xpd = NA, srt = 45, adj = if(labelsAtTop) 0 else 1)
}


showFileElements =
function(elInfo, left, right, bottom, top = 1,
         color = if(is.data.frame(elInfo)) elInfo[,2] else "black", ...)
{
    coords = computeFileLineCoords(elInfo, left, right, bottom, top = top)
    
    if(length(color) != nrow(coords))
        color = rep(color, each = 3)

    cbind(as.data.frame(coords), color = color)
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
    #
    # Draws the lines specified in the coords matrix
    # but groups these into separate calls based on the
    # colors specified in col
    # This is because lines() only uses the first color
    # and is not vectorized in the col parameter.
    #
function(coords, col = coords[, 3], ...)    
{
    groups =  split(1:length(col), col)
    # Could do this with by(), tapply(), aggregate()
    mapply(function(idx, col) {
             lines(coords[idx, 1:2], col = col, ...)
           }, groups, names(groups))
   
}
    
