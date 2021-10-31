origShowFiles =
    #
    #
    #
function(dir = "", vals = getLineLengths(files), files = getRFiles(dir, pattern), pattern = "\\.[RrSsQq]$",
         labelsAtTop = TRUE, labels = stripCommonPrefix(names(vals)),
         legend = TRUE, cex = 1, mar = NA, order = FALSE,
         ..., drawLines = TRUE, main = dir)
{
    if(order) {
        o = orderFiles(vals, TRUE)
        vals = vals[o]
        labels = labels[o]
    }
    
    mkEmptyPlot(labels, main, labelsAtTop, cex = cex, mar = mar, ...)
    
    showFileOutlines(vals, main = dir, drawLines = drawLines, ...)
#    if(!missing(legend) || isTRUE(legend))
#        mkLegend(legend, vals)
}



showFileOutlines =
function(vals, drawLines = TRUE, border = NULL, ...)
{
    x0 = seq(.5, by = 1, length = length(vals))

    nlines = sapply(vals, function(x) if(is.data.frame(x)) nrow(x) else length(x))
    bottom = 1 - nlines/max(nlines)
    
    ans = NULL
    if(drawLines) {
        ans = mapply(showFileElements, vals, x0, x0 + 1, bottom, MoreArgs = list(top = 1, ...), SIMPLIFY = FALSE)
        tmp = do.call(rbind, ans)
        # drawLines(tmp[, 1:2], tmp[[3]], ...)
        rect(tmp[,1], tmp[,2], tmp[,3], tmp[,4], col = tmp[[5]], border = border, ...)        
    }

    rect(x0, bottom, x0 + 1, rep(1, length(vals)))

   invisible(ans)
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
function(fileNames, main = "", labelsAtTop = TRUE, mar = NA, cex = 1, srt = 45, ...)
{
    opar = par(no.readonly = TRUE)
    on.exit(par(mar = opar$mar))
        
    if(is.na(mar)) {
        # nc = max(nchar(fileNames))
        nc = ceiling(sin(srt/180*pi)*cex*max(nchar(fileNames))/3)
        # old approach: ad hoc
        # mar = floor(cex*nc/3) # get correct scaling for cex and number of lines.
        mar = nc
    }
    
    if(length(mar) == 1)
        mar = c(mar, 1, 2, 1)
    # otherwise, mar better be of length 4. We don't need to raise an error as
    # par(mar = mar) will if it is not of length 4.

    if(labelsAtTop) {
        mar[3] = mar[1] + 3 # extra 2 lines is for the title.
        mar[1] = 1
        y = par("usr")[4]*1.01
    } else
        par("usr")[3]*1.2
    
    par(mar = mar)
    plot(0, xlim = c(1, length(fileNames)), ylim = c(0, 1), yaxs = "i", type = "n", axes = FALSE, xlab = "", ylab = "", ...)
    title(main, line = floor(par()$mar[3]) - 1)
    box()

    text(labels = fileNames, x = 1:length(fileNames), y = y, xpd = NA, srt = srt, adj = if(labelsAtTop) 0 else 1, cex = cex, ...)
}



showFileElements =
function(elInfo, left, right, bottom, top = 1,
         color = if(is.data.frame(elInfo)) elInfo[,2] else "black", ...)
{
    coords = computeFileLineCoords(elInfo, left, right, bottom, top = top)

    if(length(color) != nrow(coords))
        color = rep(color, nrow(coords))  # each = 3)

    cbind(as.data.frame(coords), color = color)
}

computeFileLineCoords =
function(elInfo, left, right, bottom, top = 1)
{
    if(is.data.frame(elInfo))
        elInfo = elInfo[,1]

    len = length(elInfo)

    if(len == 0)
        return(matrix(0, 0, 4))

    x0 = rep(left, len)
    x1 = left + (right - left)*elInfo/max(elInfo)    

    tmp = seq(top, bottom, length = len + 1L)
    y1 = tmp[- (len + 1L)]
    y0 = tmp[-1]
    
    cbind(x0, y0, x1, y1)
}

if(FALSE) {
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
}



# Not used if we use rectangles. So check the code to see if used.
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

##################

mkLegend =
function(x, colorMap = TypeColorMap, order = FALSE)
{
    # if order is TRUE or is an integer
#    if(is.logical(order) && order)
#        order = order()
    types = unique(unlist(lapply(x, `[[`, "type")))
    m = match(types, names(colorMap))

    m = m[ order(names(colorMap)[m]) ]
        # Compute location based on the heights of the rectangles
        # so we find blank areas.
        # if none, then should have put this beside the plot which
        # is quite a different strategy.
    legend(1, .4, legend = names(colorMap)[m], fill = colorMap[m])
}
