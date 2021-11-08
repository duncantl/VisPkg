origShowFiles =
    #
    #
    #
function(dir = "", vals = getLineLengths(files), files = getRFiles(dir, pattern), pattern = "\\.[RrSsQq]$",
         labelsAtTop = TRUE, labels = stripCommonPrefix(names(vals)),
         legend = !is.null(colorMap), cex = 1, mar = NA, order = FALSE,
         colorMap = if(inherits(vals, ("ToplevelTypeInfoList"))) TypeColorMap else NULL, # make a method.
         border = NULL,
         ..., drawLines = TRUE, main = dir)
{
    if(order) {
        o = orderFiles(vals, TRUE)
        vals = vals[o]
        labels = labels[o]
    }
    
    mkEmptyPlot(labels, main, labelsAtTop, cex = cex, mar = mar, ...)
    
    showFileOutlines(vals, main = dir, drawLines = drawLines, colorMap = colorMap, border = border, ...)
    if(!missing(legend) || isTRUE(legend))
        mkLegend(vals, legend, colorMap = colorMap)
}



showFileOutlines =
function(vals, drawLines = TRUE, border = NULL, colorMap = NULL, ...)
{
    x0 = seq(0, by = 1, length = length(vals))

    nlines = sapply(vals, function(x) if(is.data.frame(x)) nrow(x) else length(x))
    bottom = 1 - nlines/max(nlines)
    
    ans = NULL
    if(drawLines) {
        ans = mapply(showFileElements, vals, x0, x0 + 1, bottom, MoreArgs = list(top = 1, ...), SIMPLIFY = FALSE)
        tmp = do.call(rbind, ans)
        colors = mapColors(tmp[[5]], colorMap)
        rect(tmp[,1], tmp[,2], tmp[,3], tmp[,4], col = colors, border = border, xpd = TRUE, ...)        
    }

    rect(x0, bottom, x0 + 1, rep(1, length(vals)))

    invisible(ans)
}

mkEmptyPlot =
function(fileNames, main = "", labelsAtTop = TRUE, mar = NA, cex = 1, srt = 45, ...)
{
    opar = par(no.readonly = TRUE)
    on.exit(par(mar = opar$mar))
        
    if(length(mar) ==1 && is.na(mar)) 
        # nc = max(nchar(fileNames))
        mar = ceiling(sin(srt/180*pi)*cex*max(nchar(fileNames))/3)
        # old approach: ad hoc
        # mar = floor(cex*nc/3) # get correct scaling for cex and number of lines.
    
    if(length(mar) == 1)
        mar = c(mar, 1, 2, 1)
    # otherwise, mar better be of length 4. We don't need to raise an error as
    # par(mar = mar) will if it is not of length 4.

    if(labelsAtTop) {
        mar[3] = mar[1] + 3 # extra 2 lines is for the title.
        mar[1] = 1
        y = par("usr")[4]*1.01
    } else {
        y = par("usr")[3]*1.075
    }
    
    par(mar = mar)
    plot(0, xlim = c(0, length(fileNames)), ylim = c(-0.1, 1), yaxs = "i", type = "n", axes = FALSE, xlab = "", ylab = "", ...)
    title(main, line = floor(par()$mar[3]) - 1)
    box()

    text(labels = fileNames, x = seq(.5, by = .5, length = length(fileNames)), y = y, xpd = NA, srt = srt, adj = if(labelsAtTop) 0 else 1, cex = cex, ...)
}


showFileElements =
function(elInfo, left, right, bottom, top = 1,
         color = if(is.data.frame(elInfo)) elInfo[,2] else "black",
         ...)
{
    coords = computeFileLineCoords(elInfo, left, right, bottom, top = top)

#    if(length(color) != nrow(coords))
#        color = rep(color, nrow(coords))  # each = 3)

    ans = as.data.frame(coords)
    ans$type = color
    ans
}

computeFileLineCoords =
function(elInfo, left, right, bottom, top = 1)
    UseMethod("computeFileLineCoords")

computeFileLineCoords.numeric = computeFileLineCoords.integer =
function(elInfo, left, right, bottom, top = 1)
{
    len = length(elInfo)

    if(len == 0)
        return(matrix(0, 0, 4))

    x0 = rep(left, len)
    x1 = left + (right - left)*elInfo/max(elInfo, na.rm = TRUE)    

    tmp = seq(top, bottom, length = len + 1L)
    y1 = tmp[- (len + 1L)]
    y0 = tmp[-1]
    
    cbind(x0, y0, x1, y1)
}

computeFileLineCoords.data.frame =
    # Define for data.frame or the more specific class.
function(elInfo, left, right, bottom, top = 1)
    computeFileLineCoords(elInfo[,1], left, right, bottom, top)

computeFileLineCoords.ToplevelSizeInfo =
    # Define for data.frame or the more specific class.
function(elInfo, left, right, bottom, top = 1)
{
    ans = NextMethod()

    sz = elInfo$size/sum(elInfo$size)
    sz2 = cumsum(sz)
    tmp = top - sz2*(top-bottom)
    ans[, "y0"] = tmp
    ans[, "y1"] = c(top, tmp [-length(tmp) ])
    ans
}


##################

mkLegend =
function(x, pos = c(0, .0), colorMap = TypeColorMap, order = FALSE)
{
    if(isTRUE(pos))
        pos = c(0, .0)  # Annoying to have this in two places. As we experiment with good default location with only one file, this involves a lot of changes.
    
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
    legend(pos[1], pos[2], legend = names(colorMap)[m], fill = colorMap[m], xpd = TRUE)
}


########
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


mapColors =
function(vals, colorMap)    
{
    if(length(colorMap) == 0)
        return(rep("black", length(vals)))
    
    ans = colorMap[vals]
    ans[ !(vals %in% names(colorMap)) ] = "black"
    ans
}
