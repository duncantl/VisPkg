showFiles =
    #
    #
    #
function(dir, files = list.files(dir, full.names = TRUE, ...), ...)
{
    vals = lapply(files, function(f) structure(nchar(readLines(f)), class = "FileLineLength"))
    names(vals) = basename(files)
    showFileOutlines(vals, main = dir)
}

showFileOutlines =
function(vals, main = "")
{
    opar = par(no.readonly = TRUE)
    on.exit(par(mar = opar$mar))
    nc = max(nchar(names(vals)))
    par(mar = c(floor(nc/10), 1, 2, 1))
    plot(0, xlim = c(1, length(vals)), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
    box()
    text(labels = names(vals), x = 1:length(vals), y = par("usr")[3]*1.2, xpd = NA, srt = 45, adj = 1)

    title(main)
    
    x0 = seq(.5, by = 1, length = length(vals))
    nlines = sapply(vals, length)
    bottom = 1 - nlines/max(nlines)
    rect(x0, bottom, x0 + 1, rep(1, length(vals)))

    ans = mapply(showFileElements, vals, x0, x0 + 1, bottom, MoreArgs = list(top = 1))
    ans = do.call(rbind, ans)
    lines(ans)
    
}

showFileElements =
function(elInfo, left, right, bottom, top = 1)
{
    # elInfo will initially be the number of characters per line
    # so we want to draw each line between left and right proportionally
    # to that length.

    # lines(c(10, 10.5, NA, 10, 10.6) + 1, c(.9, .9, NA, .8, .8), col = "red")
    len = length(elInfo)
    yv = seq(top, bottom, length = len)
    y = x = rep(NA, 3 * len)
    i = seq(1, by = 1, length = 3*len)
    y[ !(i %% 0) ] = rep(yv, each = 2)

    i = seq(1, by = 3, length = len)
    x[i] = left
    x[i+1] = left + (right - left)*elInfo/max(elInfo)
      
    cbind(x, y)
}
