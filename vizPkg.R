showFiles =
    #
    #
    #
function(dir, files = list.files(dir, full.names = TRUE, ...), ...)
{
    vals = lapply(files, function(f) structure(nchar(readLines(f)), class = "FileLineLength"))
    names(vals) = basename(files)

    opar = par(no.readonly = TRUE)
    on.exit(par(mar = opar$mar))
    nc = max(nchar(files))
    par(mar = c(floor(nc/10), 1, 2, 1))
    plot(0, xlim = c(1, length(vals)), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
    box()
    #axis(1)
    text(labels = names(vals), x = 1:length(vals), y = par("usr")[3]*1.2, xpd = NA, srt = 45, adj = 1)
    #    mtext(names(vals), 1, 1, outer = TRUE, srt = pi/4)

    x0 = seq(.5, by = 1, length = length(vals))
    nlines = sapply(vals, length)
    rect(x0, 1- nlines/max(nlines), x0 + 1, rep(1, length(vals)))
    title(dir)
}
