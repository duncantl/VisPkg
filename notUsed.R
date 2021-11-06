if(FALSE) {
    
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
}
