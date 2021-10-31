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
