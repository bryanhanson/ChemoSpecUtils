prepLegendCoords<- function(spectra,leg.loc,y.min,y.max,x.min,x.max) {
if (is.list(leg.loc))
{
  leg.loc$x <- (leg.loc$x) * (x.max - x.min) + x.min
  leg.loc$y <- (leg.loc$y) * (y.max - y.min) + y.min
  return(leg.loc) 
}
  else
  {
    return(leg.loc)
  }
}

