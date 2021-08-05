prepLegendCoords<- function(leg.loc) {
if (is.list(leg.loc))
{
  if ( exists('x' , where=leg.loc) && (exists('y' ,where=leg.loc)) )
  {
    return(leg.loc)
  }
}
  else
  {
    if (leg.loc == "topright") {
      lab.x <- 0.9
      lab.y <- 0.9
    }
    if (leg.loc == "topleft") {
      lab.x <- 0.01
      lab.y <- 0.9
    }
    if (leg.loc == "bottomright") {
      lab.x <- 0.9
      lab.y <- 0.2
    }
    if (leg.loc == "bottomleft") {
      lab.x <- 0.01
      lab.y <- 0.2
    }
    if (leg.loc == "bottom") {
      lab.x <- 0.45
      lab.y <- 0.2
    }
    
    if (leg.loc == "top") {
      lab.x <- 0.45
      lab.y <- 0.9
    }
    
    if (leg.loc == "left") {
      lab.x <- 0.01
      lab.y <- 0.5
    }
    
    if (leg.loc == "right") {
      lab.x <- 0.9
      lab.y <- 0.5
    }
    
    if (leg.loc == "middle") {
      lab.x <- 0.45
      lab.y <- 0.5
    }
     leg.loc=list(x=lab.x,y=lab.y) 
     return(leg.loc)
  }
}

