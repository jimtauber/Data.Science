fibonacci <- function(x,len = 10){
  xactive <- x
  xold <- x 
  xworking <- xold + xactive
  if ( x <= 0){
    print("Please enter a positive integer")
  }
  
    print(x)
    for (i in 1:len)  {
      xworking <- xold + xactive
      print(xworking)
      xold <- xactive
      xactive <- xworking
      ## print(xworking)
  }
  
}


