divider <- function(x,y) {
        result <- x/y
        print(result)
        
}

multiplier <- function(x, y) {
        
        x * y * pi
}



#farienhight to celcuis 
c_temp <- function(x){
        round(tc=((5/9)*(x - 32)))
}

x <- 1
for(i in 1:64){
        
        x <- x + x 
}
y <- x/100
y

nott_temp <- read.table("nott.txt", skip=2)

for (i in nott_temp){
        
        x <- c_temp(i)
        [i] <- x
}
