col1 = c(1,1,2,10)
col2 = c(2,2,4,15)
col3 = c(3,3,3,18)
col4 = c(3,3,3,21)
col5 = c(1,1,1,16)
bbb = rbind(col1,col2,col3,col4,col5)

k=5
numerX=rep(0,k)
numerY=rep(0,k)
denom = sum(1/bbb[1:k,4])
for (j in 1:k) {
  numerX[j] = bbb[j,2]*((1/bbb[j,4])/denom)
  numerY[j] = bbb[j,3]*((1/bbb[j,4])/denom)
}
estX = sum(numerX)
estY = sum(numerY)
estXY = cbind(estX,estY)

  nn = lapply(bbb[,], function(x) sapply(x[ , 2:3],
                                      function(x) print(x[])))
                                      
                                      
                                      sapply(bbb[,4], function(y) {(1/y)/denom})
  
  
                                      sapply(bbb[,4], function(y) {(1/y)/denom})
                                      
  cccXY = lapply(bbb, 
                 function(x) sapply(x[ , 2:3], 
                                    function(x) x[,4]))
cccXY = do.call("rbind", cccXY)
}

estXY = lapply(closeXY, 
               function(x) sapply(x[ , 2:3], 
                                  function(x) mean(x[1:k])))
estXY = do.call("rbind", estXY)