refs = seq(0, by = 45, length  = 9)
angles = 120
q = sapply(angles, function(z) which.min(abs(z - refs)))
typeof(q)
c(refs[1:8], 0)[q]
typeof(q)
q