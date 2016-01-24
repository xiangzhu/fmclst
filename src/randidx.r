# This function computes the Rand index.

RI <- function(cls1,cls2){
	L <- length(cls1)
	A=B=C=D=0
	for(i in 1:(L-1)){
		for(j in (i+1):L){
			if ( (cls1[i]==cls1[j]) && (cls2[i]==cls2[j]) ){
				A <- A+1
			} else if ( (cls1[i]!=cls1[j]) && (cls2[i]!=cls2[j]) ){
				B <- B+1	
			} else if ( (cls1[i]==cls1[j]) && (cls2[i]!=cls2[j]) ){
				C <- C+1
			} else
				D <- D+1
}
}
	 if ( (cls1[L]==cls1[L]) && (cls2[L]==cls2[L]) ){
                                A <- A+1
         } else if ( (cls1[L]!=cls1[L]) && (cls2[L]!=cls2[L]) ){
                                B <- B+1        
         } else if ( (cls1[L]==cls1[L]) && (cls2[L]!=cls2[L]) ){
                                C <- C+1
         } else
                                D <- D+1

	randidx <- (A+B) / (A+B+C+D)
	return(randidx)
}
