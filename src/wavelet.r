# This function computes the Haar wavelet coefficents of the 
# multivariate time series.

# This function depends on the R pacakge wmtsa.

WFE <- function(series){
  
    calcEnergies <- function(wavdecomp) {
        level <- length(wavdecomp$data) - 1
        energyD <- rep.int(0, level)
        energyD[1] <- sum(wavdecomp$data[[1]]^2)
        for (i in 1:level) {
            energyD[i] <- sum((wavdecomp$data[[i]])^2)
        }
        return(energyD)
    }
    
    max.level <- as.integer(ceiling(logb(length(series[1, ]), base = 2)))
    true.level <- as.integer(floor(logb(length(series[1, ]), base = 2)))
    
    if (max.level != true.level) {
        npad <- 2^max.level - length(series[1, ])
        for (i in 1:npad) {
            series <- cbind(series, rep(0, nrow(series)))
        }
    }
    
    energies <- matrix(0, nrow = nrow(series), ncol = max.level)
    for (i in 1:nrow(series)) {
        wavdecomp <- wmtsa::wavDWT(series[i, ], n.levels = max.level, wavelet = "haar")
        energies[i, ] <- calcEnergies(wavdecomp)
    }
    
    sumEnergies <- colSums(energies)
    final_level <- max.level
    for (i in 1:(max.level - 1)) {
        if (sumEnergies[i] < sumEnergies[i + 1]) {
            final_level <- i
            break
        }
    }
    
    wavdecomp <- wmtsa::wavDWT(series[1, ], n.levels = final_level, wavelet = "haar")
    out.series <- wavdecomp$data[[final_level + 1]]
    for (i in 2:nrow(series)) {
        wavdecomp <- wmtsa::wavDWT(series[i, ], n.levels = final_level, wavelet = "haar")
        out.series <- rbind(out.series, wavdecomp$data[[final_level + 1]])
    }
    
    return(out.series)
}

