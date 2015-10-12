library(plotrix)

t3 <- c("*Mass partitioning*:  the relative distribution of biomass between different tree tissue components
 such as leaves, branches, boles and roots.  

*Carbon allocation*:  the fraction of net primary productivity distributed to different ecosystem 
components such as specific tissue components or total belowground pools.
        ")

plot.new()
textbox(c(0,1), .75,t3, justify='l', cex=0.6,
        leading=2, font=4, border="black", lty=2, lwd=3, margin=c(-.2, 0.05,-.1, .05))

