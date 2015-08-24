totalroot<-read.csv("totalroot.csv")
plot(mass~length, data=totalroot, pch=c("A","E")[co2], col=c(1:2)[water], ylab=expression(bold(Root~Length~(km ~ m^-3))), main=(Relationship~between~Root~Length~and~Root~Mass), xlim=c(0, 150), ylim=c(0,25), xlab=expression(bold(Root~Mass~(kg ~ m^-3))))
legend("topleft", levels(totalroot$water),col=c(1:2),title= ("Drought"), lwd=3)


#dev.off()

#how to move labels??