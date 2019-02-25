library(deSolve)
parameters <- c(k_r = 0.0005, k_f = 0.005)
state      <- c(A=800, B=400, C=0, E=100)
Concentration <- function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    dA <- 0
    dB <- k_r*C*E - k_f*B*E
    dC <- -dB
    dE <- 0
    list(c(dA,dB,dC,dE))
  }
       )
}

times <- seq(0,100,by = 0.1)
out   <- ode(y = state, times = times, func=Concentration, parms=parameters)
par(oma=c(0,0,3,0))
plot(out[,"time"],out[,"A"],type='l',col='red',xlab = "time",ylab="Concentration",ylim=c(0,900))
lines(out[,"time"],out[,"B"],col='black')
lines(out[,"time"],out[,"C"],col='blue')
lines(out[,"time"],out[,"E"],col='green')
mtext(outer = TRUE, side=3,"Concentration of A,B,C,E in ODE System")
#plot(out[,"B"],out[,"C"],pch=".",xlab="B",ylab="C")