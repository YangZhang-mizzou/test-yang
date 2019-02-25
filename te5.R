library(deSolve)
rate3  = 10
rate4 = 10
parameters <- c(k3_r = 0.001, k3_f = 0.01, k4_r = 0.0001, k4_f = 0.001 )
state      <- c(A=2000,B=600,C=0,D=900,F=0,G=0,E3=50,E4=50)

Concentration <- function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    dA  <- 0
    dB  <- k3_r*C*E3 + k4_f*C*E4 - k3_f*E3*B - k4_r*B*F*G*E4
    dC  <- k3_f*B*E3 + k4_r*B*F*G*E4 - k4_f*C*E4 - k3_r*C*E3;
    dD  <- 0
    dE3 <- 0
    dE4 <- 0
    dF  <- k4_f*E4*C - k4_r*B*F*G*E3;
    dG  <- dF
    list(c(dA,dB,dC,dD,dF,dG,dE3,dE4))
  }
  )
}

times <- seq(0,100,by = 0.1)
time <- seq(0,99.9,by = 0.1)
time_2 <- seq(0,99.8,by = 0.1)

out   <- ode(y = state, times = times, func=Concentration, parms=parameters)
derivative_A <- diff(out[,"A"])/diff(times)
derivative_B <- diff(out[,"B"])/diff(times)
derivative_C<- diff(out [,"C"])/diff(times)
derivative_D <- diff(out[,"D"])/diff(times)
derivative_F <- diff(out[,"F"])/diff(times)
derivative_G <- diff(out[,"G"])/diff(times)

derivative_A2 <- diff(derivative_A)/diff(time)
derivative_B2 <- diff(derivative_B)/diff(time)
derivative_C2 <- diff(derivative_C)/diff(time)
derivative_D2 <- diff(derivative_D)/diff(time)
derivative_F2 <- diff(derivative_F)/diff(time)
derivative_G2 <- diff(derivative_G)/diff(time)

par(oma=c(0,0,3,0))
plot(out[,"time"],out[,"A"],type='l',col='red',xlab = "time",ylab="Concentration",ylim=c(0,1000))
lines(out[,"time"],out[,"B"],col='black')
lines(out[,"time"],out[,"C"],col='blue')
lines(out[,"time"],out[,"D"],col='orange')
lines(out[,"time"],out[,"F"],col='green')
lines(out[,'time'],out[,'G'],col='yellow')
mtext(outer = TRUE, side=3,"ODE System(k3/k-3=k4/k-4=0.1)")
options('max.print' = 100000)
getOption('max.print')
print(out)

plot(time,derivative_A,type='l',col='red',xlab = "time",ylab="derivative_1",ylim=c(-1000,1000))
lines(time,derivative_B,col='black')
lines(time,derivative_C,col='blue')
lines(time,derivative_D,col='orange')
lines(time,derivative_F,col='green')
lines(time,derivative_G,col='yellow')
mtext(outer = TRUE, side=3,"ODE System (1ST derivative)")

plot(time_2,derivative_A2,type='l',col='red',xlab = "time",ylab="derivative_1",ylim=c(-1000,1000))
lines(time_2,derivative_B2,col='black')
lines(time_2,derivative_C2,col='blue')
lines(time_2,derivative_D2,col='orange')
lines(time_2,derivative_F2,col='green')
lines(time_2,derivative_G2,col='yellow')
mtext(outer = TRUE, side=3,"ODE System (2ND derivative)")