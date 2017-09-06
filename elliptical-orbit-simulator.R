#Creating a web-based space mission visualization (WSMV) using rgl. 
library(rgl)

#References:
#2D Geometry: http://www.physics.csbsju.edu/orbit/orbit.2d.html
#3D Geometry: http://www.physics.csbsju.edu/orbit/orbit.3d.html

#Initialize an ellipse
a <- 1    #semi-major axis
e <- 1/sqrt(2)  #eccentricity
b <- a * sqrt(1 - e^2) #semi-minor axis 
c <- e*a #dist from center to a focus

#Dist from a focus to center
x.c <- -c
y.c <- 0
z.c <- 0

#Creating numerical sequence
u <- seq(-pi,pi, length.out=80)

#Generate x and y 
x <- a * cos(u) - e
y <- a * sqrt(1-e^2) * sin(u)
z <- rep(0, times=80)

c.x <- a * cos(u) - c
#Generate points for outer circle
c.y <- a * sin(u) 
c.z <- rep(0, times=80)

#Plot
plot(x,y,type="l", ylim=c(-a*sin(pi/2), a*sin(pi/2)),asp=1)
points(c.x,c.y,type="l", col="green")

#Add 3D orbit
#Initialize 3 arrays to hold ellipse values
x.inc <- rep(0, times=80)
y.inc <- rep(0, times=80)
z.inc <- rep(0, times=80)

for (i in 1:80) {
  point <- rotate3d(c(x[i],y[i],z[i]),pi/5,0,1,0)
  x.inc[i] <- point[1]
  y.inc[i] <- point[2]
  z.inc[i] <- point[3]
}

#Transform focus for central body
center <- rotate3d(c(x.c,y.c,z.c),pi/5,0,1,0)
# Yaw, longitude of the ascending node (omega), rotate about z-axis
x.om <- rep(0, times=80)
y.om <- rep(0, times=80)
z.om <- rep(0, times=80)

for (i in 1:80) {
  point <- rotate3d(c(x.inc[i],y.inc[i],z.inc[i]),pi/4,0,0,1)
  x.om[i] <- point[1]
  y.om[i] <- point[2]
  z.om[i] <- point[3]
}

#Transform focus for the central body
center <- rotate3d(c(center[1],center[2],center[3]),pi/4,0,0,1)
# Roll, Right ascension of the ascending node (raan), rotate about x-axis
x.raan <- rep(0, times=80)
y.raan <- rep(0, times=80)
z.raan <- rep(0, times=80)

for (i in 1:80) {
  point <- rotate3d(c(x.om[i],y.om[i],z.om[i]),pi/4,1,0,0)
  x.raan [i] <- point[1]
  y.raan [i] <- point[2]
  z.raan [i] <- point[3]
}

#Transform focus for central body
center <- rotate3d(c(center[1],center[2],center[3]),pi/4,1,0,0)

#Plot ellipses
par3d(zoom=0.8, windowRect = c(200, 5, 800, 400))
mfrow3d(nr = 1, nc = 3, sharedMouse = TRUE) 

plot3d(x=x.inc,y=y.inc,z=z.inc, type="l", col="red")
next3d()
plot3d(x=x.om,y=y.om,z=z.om, type="l", col="blue")
next3d()
plot3d(x=x.raan ,y=y.raan ,z=z.raan, type="l", col="green")
rglwidget()

#open 3D
open3d(useNULL = rgl.useNULL())
## wgl 
##   3

#plot: origin around a sphere
par3d(zoom=0.8) #set parameter
plot3d(x.raan,y.raan,z.raan,type="l", col="red")
spheres3d(center[1],center[2],center[3],radius=0.01, col="red")
spheres3d(0,0,0,radius=0.1, col="blue")
rglwidget()

