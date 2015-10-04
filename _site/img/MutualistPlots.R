####################################################
#                                                  #
#           all of the things, indexed             #
#                                                  #
####################################################

	births <- function(b, mu, N){b - mu*N}
	deaths <- function(d, nu, N){d + nu*N}
	single.pop <- function(b, d, mu, nu, N) {births(b, mu, N) - deaths(d, nu, N)}

	two.pops.net.add <- function(b, d, mu, nu, N, N2){(b + N2 - mu*N) - (d - N2 + nu*N)}
	two.pops.net.mult <- function(b, d, mu, nu, N, N2){(b - (mu*N*N2)) - (d + (nu*N*N2))}

	N.min <- 0
	N.max <- 7
	Y.max <- 18
	N2 <- 4/5
	b <- 10
	d <- 0
	nu <- 1
	mu <- 1

	n.seq <- seq(N.min, N.max, length.out = 101)

	single <- single.pop(b = b , d = d , nu = nu , mu = mu , N = n.seq)*n.seq
	two.pops.add <- two.pops.net.add(b = b , d = d , nu = nu , mu = mu , N = n.seq , N2 = N2)*n.seq
	single.pc <- single.pop(b = b , d = d , nu = nu , mu = mu , N = n.seq)
	two.pops.add.pc <- two.pops.net.add(b = b , d = d , nu = nu , mu = mu , N = n.seq , N2 = N2)
	single.births <- births(b = b , mu = mu , N = n.seq)
	single.births.n2 <- births(b = b , mu = mu , N = n.seq) + N2
	single.deaths <- deaths(d = d, nu = nu , N = n.seq)
	single.deaths.n2 <- deaths(d = d, nu = nu , N = n.seq) - N2

	png("/Users/christophermoore/Desktop/example-app/img/banner.png", width = 700, height = 300)
	# pdf("/Users/christophermoore/Documents/Application_materials/ResearchPlan/MutDens.pdf", width = 8, height = 4.25)
	par(mfrow = c(1 , 2) , mar = c(0,0,0,0) , cex = 1.25, oma = c(2,2,0,2))
	plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(N.min,N.max), ylim = c(0,Y.max))
	polygon(c(n.seq, rev(n.seq)), c(single.births, rev(single.births.n2)), border = F, col = rgb(0,0,1,0.1,NULL,1))
	polygon(c(n.seq, rev(n.seq)), c(single.deaths, rev(single.deaths.n2)), border = F, col = rgb(1,0,0,0.1,NULL,1))
	polygon(c(n.seq, rev(n.seq)), c(single, rev(two.pops.add)), border = F, col = rgb(0,0,0,0.1,NULL,1))
	polygon(c(n.seq, rev(n.seq)), c(single.pc, rev(two.pops.add.pc)), border = F, col = rgb(0,0,0,0.1,255))

	curve(births(b = b , mu = mu , N = x) ,  , from = N.min , to = N.max , add = T , col = "blue")
	curve(deaths(d = d , nu = nu , N = x) ,  , from = N.min , to = N.max , add = T , col = "red")

	curve(births(b = b , mu = mu , N = x)+ N2  , from = N.min , to = N.max , add = T , col = "blue" , lwd = 2)
	curve(deaths(d = d , nu = nu , N = x)-N2  , from = N.min , to = N.max , add = T , col = "red" , lwd = 2)
	curve(two.pops.net.add(b = b , d = d , nu = nu , mu = mu , N = x , N2 = N2) , from = N.min , to = N.max , lwd = 2 , lty = 1 , add =T)
	curve(two.pops.net.add(b = b , d = d , nu = nu , mu = mu , N = x , N2 = N2)*x , from = N.min , to = N.max , lwd = 2 , lty = 2 , add =T)

	curve(single.pop(b = b , d = d , nu = nu , mu = mu , N = x)*x , from = N.min , to = N.max , lwd = 1 , lty = 2 , add =T)
	curve(single.pop(b = b , d = d , nu = nu , mu = mu , N = x) , from = N.min , to = N.max , add = T)

	mtext(expression((dN[1]/dt)(1/N[1])) , 2 , line = 0.25 , las = 0, cex = 1.25)
	mtext(expression(N[2]) , 1 , line = 0.5, cex = 1.25)
	# mtext("Mutualist density independence" , side = 3, line = -1 , cex = 1)
	# legend("topleft", "Density independent mutualism", bty = "n", adj = c(.05,-0.25))
	abline(h = 0, v = 0 , lwd = 1.5)
	box()


	two.pops.mult <- two.pops.net.mult(b = b , d = d , nu = nu , mu = mu , N = n.seq , N2 = N2)*n.seq
	two.pops.mult.pc <- two.pops.net.mult(b = b , d = d , nu = nu , mu = mu , N = n.seq , N2 = N2)
	single.births.n2.mult <- births(b = b , mu = mu , N = n.seq*N2)
	single.deaths.n2.mult <- deaths(d = d, nu = nu , N = n.seq*N2)

	plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(N.min,N.max), ylim = c(0,Y.max))

	polygon(c(n.seq, rev(n.seq)), c(single.births, rev(single.births.n2.mult)), border = F, col = rgb(0,0,1,0.1,NULL,1))
	polygon(c(n.seq, rev(n.seq)), c(single.deaths, rev(single.deaths.n2.mult)), border = F, col = rgb(1,0,0,0.1,NULL,1))
	polygon(c(n.seq, rev(n.seq)), c(single, rev(two.pops.mult)), border = F, col = rgb(0,0,0,0.1,255))
	polygon(c(n.seq, rev(n.seq)), c(single.pc, rev(two.pops.mult.pc)), border = F, col = rgb(0,0,0,0.1,255))

	curve(single.pop(b = b , d = d , nu = nu , mu = mu , N = x) , from = N.min , to = N.max , add = T)
	curve(single.pop(b = b , d = d , nu = nu , mu = mu , N = x)*x , from = N.min , to = N.max , lwd = 1 , lty = 2 , add =T)

	curve(births(b = b , mu = mu , N = x) , from = N.min , to = N.max , add = T , col = "blue")
	curve(deaths(d = d , nu = nu , N = x) , from = N.min , to = N.max , add = T , col = "red")

	curve(two.pops.net.mult(b = b, d = d , nu = nu , mu = mu , N = x , N2 = N2) , from = N.min , to = N.max , lwd = 2 , lty = 1 , add =T)
	curve(two.pops.net.mult(b = b , d = d , nu = nu , mu = mu , N = x , N2 = N2)*x , from = N.min , to = N.max , lwd = 2 , lty = 2 , add =T)

	curve(births(b = b , mu = mu , N = x*N2)  , from = N.min , to = N.max , add = T , col = "blue" , lwd = 2)
	curve(deaths(d = d , nu = nu , N = x*N2)  , from = N.min , to = N.max , add = T , col = "red" , lwd = 2)

	mtext(expression((dN[1]/dt)) , 4 , line = 0.5 , las = 0, cex = 1.25)
	mtext(expression(N[2]) , 1 , line = 0.5, cex = 1.25)
	abline(h = 0 , v = 0 ,lwd = 1.5)
	# mtext("Mutualist density dependence" , side = 3, line = -0.5 , cex = 1)
	mtext("Density" , side = 1, line = 0.5 , outer = T , cex = 1.25)
	# legend("topleft", "Density dependent mutualism", bty = "n", adj = c(.05,-0.25))
	box()
	dev.off()