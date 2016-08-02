rm(list = ls())
gc()

# load libraries and functions
	library(rootSolve)
	library(phaseR)
	library(animation)
	source("~/Dropbox/MutPopDyn/NumericalStabilityAnalysis/Z_Test_Simluations/Functions/TestStability.R")
	source("~/Dropbox/MutPopDyn/NumericalStabilityAnalysis/Z_Test_Simluations/Functions/TestVectorField.R")
	source("~/Dropbox/MutPopDyn/NumericalStabilityAnalysis/Z_Test_Simluations/Functions/TestNullClines.R")
	source("~/Dropbox/MutPopDyn/NumericalStabilityAnalysis/Z_Test_Simluations/Functions/TestNullClines_NoMut.R")
	source("~/Dropbox/MutPopDyn/NumericalStabilityAnalysis/Z_Test_Simluations/Functions/Trajectory.R")


# model
	model <-function(t, y, parameters) {
		with(as.list(c(y, parameters)),{
			  dx = r*x*(1 - (x/K)) - (a*y*x)/(x + RO)
			  dy = (e*a*y*x)/(x + RO) - (m*y)
			 return(list(c(dx, dy)))
			})
		}

# phase plane
	saveVideo({
		frames <- 200
		time = 10
		ani.options(interval = time/frames, nmax = frames)
	
	for (i in 1:frames){
	parspace <- list(r = 1, K = 2, e = 0.5, RO = 0.5, m = 0.5, seq(0.9, 2.6, length.out = frames)[i], eta = 1, theta = 1)

	state <- c(x = 0.5, y = 0.2)
	times <- seq (0, 150, 0.1)

	# plot direction field and equilibria
		x.lims <- c(-0.1, 2)
		y.lims <- c(-0.1, 1)
	# mar = c(4,4,1,1)
	par(mfrow = c(2, 1), pin = c(2.5, 2.5), oma = c(0,0,0,0), ann = F)
		ff(deriv = model, x.lim = x.lims, y.lim = y.lims, parameters = parspace, points = 15, add = F, arrow.type = "proportional", colour = "grey30")
		mtext(text = paste0("a = ", format(round(parspace[["a"]], digits = 2), nsmall = 2)), cex = 1.25)
		abline(h = 0, v = 0)
		n <- nc(deriv = model, x.lim = x.lims, y.lim = y.lims, parameters = parspace, points = 200)
		tt(deriv = model, parameters = parspace, y0 = state, t.start = 1, t.end = 300, t.step = 0.1)
		mtext(side = 1, text = "Prey density (H)", line = 2.5)
		mtext(side = 2, text = "Predator density (P)", line = 2.5)

	par(new = T, mfrow = c(2, 1), pin = c(3.75, 1.75), ann = F)
		out <- ode(y = state, times = times, func = model, parms = parspace, hmax = 100)
		plot(x = out[,1], y = out[,2], type = "l", col = "#CC0000", lwd = 2, ylim = c(0, 2), ann = F)
		lines(x = out[,1], y = out[,3], lwd = 2, col = "#0000CC")
		mtext(side = 1, text = "Time", line = 2.5)
		mtext(side = 2, text = "Density of\nprey (red) and predator (blue)", line = 2.5)
	print(i)
	}
	}, video.name = "~/Desktop/bifurcation.mp4"
	)

# eigenvalues

	x.space <- seq(0.1, 2.1, 0.1)
	y.space <- seq(0.1, 1, 0.1)
	space.mat <- matrix(data = NA, nrow = length(x.space)*length(y.space), ncol = 2)	
	a.vals <- seq(0.9, 2.6, length.out = frames)
	eq.mat <- matrix(data = NA, nrow = length(a.vals), ncol = 5)
	colnames(eq.mat) <- c("a", "H", "P", "Lambda1", "Lambda2")
	conj.mat <- matrix(data = NA, nrow = length(a.vals), ncol = 2)
	colnames(conj.mat) <- c("Conjugate1", "Conjugate2")
	eigen.mat <- matrix(data = NA, nrow = length(a.vals), ncol = 2)
	colnames(eigen.mat) <- c("Eigenvalue1", "Eigenvalue2")
	eq.mat[,1] <- a.vals

	for (i in 1:frames){
		parspace <- list(r = 1, K = 2, e = 0.5, RO = 0.5, m = 0.5, a = a.vals[i], eta = 1, theta = 1)

		for (j in seq_along(x.space)){
			for (k in seq_along(y.space)){
				y <- c(x = x.space[j], y = y.space[k])
				STO <- stode(y = y, func = model, parms = parspace, positive = T)
				space.mat[((j-1)*length(y.space)+k),] <- STO$y
			} # end of k (y.space) loop
		} # end of j (x.space) loop

	eqs <- unique(x = round(x = space.mat, digits = 2)) # find unique equilibria
	if (anyNA(eqs) == T) {eqs <- eqs[-which(is.na(eqs), arr.ind = T)[,1],]} # remove NA values
	if (length(eqs) > 0) {eqs <- eqs[-which(eqs < 0.1, arr.ind = T)[,1],]} # eliminate boundary equilibria
	if (length(eqs) == 2) {eq.mat[i,c(2,3)] <- eqs
	eq.mat[i,c(4,5)] <- 
	Re(eigen(jacobian.full(y = c(x = eqs[1], y = eqs[2]), func = model, parms = parspace))$values)
	conj.mat[i,] <- Im(eigen(jacobian.full(y = c(x = eqs[1], y = eqs[2]), func = model, parms = parspace))$values)
	eigen.mat[i,] <- eigen(jacobian.full(y = c(x = eqs[1], y = eqs[2]), func = model, parms = parspace))$values
		} # end of eigenvalue cals
	} # end of a.vals


	png("~/Desktop/bifurcation_eigen.png")
	plot(0, type = "n", ylim = c(-.2, 0.1), xlim = c(min(a.vals), max(a.vals)), ann = F)
	mtext(side = 1, text = "a", line = 2.5)
	mtext(side = 2, text = expression(lambda[max]), line = 2.5)
	abline(h = 0)

	firt.complex.eig <- eq.mat[min(which(conj.mat > 0, arr.ind = T)[,1])]
	first.eig <- eq.mat[max(which(is.na(conj.mat), arr.ind = T)[,1])]
	pos.eig <- eq.mat[min(which(eq.mat[,c(4,5)] > 0, arr.ind = T)[,1])]
	abline(v = c(firt.complex.eig, first.eig, pos.eig), col = "grey60", lwd = 2, lty = 2)
	mtext(text = round(c(firt.complex.eig, first.eig, pos.eig), digits = 2), at = c(firt.complex.eig, first.eig, pos.eig), side = 3, cex = 0.85)

	max.lambda <- apply(matrix(c(eq.mat[,4], eq.mat[,5]), ncol = 2), 1, max, na.rm = T)
	max.lambda[which(is.infinite(max.lambda))] <- NA
	lines(x = eq.mat[,1], y = max.lambda, lwd = 2, col = "red")
	dev.off()