rm(list = ls())
gc()

# load libraries and functions
	library(rootSolve)
	library(phaseR)
	source("~/Dropbox/MutPopDyn/NumericalStabilityAnalysis/Functions/TestStability.R")
	source("~/Dropbox/MutPopDyn/NumericalStabilityAnalysis/Functions/TestVectorField.R")
	source("~/Dropbox/MutPopDyn/NumericalStabilityAnalysis/Functions/TestNullClines.R")
	source("~/Dropbox/MutPopDyn/NumericalStabilityAnalysis/Functions/TestNullClines_NoMut.R")


# model
	model <-function(t, y, parameters) {
		with(as.list(c(y, parameters)),{
			  dx = r*x*(1 - (x/K)) - (a*y*x)/(x + RO)
			  dy = (e*a*y*x)/(x + RO) - (m*y)
			 return(list(c(dx, dy)))
			})
		}

saveVideo({
	n <- 200
	time = 10
	ani.options(interval = time/n, nmax = n)

for (i in 1:200){
parspace <- list(
	r = 1,
	K = 2,
	e = 0.5,
	RO = 0.5,
	m = 0.5,
	a = seq(0.9, 2.6, length.out = 200)[i],#1.5, 
	eta = 1,
	theta = 1
		)


# plot direction field and equilibria
	x.lims <- c(-0.1, 2)
	y.lims <- c(-0.1, 1)

par(mar = c(4,4,1,1), oma = c(0,0,1,1))
	ff(deriv = model, x.lim = x.lims, y.lim = y.lims, parameters = parspace, points = 15, add = F, arrow.type = "proportional", colour = "grey30", xlab = "Prey density (H)", ylab = "Predator density (P)")
	mtext(text = paste0("a = ", format(round(parspace[["a"]], digits = 2), nsmall = 2)))
	abline(h = 0, v = 0)
	n <- nc(deriv = model, x.lim = x.lims, y.lim = y.lims, parameters = parspace, points = 200)
	tt(deriv = model, parameters = parspace, y0 = c(x = 0.5, y = 0.2), t.start = 1, t.end = 300, t.step = 0.1)
print(i)
}
}, video.name = "~/Desktop/Points.mp4"
)