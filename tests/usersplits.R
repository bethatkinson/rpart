# Any necessary setup
library(rpart)
options(na.action="na.omit")
options(digits=4) # to match earlier output
set.seed(1234)

mystate <- data.frame(state.x77, region=factor(state.region))
names(mystate) <- c("population","income" , "illiteracy","life" ,
       "murder", "hs.grad", "frost",     "area",      "region")
#
# Test out the "user mode" functions, with an anova variant
#

# The 'evaluation' function.  Called once per node.
#  Produce a label (1 or more elements long) for labeling each node,
#  and a deviance.  The latter is
#	- of length 1
#       - equal to 0 if the node is "pure" in some sense (unsplittable)
#       - does not need to be a deviance: any measure that gets larger
#            as the node is less acceptable is fine.
#       - the measure underlies cost-complexity pruning, however
temp1 <- function(y, wt, parms) {
    wmean <- sum(y*wt)/sum(wt)
    rss <- sum(wt*(y-wmean)^2)
    list(label= wmean, deviance=rss)
    }

# The split function, where most of the work occurs.
#   Called once per split variable per node.
# If continuous=T
#   The actual x variable is ordered
#   y is supplied in the sort order of x, with no missings,
#   return two vectors of length (n-1):
#      goodness = goodness of the split, larger numbers are better.
#                 0 = couldn't find any worthwhile split
#        the ith value of goodness evaluates splitting obs 1:i vs (i+1):n
#      direction= -1 = send "y< cutpoint" to the left side of the tree
#                  1 = send "y< cutpoint" to the right
#         this is not a big deal, but making larger "mean y's" move towards
#         the right of the tree, as we do here, seems to make it easier to
#         read
# If continuos=F, x is a set of integers defining the groups for an
#   unordered predictor.  In this case:
#       direction = a vector of length m= "# groups".  It asserts that the
#           best split can be found by lining the groups up in this order
#           and going from left to right, so that only m-1 splits need to
#           be evaluated rather than 2^(m-1)
#       goodness = m-1 values, as before.
#
# The reason for returning a vector of goodness is that the C routine
#   enforces the "minbucket" constraint. It selects the best return value
#   that is not too close to an edge.
temp2 <- function(y, wt, x, parms, continuous) {
    # Center y
    n <- length(y)
    y <- y- sum(y*wt)/sum(wt)

    if (continuous) {
	# continuous x variable
	temp <- cumsum(y*wt)[-n]

	left.wt  <- cumsum(wt)[-n]
	right.wt <- sum(wt) - left.wt
	lmean <- temp/left.wt
	rmean <- -temp/right.wt
	goodness <- (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2)
	list(goodness= goodness, direction=sign(lmean))
	}
    else {
	# Categorical X variable
	ux <- sort(unique(x))
	wtsum <- tapply(wt, x, sum)
	ysum  <- tapply(y*wt, x, sum)
	means <- ysum/wtsum

	# For anova splits, we can order the categories by their means
	#  then use the same code as for a non-categorical
	ord <- order(means)
	n <- length(ord)
	temp <- cumsum(ysum[ord])[-n]
	left.wt  <- cumsum(wtsum[ord])[-n]
	right.wt <- sum(wt) - left.wt
	lmean <- temp/left.wt
	rmean <- -temp/right.wt
	list(goodness= (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2),
	     direction = ux[ord])
	}
    }

# The init function:
#   fix up y to deal with offsets
#   return a dummy parms list
#   numresp is the number of values produced by the eval routine's "label"
#   numy is the number of columns for y
#   summary is a function used to print one line in summary.rpart
# In general, this function would also check for bad data, see rpart.poisson
#   for instace.
temp3 <- function(y, offset, parms, wt) {
    if (!is.null(offset)) y <- y-offset
    list(y=y, parms=0, numresp=1, numy=1,
	      summary= function(yval, dev, wt, ylevel, digits ) {
		  paste("  mean=", format(signif(yval, digits)),
			", MSE=" , format(signif(dev/wt, digits)),
			sep='')
	     })
    }


alist <- list(eval=temp1, split=temp2, init=temp3)

fit1 <- rpart(income ~population +illiteracy  + murder + hs.grad + region,
	     mystate, control=rpart.control(minsplit=10, xval=0),
	     method=alist)

fit2 <- rpart(income ~population +illiteracy + murder + hs.grad + region,
	     mystate, control=rpart.control(minsplit=10, xval=0),
	      method='anova')

# Other than their call statement, and a longer "functions" component in
#  fit1, fit1 and fit2 should be identical.
all.equal(fit1$frame, fit2$frame)
all.equal(fit1$splits, fit2$splits)
all.equal(fit1$csplit, fit2$csplit)
all.equal(fit1$where, fit2$where)
all.equal(fit1$cptable, fit2$cptable)

# Now try xpred on it
xvtemp <- rep(1:5, length.out=50)
xp1 <- xpred.rpart(fit1, xval=xvtemp)
xp2 <- xpred.rpart(fit2, xval=xvtemp)
aeq <- function(x,y) all.equal(as.vector(x), as.vector(y))
aeq(xp1, xp2)

fit3 <- rpart(income ~population +illiteracy + murder + hs.grad + region,
	     mystate, control=rpart.control(minsplit=10, xval=xvtemp),
	      method='anova')
zz <- apply((mystate$income - xp1)^2,2, sum)
aeq(zz/fit1$frame$dev[1], fit3$cptable[,4])  #reproduce xerror

zz2 <- sweep((mystate$income-xp1)^2,2, zz/nrow(xp1))
zz2 <- sqrt(apply(zz2^2, 2, sum))/ fit1$frame$dev[1]
aeq(zz2, fit3$cptable[,5])          #reproduce se(xerror)

