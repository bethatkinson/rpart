asSQL <- function(model, ...) UseMethod("asSQL")

asSQL.rpart <- function(model, ...)
{
    if (!inherits(model, "rpart")) stop(Rtxt("Not a legitimate rpart tree"))
    target  <- as.character(attr(model$terms, "variables")[2]) # name of the dependent variable
    frm     <- model$frame # a dataframe containing the nodes of the tree
    names   <- row.names(frm) # the (unique) node numbers that follow a binary ordering indexed by node depth
    ds.size <- model$frame[1,]$n # total number of rows in the data
    ordered <- sort(frm$n, decreasing = TRUE, index.return = TRUE)$ix

    cat("CASE ")
    for (i in ordered)
    {
        if (frm[i,1] == "<leaf>")
        {
            cat("WHEN \n")
            yval <- frm[i,]$yval
            pth <- path.rpart(model,
                              nodes = as.numeric(names[i]),
                              print.it = FALSE,
                              sql = TRUE)
            pth <- unlist(pth)[-1]
            if (length(pth) == 0) {pth <- "True"}

            cat(sprintf("   %s\n", pth, sep = ""))
            cat(sprintf("   %s %s\n",
                        "THEN", yval))
        }
    }
    cat("\n")
    invisible(ordered)
}
