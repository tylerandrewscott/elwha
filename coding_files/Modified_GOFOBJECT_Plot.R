function (x, ..., cex.axis = 0.7, plotlogodds = FALSE, main = "Goodness-of-fit diagnostics", 
    normalize.reachability = FALSE, verbose = FALSE) 
{
    color <- "gray75"
    all.gof.vars <- ergm.rhs.formula(x$GOF)
    statsno <- length(all.gof.vars)
    for (i in seq(along = all.gof.vars)) {
        all.gof.vars[i] <- match.arg(all.gof.vars[i], c("distance", 
            "triadcensus", "espartners", "dspartners", "odegree", 
            "idegree", "degree", "model"))
    }
    GOF <- as.formula(paste("~", paste(all.gof.vars, collapse = "+")))
    if (statsno == 0) {
        stop("The gof object does not contain any statistics!\n")
    }
    n <- x$network.size
    for (statname in all.gof.vars) {
        if ("model" == statname) {
            nstats <- length(x$obs.model)
            if (min(x$pval.model[, "MC p-value"]) < 0) {
                pval.max <- max((1:nstats)[x$pval.model[1:nstats, 
                  "MC p-value"] < 1]) + 3
            }
            else {
                pval.max <- max((1:nstats)[x$obs.model[1:nstats] > 
                  0]) + 3
            }
            if (is.finite(pval.max) & pval.max < nstats) {
                model <- c(1:pval.max)
            }
            else {
                model <- c(1:nstats)
            }
            if (plotlogodds) {
                odds <- x$psim.model
                odds[!is.na(odds)] <- log(odds[!is.na(odds)]/(1 - 
                  odds[!is.na(odds)]))
                odds.obs <- x$pobs.model
                odds.obs[!is.na(odds.obs)] <- log(odds.obs[!is.na(odds.obs)]/(1 - 
                  odds.obs[!is.na(odds.obs)]))
                odds.bds <- x$bds.model
                odds.bds[!is.na(odds.bds)] <- log(odds.bds[!is.na(odds.bds)]/(1 - 
                  odds.bds[!is.na(odds.bds)]))
                mininf <- min(min(odds[!is.infinite(odds)]), 
                  min(odds.obs[!is.infinite(odds.obs)]), min(odds.bds[!is.infinite(odds.bds)]))
                maxinf <- max(max(odds[!is.infinite(odds)]), 
                  max(odds.obs[!is.infinite(odds.obs)]), max(odds.bds[!is.infinite(odds.bds)]))
                odds[is.infinite(odds) & odds > 0] <- maxinf
                odds[is.infinite(odds) & odds < 0] <- mininf
                odds.obs[is.infinite(odds.obs) & odds.obs > 0] <- maxinf
                odds.obs[is.infinite(odds.obs) & odds.obs < 0] <- mininf
                odds.bds[is.infinite(odds.bds) & odds.bds > 0] <- maxinf
                odds.bds[is.infinite(odds.bds) & odds.bds < 0] <- mininf
                out <- odds
                out.obs <- odds.obs
                out.bds <- odds.bds
                ylab <- "log-odds for the statistic"
            }
            else {
                out <- x$psim.model
                out.obs <- x$pobs.model
                out.bds <- x$bds.model
                ylab <- "statistic"
            }
            pnames <- names(out.obs)
            ymin <- min(min(out, na.rm = TRUE), min(out.obs, 
                na.rm = TRUE))
            ymax <- max(max(out, na.rm = TRUE), max(out.obs, 
                na.rm = TRUE))
            boxplot(data.frame(out[, model]), xlab = "model statistics", 
                ylab = ylab, names = pnames, cex.axis = cex.axis, 
                outline = FALSE, ylim = c(ymin, ymax), ...)
            points(seq(along = model), out.bds[1, model], pch = 1, 
                cex = 0.75)
            points(seq(along = model), out.bds[2, model], pch = 1, 
                cex = 0.75)
            lines(seq(along = model), out.bds[1, model], pch = 18, 
                lty = 1, lwd = 1, col = color)
            lines(seq(along = model), out.bds[2, model], pch = 18, 
                lty = 1, lwd = 1, col = color)
            points(seq(along = model), out.obs[model], pch = 16, 
                cex = 0.75)
            lines(seq(along = model), out.obs[model], lty = 1, 
                lwd = 3)
        }
        if ("degree" == statname) {
            if (min(x$pval.deg[, "MC p-value"]) < 0) {
                pval.max <- max((1:(n - 1))[x$pval.deg[1:(n - 
                  1), "MC p-value"] < 1]) + 3
            }
            else {
                pval.max <- max((1:(n - 1))[x$obs.deg[1:(n - 
                  1)] > 0]) + 3
            }
            if (is.finite(pval.max) & pval.max < n) {
                deg <- c(1:pval.max)
            }
            else {
                deg <- c(1:n)
            }
            if (plotlogodds) {
                odds <- x$psim.deg
                odds[!is.na(odds)] <- log(odds[!is.na(odds)]/(1 - 
                  odds[!is.na(odds)]))
                odds.obs <- x$pobs.deg
                odds.obs[!is.na(odds.obs)] <- log(odds.obs[!is.na(odds.obs)]/(1 - 
                  odds.obs[!is.na(odds.obs)]))
                odds.bds <- x$bds.deg
                odds.bds[!is.na(odds.bds)] <- log(odds.bds[!is.na(odds.bds)]/(1 - 
                  odds.bds[!is.na(odds.bds)]))
                mininf <- min(min(odds[!is.infinite(odds)]), 
                  min(odds.obs[!is.infinite(odds.obs)]), min(odds.bds[!is.infinite(odds.bds)]))
                maxinf <- max(max(odds[!is.infinite(odds)]), 
                  max(odds.obs[!is.infinite(odds.obs)]), max(odds.bds[!is.infinite(odds.bds)]))
                odds[is.infinite(odds) & odds > 0] <- maxinf
                odds[is.infinite(odds) & odds < 0] <- mininf
                odds.obs[is.infinite(odds.obs) & odds.obs > 0] <- maxinf
                odds.obs[is.infinite(odds.obs) & odds.obs < 0] <- mininf
                odds.bds[is.infinite(odds.bds) & odds.bds > 0] <- maxinf
                odds.bds[is.infinite(odds.bds) & odds.bds < 0] <- mininf
                out <- odds
                out.obs <- odds.obs
                out.bds <- odds.bds
                ylab <- "log-odds for a node"
            }
            else {
                out <- x$psim.deg
                out.obs <- x$pobs.deg
                out.bds <- x$bds.deg
                ylab <- "proportion of nodes"
            }
            pnames <- c(deg) - 1
            ymin <- min(min(out, na.rm = TRUE), min(out.obs, 
                na.rm = TRUE))
            ymax <- max(max(out, na.rm = TRUE), max(out.obs, 
                na.rm = TRUE))
            boxplot(data.frame(out[, deg]), xlab = "degree", 
                ylab = ylab, names = pnames, cex.axis = cex.axis, 
                outline = FALSE, ylim = c(ymin, ymax), ...)
            points(seq(along = deg), out.bds[1, deg], pch = 1, 
                cex = 0.75)
            points(seq(along = deg), out.bds[2, deg], pch = 1, 
                cex = 0.75)
            lines(seq(along = deg), out.bds[1, deg], pch = 18, 
                lty = 1, lwd = 1, col = color)
            lines(seq(along = deg), out.bds[2, deg], pch = 18, 
                lty = 1, lwd = 1, col = color)
            points(seq(along = deg), out.obs[deg], pch = 16, 
                cex = 0.75)
            lines(seq(along = deg), out.obs[deg], lty = 1, lwd = 3)
        }
        if ("odegree" == statname) {
            if (min(x$pval.odeg[, "MC p-value"]) < 0) {
                pval.max <- max((1:(n - 1))[x$pval.odeg[1:(n - 
                  1), "MC p-value"] < 1]) + 3
            }
            else {
                pval.max <- max((1:(n - 1))[x$obs.odeg[1:(n - 
                  1)] > 0]) + 3
            }
            if (is.finite(pval.max) & pval.max < n) {
                odeg <- c(1:pval.max)
            }
            else {
                odeg <- c(1:n)
            }
            if (plotlogodds) {
                odds <- x$psim.odeg
                odds[!is.na(odds)] <- log(odds[!is.na(odds)]/(1 - 
                  odds[!is.na(odds)]))
                odds.obs <- x$pobs.odeg
                odds.obs[!is.na(odds.obs)] <- log(odds.obs[!is.na(odds.obs)]/(1 - 
                  odds.obs[!is.na(odds.obs)]))
                odds.bds <- x$bds.odeg
                odds.bds[!is.na(odds.bds)] <- log(odds.bds[!is.na(odds.bds)]/(1 - 
                  odds.bds[!is.na(odds.bds)]))
                mininf <- min(min(odds[!is.infinite(odds)]), 
                  min(odds.obs[!is.infinite(odds.obs)]), min(odds.bds[!is.infinite(odds.bds)]))
                maxinf <- max(max(odds[!is.infinite(odds)]), 
                  max(odds.obs[!is.infinite(odds.obs)]), max(odds.bds[!is.infinite(odds.bds)]))
                odds[is.infinite(odds) & odds > 0] <- maxinf
                odds[is.infinite(odds) & odds < 0] <- mininf
                odds.obs[is.infinite(odds.obs) & odds.obs > 0] <- maxinf
                odds.obs[is.infinite(odds.obs) & odds.obs < 0] <- mininf
                odds.bds[is.infinite(odds.bds) & odds.bds > 0] <- maxinf
                odds.bds[is.infinite(odds.bds) & odds.bds < 0] <- mininf
                out <- odds
                out.obs <- odds.obs
                out.bds <- odds.bds
                ylab <- "log-odds for a node"
            }
            else {
                out <- x$psim.odeg
                out.obs <- x$pobs.odeg
                out.bds <- x$bds.odeg
                ylab <- "proportion of nodes"
            }
            pnames <- c(odeg) - 1
            ymin <- min(min(out, na.rm = TRUE), min(out.obs, 
                na.rm = TRUE))
            ymax <- max(max(out, na.rm = TRUE), max(out.obs, 
                na.rm = TRUE))
            boxplot(data.frame(out[, odeg]), main = "out degree", 
                ylab = ylab, names = pnames, cex.axis = cex.axis, 
                outline = FALSE, ylim = c(ymin, ymax), ...)
            points(seq(along = odeg), out.bds[1, odeg], pch = 1, 
                cex = 0.75)
            points(seq(along = odeg), out.bds[2, odeg], pch = 1, 
                cex = 0.75)
            lines(seq(along = odeg), out.bds[1, odeg], pch = 18, 
                lty = 1, lwd = 1, col = color)
            lines(seq(along = odeg), out.bds[2, odeg], pch = 18, 
                lty = 1, lwd = 1, col = color)
            points(seq(along = odeg), out.obs[odeg], pch = 16, 
                cex = 0.75)
            lines(seq(along = odeg), out.obs[odeg], lty = 1, 
                lwd = 3)
        }
        if ("idegree" == statname) {
            if (min(x$pval.ideg[, "MC p-value"]) < 0) {
                pval.max <- max((1:(n - 1))[x$pval.ideg[1:(n - 
                  1), "MC p-value"] < 1]) + 3
            }
            else {
                pval.max <- max((1:(n - 1))[x$obs.ideg[1:(n - 
                  1)] > 0]) + 3
            }
            if (is.finite(pval.max) & pval.max < n) {
                ideg <- c(1:pval.max)
            }
            else {
                ideg <- c(1:n)
            }
            if (plotlogodds) {
                odds <- x$psim.ideg
                odds[!is.na(odds)] <- log(odds[!is.na(odds)]/(1 - 
                  odds[!is.na(odds)]))
                odds.obs <- x$pobs.ideg
                odds.obs[!is.na(odds.obs)] <- log(odds.obs[!is.na(odds.obs)]/(1 - 
                  odds.obs[!is.na(odds.obs)]))
                odds.bds <- x$bds.ideg
                odds.bds[!is.na(odds.bds)] <- log(odds.bds[!is.na(odds.bds)]/(1 - 
                  odds.bds[!is.na(odds.bds)]))
                mininf <- min(min(odds[!is.infinite(odds)]), 
                  min(odds.obs[!is.infinite(odds.obs)]), min(odds.bds[!is.infinite(odds.bds)]))
                maxinf <- max(max(odds[!is.infinite(odds)]), 
                  max(odds.obs[!is.infinite(odds.obs)]), max(odds.bds[!is.infinite(odds.bds)]))
                odds[is.infinite(odds) & odds > 0] <- maxinf
                odds[is.infinite(odds) & odds < 0] <- mininf
                odds.obs[is.infinite(odds.obs) & odds.obs > 0] <- maxinf
                odds.obs[is.infinite(odds.obs) & odds.obs < 0] <- mininf
                odds.bds[is.infinite(odds.bds) & odds.bds > 0] <- maxinf
                odds.bds[is.infinite(odds.bds) & odds.bds < 0] <- mininf
                out <- odds
                out.obs <- odds.obs
                out.bds <- odds.bds
                ylab <- "log-odds for a node"
            }
            else {
                out <- x$psim.ideg
                out.obs <- x$pobs.ideg
                out.bds <- x$bds.ideg
                ylab <- "proportion of nodes"
            }
            pnames <- c(ideg) - 1
            ymin <- min(min(out, na.rm = TRUE), min(out.obs, 
                na.rm = TRUE))
            ymax <- max(max(out, na.rm = TRUE), max(out.obs, 
                na.rm = TRUE))
            boxplot(data.frame(out[, ideg]), main = "in degree", 
                ylab = ylab, names = pnames, cex.axis = cex.axis, 
                outline = FALSE, ylim = c(ymin, ymax), ...)
            points(seq(along = ideg), out.bds[1, ideg], pch = 1, 
                cex = 0.75)
            points(seq(along = ideg), out.bds[2, ideg], pch = 1, 
                cex = 0.75)
            lines(seq(along = ideg), out.bds[1, ideg], pch = 18, 
                lty = 1, lwd = 1, col = color)
            lines(seq(along = ideg), out.bds[2, ideg], pch = 18, 
                lty = 1, lwd = 1, col = color)
            points(seq(along = ideg), out.obs[ideg], pch = 16, 
                cex = 0.75)
            lines(seq(along = ideg), out.obs[ideg], lty = 1, 
                lwd = 3)
        }
        if ("espartners" == statname) {
            pval.max <- max((1:(n - 1))[x$pval.espart[1:(n - 
                1), "MC p-value"] < 1]) + 3
            if (is.finite(pval.max) & pval.max < n) {
                espart <- c(1:pval.max)
            }
            else {
                espart <- c(1:(n - 1))
            }
            if (plotlogodds) {
                odds <- x$psim.espart
                odds[!is.na(odds)] <- log(odds[!is.na(odds)]/(1 - 
                  odds[!is.na(odds)]))
                odds.obs <- x$pobs.espart
                odds.obs[!is.na(odds.obs)] <- log(odds.obs[!is.na(odds.obs)]/(1 - 
                  odds.obs[!is.na(odds.obs)]))
                odds.bds <- x$bds.espart
                odds.bds[!is.na(odds.bds)] <- log(odds.bds[!is.na(odds.bds)]/(1 - 
                  odds.bds[!is.na(odds.bds)]))
                mininf <- min(min(odds[!is.infinite(odds)]), 
                  min(odds.obs[!is.infinite(odds.obs)]), min(odds.bds[!is.infinite(odds.bds)]))
                maxinf <- max(max(odds[!is.infinite(odds)]), 
                  max(odds.obs[!is.infinite(odds.obs)]), max(odds.bds[!is.infinite(odds.bds)]))
                odds[is.infinite(odds) & odds > 0] <- maxinf
                odds[is.infinite(odds) & odds < 0] <- mininf
                odds.obs[is.infinite(odds.obs) & odds.obs > 0] <- maxinf
                odds.obs[is.infinite(odds.obs) & odds.obs < 0] <- mininf
                odds.bds[is.infinite(odds.bds) & odds.bds > 0] <- maxinf
                odds.bds[is.infinite(odds.bds) & odds.bds < 0] <- mininf
                out <- odds
                out.obs <- odds.obs
                out.bds <- odds.bds
                ylab <- "log-odds for an edge"
            }
            else {
                out <- x$psim.espart
                out.obs <- x$pobs.espart
                out.bds <- x$bds.espart
                ylab <- "proportion of edges"
                mininf <- min(min(out), min(out.obs), min(out.bds))
                maxinf <- max(max(out), max(out.obs), max(out.bds))
            }
            pnames <- c(espart) - 1
            ymin <- min(min(out, na.rm = TRUE), min(out.obs, 
                na.rm = TRUE))
            ymax <- max(max(out, na.rm = TRUE), max(out.obs, 
                na.rm = TRUE))
            boxplot(data.frame(out[, espart]), main = "edge-wise shared partners", 
                ylab = ylab, names = pnames, cex.axis = cex.axis, 
                outline = FALSE, ylim = c(ymin, ymax), ...)
            points(seq(along = espart), out.bds[1, espart], pch = 1, 
                cex = 0.75)
            points(seq(along = espart), out.bds[2, espart], pch = 1, 
                cex = 0.75)
            lines(seq(along = espart), out.bds[1, espart], pch = 18, 
                lty = 1, lwd = 1, col = color)
            lines(seq(along = espart), out.bds[2, espart], pch = 18, 
                lty = 1, lwd = 1, col = color)
            points(seq(along = espart), out.obs[espart], pch = 16, 
                cex = 0.75)
            lines(seq(along = espart), out.obs[espart], lty = 1, 
                lwd = 3)
        }
        if ("dspartners" == statname) {
            pval.max <- max((1:(n - 1))[x$pval.dspart[1:(n - 
                1), "MC p-value"] < 1]) + 3
            if (is.finite(pval.max) & pval.max < n) {
                dspart <- c(1:pval.max)
            }
            else {
                dspart <- c(1:n)
            }
            if (plotlogodds) {
                odds <- x$psim.dspart
                odds[!is.na(odds)] <- log(odds[!is.na(odds)]/(1 - 
                  odds[!is.na(odds)]))
                odds.obs <- x$pobs.dspart
                odds.obs[!is.na(odds.obs)] <- log(odds.obs[!is.na(odds.obs)]/(1 - 
                  odds.obs[!is.na(odds.obs)]))
                odds.bds <- x$bds.dspart
                odds.bds[!is.na(odds.bds)] <- log(odds.bds[!is.na(odds.bds)]/(1 - 
                  odds.bds[!is.na(odds.bds)]))
                mininf <- min(min(odds[!is.infinite(odds)]), 
                  min(odds.obs[!is.infinite(odds.obs)]), min(odds.bds[!is.infinite(odds.bds)]))
                maxinf <- max(max(odds[!is.infinite(odds)]), 
                  max(odds.obs[!is.infinite(odds.obs)]), max(odds.bds[!is.infinite(odds.bds)]))
                odds[is.infinite(odds) & odds > 0] <- maxinf
                odds[is.infinite(odds) & odds < 0] <- mininf
                odds.obs[is.infinite(odds.obs) & odds.obs > 0] <- maxinf
                odds.obs[is.infinite(odds.obs) & odds.obs < 0] <- mininf
                odds.bds[is.infinite(odds.bds) & odds.bds > 0] <- maxinf
                odds.bds[is.infinite(odds.bds) & odds.bds < 0] <- mininf
                out <- odds
                out.obs <- odds.obs
                out.bds <- odds.bds
                ylab <- "log-odds for an edge"
            }
            else {
                out <- x$psim.dspart
                out.obs <- x$pobs.dspart
                out.bds <- x$bds.dspart
                ylab <- "proportion of dyads"
                mininf <- min(min(out), min(out.obs), min(out.bds))
                maxinf <- max(max(out), max(out.obs), max(out.bds))
            }
            pnames <- c(dspart) - 1
            ymin <- min(min(out, na.rm = TRUE), min(out.obs, 
                na.rm = TRUE))
            ymax <- max(max(out, na.rm = TRUE), max(out.obs, 
                na.rm = TRUE))
            boxplot(data.frame(out[, dspart]), xlab = "dyad-wise shared partners", 
                ylab = ylab, names = pnames, cex.axis = cex.axis, 
                outline = FALSE, ylim = c(ymin, ymax), ...)
            points(seq(along = dspart), out.bds[1, dspart], pch = 1, 
                cex = 0.75)
            points(seq(along = dspart), out.bds[2, dspart], pch = 1, 
                cex = 0.75)
            lines(seq(along = dspart), out.bds[1, dspart], pch = 18, 
                lty = 1, lwd = 1, col = color)
            lines(seq(along = dspart), out.bds[2, dspart], pch = 18, 
                lty = 1, lwd = 1, col = color)
            points(seq(along = dspart), out.obs[dspart], pch = 16, 
                cex = 0.75)
            lines(seq(along = dspart), out.obs[dspart], lty = 1, 
                lwd = 3)
        }
        if ("triadcensus" == statname) {
            if (plotlogodds) {
                odds <- x$psim.triadcensus
                odds[!is.na(odds)] <- log(odds[!is.na(odds)]/(1 - 
                  odds[!is.na(odds)]))
                odds.obs <- x$pobs.triadcensus
                odds.obs[!is.na(odds.obs)] <- log(odds.obs[!is.na(odds.obs)]/(1 - 
                  odds.obs[!is.na(odds.obs)]))
                odds.bds <- x$bds.triadcensus
                odds.bds[!is.na(odds.bds)] <- log(odds.bds[!is.na(odds.bds)]/(1 - 
                  odds.bds[!is.na(odds.bds)]))
                mininf <- min(min(odds[!is.infinite(odds)]), 
                  min(odds.obs[!is.infinite(odds.obs)]), min(odds.bds[!is.infinite(odds.bds)]))
                maxinf <- max(max(odds[!is.infinite(odds)]), 
                  max(odds.obs[!is.infinite(odds.obs)]), max(odds.bds[!is.infinite(odds.bds)]))
                odds[is.infinite(odds) & odds > 0] <- maxinf
                odds[is.infinite(odds) & odds < 0] <- mininf
                odds.obs[is.infinite(odds.obs) & odds.obs > 0] <- maxinf
                odds.obs[is.infinite(odds.obs) & odds.obs < 0] <- mininf
                odds.bds[is.infinite(odds.bds) & odds.bds > 0] <- maxinf
                odds.bds[is.infinite(odds.bds) & odds.bds < 0] <- mininf
                out <- odds
                out.obs <- odds.obs
                out.bds <- odds.bds
                ylab <- "log-odds for a triad"
            }
            else {
                out <- x$psim.triadcensus
                out.obs <- x$pobs.triadcensus
                out.bds <- x$bds.triadcensus
                ylab <- "proportion of triads"
                mininf <- min(min(out), min(out.obs), min(out.bds))
                maxinf <- max(max(out), max(out.obs), max(out.bds))
            }
            triadcensus <- dimnames(x$sim.triadcensus)[[2]]
            pnames <- dimnames(x$sim.triadcensus)[[2]]
            ymin <- min(min(out, na.rm = TRUE), min(out.obs, 
                na.rm = TRUE))
            ymax <- max(max(out, na.rm = TRUE), max(out.obs, 
                na.rm = TRUE))
            boxplot(data.frame(out[, triadcensus]), xlab = "triad census", 
                ylab = ylab, names = pnames, cex.axis = cex.axis, 
                outline = FALSE, ylim = c(ymin, ymax), ...)
            points(seq(along = triadcensus), out.bds[1, triadcensus], 
                pch = 1, cex = 0.75)
            points(seq(along = triadcensus), out.bds[2, triadcensus], 
                pch = 1, cex = 0.75)
            lines(seq(along = triadcensus), out.bds[1, triadcensus], 
                pch = 18, lty = 1, lwd = 1, col = color)
            lines(seq(along = triadcensus), out.bds[2, triadcensus], 
                pch = 18, lty = 1, lwd = 1, col = color)
            points(seq(along = triadcensus), out.obs[triadcensus], 
                pch = 16, cex = 0.75)
            lines(seq(along = triadcensus), out.obs[triadcensus], 
                lty = 1, lwd = 3)
        }
        if ("distance" == statname) {
            pval.max <- max((1:(n - 1))[x$pval.dist[1:(n - 1), 
                "MC p-value"] < 1]) + 3
            if (is.finite(pval.max) & pval.max < n) {
                dist <- c(1:pval.max, n)
            }
            else {
                dist <- c(1:n)
            }
            pnames <- paste(dist)
            pnames[length(dist)] <- "NR"
            if (plotlogodds) {
                odds <- x$psim.dist
                odds[!is.na(odds)] <- log(odds[!is.na(odds)]/(1 - 
                  odds[!is.na(odds)]))
                odds.obs <- x$pobs.dist
                odds.obs[!is.na(odds.obs)] <- log(odds.obs[!is.na(odds.obs)]/(1 - 
                  odds.obs[!is.na(odds.obs)]))
                odds.bds <- x$bds.dist
                odds.bds[!is.na(odds.bds)] <- log(odds.bds[!is.na(odds.bds)]/(1 - 
                  odds.bds[!is.na(odds.bds)]))
                oodds <- is.infinite(odds) | is.na(odds)
                oodds.obs <- is.infinite(odds.obs) | is.na(odds.obs)
                oodds.bds <- is.infinite(odds.bds) | is.na(odds.bds)
                mininf <- min(min(odds[!oodds]), min(odds.obs[!oodds.obs]), 
                  min(odds.bds[!oodds.bds]))
                maxinf <- max(max(odds[!oodds]), max(odds.obs[!oodds.obs]), 
                  max(odds.bds[!oodds.bds]))
                odds[is.infinite(odds) & odds > 0] <- maxinf
                odds[is.infinite(odds) & odds < 0] <- mininf
                odds.obs[is.infinite(odds.obs) & odds.obs > 0] <- maxinf
                odds.obs[is.infinite(odds.obs) & odds.obs < 0] <- mininf
                odds.bds[is.infinite(odds.bds) & odds.bds > 0] <- maxinf
                odds.bds[is.infinite(odds.bds) & odds.bds < 0] <- mininf
                odds.bds[1, ][is.na(odds.bds[1, ])] <- mininf
                odds.bds[2, ][is.na(odds.bds[2, ])] <- maxinf
                out <- odds
                out.obs <- odds.obs
                out.bds <- odds.bds
                ylab <- "log-odds for a dyad"
            }
            else {
                out <- x$psim.dist
                out.obs <- x$pobs.dist
                out.bds <- x$bds.dist
                ylab <- "proportion of dyads"
                mininf <- min(min(out), min(out.obs), min(out.bds))
                maxinf <- max(max(out), max(out.obs), max(out.bds))
            }
            if (normalize.reachability) {
                mdist <- max(dist, na.rm = TRUE)
                totrange <- range(out.bds[1, ][out.bds[1, ] > 
                  out.bds[1, mdist]], out.bds[2, ][out.bds[2, 
                  ] < out.bds[2, mdist]])
                out[, mdist] <- (out[, mdist] - out.bds[1, mdist]) * 
                  diff(totrange)/diff(out.bds[, mdist]) + totrange[1]
                out.obs[mdist] <- (out.obs[mdist] - out.bds[1, 
                  mdist]) * diff(totrange)/diff(out.bds[, mdist]) + 
                  totrange[1]
                out.bds[, mdist] <- totrange
            }
            ymin <- min(min(out, na.rm = TRUE), min(out.obs, 
                na.rm = TRUE))
            ymax <- max(max(out, na.rm = TRUE), max(out.obs, 
                na.rm = TRUE))
            if (!plotlogodds) {
                ymin <- max(0, ymin)
                ymax <- min(1, ymax)
            }
            boxplot(data.frame(out[, dist]), main = "minimum geodesic distance", 
                ylab = ylab, names = pnames, cex.axis = cex.axis, 
                outline = FALSE, ylim = c(ymin, ymax), ...)
            points(seq(along = dist), out.bds[1, dist], pch = 1, 
                cex = 0.75)
            points(seq(along = dist), out.bds[2, dist], pch = 1, 
                cex = 0.75)
            lines(seq(along = dist)[-length(dist)], out.bds[1, 
                dist][-length(dist)], pch = 18, lty = 1, lwd = 1, 
                col = color)
            lines(seq(along = dist)[-length(dist)], out.bds[2, 
                dist][-length(dist)], pch = 18, lty = 1, lwd = 1, 
                col = color)
            points(seq(along = dist), out.obs[dist], pch = 16, 
                cex = 0.75)
            lines(seq(along = dist)[-length(dist)], out.obs[dist][-length(dist)], 
                lty = 1, lwd = 3)
        }
    }
    mtext(main, side = 3, outer = TRUE, cex = 1.5, padj = 2)
    invisible()
}
<environment: namespace:ergm>
> 