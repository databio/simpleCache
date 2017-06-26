for (i in 1:5) {
	cacheName = paste0("normSample_", i);
	simpleCache(cacheName, { rnorm(1e6, 0,1) }, noload=TRUE, recreate=TRUE, timer=TRUE)
}
setCacheDir(getwd())

refreshPackage("simpleCache"); library(simpleCache)
simpleCache("test", { rnorm(1e7, 0,1) }, noload=TRUE, recreate=TRUE, timer=TRUE, batch=TRUE, batchMethod="slurm", batchDir=getwd())

    list2dt = function(x) {
        nn = names(x)
        if (is.null(nn)) 
            names(x) = rep.int("", length(x))
        as.data.table(x)
    }





tmp = batchtools::makeRegistry(NA)
tmp$cluster.functions = batchtools::makeClusterFunctionsSlurm(
	template = system.file("templates/slurm-simple.tmpl", package = "batchtools"))

tmp$cluster.functions = batchtools::makeClusterFunctionsSlurm(
	template = "~/code/simpleCache/slurm-advanced.tmpl")
tmp$cluster.functions
tmp

tmp
system.file("templates/slurm-simple.tmpl", package = "batchtools")

args = list(cacheName="testCache3", instruction = list(substitute( rnorm(1e7, 0,1)) ))
list2dt(args)
list2dt(list(args))

batchtools:::list2dt(args)

ids = batchtools::batchMap(fun = simpleCache, args = args, reg = tmp)
ids
getJobTable(reg=tmp)
getJobPars(reg=tmp)$pars
testJob(1, reg=tmp)
testJob(2, reg=tmp)
testJob(3, reg=tmp)

tmp
tmp$defs$pars
data.table(cacheName="testCache3", instruction = list(substitute( rnorm(1e7, 0,1))))

tmp$defs$pars




submitJobs()
# walltime is in minutes
res = list(ncpus = 1, memory = 1000, walltime=60, partition="economy")

batchtools::submitJobs(reg = tmp, res=res)
killJobs()

library(batchtools)
getJobTable()
getJobPars()
findJobs()
getStatus()
tmp
grepLogs(pattern = "parallelMap", reg = tmp)

getJobTable(reg=registry)
loadResult(1, reg=registry)
getJobPars(1, reg=registry)$pars
testJob(1, reg=registry)
loadResult(1, reg=registry)
loadResult(2, reg=registry)
getStatus(1, reg=registry)
showLog(1, reg=registry)
loadResult(2)
loadResult(2)


refreshPackage("simpleCache")
library(simpleCache)
setCacheDir(getwd())
registry = batchtools::makeRegistry(NA)
registry$cluster.functions = batchtools::makeClusterFunctionsSlurm(
	template = "~/code/simpleCache/slurm-advanced.tmpl")
registry

res = list(ncpus=1, memory= 1000, walltime=60, partition="economy")

simpleCache("testBatch", {
	rnorm(1e7, 0,1)
	}, batchRegistry=registry, batchResources=res, recreate=TRUE)



go()
te()
sweepRegistry(registry)


testBatch
simpleCache("testBatch")
rm(testBatch)









batchMap2 = function (fun, ..., args = list(), more.args = list(), reg = getDefaultRegistry()) 
{
    list2dt = function(x) {
        nn = names(x)
        if (is.null(nn)) 
            names(x) = rep.int("", length(x))
        as.data.table(x)
    }
    assertRegistry(reg, writeable = TRUE, strict = TRUE)
    if (nrow(reg$defs) > 0L) 
        stop("Registry must be empty")
    assertFunction(fun)
    assert(checkList(args), checkDataFrame(args))
    assertList(more.args, names = "strict")
    if (length(args) > 0L) {
        if (length(list(...)) > 0L) 
            stop("You may only provide arguments via '...' *or* 'args'")
        ddd = list2dt(args)
    }
    else {
        ddd = list2dt(list(...))
    }
    if (".job" %chin% names(ddd)) 
        stop("Name '.job' not allowed as parameter name (reserved keyword)")
    if (any(dim(ddd) == 0L)) 
        return(noIds())
    batchtools:::info("Adding %i jobs ...", nrow(ddd))
    writeRDS(fun, file = file.path(reg$file.dir, "user.function.rds"))
    if (length(more.args) > 0L) 
        writeRDS(more.args, file = file.path(reg$file.dir, "more.args.rds"))
    ids = seq_row(ddd)
    reg$defs = data.table(def.id = ids, pars = .mapply(list, 
        dots = ddd, MoreArgs = list()), key = "def.id")
    reg$status = data.table(job.id = ids, def.id = ids, submitted = NA_real_, 
        started = NA_real_, done = NA_real_, error = NA_character_, 
        memory = NA_real_, resource.id = NA_integer_, batch.id = NA_character_, 
        log.file = NA_character_, job.hash = NA_character_, key = "job.id")
    saveRegistry(reg)
    invisible(allIds(reg))
}
