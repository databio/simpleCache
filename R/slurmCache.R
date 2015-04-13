#Functions for SLURM binding within R... wow!
#' Returns a slurm parameterization object for use in simpleCache();
#'
#' @param preamble R code to be executed before the the instruction
#' @param submit Toggle to flag submission or not (Default:True)
#' @param hpcFolder	High Performance Computing folder (location to store
#'			submission files)
#' @param jobName Slurm job name.
#' @param mem	Slurm Memory requested.
#' @param cores Slurm Number of cores.
#' @param partition Slurm partition (queue) to submit to.
#' @param timeLimit Slurm time limit.
#' @param sourceProjectInit Should I also source the projectInit first? Default: TRUE)
#' @export
#' @examples
#' slurmParam = getSlurmParams(mem="6000", cores="2")
getSlurmParams = function(preamble="", submit=TRUE, hpcFolder="slurm", jobName="test", mem="4000", cores="5", partition="develop", timeLimit="02:00:00", sourceProjectInit=TRUE) {
	slurmSettings=list();
	slurmSettings$preamble		=preamble
	slurmSettings$submit			=submit
	slurmSettings$hpcFolder		=hpcFolder 
	slurmSettings$jobName		=jobName
	slurmSettings$mem			=mem
	slurmSettings$cores			=cores 
	slurmSettings$partition		=partition
	slurmSettings$timeLimit		=timeLimit 
	slurmSettings$sourceProjectInit	=sourceProjectInit
	return(slurmSettings)
}

#' Given arguments from a slurmParams object, creates and (possibly) submits a 
#' slurm job. To be used with getSlurmParams for ease.
#'
#' @param rcode R code to run on the cluster.
#' @param preamble R code to be executed before the the instruction
#' @param submit Toggle to flag submission or not (Default:True)
#' @param hpcFolder	High Performance Computing folder (location to store
#'			submission files)
#' @param jobName Slurm job name.
#' @param mem	Slurm Memory requested.
#' @param cores Slurm Number of cores.
#' @param partition Slurm partition (queue) to submit to.
#' @param timeLimit Slurm time limit.
#' @param sourceProjectInit Should I also source the projectInit first? Default: TRUE)
#' @export
#' @examples
#' slurmParams = getSlurmParams(mem="6000", cores="2", hpcFolder="~")
#' with(slurmParams, 
#' buildSlurmScript("1+1", preamble, submit, hpcFolder, jobName, mem, cores,
#' partition, timeLimit, sourceProjectInit))

buildSlurmScript = function(rcode, preamble="", submit=FALSE, hpcFolder="slurm", jobName="test", mem="4000", cores="1", partition="develop", timeLimit="02:00:00", sourceProjectInit=TRUE) {
	if (! file.exists(paste0(hpcFolder, "/")) ) {
		stop(paste0(hpcFolder, " is not a directory"));
	}

	if (sourceProjectInit) {
		spi = paste0("source('", getOption("PROJECT.INIT"), "')\n");
	} else {
		spi = "";
	}
	rcode = paste0(preamble, "\n", rcode);
	#escape $; otherwise, bash will interpret them as bash variables,
	#though they are embedded in R script within bash.
	rcode= gsub("\\$", "\\\\$", rcode);

	script = paste0( "#!/bin/bash
#SBATCH --job-name=", jobName, "
#SBATCH --mem-per-cpu=", mem, "
#SBATCH --cpus-per-task=", cores, "            # 1 task on 1 CPU  
#SBATCH -m block

#SBATCH --partition=", partition, "
#SBATCH --ntasks=1
#SBATCH --time=", timeLimit, "

#set output file:
#SBATCH --output ", hpcFolder, "/", jobName, ".log\n",
"echo 'Compute node:' `hostname`\n",
"echo 'Start time:' `date +'%Y-%m-%d %T'`\n", 
"R --no-save<<END\n",
spi,
rcode,
"\nsessionInfo()\n",
"\nEND\n",
"echo 'End time:' `date +'%Y-%m-%d %T'`\n"); #end paste

message("writing file: ", paste0(hpcFolder, "/", jobName, ".sub"));
write(script, file=paste0(hpcFolder, "/", jobName, ".sub"))
if (submit) { 
	message("submitting job: ", paste0(hpcFolder, "/", jobName, ".sub"));
	system(paste0("sbatch ", hpcFolder, "/", jobName, ".sub"));
}
return(script);
#`sbatch $hpcDir/regionSetOverlap$species$sampleID.sub`; #SUBMIT JOB
} #buildSlurmScript



