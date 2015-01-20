#Functions for SLURM binding within R... wow!
#' Returns a slurm parameterization object for use in simpleCache();

#' @export
getSlurmParams = function(preamble="", submit=T, hpcFolder="slurm", jobName="test", mem="4000", cores="5", partition="develop", timeLimit="02:00:00", sourceProjectInit=TRUE) {
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
#' slurm job.
#' 
#' @export
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
	rcode= str_replace(rcode, "\\$", "\\\\$");

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



