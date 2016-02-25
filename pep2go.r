#!/usr/bin/env Rscript

args <- commandArgs(TRUE)
nargs <- length(args)

bld <- system('bold=`tput bold`;echo "${bold}"', intern=TRUE)
nrml <- system('normal=`tput sgr0`;echo "${normal}"', intern=TRUE)

library("biomaRt")
cat("Loading ensembl data...\n")
ensembl <- useMart("ENSEMBL_MART_ENSEMBL", dataset="hsapiens_gene_ensembl", host="www.ensembl.org")
# Alternative if biomart is down: host="jul2015.archive.ensembl.org", path="biomart/martservice", archive=FALSE)
cat('Loaded H. sapiens genome data\n\n')

if (nargs==0) {
	cat(bld,"Please provide a TSV input file (or hit enter to list files in this directory):\n\t",sep='',nrml)
		con <- file("stdin")
		filein <- readLines(con,1)
		close(con)
	if (filein=='') {
		lslist <- strsplit(system("ls -l | grep ^- | awk '{print $9}' | sed -e 's/$/,,,/'", intern=TRUE),',,,') # uses arbitrary ,,, separator to split files in list with certainty rather than spaces which may be present but escaped "\ " within filenames
		print(paste(lslist))
		cat(bld,"\nSelect a file (1 to ",length(lslist),"):\n\t",sep='',nrml)
			con <- file("stdin")
			selnum <- readLines(con,1)
			close(con)
			iselnum <- as.integer(selnum)
		if (is.na(iselnum)) {
			repeat {
				print(paste(lslist))
				cat(bld,"\nPlease enter a file number (1 to ",length(lslist)," or hit enter to abort):\n\t",sep='',nrml)
					con <- file("stdin")
					selnum <- readLines(con,1)
					close(con)
				if (is.na(iselnum) && selnum != '') {
					# continue to ask for a number
				} else if (selnum > 0 && selnum <= length(lslist)) {
					filein <- unlist(lslist[iselnum])
					break
				} else if (selnum == '') {
					quit()
				}
			}
		} else if (iselnum > 0 && iselnum <= length(lslist)) {
			filein <- unlist(lslist[iselnum])
			cat('\n')
		} else {
			repeat {
				print(paste(lslist))
				cat(bld,"\nSelect a file (1 to ",length(lslist)," or hit enter to abort):\n\t",sep='',nrml)
					con <- file("stdin")
					selnum <- readLines(con,1)
					close(con)
				if (is.na(iselnum) && selnum != '') {
					# continue to ask for a number
				} else if (selnum > 0 && selnum <= length(lslist)) {
					filein <- unlist(lslist[iselnum])
					break
				} else if (selnum == '') {
					quit()
				}
			}
		}
	}
	cat(filein,'selected\n')
	tsvin <- read.delim(filein, header=FALSE) # the header will be checked later
	prein <- unlist(strsplit(filein,'[.]')[1])[1]
	suffin <- unlist(strsplit(filein,'[.]')[1])[2]
	outfile <- paste(c(prein,'_GO.',suffin), collapse="")
	cat(bld,"\nUsing ",outfile," as output - hit enter to confirm or type a different filename:\n\t",sep='',nrml)
		con <- file("stdin")
		checkout <- readLines(con,1)
		close(con)
	if (checkout!='') {
		outfile <- checkout
	}
} else {
	if (nargs==1) {
	cat(bld,"Using ",args[1]," as input\n",sep='',nrml)
	filein <- args[1]
	tsvin <- read.delim(filein, header=FALSE) # the header will be checked later
	prein <- unlist(strsplit(filein,'[.]')[1])[1]
	suffin <- unlist(strsplit(filein,'[.]')[1])[2]
	outfile <- paste(c(prein,'_GO.',suffin), collapse="")
	cat("\nUsing ",outfile," as output. ",bld,"Press enter to confirm or provide a filename:\n\t",sep='',nrml)
		con <- file("stdin")
		checkout <- readLines(con,1)
		close(con)
	if (checkout!='') {
		outfile <- checkout
	}
	} else if (nargs==2) {
		filein <- args[1]
		outfile <- args[2]
		cat("Using",args[1],"as input, and",args[2],"as output\n")
		} else {
			cat("Please only provide parameters of input (and optionally output) to this script\n")
		}
}

if (length(tsvin)>1) {
	possheader <- as.character(read.delim(filein, header=FALSE)[1,1])
	headernums <- '[1]'
	for (i in 2:length(tsvin)) {
		nextnum <- paste(c('[',i,']'),collapse='')
		headernums <- paste(headernums,nextnum,sep='\t')
		nextposs <- as.character(read.delim(filein, header=FALSE)[1,i])
		possheader <- paste(possheader,nextposs,sep='\t')
	}
	cat(headernums,'\n',possheader,bld,'\n\nAre these column labels (i.e. does the file have header rows)? (y/n)\n\t',sep='',nrml)
} else if (length(tsvin)==1) {
	possheader <- as.character(read.delim(filein, header=FALSE)[1,1])
	headernums <- '[1]'
	cat(possheader,bld,'\n\nIs this a column label (i.e. does the file have a header row)? (y/n)\n\t',sep='',nrml)
} else {
	cat('Sorry, the file you loaded contains no data... Hit enter to quit\n') # maybe add option to head(file), but this is going beyond what's necessary
		con <- file("stdin")
		checkout <- readLines(con,1)
		close(con)
	quit()
}
		con <- file("stdin")
		headerq <- readLines(con,1)
		close(con)
	flett = substring(headerq,1,1)
	if (flett!='y') {
		if (flett!='n') {
			repeat {
				cat(headernums,'\n',possheader,bld,'\n\nAre these fields headers? (y/n)\n\t',sep='',nrml)
					con <- file("stdin")
					headerq <- readLines(con,1)
					close(con)
					flett = substring(headerq,1,1)
				if (flett=='y') {
					haslabels <- TRUE
					break
				} else if (flett == 'n') {
					haslabels <- FALSE
					break
				}
			}
		} else {
			haslabels <- FALSE
		}
	} else {
		haslabels <- TRUE
	}

	cat("\nUsing column 1 (\"",as.character(unlist(tsvin[1])[1]),"\") as Ensembl peptide ID (ENSP...). ",bld,"Hit enter to confirm, or select another column number:\n\t",sep='',nrml)
		con <- file("stdin")
		selnum <- readLines(con,1)
		close(con)
		iselnum <- as.integer(selnum)
	if (selnum == '') {
		selcol <- 1
	} else if(is.na(iselnum)) {
		repeat {
			cat(bld,"\nPlease enter a number or hit enter to use column 1 (\"",as.character(unlist(tsvin[1])[1]),"\") as Ensembl peptide ID (ENSP...):\n\t",sep='',nrml)
				con <- file("stdin")
				selnum <- readLines(con,1)
				close(con)
			if (selnum == '') {
				selcol <- 1
				break
			} else if (is.na(iselnum)) {
				# continue to ask for a number
			} else if (iselnum > 0 && iselnum <= length(tsvin)) {
				selcol <- iselnum
				break
			}
		}
	} else if (iselnum > 0 && iselnum <= length(tsvin)) {
		selcol <- iselnum
	} else {
		repeat {
			cat("\nResponse not recognised - using column 1 (\"",as.character(unlist(tsvin[1])[1]),"\") as Ensembl peptide ID (ENSP...). ",bld,"Hit enter to confirm, or select another column number:\n\t",sep='',nrml)
				con <- file("stdin")
				selnum <- readLines(con,1)
				close(con)
			if (selnum == '') {
				selcol <- 1
				break
			} else if (iselnum > 0 && iselnum <= length(tsvin)) {
				selcol <- iselnum
				break
			}
		}
	}
	# tsvin <- read.delim(header=TRUE)
if (haslabels == TRUE) {
	tsvin <- read.delim(filein, header=TRUE)
}

# would be good if possible to check for duplicate values in ENSP and notify you but difficult...
# maybe check unique, strint2[] <- lapply(data.frame[selnum], as.character) or something to make them character vectors, use setdiff or x[!x %in% y] to compare differences between ensp column and unique(ensp column)

# We now have both a parsed input file and an output file (filein parsed as tsvin, and outfile)

cat("Input = ",filein,', ',nrow(tsvin),' lines long\nOutput = ',outfile,bld,"\nHit enter to confirm (merging input with GO annotations in the output), type 'g' to write only peptide IDs and their GO annotations or 'q' to cancel\n\t",sep='',nrml)
	con <- file("stdin")
	confirm <- readLines(con,1)
	close(con)
if (confirm != '') {
	if (confirm == 'q') {
		quit()
	} else if (confirm == 'g') {
		merger <- FALSE
	}
	# continue to ask for confirmation to write file
	repeat {
		cat("Input = ",filein,' ,',nrow(tsvin),' lines long\nOutput = ',outfile,bld,'\nHit enter to confirm, or type q to cancel\n\t',nrml)
			con <- file("stdin")
			confirm <- readLines(con,1)
			close(con)
		if (confirm != '') {
			if (confirm == 'q') {
				quit()
			} else if (confirm == 'g') {
				merger <- FALSE
			}
		}
	}
}
	cat('\nContacting Ensembl Biomart...\n')
	goterms <- getBM(attributes=c('ensembl_peptide_id','go_id','name_1006'), filters='ensembl_peptide_id', values=tsvin[as.integer(selcol)], mart=ensembl)
	cat('\nGot GO terms:\n')
	head(goterms)

cat(bld,"If you're happy to write these results to file, hit enter to confirm, or type q to cancel:\n\t",sep='',nrml)
	con <- file("stdin")
	confirm <- readLines(con,1)
	close(con)
if (confirm != '') {
	if (confirm == 'q') {
		quit()
	}
	# continue to ask for confirmation to write file
	repeat {
		cat(bld,"If you're happy to write these results to file, hit enter to confirm, or type q to cancel:\n\t",sep='',nrml)
			con <- file("stdin")
			confirm <- readLines(con,1)
			close(con)
		if (confirm != '') {
			if (confirm == 'q') {
				quit()
			}
		}
	}
}

if (!exists("merger")) {
	mergedresults <- merge(goterms,tsvin,by.x="ensembl_peptide_id",by.y=colnames(tsvin)[as.integer(selcol)])
	write.table(mergedresults, file=outfile, sep='\t', na="NULL", col.names=TRUE, row.names=FALSE, quote=FALSE)
} else {
	write.table(tsvin, file = outfile, sep = '\t', na = "NULL", col.names = TRUE, row.names = FALSE, quote = FALSE)
}

cat("\nFile written.\n")
