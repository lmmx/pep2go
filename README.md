pep2GO
======

Responsive R script to automate GO annotation incorporation into TSV files

Input and output files can be specified as parameters to the script, i.e.

<code language="bash">Rscript pep2go.r inputfilename.txt outputfilename.tsv</code>

If output is not provided, default behaviour is to append `_GO` to the input filename - this is confirmed on the command line, as is the option to merge the resulting annotations (or alternatively just generate a new file with only ENSP code, GO accession and GO description).
