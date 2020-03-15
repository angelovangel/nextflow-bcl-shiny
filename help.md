### nextflow-bcl-shiny
This web app is a graphical interface for the [nextflow-bcl pipeline](https://github.com/angelovangel/nextflow-bcl). The pipeline runs in a docker container by default.   
It executes the Illumina programs `InterOp summary` and `bcl2fastq`, saves the fastq files in `results-bcl/fastq/`, and generates a MultiQC report in `results-bcl`. The input is an Illumina run folder (with bcl files) and a SampleSheet.csv file.

*Tips:*   

- You can always restart the app by clicking on the nextflow-bcl title
- The number of cores used is determined dynamically based on available cores and number of samples in sample sheet
- You can start the run with Nextflow Tower, then go to the provided url and monitor it there!
