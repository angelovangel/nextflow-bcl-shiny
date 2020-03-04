# function defining the ncct modal
# 
ncct_modal <- function() {
  modalDialog(size = "l", 
              footer = tagList(modalButton("Cancel"), actionButton("ncct_ok", "OK")),
    textInput("customer", "Customer name", 
              placeholder = "Enter name of PI"),
    textInput("project_id", "Project ID", 
              placeholder = "Enter project ID"),
    selectizeInput("ncct_contact", "Contact at NCCT", 
                   choices = c("a", "b")
                   ),
    selectizeInput("project_type", "Type of project", 
                   choices = c("DNA-seq", 
                               "RNA-seq", 
                               "Amplicon-seq")
                   ),
    selectizeInput("lib_prep", "Library prep kit", 
                   choices = c("Illumina Nextera DNA Flex", 
                               "Illumina TruSeq Nano",
                               "Zymo RiboFree Total RNA", 
                               "Zymo Quick-16S", 
                               "Custom 16S", 
                               "Custom amplicon")
                   ),
    selectizeInput("indexing", "Index kit", 
                   choices = c("a",
                               "b")
                   ),
    # selectizeInput("seq_platform", "Sequencing platform", 
    #                choices = c("MiSeq", 
    #                            "NextSeq 500", 
    #                            "iSeq")
    #                ),
    selectizeInput("seq_setup", "Sequencing setup", 
                   choices = c("MiSeq Reagent Nano Kit v2 (300 cycles)", 
                               "MiSeq Reagent Micro Kit v2 (300 cycles)", 
                               "MiSeq Reagent Kit v2 (500 cycles)", 
                               "MiSeq Reagent Kit v2 (300 cycles)", 
                               "MiSeq Reagent Kit v2 (50 cycles)", 
                               "MiSeq Reagent Kit v3 (600 cycles)", 
                               "MiSeq Reagent Kit v3 (150 cycles)", 
                               "NextSeq 500 Mid Output Kit v2 (150 cycles)", 
                               "NextSeq 500 Mid Output Kit v2 (300 cycles)", 
                               "NextSeq 500 High Output Kit v2 (75 cycles)", 
                               "NextSeq 500 High Output Kit v2 (150 cycles)", 
                               "NextSeq 500 High Output Kit v2 (300 cycles)")
                   )
    
  )
}