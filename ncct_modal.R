# function defining the ncct modal
# 
ncct_modal <- function() {
  modalDialog(size = "m", 
              footer = tagList(modalButton("Cancel"), actionButton("ncct_ok", "OK")),
    textInput("customer", "Customer name", width = '90%', 
              placeholder = "Enter name of PI"),
    textInput("project_id", "Project ID", width = '90%', 
              placeholder = "Enter project ID"),
    selectizeInput("ncct_contact", "Contact at NCCT", width = '90%', 
                   choices = c("a", "b", "c")
                   ),
    selectizeInput("project_type", "Type of project", width = '90%', 
                   choices = c("DNA-seq", 
                               "RNA-seq", 
                               "Amplicon-seq")
                   ),
    selectizeInput("lib_prep", "Library prep kit", width = '90%', 
                   choices = c("Illumina Nextera DNA Flex", 
                               "Illumina TruSeq Nano",
                               "Zymo RiboFree Total RNA", 
                               "Zymo Quick-16S", 
                               "Custom 16S", 
                               "Custom amplicon")
                   ),
    selectizeInput("indexing", "Index kit", width = '90%', 
                   choices = c("a",
                               "b")
                   ),
    # selectizeInput("seq_platform", "Sequencing platform", 
    #                choices = c("MiSeq", 
    #                            "NextSeq 500", 
    #                            "iSeq")
    #                ),
    selectizeInput("seq_setup", "Sequencing setup", width = '90%', 
                   choices = c("iSeq 100 i1 Reagent (300 cycles)",
                               "MiSeq Reagent Nano Kit v2 (300 cycles)", 
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