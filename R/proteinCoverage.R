proteinCoverage <- function(proteinId){

    uniprot_url <- "http://www.uniprot.org/uniprot/"



    uniprot_request <- paste0(uniprot_url, proteinID, '.fasta')

    protein_sequence <- httr::GET(uniprot_request)


    protein_sequence   <- httr::content(protein_sequence, encoding = "UTF-8")

    # Keep only the sequence
    protein_sequence <- gsub(x = protein_sequence,
                             replacement = '',
                             pattern =  '.*.SV=.\n',
                             ignore.case = T)

    # Remove the \n
    protein_sequence <- gsub(x = protein_sequence,
         replacement = '',
         pattern = '\n',
         ignore.case = T)


    # Now Match the peptides and add the Start/Finish
}


