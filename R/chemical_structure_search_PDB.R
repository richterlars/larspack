#' Chemical Structure search against PDB
#'
#' @param smiles SMILES string of type character
#'
#' @return a list objects with two elements, element 'pdb' contains a character vector of pdb ids and element 'smiles' contains a vector of smiles strings of the matched ligands
#'
#' @examples
#' get_pdbFromMol(smiles = "CC(=O)OC1=CC=CC=C1C(=O)O", tanimoto_treshold = 0.9)
#' $`pdb`
#' [1] "4EFU" "6EY8" "6EY9"
#' $smiles
#' [1] "Cc1cccc(c1)Cc2c3cc(c(cc3[nH]n2)O)C(=O)N(C)Cc4ccccc4"    
#' [2] "CN(Cc1ccc(cc1)Cl)C(=O)c2cc3c(cc2O)[nH]nc3Cc4ccccc4"     
#' [3] "Cc1cccc(c1)Cc2c3cc(c(cc3[nH]n2)O)C(=O)N(C)Cc4ccc(cc4)Cl"
#' @describeIn
#' Utilizing PDB Web API to search PDB complexes with share similarity with input ligand
#' https://www.rcsb.org/pdb/software/rest.do#descEntity
#'
#' @export

get_pdbFromMol <- function(smiles, exactSearch = FALSE, tanimoto_treshold = 0.9){
  smiles <- smiles %>%
    URLencode() %>%
    stringr::str_replace_all("#","%23")
  
  if (exactSearch) {
    query <- stringr::str_c("http://www.rcsb.org/pdb/rest/smilesQuery?smiles=", smiles, "&search_type=exact")
  } else {
    query <- stringr::str_c("http://www.rcsb.org/pdb/rest/smilesQuery?smiles=", smiles, "&search_type=similarity&similarity=", tanimoto_treshold)
  }
  content <- httr::GET(query) %>%
    httr::content()
  
  structure_ids <- xml2::xml_find_all(content, ".//ligand") %>%
    xml2::xml_attr("structureId")
  
  smiles <- xml2::xml_find_all(content, ".//ligand/smiles") %>% 
    xml2::xml_text()
  
  list(pdb = structure_ids, smiles = smiles)
  
}
