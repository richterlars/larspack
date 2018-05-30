#' Chemical Structure search against PDB
#'
#' @param smiles SMILES string of type character
#'
#' @return character vector of PDB codes
#'
#' @examples
#' > get_pdb_id("CC(=O)OC1=CC=CC=C1C(=O)O", 0.9)
#' [1] "1OXR" "1TGM" "2QQT" "3FCQ" "3GCL" "3IAZ" "4NSB"
#'
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
    query <- stringr::str_c("http://www.rcsb.org/pdb/rest/smilesQuery?smiles=", smiles, "&search_type=similarity&similarity=", tc)
  }
    content <- httr::GET(query) %>%
    httr::content()

  xml2::xml_find_all(content, ".//ligand") %>%
    xml2::xml_attr("structureId")

  }
