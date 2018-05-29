#' Canonicalizes SMILES according to RDKit
#'
#' @param smiles SMILES string of type character
#'
#' @return RDKit canonical SMILES
#'
#' @examples rdkit_canon("CCO")
#'
#' @describeIn
#' #You need to set anaconda environment prior to running RDKit code
#' reticulate::use_condaenv(condaenv = "my-rdkit-env", conda = "/Users/lrichter/miniconda3/bin/conda")
#' reticulate::use_condaenv(condaenv = "my-rdkit-env", conda = "/Users/lrichter/miniconda3/bin/conda")
#' @export

rdkit_canon <- function(smiles) {

  Chem <- reticulate::import("rdkit.Chem")
  mol <- Chem$MolFromSmiles(smiles)
  Chem$MolToSmiles(mol)
}
