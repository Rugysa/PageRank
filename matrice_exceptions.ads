-- Définition d'une exception commune à toutes les Matrices
package Matrice_Exceptions is

	INDICE_INVALIDE_EXCEPTION : Exception;
    	PRODUIT_INDEFINI_EXCEPTION  : Exception;	-- une clé est absente d'une Matrice
	SOMME_INDEFINIE_EXCEPTION : Exception;

end Matrice_Exceptions;
