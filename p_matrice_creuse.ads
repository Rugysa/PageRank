with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package P_Matrice_Creuse is
	procedure matricecreuse(K       : Integer; epsilon : Float; alpha : Float;
        prefixe : Unbounded_String; N : Integer; N2 : Integer;
        nom_sujet   : String);
end P_Matrice_Creuse;
