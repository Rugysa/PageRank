with Ada.Text_IO;            use Ada.Text_IO;
with Matrice_Exceptions;         use Matrice_Exceptions;
with Ada.Unchecked_Deallocation;

package body Matrice_creuse is

    procedure Free_Colonne is
            new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_Liste_Colonne);
            
    procedure Free_Ptr_Colonne is
            new Ada.Unchecked_Deallocation (Object => T_Colonne, Name => T_Ptr_Colonne);
	
procedure Initialiser(Mat : out T_Matrice_Creuse; Taille_Ligne : in Integer; Taille_Colonne : in Integer) is
	begin
		-- Vérification que les indices sont correctes
		if Taille_Colonne <=0 or else Taille_Ligne <=0 then
			raise INDICE_INVALIDE_EXCEPTION;
		end if;
		
		-- Définition de la taille de la matrice
		Mat.Nb_Ligne := Taille_Ligne;
		Mat.Nb_Colonne := Taille_Colonne;
	
		-- Définition de la matrice
		Mat.Matrice_Creuse := null;
		
end Initialiser;
			
procedure Detruire (Mat : in out T_Matrice_Creuse) is

	-- Détruit les éléments d'une colonne
	procedure Detruire_Colonne (Liste_Colonne : in out T_Liste_Colonne) is
	    begin
		if  Liste_Colonne /= null then
		    Detruire_Colonne(Liste_Colonne.all.Suivant);
		    Free_Colonne(Liste_Colonne);
		end if;
	   end Detruire_Colonne;
	   
	 -- Détruire les éléments de la liste de colonne
	procedure Detruire_Ptr_Colonne (Liste_Ptr_Colonne : in out T_Ptr_Colonne) is
	    begin
		if  Liste_Ptr_Colonne /= null then
		    Detruire_Ptr_Colonne(Liste_Ptr_Colonne.all.Colonne_Suivante);
			-- Détruit les éléments de la colonne actuelle
		    Detruire_Colonne(Liste_Ptr_Colonne.all.Colonne_Actuelle);
		    Free_Ptr_Colonne(Liste_Ptr_Colonne);
		end if;
	   end Detruire_Ptr_Colonne;
    
    begin
    	
    	-- Détruire la liste de colonne
    	Detruire_Ptr_Colonne(Mat.Matrice_Creuse);
    	
    end Detruire;

procedure Transposer(Mat : in T_Matrice_Creuse; Mat_Res: out T_Matrice_Creuse) is
		Curseur_Colonne: T_Ptr_Colonne;
		Curseur_Liste_Colonne: T_Liste_Colonne;
	begin
		-- Définition de la taille de la matrice
		Mat_Res.Nb_Ligne := Mat.Nb_Colonne;
		Mat_Res.Nb_Colonne := Mat.Nb_Ligne;
		
		-- Définition des coefficients de la matrice transposée
		Curseur_Colonne := Mat.Matrice_Creuse;
		while Curseur_Colonne /= null loop -- Parcours de des colonnes de la matrice
			Curseur_Liste_Colonne := Curseur_Colonne.all.Colonne_Actuelle;
			while Curseur_Liste_Colonne /= null loop -- Parcours des éléments d'une colonne
				Enregistrer(Mat_Res,Curseur_Colonne.all.Num_Colonne,Curseur_Liste_Colonne.all.Ligne,Curseur_Liste_Colonne.all.Valeur);
				Curseur_Liste_Colonne := Curseur_Liste_Colonne.all.Suivant;
			end loop;
			Curseur_Colonne := Curseur_Colonne.all.Colonne_Suivante;
		end loop;
end Transposer;


procedure Produit(A : in T_Matrice_Creuse; B : in T_Matrice_Creuse; Mat_Res : out T_Matrice_Creuse) is
	Valeur : T_Reel;
	Curseur_Colonne : T_Ptr_Colonne;
	Curseur_Liste_Colonne : T_Liste_Colonne;
	Num_Colonne : Integer;
	begin
		-- Vérification de la compatibilité des matrices pour le produit matriciel
		if A.Nb_Colonne /= B.Nb_Ligne then
			raise PRODUIT_INDEFINI_EXCEPTION;
		end if;
		
		-- Définition de la taille de la matrice
		Mat_Res.Nb_Ligne := A.Nb_Ligne;
		Mat_Res.Nb_Colonne := B.Nb_Colonne;
		
		-- Calcul des coefficients de la matrice résultat
		Curseur_Colonne := B.Matrice_Creuse;
		while Curseur_Colonne /= null loop -- Parcours de des colonnes de la matrice B
			Num_Colonne := Curseur_Colonne.all.Num_Colonne;
			for i in 1.. A.Nb_Ligne loop -- Boucle sur toutes les lignes de A
				Valeur := Zero;	-- Initialisation de coef(i,Num_Colonne) de Mat_Res
				Curseur_Liste_Colonne := Curseur_Colonne.all.Colonne_Actuelle;
				while Curseur_Liste_Colonne /= null loop -- Parcours des éléments d'une colonne
					Valeur := Valeur + Obtenir_Val(A,i,Curseur_Liste_Colonne.all.Ligne)* Curseur_Liste_Colonne.all.Valeur;
					Curseur_Liste_Colonne := Curseur_Liste_Colonne.all.Suivant;
				end loop;
				Enregistrer(Mat_Res,i,Num_Colonne,Valeur);
			end loop;
			Curseur_Colonne := Curseur_Colonne.all.Colonne_Suivante;
		end loop;
		
end Produit;

function Produit_Tab_Creux (Tab : in T_Tableau; Creux : in T_Matrice_Creuse) return T_Tableau is 
	Res : T_Tableau;
	Curseur_Colonne : T_Ptr_Colonne;
	Curseur_Liste_Colonne : T_Liste_Colonne;
	Num_Colonne : Integer;
	begin
		-- Vérification que le tableau et la matrice sont de bonnes tailles
		if Tab.Taille /= Creux.Nb_Ligne then
			raise PRODUIT_INDEFINI_EXCEPTION;
		end if;
		
		-- Initialisation du tableau résultat
		Res.Taille := Tab.Taille;
		for j in 1..Taille_Tab loop
			Res.Elements(j) := Zero;
		end loop;
		
		-- Remplissage du tableau résultat
		Curseur_Colonne := Creux.Matrice_Creuse;
		while Curseur_Colonne /= null loop -- Parcours de des colonnes de la matrice B
			Num_Colonne := Curseur_Colonne.all.Num_Colonne;
			Curseur_Liste_Colonne := Curseur_Colonne.all.Colonne_Actuelle;
			while Curseur_Liste_Colonne /= null loop -- Parcours des éléments d'une colonne	
				Res.Elements(Num_Colonne) :=  Res.Elements(Num_Colonne)  + Tab.Elements(Curseur_Liste_Colonne.all.Ligne)*Curseur_Liste_Colonne.all.Valeur;
				Curseur_Liste_Colonne := Curseur_Liste_Colonne.all.Suivant;
			end loop;
			Curseur_Colonne := Curseur_Colonne.all.Colonne_Suivante;
		end loop;
		return Res;
end Produit_Tab_Creux;		

procedure Copier(Mat : in T_Matrice_Creuse; Copie : out T_Matrice_Creuse) is
	Curseur_Colonne : T_Ptr_Colonne;
	Curseur_Liste_Colonne : T_Liste_Colonne;
	begin
		
		-- Définition de la taille de la matrice
		Copie.Nb_Ligne := Mat.Nb_Ligne;
		Copie.Nb_Colonne := Mat.Nb_Colonne;
		
		-- Copie des coefficients de Mat dans Copie
		Curseur_Colonne := Mat.Matrice_Creuse;
		while Curseur_Colonne /= null loop -- Parcours de des colonnes de la matrice Mat
			Curseur_Liste_Colonne := Curseur_Colonne.all.Colonne_Actuelle;
			while Curseur_Liste_Colonne /= null loop -- Parcours des éléments d'une colonne	
				Enregistrer(Copie,Curseur_Liste_Colonne.all.Ligne,Curseur_Colonne.all.Num_Colonne,Curseur_Liste_Colonne.all.Valeur);
				Curseur_Liste_Colonne := Curseur_Liste_Colonne.all.Suivant;
			end loop;
			Curseur_Colonne := Curseur_Colonne.all.Colonne_Suivante;
		end loop;
end Copier;

procedure Sommer(A : in T_Matrice_Creuse; B : in T_Matrice_Creuse; Mat_Res : out T_Matrice_Creuse)is
	Valeur : T_Reel;
	Curseur_Colonne : T_Ptr_Colonne;
	Curseur_Liste_Colonne : T_Liste_Colonne;
	Num_Ligne: Integer;
	Num_Colonne : Integer;
	begin
		-- Vérification de la compatibilité des matrices pour la somme matriciel
		if A.Nb_Colonne /= B.Nb_Colonne or else A.Nb_Ligne /= B.Nb_Ligne  then
			raise SOMME_INDEFINIE_EXCEPTION;
		end if;
		
		-- Définition de la taille de la matrice
		Mat_Res.Nb_Ligne := A.Nb_Ligne;
		Mat_Res.Nb_Colonne := A.Nb_Colonne;
		
		-- Calcul des coefficients de la matrice résultat
		
		-- On regarde tous les coefficients non  nuls de A
		Curseur_Colonne := A.Matrice_Creuse;
		while Curseur_Colonne /= null loop -- Parcours de des colonnes de la matrice A
			Num_Colonne := Curseur_Colonne.all.Num_Colonne;
			Curseur_Liste_Colonne := Curseur_Colonne.all.Colonne_Actuelle;
			while Curseur_Liste_Colonne /= null loop -- Parcours des éléments d'une colonne	
				Num_Ligne := Curseur_Liste_Colonne.all.Ligne;
				Valeur := Curseur_Liste_Colonne.all.Valeur + Obtenir_Val(B,Num_Ligne,Num_Colonne);
				Enregistrer(Mat_Res,Num_Ligne,Num_Colonne, Valeur); -- On met le résultat de l'addition dans la matrice Res
				Curseur_Liste_Colonne := Curseur_Liste_Colonne.all.Suivant;
			end loop;
			Curseur_Colonne := Curseur_Colonne.all.Colonne_Suivante;
		end loop;
		
		-- On regarde tous les coefficients non nuls de B
		Curseur_Colonne := B.Matrice_Creuse;
		while Curseur_Colonne /= null loop -- Parcours de des colonnes de la matrice B
			Num_Colonne := Curseur_Colonne.all.Num_Colonne;
			Curseur_Liste_Colonne := Curseur_Colonne.all.Colonne_Actuelle;
			while Curseur_Liste_Colonne /= null loop -- Parcours des éléments d'une colonne	
				Num_Ligne := Curseur_Liste_Colonne.all.Ligne;
				Valeur := Obtenir_Val(A,Num_Ligne,Num_Colonne) + Curseur_Liste_Colonne.all.Valeur;
				Enregistrer(Mat_Res,Num_Ligne,Num_Colonne, Valeur); -- On met le résultat de l'addition dans la matrice Res
				Curseur_Liste_Colonne := Curseur_Liste_Colonne.all.Suivant;
			end loop;
			Curseur_Colonne := Curseur_Colonne.all.Colonne_Suivante;
		end loop;
end Sommer;

procedure Enregistrer(Mat : in out T_Matrice_Creuse; Ind_Ligne : in Integer; Ind_Colonne : in Integer; Valeur : in T_Reel) is

	-- Enregistre une valeur dans une colonne donnée
	procedure Enregistrer_Colonne (Liste_Colonne : in out T_Liste_Colonne; Ind_Ligne: in Integer; Valeur : in T_Reel) is
		begin
			if Liste_Colonne=null then
				Liste_Colonne := new T_Cellule;
				Liste_Colonne.all.Valeur := Valeur;
				Liste_Colonne.all.Ligne := Ind_Ligne;
				Liste_Colonne.all.Suivant := null;
			elsif Liste_Colonne.all.Ligne = Ind_Ligne then
				Liste_Colonne.all.Valeur := Valeur;
			else
				Enregistrer_Colonne(Liste_Colonne.all.Suivant, Ind_Ligne, Valeur);
			end if;
	end Enregistrer_Colonne;
	
	-- Détruit une valeur dans une colonne donnée
	procedure Detruire_Cellule(Liste_Colonne : in out T_Liste_Colonne; Ind_Ligne : in Integer) is
	Cuseur_Liste_Colonne: T_Liste_Colonne;
	precedent : T_Liste_Colonne;
	begin
		Cuseur_Liste_Colonne := Liste_Colonne;
		precedent := null;
		-- Parcours de la liste jusqu'à trouver l'élément à la bonne ligne (si il existe)
		while Cuseur_Liste_Colonne.all.Ligne /= Ind_Ligne loop
			precedent := Cuseur_Liste_Colonne;
			Cuseur_Liste_Colonne := Cuseur_Liste_Colonne.all.Suivant;
		end loop;
		if precedent = null then 
			-- On supprime l'élément de tête
			precedent := Liste_Colonne;
			Liste_Colonne:=Liste_Colonne.all.Suivant;
			Free_Colonne(precedent);
		else
			Precedent.all.Suivant :=Cuseur_Liste_Colonne.all.Suivant;
			Free_Colonne(Cuseur_Liste_Colonne);
		end if;
	end Detruire_Cellule;
	
	-- Créer la case contenant le pointeur d'une colonne si cette case n'existe pas
	procedure Enregistrer_Ptr_Colonne (Liste_Ptr_Colonne : in out T_Ptr_Colonne; Ind_Colonne: in Integer) is
		begin
			if Liste_Ptr_Colonne =null then
				Liste_Ptr_Colonne := new T_Colonne;
				Liste_Ptr_Colonne.all.Num_Colonne := Ind_Colonne;
				Liste_Ptr_Colonne.all.Colonne_Actuelle := null;
				Liste_Ptr_Colonne.all.Colonne_Suivante := null;
			elsif Liste_Ptr_Colonne.all.Num_Colonne /= Ind_Colonne then
				Enregistrer_Ptr_Colonne(Liste_Ptr_Colonne.all.Colonne_Suivante, Ind_Colonne);
			end if;
	end Enregistrer_Ptr_Colonne;
	
	-- Détruit la case contenant le pointeur vers la colonne Ind_Colonne
	procedure Detruire_Ptr_Colonne(Liste_Ptr_Colonne : in out T_Ptr_Colonne ; Ind_Colonne: in Integer) is
	Cuseur_Ptr_Colonne : T_Ptr_Colonne;
	precedent : T_Ptr_Colonne;
	begin
		Cuseur_Ptr_Colonne := Liste_Ptr_Colonne;
		precedent := null;
		-- Parcours de la liste jusqu'à trouver la bonne colonne (si elle existe)
		while Cuseur_Ptr_Colonne.all.Num_Colonne /= Ind_Colonne loop
			precedent := Cuseur_Ptr_Colonne;
			Cuseur_Ptr_Colonne := Cuseur_Ptr_Colonne.all.Colonne_Suivante;
		end loop;
		-- On supprime que si la colonne Actuelle (de num Ind_Colonne) est vide
		if Cuseur_Ptr_Colonne.all.Colonne_Actuelle = null then
			if precedent = null then 
				-- On supprime l'élément de tête
				precedent := Cuseur_Ptr_Colonne;
				Liste_Ptr_Colonne:=Liste_Ptr_Colonne.all.Colonne_Suivante;
				Cuseur_Ptr_Colonne := null;
				precedent.all.Colonne_Suivante := null;
				Free_Ptr_Colonne(precedent);
			else
				precedent.all.Colonne_Suivante :=Cuseur_Ptr_Colonne.all.Colonne_Suivante;
				precedent := null;
				Cuseur_Ptr_Colonne.all.Colonne_Suivante := null;
				Free_Ptr_Colonne(Cuseur_Ptr_Colonne);
			end if;
		end if;
	end Detruire_Ptr_Colonne;
	
	
	Colonne_Enregistrement : T_Ptr_Colonne;
	
	begin	
		-- Vérification des indices
		if Ind_Colonne <=0 or else Ind_Ligne <=0 or else Ind_Ligne > Mat.Nb_Ligne or else Ind_Colonne > Mat.Nb_Colonne then
			raise INDICE_INVALIDE_EXCEPTION;
		end if;
		
		-- On crée la case contenant le pointeur vers la colonne si elle n'existe pas
		Enregistrer_Ptr_Colonne(Mat.Matrice_Creuse,Ind_Colonne);
		-- On parcourt la liste pour trouver sur la colonne Ind_Colonne
		Colonne_Enregistrement := Mat.Matrice_Creuse;
		while Colonne_Enregistrement.all.Num_Colonne /= Ind_Colonne loop
				Colonne_Enregistrement := Colonne_Enregistrement.all.Colonne_Suivante;
		end loop;
			
		-- Disjonction de cas pour savoir si on doit enregistrer la valeur ou supprimer la case mémoire si elle vaut Zero
		if Valeur = Zero and then Obtenir_Val(Mat,Ind_Ligne, Ind_Colonne) /= Zero then
			-- Destruction de la cellule où le Zero apparaît
			Detruire_Cellule(Colonne_Enregistrement.all.Colonne_Actuelle,Ind_Ligne);
			-- Destruction de la case contenant le pointeur de la colonne si la colonne devient vide (que des Zero)
			Detruire_Ptr_Colonne(Mat.Matrice_Creuse, Ind_Colonne);
		elsif Valeur /= Zero then
			-- Enregistrement de valeur
			Enregistrer_Colonne(Colonne_Enregistrement.all.Colonne_Actuelle,Ind_Ligne,Valeur);
		end if;
end Enregistrer;

procedure Produit_Const (Const : in T_Reel; Mat : in out T_Matrice_Creuse) is
	Curseur_Colonne : T_Ptr_Colonne;
	Curseur_Liste_Colonne : T_Liste_Colonne;
	begin
		-- Calcul les nouveaux coefficients de la matrice
		Curseur_Colonne := Mat.Matrice_Creuse;
		while Curseur_Colonne /= null loop -- Parcours de des colonnes de la matrice Mat
			Curseur_Liste_Colonne := Curseur_Colonne.all.Colonne_Actuelle;
			while Curseur_Liste_Colonne /= null loop -- Parcours des éléments d'une colonne	
				Enregistrer(Mat,Curseur_Liste_Colonne.all.Ligne,Curseur_Colonne.all.Num_Colonne, Const * Curseur_Liste_Colonne.all.Valeur);
				Curseur_Liste_Colonne := Curseur_Liste_Colonne.Suivant;
			end loop;
			Curseur_Colonne := Curseur_Colonne.all.Colonne_Suivante;
		end loop;
end Produit_Const;


function Obtenir_Val(Mat: in T_Matrice_Creuse; Ind_Ligne : in Integer; Ind_Colonne :in Integer) return T_Reel is
	Curseur_Colonne : T_Ptr_Colonne;
	Curseur_Liste_Colonne : T_Liste_Colonne;
	begin	
		-- Vérification des indices
		if Ind_Colonne <=0 or else Ind_Ligne <=0 or else Ind_Ligne > Mat.Nb_Ligne or else Ind_Colonne > Mat.Nb_Colonne then
			raise INDICE_INVALIDE_EXCEPTION;
		end if;

		-- Parcourir la liste jusqu’à trouver la colonne Num_Colonne 
		Curseur_Colonne := Mat.Matrice_Creuse;
		while Curseur_Colonne /= null and then Curseur_Colonne.all.Num_Colonne /= Ind_Colonne loop
			Curseur_Colonne := Curseur_Colonne.all.Colonne_Suivante;
		end loop;
			
		-- Retourner la valeur aux coordonnées (Ind_Ligne,Ind_Colonne)
		if  Curseur_Colonne = null then 
			return Zero;	
		else
			Curseur_Liste_Colonne :=Curseur_Colonne.all.Colonne_Actuelle;
			while Curseur_Liste_Colonne /= null loop -- Parcours des éléments de la colonne Ind_Colonne
				if Curseur_Liste_Colonne.all.Ligne = Ind_Ligne then
					return Curseur_Liste_Colonne.all.Valeur;
				end if;
				Curseur_Liste_Colonne := Curseur_Liste_Colonne.all.Suivant;
			end loop;
			return Zero;
		end if;
end Obtenir_Val;
	
procedure Sommer_Const(Const : in T_Reel ; Mat : in out T_Matrice_Creuse) is
	Curseur_Colonne : T_Ptr_Colonne;
	Curseur_Liste_Colonne : T_Liste_Colonne;
	Val : T_Reel;
	begin
		-- Calcul les nouveaux coefficients de la matrice
		Curseur_Colonne := Mat.Matrice_Creuse;
		while Curseur_Colonne /= null loop -- Parcours de des colonnes de la matrice Mat
			Curseur_Liste_Colonne := Curseur_Colonne.all.Colonne_actuelle;
			while Curseur_Liste_Colonne /= null loop -- Parcours des éléments d'une colonne	
				Val := Const + Curseur_Liste_Colonne.all.Valeur;
				Enregistrer(Mat,Curseur_Liste_Colonne.all.Ligne,Curseur_Colonne.all.Num_Colonne, Val);
				Curseur_Liste_Colonne := Curseur_Liste_Colonne.all.Suivant;
			end loop;
			Curseur_Colonne := Curseur_Colonne.all.Colonne_Suivante;
		end loop;
end Sommer_Const;

procedure Afficher(Mat : in T_Matrice_Creuse) is
	begin
	
		-- Affiche chaque coefficient de la matrice, avec chaque ligne entre deux "|"
		for i in 1..Mat.Nb_Ligne loop
			Put("| ");		
			for j in 1..Mat.Nb_Colonne loop
				Afficher_T_Reel(Obtenir_Val(Mat,i,j));
				Put(" ");
			end loop;
			Put("|");
			New_Line;
		end loop;
end Afficher;

function Est_Vide(Mat : in T_Matrice_Creuse) return Boolean is
	begin
		return Mat.Matrice_Creuse = null;
end Est_Vide;

function Nombre_Lignes(Mat : in T_Matrice_Creuse) return Integer is
	begin
		return Mat.Nb_Ligne;
end Nombre_Lignes;

function Nombre_Colonnes(Mat : in T_Matrice_Creuse) return Integer is
	begin
		return Mat.Nb_Colonne;
end Nombre_Colonnes;


end Matrice_creuse;		
