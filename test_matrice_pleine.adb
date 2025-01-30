with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Matrice_Exceptions;         use Matrice_Exceptions;


with Matrice_Pleine;


procedure tesT_Matrice_Pleine is
--	package Double_IO is new Ada.Text_IO.Float_IO (T_Double);
-- use Double_IO;

-- Instanciation des matrices
package Matrice_Reel is
		new Matrice_Pleine( T_Reel => Float, Num_Colonne => 10, Num_Ligne => 10, Zero => 0.0);
	use Matrice_Reel;
	
-- Instanciation de la fonction Afficher
procedure Afficher_T_Reel_Float (Val : in Float) is
	begin
		Put(Val,1);
end Afficher_T_Reel_Float;

procedure Afficher_Mat is
            new Matrice_Reel.Afficher(Afficher_T_Reel_Float);

procedure Tester_Initialiser is
	Mat : T_Matrice_Pleine;
	begin
		begin
			-- Test d'une matrice carré à valeur nulle
			Initialiser(Mat, 4,4,0.0);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(Mat) = 4);
			pragma Assert (Nombre_Colonnes(Mat) = 4);
			-- Vérification des coefficients
			for i in 1..Nombre_Lignes(Mat) loop
				for j in 1..Nombre_Colonnes(Mat) loop
					pragma Assert (Obtenir_Val(Mat,i,j) = 0.0);
				end loop;
			end loop;
			
			-- Test d'une matrice ligne avec une valeur positive
			Initialiser(Mat, 1,4,6.0);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(Mat) = 1);
			pragma Assert (Nombre_Colonnes(Mat) = 4);
			-- Vérification des coefficients
			for i in 1..Nombre_Lignes(Mat) loop
				for j in 1..Nombre_Colonnes(Mat) loop
					pragma Assert (Obtenir_Val(Mat,i,j) = 6.0);
				end loop;
			end loop;
			
			-- Test d'une matrice ligne avec une valeur négative
			Initialiser(Mat, 1,4,-26.56);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(Mat) = 1);
			pragma Assert (Nombre_Colonnes(Mat) = 4);
			-- Vérification des coefficients
			for i in 1..Nombre_Lignes(Mat) loop
				for j in 1..Nombre_Colonnes(Mat) loop
					pragma Assert (Obtenir_Val(Mat,i,j) = -26.56);
				end loop;
			end loop;
			
			-- Cas problématiques
			
			-- Dimension colonne négative
			Initialiser(Mat,5,-2,1.0);
			
			exception 
				when INDICE_INVALIDE_EXCEPTION => Put_Line("Dimensions nulles ou négatives -> matrice non initialisée");
			end;
		-- Dimension ligne négative
		Initialiser(Mat,-1,0,0.0);
		
		exception 
			when INDICE_INVALIDE_EXCEPTION => Put_Line("Dimensions nulles ou négatives -> matrice non initialisée");
		
		Put_Line("Fin Tester_Initialiser");
		
end Tester_Initialiser;

procedure Tester_Enregistrer is
	Mat: T_Matrice_Pleine;
	begin
		begin
			begin
				-- Cas d'enregistrement de valeur positive dans une matrice carré
				Initialiser(Mat, 4,4,0.0);
				
				-- Enregistrement sur un coefficient diagonal
				Enregistrer(Mat,2,2,50.0);
				-- Enregistrement sur un coefficient non diagonal
				Enregistrer(Mat,3,1,46.0);
				-- Vérification des valeurs
				pragma Assert (Obtenir_Val(Mat,2,2) = 50.0);
				pragma Assert (Obtenir_Val(Mat,3,1) = 46.0);
				
				
				-- Cas d'enregistrement de valeurs négatives dans une matrice ligne
				Initialiser(Mat, 1,4,0.0);
				
				Enregistrer(Mat,1,3,-15.75);
				Enregistrer(Mat,4,2,-5.0);
				-- Vérification des valeurs
				pragma Assert (Obtenir_Val(Mat,1,3) = -15.75);
				pragma Assert (Obtenir_Val(Mat,4,2) = -5.0);
				-- Ré enregistrement avec une autre valeur aux mêmes coordonnées
				Enregistrer(Mat,4,2,-59.0);
				pragma Assert (Obtenir_Val(Mat,4,2) = -59.0);

				-- Cas problématiques
				
				-- On enregistre sur une ligne qui n'existe pas dans la matrice
				Enregistrer(Mat,5,3,-15.75);
				
				exception 
					when INDICE_INVALIDE_EXCEPTION => Put_Line("Indices nuls ou négatifs -> Valeur non enregistrée");
				end;
			-- Les coordonnées sont négatives
			Enregistrer(Mat,-4,-2,-5.0);

			
			exception 
					when INDICE_INVALIDE_EXCEPTION => Put_Line("Indices nuls ou négatifs -> Valeur non enregistrée");
			end;
		-- Une coordonnée est nulle
		Enregistrer(Mat,1,0,-5.0);
		exception 
				when INDICE_INVALIDE_EXCEPTION => Put_Line("Indices nuls ou négatifs -> Valeur non enregistrée");
		
		Put_Line("Fin Tester_Enregistrer");
end Tester_Enregistrer;

procedure Tester_Obtenir_Valeur is
	Mat : T_Matrice_Pleine;
	begin
		begin
			begin
				-- Cas classiques
		
				Initialiser(Mat,4,4,0.0);
				Enregistrer(Mat,2,2,50.0);
				Enregistrer(Mat,3,1,46.0);
				
				-- Coordonnées où les valeurs ont été enregistrées et sont non nulles
				pragma Assert (Obtenir_Val(Mat,2,2) = 50.0);
				pragma Assert (Obtenir_Val(Mat,3,1) = 46.0);
				
				-- Coordonnées où les valeurs n'ont pas été enregistrées et sont nulles
				pragma Assert (Obtenir_Val(Mat,1,3) = 0.0);
				pragma Assert (Obtenir_Val(Mat,1,3) = 0.0);
				
				
				-- Cas problématiques
				-- Indice de ligne trop grand
				Put(Obtenir_Val(Mat,5,2));
				
				exception 
					when INDICE_INVALIDE_EXCEPTION => Put_Line("Indices nuls ou négatifs -> Valeur non lue");
				end;
				
			-- Indice de ligne négatif
			Put(Obtenir_Val(Mat,-3,1));
			
			exception 
					when INDICE_INVALIDE_EXCEPTION => Put_Line("Indices nuls ou négatifs -> Valeur non lue");
			end;
		-- Indice de ligne nul et indice de colonne négatif
		Put(Obtenir_Val(Mat,-3,0));
		
		exception 
					when INDICE_INVALIDE_EXCEPTION => Put_Line("Indices nuls ou négatifs -> Valeur non lue");
		Put_Line("Fin Tester_Obtenir_Valeur");
		
end Tester_Obtenir_Valeur;

procedure Tester_Transposer is 
	Mat : T_Matrice_Pleine;
	Trans : T_Matrice_Pleine;
	begin
		-- Cas de transposée d'une matrice carrée à valeur positive
		Initialiser(Mat,4,4,0.0);
		Enregistrer(Mat,2,2,50.0);
		Enregistrer(Mat,3,1,46.0);
		
		Trans := Transposer(Mat);
		-- Vérification des dimensions
		pragma Assert (Nombre_Lignes(Mat)=Nombre_Colonnes(Trans));
		pragma Assert (Nombre_Lignes(Trans)=Nombre_Colonnes(Mat));
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (Obtenir_Val(Mat,i,j) = Obtenir_Val(Trans,j,i));
			end loop;
		end loop;
		
		-- Cas de transposée d'une matrice non carrée à valeur positive et négative
		Initialiser(Mat,3,2,0.0);
		Enregistrer(Mat,2,2,50.0);
		Enregistrer(Mat,3,1,46.0);
		Enregistrer(Mat,1,2,-15.75);
		
		Trans := Transposer(Mat);
		-- Vérification des dimensions
		pragma Assert (Nombre_Lignes(Mat)=Nombre_Colonnes(Trans));
		pragma Assert (Nombre_Lignes(Trans)=Nombre_Colonnes(Mat));
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (Obtenir_Val(Mat,i,j) = Obtenir_Val(Trans,j,i));
			end loop;
		end loop;
		Put_Line("Fin Tester_Transposer");
		
end Tester_Transposer;

procedure Tester_Copier is
	Mat : T_Matrice_Pleine;
	Res : T_Matrice_Pleine;
	begin
		-- Copie d'une matrice carrée à valeur positive
		Initialiser(Mat,4,4,0.0);
		Enregistrer(Mat,2,2,50.0);
		Enregistrer(Mat,3,1,46.0);
		
		Copier(Mat,Res);
		-- Vérification des dimensions
		pragma Assert (Nombre_Lignes(Mat)=Nombre_Lignes(Res));
		pragma Assert (Nombre_Colonnes(Res)=Nombre_Colonnes(Mat));
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (Obtenir_Val(Mat,i,j) = Obtenir_Val(Res,i,j));
			end loop;
		end loop;
		
		-- Copie d'une matrice non carrée à valeur positive et négative
		Initialiser(Mat, 3,2,6.0);
		Enregistrer(Mat,2,2,50.0);
		Enregistrer(Mat,3,1,46.0);
		Enregistrer(Mat,1,2,-15.75);
		
		Copier(Mat,Res);
		-- Vérification des dimensions
		pragma Assert (Nombre_Lignes(Mat)=Nombre_Lignes(Res));
		pragma Assert (Nombre_Colonnes(Res)=Nombre_Colonnes(Mat));
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (Obtenir_Val(Mat,i,j) = Obtenir_Val(Res,i,j));
			end loop;
		end loop;
		
		-- Copie d'une matrice ligne à valeur négative
		Initialiser(Mat, 1,4,-26.56);
		Enregistrer(Mat,1,3,-15.75);
		Enregistrer(Mat,1,4,-465.0);
		
		Copier(Mat,Res);
		-- Vérification des dimensions
		pragma Assert (Nombre_Lignes(Mat)=Nombre_Lignes(Res));
		pragma Assert (Nombre_Colonnes(Res)=Nombre_Colonnes(Mat));
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (Obtenir_Val(Mat,i,j) = Obtenir_Val(Res,i,j));
			end loop;
		end loop;
		
		Put_Line("Fin Tester_Copier");
		
end Tester_Copier;

procedure Tester_Produit is
	A : T_Matrice_Pleine;
	B : T_Matrice_Pleine;
	Res : T_Matrice_Pleine;
	somme_ligneA_colonneB: float;
	begin
		begin
			-- Produit de deux matrices carrées à valeur positive et négative
			Initialiser(A,4,4,0.0);
			Initialiser(B,4,4,5.0);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,46.0);
			Enregistrer(A,1,3,-15.75);
			
			Produit(A,B,Res);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(Res)=Nombre_Lignes(A));
			pragma Assert (Nombre_Colonnes(Res)=Nombre_Colonnes(B));
			-- Vérification des coefficients
			for i in 1..Nombre_Lignes(A) loop
				for j in 1.. Nombre_Colonnes(B) loop
					somme_ligneA_colonneB := 0.0;
					for k in 1.. Nombre_Colonnes(A) loop
						somme_ligneA_colonneB := somme_ligneA_colonneB + Obtenir_Val(A,i,k)*Obtenir_Val(B,k,j);
					end loop;
					pragma Assert(somme_ligneA_colonneB -Obtenir_Val(Res,i,j)<10.0**6);
				end loop;
			end loop;
			
			-- Produit de d'une matrice carrée et d'une matrice non carrée à valeur positive et négative
			Initialiser(A,4,4,0.0);
			Initialiser(B,4,2,-1.5);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,4.0);
			Enregistrer(A,1,3,-15.75);
			
			Produit(A,B,Res);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(Res)=Nombre_Lignes(A));
			pragma Assert (Nombre_Colonnes(Res)=Nombre_Colonnes(B));
			-- Vérification des coefficients
			for i in 1..Nombre_Lignes(A) loop
				for j in 1.. Nombre_Colonnes(B) loop
					somme_ligneA_colonneB := 0.0;
					for k in 1.. Nombre_Colonnes(A) loop
						somme_ligneA_colonneB := somme_ligneA_colonneB + Obtenir_Val(A,i,k)*Obtenir_Val(B,k,j);
					end loop;
					pragma Assert(somme_ligneA_colonneB = Obtenir_Val(Res,i,j));
				end loop;
			end loop;
			
			-- Cas d'erreurs
			-- Dimensions incompatibles
			Initialiser(A,4,5,0.0);
			Initialiser(B,4,2,-1.5);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,4.0);
			Enregistrer(A,1,3,-15.75);
			
			Produit(A,B,Res);
			exception
				when PRODUIT_INDEFINI_EXCEPTION => Put_Line("Dimensions des matrices incompatibles");
			end;
		
		Put_Line("Fin Tester_Produit");
		
end Tester_Produit;

procedure Tester_Produit_f is
	A : T_Matrice_Pleine;
	B : T_Matrice_Pleine;
	Res : T_Matrice_Pleine;
	somme_ligneA_colonneB: float;
	begin
		begin
			-- Produit de deux matrices carrées à valeur positive et négative
			Initialiser(A,4,4,0.0);
			Initialiser(B,4,4,5.0);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,46.0);
			Enregistrer(A,1,3,-15.75);
			
			Res := Produit_f(A,B);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(Res)=Nombre_Lignes(A));
			pragma Assert (Nombre_Colonnes(Res)=Nombre_Colonnes(B));
			-- Vérification des coefficients
			for i in 1..Nombre_Lignes(A) loop
				for j in 1.. Nombre_Colonnes(B) loop
					somme_ligneA_colonneB := 0.0;
					for k in 1.. Nombre_Colonnes(A) loop
						somme_ligneA_colonneB := somme_ligneA_colonneB + Obtenir_Val(A,i,k)*Obtenir_Val(B,k,j);
					end loop;
					pragma Assert(somme_ligneA_colonneB = Obtenir_Val(Res,i,j));
				end loop;
			end loop;
			
			-- Produit de d'une matrice carrée et d'une matrice non carrée à valeur positive et négative
			Initialiser(A,4,4,0.0);
			Initialiser(B,4,2,-1.5);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,4.0);
			Enregistrer(A,1,3,-15.75);
			
			Res := Produit_f(A,B);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(Res)=Nombre_Lignes(A));
			pragma Assert (Nombre_Colonnes(Res)=Nombre_Colonnes(B));
			-- Vérification des coefficients
			for i in 1..Nombre_Lignes(A) loop
				for j in 1.. Nombre_Colonnes(B) loop
					somme_ligneA_colonneB := 0.0;
					for k in 1.. Nombre_Colonnes(A) loop
						somme_ligneA_colonneB := somme_ligneA_colonneB + Obtenir_Val(A,i,k)*Obtenir_Val(B,k,j);
					end loop;
					pragma Assert(somme_ligneA_colonneB = Obtenir_Val(Res,i,j));
				end loop;
			end loop;
			
			-- Cas d'erreurs
			-- Dimensions incompatibles
			Initialiser(A,4,5,0.0);
			Initialiser(B,4,2,-1.5);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,4.0);
			Enregistrer(A,1,3,-15.75);
			
			Res := Produit_f(A,B);
			exception
				when PRODUIT_INDEFINI_EXCEPTION => Put_Line("Dimensions des matrices incompatibles");
			end;
		Put_Line("Fin Tester_Produit_f");
		
end Tester_Produit_f;

procedure Tester_Sommer is
	A : T_Matrice_Pleine;
	B : T_Matrice_Pleine;
	Res : T_Matrice_Pleine;
	begin
		begin
			-- Cas d'addition de matrices carrées à valeur positive et négative
			Initialiser(A,4,4,0.0);
			Initialiser(B,4,4,5.0);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,46.0);
			Enregistrer(A,1,3,-15.75);
			
			Sommer(A,B,Res);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(A)=Nombre_Lignes(Res));
			pragma Assert (Nombre_Colonnes(A)=Nombre_Colonnes(Res));
			-- Vérification des coefficients
			for i in 1..Nombre_Lignes(A) loop
				for j in 1..Nombre_Colonnes(A) loop
					pragma Assert (Obtenir_Val(A,i,j) + Obtenir_Val(B,i,j) = Obtenir_Val(Res,i,j));
				end loop;
			end loop;

			-- Cas d'addition de matrices non carrées à valeur positive et négative
			Initialiser(A,4,2,0.0);
			Initialiser(B,4,2,-1.5);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,4.0);
			Enregistrer(A,1,2,-15.75);
			
			Sommer(A,B,Res);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(A)=Nombre_Lignes(Res));
			pragma Assert (Nombre_Colonnes(A)=Nombre_Colonnes(Res));
			-- Vérification des coefficients
			for i in 1..Nombre_Lignes(A) loop
				for j in 1..Nombre_Colonnes(A) loop
					pragma Assert (Obtenir_Val(A,i,j) + Obtenir_Val(B,i,j) = Obtenir_Val(Res,i,j));
				end loop;
			end loop;
			
			-- Cas d'erreurs
			-- Taille de matrice incompatible
			Initialiser(A,4,5,0.0);
			Initialiser(B,4,2,-1.5);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,4.0);
			Enregistrer(A,1,3,-15.75);
			
			Sommer(A,B,Res);
			exception
				when SOMME_INDEFINIE_EXCEPTION => Put_Line("Dimensions des matrices incompatibles");
			end;
			
		Put_Line("Fin Tester_Sommer");
end Tester_Sommer;
	
procedure Tester_Sommer_f is
	A : T_Matrice_Pleine;
	B : T_Matrice_Pleine;
	Res : T_Matrice_Pleine;
	begin
		begin
			-- Cas d'addition de matrices carrées à valeur positive et négative
			Initialiser(A,4,4,0.0);
			Initialiser(B,4,4,5.0);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,46.0);
			Enregistrer(A,1,3,-15.75);
			
			Res := Sommer_f(A,B);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(A)=Nombre_Lignes(Res));
			pragma Assert (Nombre_Colonnes(A)=Nombre_Colonnes(Res));
			-- Vérification des coefficients
			for i in 1..Nombre_Lignes(A) loop
				for j in 1..Nombre_Colonnes(A) loop
					pragma Assert (Obtenir_Val(A,i,j) + Obtenir_Val(B,i,j) = Obtenir_Val(Res,i,j));
				end loop;
			end loop;

			-- Cas d'addition de matrices non carrées à valeur positive et négative
			Initialiser(A,4,2,0.0);
			Initialiser(B,4,2,-1.5);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,4.0);
			Enregistrer(A,1,2,-15.75);
			
			Res := Sommer_f(A,B);
			-- Vérification des dimensions
			pragma Assert (Nombre_Lignes(A)=Nombre_Lignes(Res));
			pragma Assert (Nombre_Colonnes(A)=Nombre_Colonnes(Res));
			-- Vérification des coefficients
			for i in 1..Nombre_Lignes(A) loop
				for j in 1..Nombre_Colonnes(A) loop
					pragma Assert (Obtenir_Val(A,i,j) + Obtenir_Val(B,i,j) = Obtenir_Val(Res,i,j));
				end loop;
			end loop;
			
			-- Cas d'erreurs
			-- Taille de matrice incompatible
			Initialiser(A,4,5,0.0);
			Initialiser(B,4,2,-1.5);
			Enregistrer(A,2,2,50.0);
			Enregistrer(A,3,1,4.0);
			Enregistrer(A,1,3,-15.75);
			
			Res := Sommer_f(A,B);
			exception
				when SOMME_INDEFINIE_EXCEPTION => Put_Line("Dimensions des matrices incompatibles");
			end;
			Put_Line("Fin Tester_Sommer_f");
		
end Tester_Sommer_f;

procedure Tester_Produit_Const is
	Mat :  T_Matrice_Pleine;
	Copie : T_Matrice_Pleine;
	begin
		-- Cas d'une matrice carrée
		Initialiser(Mat,5,5,1.0);
		Copier(Mat,Copie); -- Copie de la matrice de départ
		Produit_Const(5.0,Mat);
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (5.0*Obtenir_Val(Copie,i,j) = Obtenir_Val(Mat,i,j));
			end loop;
		end loop;
		
		-- Cas d'une matrice non carrée
		Initialiser(Mat,2,6,1.0);
		Copier(Mat,Copie); -- Copie de la matrice de départ
		Produit_Const(-3.84653,Mat);
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (-3.84653*Obtenir_Val(Copie,i,j) = Obtenir_Val(Mat,i,j));
			end loop;
		end loop;
		
		-- Cas d'une matrice colonne
		Initialiser(Mat,7,1,-1.0);
		Copier(Mat,Copie);
		Produit_Const(-3.84653,Mat); -- Copie de la matrice de départ
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (-3.84653*Obtenir_Val(Copie,i,j) = Obtenir_Val(Mat,i,j));
			end loop;
		end loop;
		
		Put_Line("Fin Tester_Produit_Const");
		
end Tester_Produit_Const;
			
procedure Tester_Sommer_Const is
	Mat :  T_Matrice_Pleine;
	Copie :  T_Matrice_Pleine;
	begin
		-- Cas d'une matrice carrée
		Initialiser(Mat,5,5,1.0);
		Copier(Mat,Copie); -- Copie de la matrice de départ
		Sommer_Const(5.0,Mat);
		-- Vérification des coefficients
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (5.0+Obtenir_Val(Copie,i,j) = Obtenir_Val(Mat,i,j));
			end loop;
		end loop;
		
		-- Cas d'une non matrice carrée
		Initialiser(Mat,2,6,1.45);
		Copier(Mat,Copie); -- Copie de la matrice de départ
		Sommer_Const(-3.84653,Mat);
		-- Vérification des coefficient
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (-3.84653+Obtenir_Val(Copie,i,j) = Obtenir_Val(Mat,i,j));
			end loop;
		end loop;
		
		-- Cas d'une matrice colonne
		Initialiser(Mat,7,1,-1.0);
		Copier(Mat,Copie); -- Copie de la matrice de départ
		Sommer_Const(-3.84653,Mat);
		-- Vérification des coefficient
		for i in 1..Nombre_Lignes(Mat) loop
			for j in 1..Nombre_Colonnes(Mat) loop
				pragma Assert (-3.84653+Obtenir_Val(Copie,i,j) = Obtenir_Val(Mat,i,j));
			end loop;
		end loop;
		
		Put_Line("Fin Tester_Sommer_Const");
		
end Tester_Sommer_Const;

procedure Tester_Ligne_Vide is
	Mat : T_Matrice_Pleine;
	begin
		-- Cas d'une matrice carré avec deux lignes non nulles
		Initialiser(Mat,4,4,0.0);
		Enregistrer(Mat,1,2,1.897);
		Enregistrer(Mat,3,2,-1.897);
		for i in 1..Nombre_Lignes(Mat) loop
		if i=2 or else i=4 then
			pragma Assert(Ligne_Vide (i,Mat));
		else
			pragma Assert(not Ligne_Vide (i,Mat));
		end if;
	end loop;

	Put_Line("Fin Tester_Ligne_Vide");

end Tester_Ligne_Vide;

procedure Tester_Afficher is
	Mat : T_Matrice_Pleine;
	begin
		-- Affichage d'une matrice carrée avec des coefficients positifs
		Initialiser(Mat,4,5,2.75);
		Put_Line("Matrice 4x5 avec comme coefficients 2.75");
		Afficher_Mat(Mat);
		New_Line;
		
		-- Affichage d'une matrice carrée avec des coefficients nuls ou négatifs
		Initialiser(Mat,3,3,0.0);
		Enregistrer(Mat,1,2,-1.0);
		Put_Line("Matrice 3x3 avec comme coefficients 0.0 et -1.0 en 1,2");
		Afficher_Mat(Mat);
		New_Line;
		
		-- Affichage d'une matrice colonne avec des coefficients négatifs et positifs
		Initialiser(Mat,7,1,-5.0);
		Enregistrer(Mat,2,1,88.0);
		Put_Line("Matrice 7x1 avec comme coefficients -5.0 et 88.0 en 2,1");
		Afficher_Mat(Mat);
		New_Line;
		
		-- Affichage d'une matrice ligne avec des coefficients positifs
		Initialiser(Mat,1,5,-2.75);
		Enregistrer(Mat,1,1,88.0);
		Enregistrer(Mat,1,2,-8.0);
		Enregistrer(Mat,1,4,-37.6544);
		Put_Line("Matrice 1x5 avec comme coefficients 88.0 en 1,1, -8.0 en 1,2 et -37.6544) en 1,4");
		Afficher_Mat(Mat);
		New_Line;
		
		Put_Line("Fin Tester_Afficher");
		
end Tester_Afficher;

	begin
	
	Tester_Initialiser;
	New_Line;
	Tester_Enregistrer;
	New_Line;
	Tester_Obtenir_Valeur;
	New_Line;
	Tester_Copier;
	New_Line;
	Tester_Transposer;
	New_Line;
	Tester_Produit;
	New_Line;
	Tester_Produit_f;
	New_Line;
	Tester_Sommer;
	New_Line;
	Tester_Sommer_f;
	New_Line;
	Tester_Produit_Const;
	New_Line;
	Tester_Sommer_Const;
	New_Line;
	Tester_Ligne_Vide;
	New_Line;
	Tester_Afficher;
	New_Line;

	Put_Line("Fin des tests");
		
end tesT_Matrice_Pleine;
