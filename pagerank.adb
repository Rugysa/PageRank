with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with p_matrice_pleine;
with p_matrice_creuse;

--R0:Renvoyer le PageRank dans un fichier .pr et le poids de chaque noeud du graphe dans un
--fichier.prw à partir d’un graphe orienté dans un fichier .net

procedure Pagerank is

    procedure erreur (condition : Integer; i : Integer) is
    begin
        if i /= 0 then
            Put_Line ("Erreur dans l’entrée des arguments!");
            if condition = 1 then
                Put (Argument (i) & " n'est pas un argument");
                New_Line;
                Put ("Les arguments possibles sont:");
                New_Line;
                Put ("- -A <valeur de alpha> avec <valeur de alpha> dans [0,1]");
                New_Line;
                Put ("- -K <valeur> avec <valeur> l'indice k du vecteur poids, un entier positif");
                New_Line;
                Put ("- -E <valeur> avec <valeur> l'indice se précision, un réel positif");
                New_Line;
                Put ("- -R  <prefixe> avec <prefixe> le prefixe du fichier d'entrée");
                New_Line;
                Put ("- -P pour calculer avec matrice pleine");
                New_Line;
                Put ("- -C pour calculer avec matrice creuse");
                New_Line;
            elsif condition = 2 then
                Put (Argument (i + 1) & " n'est pas du bon format.");
                New_Line;
                Put ("Le bon format est :");
                New_Line;
                Put ("-K <valeur> avec <valeur> l'indice k du vecteur poids, un entier positif");
                New_Line;
            elsif condition = 3 then
                Put (Argument (i + 1) & " n'est pas du bon format.");
                New_Line;
                Put ("Le bon format est :");
                New_Line;
                Put ("-A <valeur de alpha> avec <valeur de alpha> dans [0,1]");
                New_Line;
            elsif condition = 4 then
                Put (Argument (i + 1) & " n'est pas du bon format.");
                New_Line;
                Put ("Le bon format est :");
                New_Line;
                Put ("-E <valeur> avec <valeur> l'indice se précision, un réel positif");
                New_Line;
            elsif condition = 5 then
                Put (Argument (i + 1) & " n'est pas du bon format.");
                New_Line;
                Put ("Le bon format est :");
                New_Line;
                Put ("-R  <prefixe> avec <prefixe> le prefixe du fichier d'entrée");
                New_Line;
            else
                null;
            end if;
        else
            if condition = 1 then
                New_Line;
                Put_Line ("Fichier invalide l'un des sommets est supérieur ou égale au nombre de sommet il n'appartient donc pas au graphe!!");
            elsif condition = 2 then
                New_Line;
                Put_Line ("Mauvais format du fichier Le fichier doit ếtre du format: ");
                New_Line;
                Put ("Entier(nombre de sommet)");
                New_Line;
                Put ("Entier(sommet de départ)     Entier(sommet d'arrivée)");
                New_Line;
                Put ("    .      ");
                New_Line;
                Put ("    .      ");
                New_Line;
                Put ("    .      ");
                New_Line;
                Put ("Entier(sommet de départ)     Entier(sommet d'arrivée)");
            elsif condition=3 then
                New_Line;
                Put_Line("Le fichier est beaucoup trop lourd essayer de lancer le programme avec matrice creuse ou d'augmenter la taille de la stack avec ulimit -s valeur");
            elsif condition=4 then
                New_Line;
                Put_Line("Aucun fichier de ce nom 'a été trouvé dans ce dossier entrer un autre nom de fichier ou vérifier que le fichier est bien dans le dossier");
            elsif condition=5 then
                New_Line;
                Put_Line ("Pas de fichier.");
                Put_Line ("Usage : " & Command_Name & " <fichier>");
            else 
                null;
            end if;           
        end if;
    end erreur;

    Sommet_Error : exception; --Erreur sur les valeurs de sommet du fichier à lire
    No_Argument_Error : exception; --exception pour manque d'argument
    F_sujet : Ada.Text_IO.File_Type; --Format du fichier
    N       : Integer; --Nombre de sommet du graphe
    N2      : Integer; -- nombre de vecteur du graphe
    Entier  : Integer; --dernier entier lu par le fichier
    i       : Integer; --condition de boucles while
    compt   : Integer; -- compteur du programme
    K       : Integer; --indice k du vecteur poids
    alpha   : Float; --valeur de alpha
    epsilon : Float; --conditions de précision
    prefixe : Unbounded_String; --nom du fichier de sorti
    choix   : Boolean;  -- matrice creuse si Vrai sinon matrice pleine

begin
    --Initialiser les variables
    epsilon := 0.0;
    prefixe := To_Unbounded_String ("output");
    alpha   := 0.85;
    K       := 150;
    choix   := True;

    --Traiter les arguments
    begin
        i := 1;
        if Argument_Count > 1 then
            while Argument_Count > i loop

                --Etudier la valeur de l'argument
                if Argument (i) = "-K" then
                    compt := 2;
                    K     := Integer'Value (Argument (i + 1));
                    if K < 0 then
                        raise Constraint_Error;
                    end if;
                    i := i + 2;
                elsif Argument (i) = "-P" then
                    choix := False;
                    i     := i + 1;
                elsif Argument (i) = "-A" then
                    compt := 3;
                    alpha := Float'Value (Argument (i + 1));
                    if 0.0 > alpha or alpha > 1.0 then
                        raise Constraint_Error;
                    end if;
                    i := i + 2;
                elsif Argument (i) = "-C" then
                    choix := True;
                    i     := i + 1;
                elsif Argument (i) = "-E" then
                    compt   := 4;
                    epsilon := Float'Value (Argument (i + 1));
                    if epsilon < 0.0 then
                        raise Constraint_Error;
                    end if;
                    i := i + 2;
                elsif Argument (i) = "-R" then
                    compt   := 5;
                    prefixe := To_Unbounded_String (Argument (i + 1));
                    i       := i + 2;
                else
                    compt := 1;
                    raise Constraint_Error;
                end if;
            end loop;
            if i>Argument_Count then
                raise No_Argument_Error;
            end if;
        elsif Argument_Count < 1 then
            raise No_Argument_Error;
        end if;

    end;

    --lire sujet.net
    begin
        Open (F_sujet, In_File, Argument (i));--ouvrir le fichier sujet.net
        Get (F_sujet, N);--récuperer le nombre de sommet

        --compter le nombre de vecteur dans le fichier
        compt := 0;
        while not End_Of_File (F_sujet) loop
            Get (F_sujet, Entier);
            compt := compt + 1;
            if Entier >= N then
                raise Sommet_Error;
            end if;
        end loop;
    exception
        when End_Error =>
            null;
    end;
    Close (F_sujet);
    begin
        N2 := compt / 2;
        if (2 * N2) /= compt then
            raise Data_Error;
        end if;
    end;
    
    --Choisir le programme à éxécuter
    if choix then
        Put ("Matrice creuse");
        p_matrice_creuse.matricecreuse(K,epsilon,alpha,prefixe,N,N2,Argument(i));
    else
        Put ("Matrice pleine");
        p_matrice_pleine.matricepleine
           (K, epsilon, alpha, prefixe, N, N2, Argument (i));
    end if;

exception
    when Sommet_Error      =>
        erreur (1, 0);
    when Data_Error        =>
        erreur (2, 0);
    when Constraint_Error  =>
        erreur (compt, i);
    when No_Argument_Error =>
        erreur(5,0);
    when STORAGE_ERROR =>
        erreur(3,0);
    when NAME_ERROR =>
        erreur(4,0);
end Pagerank;
