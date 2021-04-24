Module Map_Data

#Region "Chargement Map"

    Sub GiMapData(index As Integer, data As String)

        With Comptes(index)

            Try

                'GDM | 534    | 0706131721 | 755220465939692F276671264132675c756345246c4b463b43427a3a4d38556e3c722a356362224e343d3423333e722c3f3a7a4e23553555672c733d602062454e3d474b20633c6335763e63682c43554937222f79333f253235346863387a287039474d4070302532357d586327675668752a3b6a24622962426e78787373512c5853515626536239367320643c53 
                'GDM | ID Map | Indice     | Clef

                .Map = New CMap
                .MaisonMap.Clear()

                Dim separateData As String() = Split(data, "|")

                ' Je donne l'ID de la map.
                .Map.ID = separateData(1)

                LoadMapInGame(index, separateData(1), separateData(2), separateData(3))

                If .MITM = False Then

                    .Send("GI")

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMapData", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Private Sub LoadMapInGame(ByVal index As Integer, ByVal idMap As String, ByVal indice As String, ByVal clef As String)

        With Comptes(index)

            Try

                .Map.Bas = Nothing
                .Map.Droite = Nothing
                .Map.Gauche = Nothing
                .Map.Haut = Nothing

                'Si le dossier map n'existe pas, alors je le créer
                If Not IO.Directory.Exists("Maps") Then IO.Directory.CreateDirectory("Maps")

                'Si le fichier de la map n'existe pas alors je le créer et ajoute les infos dedans.
                If Not IO.File.Exists("Maps/" & idMap & "_" & indice & "X.txt") Then
                    Dim Unpacker As New SwfUnpacker
                    Unpacker.SwfUnpack(idMap & "_" & indice & "X.swf")
                End If

                'Je lis le fichier voulu. 
                Dim mapReader As New IO.StreamReader("Maps/" & idMap & "_" & indice & "X.txt")
                Dim mapData As String() = Split(mapReader.ReadLine, "|")
                mapReader.Close()

                .Map.Largeur = mapData(2)
                .Map.Hauteur = mapData(3)

                'Je prépare le nécessaire pour décrypt la map et connaitre se qu'il se trouve dessus.
                Dim preparedKey As String = prepareKey(clef)
                .Map.Handler = uncompressMap(decypherData(mapData(1), preparedKey, Convert.ToInt64(checksum(preparedKey), 16) * 2))
                .Map.Coordonnees = VarMap(idMap)

                Dim count As Integer = .Map.Handler.Count - 1
                Dim num As Integer = 0

                'J'obtient les cellules qui me permet de changer de map via les soleils.
                For i As Integer = 1 To .Map.Handler.Length - 1
                    If (.Map.Handler(i).movement > 0) Then
                        If (.Map.Handler(i).layerObject1Num = 1030) OrElse (.Map.Handler(i).layerObject2Num = 1030) OrElse (.Map.Handler(i).layerObject2Num = 4088) OrElse (.Map.Handler(i).layerObject1Num = 4088) Then
                            Dim x As Integer = getX(i, .Map.Largeur)
                            Dim y As Integer = getY(i, .Map.Largeur)
                            If If(x - 1 = y, True, x - 2 = y) Then
                                If .Map.Gauche = Nothing Then
                                    .Map.Gauche = i 'Gauche
                                End If
                            ElseIf x - (.Map.Largeur + .Map.Hauteur) + 5 = y Then 'ElseIf If(x - (.MapLargeur + .MapHauteur) + 5 = y, True, x - (.MapLargeur + .MapHauteur) + 5 = y - 1) Then
                                'If .DirectionDroite = Nothing Then
                                .Map.Droite = i 'Droite
                                'End If
                            ElseIf If(y + x = (.Map.Largeur + .Map.Hauteur) - 1, True, y + x = (.Map.Largeur + .Map.Hauteur) - 2) OrElse (y + x = (.Map.Largeur + .Map.Hauteur)) Then
                                If .Map.Bas = Nothing Then
                                    .Map.Bas = i 'Bas
                                End If
                            ElseIf y <= 0 Then
                                y = Math.Abs(y)
                                If x - y < 3 Then
                                    If .Map.Haut = Nothing Then
                                        .Map.Haut = i 'Haut
                                    End If
                                End If
                            End If
                        End If
                    End If
                Next

                ChargementDivers(index, .Map.Handler)
                Map_Viewer_Chargement(index)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "LoadMapInGame", ex.Message)

            End Try

        End With

    End Sub

    Private Sub ChargementDivers(ByVal Index As Integer, ByVal spritesHandler() As Cell)

        With Comptes(Index)

            Try

                .Map.Interaction.Clear()

                ' id sprite | nom action | nom item , id action

                For i As Integer = 1 To 1000

                    If VarInteraction.ContainsKey(spritesHandler(i).layerObject2Num) Then

                        Dim newInteraction As New CInteraction

                        With newInteraction

                            .Sprite = spritesHandler(i).layerObject2Num.ToString

                            .Cellule = i.ToString

                            .Nom = VarInteraction(spritesHandler(i).layerObject2Num).Name.ToLower

                            .Information = "disponible"

                            .Action = VarInteraction(spritesHandler(i).layerObject2Num).DicoInteraction

                        End With

                        .Map.Interaction.Add(i.ToString, newInteraction)

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(Index, .Personnage.NomDuPersonnage, "ChargementDivers", ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "Information"

    Sub GiMapAjouteEntite(index As Integer, data As String)

        With Comptes(index)

            Try

                Dim separateData As String() = Split(data, "|+")

                For i = 1 To separateData.Count - 1

                    Dim separate As String() = Split(separateData(i), ";")

                    Dim newMap As New CEntite
                    Dim newCombat As New CCombatEntite

                    With newMap

                        .Cellule = separate(0)
                        .IDUnique = separate(3)
                        .Orientation = separate(1)
                        .IDCategorie = separate(5)

                        Select Case separate(5)

                            Case -1 ' Mobs (en combat)

                                ' GM|+ 369     ; 1           ; 0 ; -1        ; 149     ; -2      ; 1571^95 ; 2          ; -1 ; -1 ; -1 ; 0 , 0 , 0 , 0 ; 18       ; 5  ; 3  ; 1 
                                ' GM|+ Cellule ; Orientation ; ? ; id Unique ; Id Mobs ; indice  ; ?       ; Level mobs ; ?  ; ?  ; ?  ; ? , ? , ? , ? ; Vitalité ; PA ; PM ; ? 

                                .Nom = VarMobs(separate(4))(CInt(separate(7) - 1)).Nom
                                .Niveau = VarMobs(separate(4))(CInt(separate(7) - 1)).Niveau
                                .ID = separate(4)

                                With newCombat

                                    .Vitalite = separate(12)
                                    .PA = separate(13)
                                    .PM = separate(14)
                                    .ResistanceNeutre = VarMobs(separate(4))(CInt(separate(7) - 1)).RésistanceNeutre
                                    .ResistanceTerre = VarMobs(separate(4))(CInt(separate(7) - 1)).RésistanceTerre
                                    .ResistanceFeu = VarMobs(separate(4))(CInt(separate(7) - 1)).RésistanceFeu
                                    .ResistanceEau = VarMobs(separate(4))(CInt(separate(7) - 1)).RésistanceEau
                                    .ResistanceAir = VarMobs(separate(4))(CInt(separate(7) - 1)).RésistanceAir
                                    .EsquivePA = VarMobs(separate(4))(CInt(separate(7) - 1)).EsquivePA
                                    .EsquivePM = VarMobs(separate(4))(CInt(separate(7) - 1)).EsquivePM

                                End With

                            Case -2 ' Mobs en combat

                                ' GM|+ 369     ; 1           ; 0 ; -1        ; 149     ; -2      ; 1571^95 ; 2          ; -1 ; -1 ; -1 ; 0 , 0 , 0 , 0 ; 18       ; 5  ; 3  ; 1 
                                ' GM|+ Cellule ; Orientation ; ? ; id Unique ; Id Mobs ; indice  ; ?       ; Level mobs ; ?  ; ?  ; ?  ; ? , ? , ? , ? ; Vitalité ; PA ; PM ; ? 

                                .Nom = VarMobs(separate(4))(CInt(separate(7) - 1)).Nom
                                .Niveau = VarMobs(separate(4))(CInt(separate(7) - 1)).Niveau
                                .ID = separate(4)

                                With newCombat

                                    .Vitalite = separate(12)
                                    .PA = separate(13)
                                    .PM = separate(14)
                                    .ResistanceNeutre = VarMobs(separate(4))(CInt(separate(7) - 1)).RésistanceNeutre
                                    .ResistanceTerre = VarMobs(separate(4))(CInt(separate(7) - 1)).RésistanceTerre
                                    .ResistanceFeu = VarMobs(separate(4))(CInt(separate(7) - 1)).RésistanceFeu
                                    .ResistanceEau = VarMobs(separate(4))(CInt(separate(7) - 1)).RésistanceEau
                                    .ResistanceAir = VarMobs(separate(4))(CInt(separate(7) - 1)).RésistanceAir
                                    .EsquivePA = VarMobs(separate(4))(CInt(separate(7) - 1)).EsquivePA
                                    .EsquivePM = VarMobs(separate(4))(CInt(separate(7) - 1)).EsquivePM

                                End With

                            Case -3 ' Mobs (Hors combat)

                                ' GM|+ 439     ; 5           ; 21      ; -2     ; 198     , 241     ; -3     ;1135^110,1138^100 ; 36 , 32 ; -1       , -1       , -1       ;0,0,0,0;-1,-1,-1;0,0,0,0; 
                                ' GM|+ Cellule ; Orientation ; Etoile% ; ID Map ; ID Mobs , Id Mobs ; Entité ;                  ; Lv , Lv ; Couleur1 , Couleur2 , Couleur3 ;?,?,?,?;Couleur1,etc... 

                                .Nom = NomMobs(separate(4))
                                .ID = separate(4)
                                .Niveau = separate(7)
                                .Etoile = separate(2)

                            Case -4 ' Pnj-------------------

                                ' GM|+ 152     ; 3           ; 0        ;-1      ; 100    ; -4     ; 9048^100 ; 0  ; -1 ; -1 ; e7b317 ;   ,   ,   ,   ,   ;   ; 0 |
                                ' GM|+ Cellule ; Orientation ; Etoiles% ; ID Map ; ID PNJ ; Entité ; ?        ; Lv ; ?  ; ?  ; ?      ; ? , ? , ? , ? , ? ; ? ; ? | Next PNJ

                                .Nom = VarPnj(separate(4))
                                .Niveau = separate(7)
                                .ID = separate(4)
                                .Etoile = separate(2)
                                .IDUnique = separate(3)
                                .Classe = "Pnj"

                            Case -5 ' Mode marchand

                                ' GM|+ 412     ; 3           ; 0       ; -82    ; Blackarne ; -5     ; 60^100        ; 0  ; 0 ; -1 ; 22e4 , 27c7   , 22ac , 1cf7     , 27c6     ; Awesomes   ; c , 77f73 , 1m , 5w3r4 ; 0
                                ' GM|+ Cellule ; Orientation ; Etoiles ; ID Map ; Nom       ; Entité ; Classe + sexe ; Lv ; ? ; ?  ; Cac  , Coiffe , Cape , Familier , Bouclier ; Nom guilde ; ? , ?     , ?  , ?     ; ID Sprite Sac du mode marchand

                                'ID Sprite Sac (se que le marchand vend) 
                                ' 0 = Tout
                                ' 1 = Equipement
                                ' 2 = Divers
                                ' 3 = Ressource

                                .Nom = separate(4)
                                .Classe = ClasseJoueur(separate(6))
                                .Sexe = SexeJoueur(separate(6))
                                .Guilde = separate(11)
                                .ModeMarchand = True
                                .ID = 99999


                                Dim separateEquipement As String() = Split(separate(10), ",")
                                If separateEquipement(1).Contains("~") Then

                                    Dim separateObvijevan As String() = Split(separateEquipement(1), "~")

                                    '  .Information &= "Coiffe : " & DicoItems(Convert.ToInt64(separateObvijevan(0), 16)).NameItem & vbCrLf
                                    Dim coiffeLv As String = Convert.ToInt64(separateObvijevan(1), 16)
                                    Dim coiffeForme As String = Convert.ToInt64(separateObvijevan(2), 16)

                                ElseIf separateEquipement(1) <> Nothing Then

                                    '.Information &= "Coiffe : " & DicoItems(Convert.ToInt64(separateEquipement(1), 16)).NameItem & vbCrLf

                                End If ' Coiffe
                                If separateEquipement(2).Contains("~") Then

                                    Dim separateObvijevan As String() = Split(separateEquipement(2), "~")

                                    ' .Information &= "Cape : " & DicoItems(Convert.ToInt64(separateObvijevan(0), 16)).NameItem & vbCrLf
                                    Dim capeLv As String = Convert.ToInt64(separateObvijevan(1), 16)
                                    Dim capeForm As String = Convert.ToInt64(separateObvijevan(2), 16)

                                ElseIf separateEquipement(2) <> Nothing Then

                                    '  .Information &= "Cape : " & DicoItems(Convert.ToInt64(separateEquipement(2), 16)).NameItem & vbCrLf

                                End If ' Cape
                                If separateEquipement(0) <> Nothing Then
                                    ' .Information &= "Cac : " & DicoItems(Convert.ToInt64(separateEquipement(0), 16)).NameItem & vbCrLf
                                End If ' Cac
                                If separateEquipement(3) <> Nothing Then
                                    ' .Information &= "Familier : " & DicoItems(Convert.ToInt64(separateEquipement(3), 16)).NameItem & vbCrLf
                                End If ' Familier
                                If separateEquipement(4) <> Nothing Then
                                    ' .Information &= "Bouclier : " & DicoItems(Convert.ToInt64(separateEquipement(4), 16)).NameItem & vbCrLf
                                End If ' Bouclier

                            Case -6 ' Percepteur

                                ' GM|+ 383     ; 1 ; 0      ; -14    ; 2l , 3d ; -6     ; 6000^110 ; 66 ; The Chosen Few ; 8 , n2bh , 1 , 9zldr
                                ' GM|+ Cellule ; ? ; Etoile ; ID Map ; Nom     ; Entité ; Sprite   ; Lv ; Nom Guilde     ; ? , ?    , ? , ?
                                .Nom = separate(4)
                                .IDUnique = separate(3)
                                .Classe = separate(5)
                                .Niveau = separate(7)
                                .Etoile = separate(2)
                                .Guilde = separate(8)

                            Case -9 ' Dragodinde

                        '252;1;-1;-9;M;-9;7002^100;Yoshimi;7;43;100

                        '   .Name = separate(4)
                     '   .Information = "Monture de : " & separate(7) & vbCrLf &
                     '           "Niveau : " & separate(8) & vbCrLf &
                     '           "Dragodinde " & DicoDragodindeId(separate(9))

                            Case -10 ' Prisme

                        ' GM|+ 256     ; 1           ; 0      ; -4     ; 1111 ; -10    ; 8101^90 ; 2 ; 4 ; 1
                        ' GM|+ Cellule ; Orientation ; Etoile ; ID Map ; Nom  ; Entité ; Sprite  ; ? ; ? ; ?

                        ' .Name = If(separate(4) = 1111, "Prisme Bontârien", "Prise Brâkmarien") ' Nom
                       ' .Information = "Niveau : " & separate(7) & vbCrLf &
                       '         "Etoile : " & separate(2)

                            Case > 0 ' Joueur

                                ' Hors Combat
                                ' GM|+ 156     ; 7           ; 0 ; 0123456   ; Linaculer ; 9       ; 90^100      ; 0                          ; 0          , 0 , 0 , 1234567           ; -1       ; -1       ; -1       ;     , 2412~16~7                  , 2411~17~15               ,          ,          ; 0   ;   ;   ;           ;                 ; 0 ;    ;   | Next tchatJoueur
                                ' GM|~ 300     ; 1           ; 0 ; 0123456   ; linaculer ; 9       ; 90^100      ; 0                          ; 0          , 0 , 0 , 1234567           ; 0        ; 1eeb13   ; 0        ; b4  , 2412~16~18                 , 2411~17~19               ,          ,          ; 1   ;   ;   ; Chernobil ; f,9zldr,x,6k26u ; 0 ; 88 ;
                                ' GM|+ Cellule ; Orientation ; ? ; Id Unique ; Nom       ; ID Race ; Classe+sexe ; Combat (Equipe bleu/rouge) ; Alignement , ? , ? , ID Unique + Level ; Couleur1 ; Couleur2 ; Couleur3 ; Cac , Coiffe (ID Objet~Lv~Forme) , Cape (ID Objet~Lv~Forme) , Familier , Bouclier ; ?   ; ? ; ? ; Guilde    ; ?               ; ? ; ?  ; ?  
                                ' En combat 
                                ' GM|+ 105     ; 1           ; 0 ; 0123456   ; Linaculer ; 9       ; 90^100      ; 0                          ; 99 ; 0          , 0 , 0 , 1234567           ; -1       ; -1       ; -1       ; 241 , 1bea                       , 6ab                      ,          ,          ; 672      ; 7  ; 3  ; 0           ; 1          ; 0        ; 2         ; 0        ; 77         ; 77         ; 0 ;   ;                         
                                ' GM|+ Cellule ; Orientation ; ? ; Id Unique ; Nom       ; ID Race ; Classe+sexe ; Combat (Equipe bleu/rouge) ; Lv ; Alignement , ? , ? , ID Unique + Level ; Couleur1 ; Couleur2 ; Couleur3 ; Cac , Coiffe (ID Objet~Lv~Forme) , Cape (ID Objet~Lv~Forme) , Familier , Bouclier ; Vitalité ; PA ; PM ; %Rés neutre ; %Rés Terre ; %Rés feu ; %Rés Eau  ; %Res air ; Esquive PA ; Esquive PM ; ? ; ? ; ? 
                                '~ = Sur une dragodinde

                                Dim calculLevel As String()
                                Dim separateEquipement As String()

                                If Comptes(index).Combat.EnCombat Then

                                    separateEquipement = Split(separate(13), ",")
                                    calculLevel = Split(separate(9), ",")

                                Else

                                    separateEquipement = Split(separate(12), ",")
                                    calculLevel = Split(separate(8), ",")

                                End If

                                .Nom = separate(4)
                                .ModeMarchand = False

                                .Sexe = SexeJoueur(separate(6))
                                .Classe = ClasseJoueur(separate(6))

                                If Comptes(index).Combat.EnCombat Then

                                    .Alignement = AlignementJoueur(calculLevel(0))
                                    .Niveau = separate(8)

                                    With newCombat

                                        .Vitalite = separate(14)
                                        .PA = separate(15)
                                        .PM = separate(16)
                                        .ResistanceNeutre = separate(17)
                                        .ResistanceTerre = separate(18)
                                        .ResistanceFeu = separate(19)
                                        .ResistanceEau = separate(20)
                                        .ResistanceAir = separate(21)
                                        .EsquivePA = separate(22)
                                        .EsquivePM = separate(23)
                                        .Equipe = separate(7)

                                    End With

                                Else

                                    .Alignement = AlignementJoueur(calculLevel(0))
                                    .Guilde = separate(16)
                                    .Niveau = CInt(calculLevel(3)) - CInt(separate(3))

                                End If

                                If separateEquipement(1).Contains("~") Then

                                    Dim separateObvijevan As String() = Split(separateEquipement(1), "~")

                                    '   .Information &= "Coiffe : " & DicoItems(Convert.ToInt64(separateObvijevan(0), 16)).NameItem & vbCrLf
                                    Dim coiffeLv As String = Convert.ToInt64(separateObvijevan(1), 16)
                                    Dim coiffeForme As String = Convert.ToInt64(separateObvijevan(2), 16)

                                ElseIf separateEquipement(1) <> Nothing Then

                                    '  .Information &= "Coiffe : " & DicoItems(Convert.ToInt64(separateEquipement(1), 16)).NameItem & vbCrLf

                                End If ' Coiffe
                                If separateEquipement(2).Contains("~") Then

                                    Dim separateObvijevan As String() = Split(separateEquipement(2), "~")

                                    '  .Information &= "Cape : " & DicoItems(Convert.ToInt64(separateObvijevan(0), 16)).NameItem & vbCrLf
                                    Dim capeLv As String = Convert.ToInt64(separateObvijevan(1), 16)
                                    Dim capeForm As String = Convert.ToInt64(separateObvijevan(2), 16)

                                ElseIf separateEquipement(2) <> Nothing Then

                                    '   .Information &= "Cape : " & DicoItems(Convert.ToInt64(separateEquipement(2), 16)).NameItem & vbCrLf

                                End If ' Cape
                                If separateEquipement(0) <> Nothing Then
                                    '    .Information &= "Cac : " & DicoItems(Convert.ToInt64(separateEquipement(0), 16)).NameItem & vbCrLf
                                End If ' Cac
                                If separateEquipement(3) <> Nothing Then
                                    '    .Information &= "Familier : " & DicoItems(Convert.ToInt64(separateEquipement(3), 16)).NameItem & vbCrLf
                                End If ' Familier
                                If separateEquipement(4) <> Nothing Then
                                    '    .Information &= "Bouclier : " & DicoItems(Convert.ToInt64(separateEquipement(4), 16)).NameItem & vbCrLf
                                End If ' Bouclier

                        End Select

                    End With

                    If .Map.Entite.ContainsKey(separate(3)) Then

                        .Map.Entite(separate(3)) = newMap

                    Else

                        .Map.Entite.Add(separate(3), newMap)

                    End If

                    If .Combat.EnCombat Then

                        If .Combat.Entite.ContainsKey(separate(3)) Then

                            .Combat.Entite(separate(3)) = newCombat

                        Else

                            .Combat.Entite.Add(separate(3), newCombat)

                        End If

                    End If

                    If separate(3) = .Personnage.ID Then

                        .Map.StopDeplacement = False
                        .Map.EnDeplacement = False
                        .Map.Bloque.Set()
                        ._Send = ""
                        .BloqueGuilde.Set()

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMapAjouteEntite", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiMapSupprimeEntite(index As Integer, data As String)

        With Comptes(index)

            Try

                ' GM|- 1234567
                ' GM|- Id Unique

                Dim idUnique As String = Mid(data, 5)

                If .Map.Entite.ContainsKey(idUnique) Then

                    .Map.Entite.Remove(idUnique)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMapSupprimeEntite", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiMapDeplacementEntite(index As Integer, data As String)

        With Comptes(index)

            Try

                ' GA  ; 1 ; -1            ; adxfcB
                ' GA0 ; 1 ; -1            ; adxfcB
                ' GA  ; ? ; ID entité Map ; Path

                Dim separateData As String() = Split(data, ";")

                Dim cellule As Integer = ReturnLastCell(Mid(separateData(3), separateData(3).Length - 1, 2))

                If .Map.Entite.ContainsKey(separateData(2)) Then

                    .Map.Entite(separateData(2)).Cellule = cellule

                End If

                If separateData(2) = .Personnage.ID.ToString Then

                    .Map.Bloque.Reset()

                    .Map.EnDeplacement = True

                    Task.Run(Sub() PauseDeplacement(index, cellule))

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMapDeplacementEntite", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Friend Async Sub PauseDeplacement(index As Integer, cellule As Integer)

        With Comptes(index)

            Try

                If ._Send <> "" Then

                    .Send(._Send)

                    ._Send = ""

                End If

                If .MITM = False Then

                    Dim changeur As Boolean = False

                    If (.Map.Handler(cellule).layerObject1Num = 1030) OrElse (.Map.Handler(cellule).layerObject2Num = 1030) OrElse
                       (.Map.Handler(cellule).layerObject1Num = 4088) OrElse (.Map.Handler(cellule).layerObject2Num = 4088) Then

                        changeur = True

                    End If

                    If .Map.PathTotal.Length < 9 Then

                        Await Task.Delay(180 * 3)

                    Else

                        Await Task.Delay(80 * 3)

                    End If

                    .Send("GKK0",
                         {"BN",
                          "GA1;501;" & .Personnage.ID.ToString,
                          "GDM",
                          "GA;905;"})

                    If changeur = False Then

                        .Map.EnDeplacement = False

                    End If

                    .Map.Bloque.Set()

                Else

                    .Map.Bloque.WaitOne(30000)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "PauseDeplacement", ex.Message)

            End Try

        End With

    End Sub

    Sub GiMapDeplacementEntiteEscalieTeleportation(index As Integer, data As String)

        With Comptes(index)

            Try

                'GA1 ; 4             ; 01234567  ; 76543210 , 342
                'GA1 ; Teleportation ; ID Joueur ; ID_Cible , Cellule

                Dim separateData As String() = Split(data, ";")

                Dim separate As String() = Split(separateData(3), ",")

                If .Map.Entite.ContainsKey(separate(0)) Then

                    .Map.Entite(separate(0)).Cellule = separate(1)

                End If

                If separate(0) = .Personnage.ID Then

                    If .MITM = False Then .Send("GKK1")
                    .Map.EnDeplacement = False
                    .Map.Bloque.Set()

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMapDeplacementEntiteEscalieTeleportation", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub 'Sufokia > escalié

    Sub GiMapDeplacementEntiteTeleportation(index As Integer, data As String)

        With Comptes(index)

            Try

                'GA ; 4             ; 01234567  ; 76543210 , 342
                'GA ; Teleportation ; ID Joueur ; ID_Cible , Cellule

                Dim separateData As String() = Split(data, ";")

                Dim separate As String() = Split(separateData(3), ",")

                If .Map.Entite.ContainsKey(separate(0)) Then

                    .Map.Entite(separate(0)).Cellule = separate(1)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMapDeplacementEntiteTeleportation", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiMapAjouteObjet(index As Integer, data As String)

        With Comptes(index)

            Try

                ' GDO+ 358     ; 7596     ; 1                          ; 1500              ; 1500           |
                ' GDO+ Cellule ; Id Objet ; Information supplémentaire ; Résistance actuel ; Résistance Max | Next

                Dim separateData As String() = Split(Mid(data, 5), "|")

                For i = 0 To separateData.Count - 1

                    Dim separate As String() = Split(separateData(i), ";")

                    Dim newMap As New CMapObjet

                    With newMap

                        .Cellule = separate(0)

                        .IdUnique = separate(1)

                        .Nom = VarItems(separate(1)).Nom

                        If separate(2) = "1" Then

                            .ResistanceMinimum = separate(3)
                            .ResistanceMaximum = separate(4)

                        End If

                    End With

                    .Map.Objet.Add(separate(0), newMap)

                Next


            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMapAjouteObjet", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiMapSupprimeObjet(index As Integer, data As String)

        With Comptes(index)

            Try

                ' GDO- 123 
                ' GDO- Cellule

                Dim id As Integer = Mid(data, 5)

                If .Map.Objet.ContainsKey(id) Then

                    .Map.Objet.Remove(id)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMapSupprimeObjet", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiMapOrientation(index As Integer, data As String)

        With Comptes(index)

            Try

                ' eD 2594870   | 7 
                ' eD Id Unique | Orientation

                Dim separationData As String() = Split(Mid(data, 3), "|")

                If .Map.Entite.ContainsKey(separationData(0)) Then

                    .Map.Entite(separationData(0)).Orientation = separationData(1)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMapOrientation", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiMapAgression(index As Integer, data As String)

        With Comptes(index)

            Try

                ' GA ; 906 ; 123456789    ; 987654321
                ' GA ; 906 ; Id agresseur ; id Agressé

                Dim separateData As String() = Split(data, ";")

                EcritureMessage(index, "[Dofus]", .Map.Entite(separateData(2)).Nom & " agresse " & .Map.Entite(separateData(3)).Nom, Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMapAgression", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "Function"

    Private Function NomMobs(ByVal name As String) As String

        Dim resultat As String = ""

        Try

            Dim separateName As String() = Split(name, ",")


            For i = 0 To separateName.Count - 1

                resultat &= VarMobs(separateName(i))(0).Nom & " , "

            Next

        Catch ex As Exception

        End Try

        Return resultat

    End Function

    Private Function ClasseJoueur(ByVal Information As String) As String

        Try

            Dim Classe As String() = {"Feca", "Osamodas", "Enutrof", "Sram", "Xelor", "Ecaflip", "Eniripsa", "Iop", "Crâ", "Sadida", "Sacrieur", "Pandawa"}
            Dim separate As String() = Split(Information, "^") ' 90^100
            Dim Résultat As Integer = Mid(separate(0), 1, Len(separate(0)) - 1) ' 90

            If Résultat < 12 Then

                Return Classe(Résultat - 1)

            End If

        Catch ex As Exception

        End Try

        Return "Inconnu"

    End Function

    Private Function SexeJoueur(ByVal Information As String) As String

        Try

            Dim sexe As String() = {"Homme", "Femme"}
            Dim separate As String() = Split(Information, "^") ' 90^100

            Dim number As Integer = Mid(separate(0), Len(separate(0)), Len(separate(0)) - 1)

            Return If(number > 1, "Inconnu", sexe(number))

        Catch ex As Exception

        End Try

        Return "Inconnu"

    End Function

    Private Function AlignementJoueur(ByVal numéro As String) As String

        Try

            Dim Alignement() As String = {"Neutre", "Bontarien", "Brâkmarien"}

            Return Alignement(numéro)

        Catch ex As Exception

        End Try

        Return "Inconnu"

    End Function

#End Region

#Region "MapViewer"

    Private Delegate Sub dlg()

    Public Sub Map_Viewer_Chargement(ByVal Index As Integer)

        With Comptes(Index)

            If .FrmUser.InvokeRequired Then

                .FrmUser.Invoke(New dlg(Sub() Map_Viewer_Chargement(Index)))

            Else

                .Map.Map_Viewer_BitMap.Dispose()
                .Map.Map_Viewer_BitMap = New Bitmap(.Map.Largeur * 80, .Map.Hauteur * 40)

                Dim b As Bitmap = New Bitmap(.Map.Largeur * 80, .Map.Hauteur * 40)
                Dim g As Graphics = Graphics.FromImage(b)

                Dim cellWidth As Double = 80
                Dim cellHeight As Double = Math.Ceiling(cellWidth / 2)

                Dim offsetX As Integer
                Dim offsetY As Integer

                Dim medianCellH As Double = cellHeight / 2
                Dim medianCellW As Double = cellWidth / 2

                Dim count As Integer = 0

                Dim left, top, right, down As Point
                Dim MyPen As New Pen(Color.Gray, 1)

                For y As Integer = 0 To 2 * .Map.Hauteur - 1

                    If (y Mod 2) = 0 Then

                        For x As Integer = 0 To .Map.Largeur - 1

                            left = New Point(offsetX + x * cellWidth, offsetY + y * medianCellH + medianCellH)
                            top = New Point(offsetX + (x * cellWidth) + medianCellW, offsetY + (y * medianCellH))
                            right = New Point(offsetX + x * cellWidth + cellWidth, offsetY + y * medianCellH + medianCellH)
                            down = New Point(offsetX + (x * cellWidth) + medianCellW, offsetY + (y * medianCellH) + cellHeight)

                            If .Map.Handler(count).active Then

                                If .Map.Handler(count).layerObject1Num = 1030 OrElse .Map.Handler(count).layerObject2Num = 1030 Then

                                    g.FillPolygon(Brushes.Yellow, New Point() {New Point(left), New Point(top), New Point(right), New Point(down)})

                                    'S'il s'agit d'une cellule avec une ldv disponible.
                                ElseIf .Map.Handler(count).lineOfSight Then

                                    'Et si je peux marcher dessus.
                                    If .Map.Handler(count).movement > 0 Then

                                        g.FillPolygon(Brushes.White, New Point() {New Point(left), New Point(top), New Point(right), New Point(down)})

                                    Else

                                        g.FillPolygon(Brushes.Gray, New Point() {New Point(left), New Point(top), New Point(right), New Point(down)})

                                    End If

                                ElseIf (.Map.Handler(count).movement = 0 AndAlso .Map.Handler(count).lineOfSight = False) OrElse .Map.Handler(count).movement = 0 Then

                                    g.FillPolygon(Brushes.Black, New Point() {New Point(left), New Point(top), New Point(right), New Point(down)})

                                End If

                            Else

                                g.FillPolygon(Brushes.Black, New Point() {New Point(left.X + 1, left.Y), New Point(top.X, top.Y + 1), right, down})

                            End If

                            g.DrawPolygon(MyPen, New Point() {left, top, right, down})

                            .Map.MapListeCelluleLeft(count) = left
                            .Map.MapListeCelluleTop(count) = top
                            .Map.MapListeCelluleRight(count) = right
                            .Map.MapListeCelluleDown(count) = down

                            count += 1

                        Next

                    Else

                        For x As Integer = 0 To .Map.Largeur - 2

                            left = New Point(offsetX + x * cellWidth + medianCellW, offsetY + y * medianCellH + medianCellH)
                            top = New Point(offsetX + x * cellWidth + cellWidth, offsetY + y * medianCellH)
                            right = New Point(offsetX + x * cellWidth + cellWidth + medianCellW, offsetY + y * medianCellH + medianCellH)
                            down = New Point(offsetX + x * cellWidth + cellWidth, offsetY + y * medianCellH + cellHeight)

                            If .Map.Handler(count).active Then


                                If .Map.Handler(count).layerObject1Num = 1030 OrElse .Map.Handler(count).layerObject2Num = 1030 Then

                                    g.FillPolygon(Brushes.Yellow, New Point() {New Point(left), New Point(top), New Point(right), New Point(down)})

                                    'S'il s'agit d'une cellule avec une ldv disponible.
                                ElseIf .Map.Handler(count).lineOfSight Then

                                    'Et si je peux marcher dessus.
                                    If .Map.Handler(count).movement > 0 Then

                                        g.FillPolygon(Brushes.White, New Point() {New Point(left), New Point(top), New Point(right), New Point(down)})

                                    Else

                                        g.FillPolygon(Brushes.Gray, New Point() {New Point(left), New Point(top), New Point(right), New Point(down)})

                                    End If

                                ElseIf (.Map.Handler(count).movement = 0 AndAlso .Map.Handler(count).lineOfSight = False) OrElse .Map.Handler(count).movement = 0 Then

                                    g.FillPolygon(Brushes.Black, New Point() {New Point(left), New Point(top), New Point(right), New Point(down)})

                                End If

                            Else

                                g.FillPolygon(Brushes.Black, New Point() {New Point(left.X + 1, left.Y), New Point(top.X, top.Y + 1), right, down})

                            End If

                            g.DrawPolygon(MyPen, New Point() {left, top, right, down})

                            .Map.MapListeCelluleLeft(count) = left
                            .Map.MapListeCelluleTop(count) = top
                            .Map.MapListeCelluleRight(count) = right
                            .Map.MapListeCelluleDown(count) = down

                            count += 1

                        Next

                    End If

                Next

                .Map.Map_Viewer_BitMap = b.Clone
                g.Dispose()
                b.Dispose()

            End If

        End With

    End Sub


    Public Sub MapViewerReste(ByVal index As Integer)

        With Comptes(index)

            If .FrmUser.InvokeRequired Then

                .FrmUser.Invoke(New dlg(Sub() MapViewerReste(index)))

            Else

                .Map.Map_Viewer_BitMap_All.Dispose()
                .Map.Map_Viewer_BitMap_All = New Bitmap(.Map.Largeur * 80, .Map.Hauteur * 40)
                .Map.Map_Viewer_BitMap_All = .Map.Map_Viewer_BitMap.Clone

                ' .FrmUser.PictureBox_MapViewer.Controls.Clear()
                '   .FrmUser.PictureBox_MapViewer.Size = New Size(.Map.Largeur * 80, .Map.Hauteur * 40)

                Dim b As Bitmap = New Bitmap(.Map.Map_Viewer_BitMap)
                Dim g As Graphics = Graphics.FromImage(b)
                Dim fon As Font = New Font("Arial", 8, FontStyle.Regular)

                For Each Pair As KeyValuePair(Of Integer, CEntite) In .Map.Entite

                    If .Combat.EnCombat Then

                        '   If Pair.Value.Combat.Equipe = .Combat.Entite(.Personnage.ID).Equipe Then

                        '       g.FillPolygon(Brushes.Cyan, New Point() { .Map.MapListeCelluleLeft(Pair.Value.Cellule), .Map.MapListeCelluleTop(Pair.Value.Cellule), .Map.MapListeCelluleRight(Pair.Value.Cellule), .Map.MapListeCelluleDown(Pair.Value.Cellule)})

                        '  Else

                        '     g.FillPolygon(Brushes.Red, New Point() { .Map.MapListeCelluleLeft(Pair.Value.Cellule), .Map.MapListeCelluleTop(Pair.Value.Cellule), .Map.MapListeCelluleRight(Pair.Value.Cellule), .Map.MapListeCelluleDown(Pair.Value.Cellule)})

                        ' End If

                    Else

                        Select Case Pair.Value.IDUnique

                            Case < 0

                                g.FillPolygon(Brushes.Red, New Point() { .Map.MapListeCelluleLeft(Pair.Value.Cellule), .Map.MapListeCelluleTop(Pair.Value.Cellule), .Map.MapListeCelluleRight(Pair.Value.Cellule), .Map.MapListeCelluleDown(Pair.Value.Cellule)})

                            Case > 0

                                g.FillPolygon(Brushes.Cyan, New Point() { .Map.MapListeCelluleLeft(Pair.Value.Cellule), .Map.MapListeCelluleTop(Pair.Value.Cellule), .Map.MapListeCelluleRight(Pair.Value.Cellule), .Map.MapListeCelluleDown(Pair.Value.Cellule)})


                                '    Case "Red" 'Ennemi
                                '        g.FillPolygon(Brushes.Red, New Point() { .MapListeCelluleLeft(Pair.SubItems(0).Text), .MapListeCelluleTop(Pair.SubItems(0).Text), .MapListeCelluleRight(Pair.SubItems(0).Text), .MapListeCelluleDown(Pair.SubItems(0).Text)})
                                '   Case "Cyan" 'Alliée
                                '       g.FillPolygon(Brushes.Cyan, New Point() { .MapListeCelluleLeft(Pair.SubItems(0).Text), .MapListeCelluleTop(Pair.SubItems(0).Text), .MapListeCelluleRight(Pair.SubItems(0).Text), .MapListeCelluleDown(Pair.SubItems(0).Text)})
                                '   Case "DarkCyan" 'Mode Marchand
                                '       g.FillPolygon(Brushes.DarkCyan, New Point() { .MapListeCelluleLeft(Pair.SubItems(0).Text), .MapListeCelluleTop(Pair.SubItems(0).Text), .MapListeCelluleRight(Pair.SubItems(0).Text), .MapListeCelluleDown(Pair.SubItems(0).Text)})
                                '   Case "YellowGreen" 'Percepteur
                                '       g.FillPolygon(Brushes.YellowGreen, New Point() { .MapListeCelluleLeft(Pair.SubItems(0).Text), .MapListeCelluleTop(Pair.SubItems(0).Text), .MapListeCelluleRight(Pair.SubItems(0).Text), .MapListeCelluleDown(Pair.SubItems(0).Text)})
                                '   Case "Pink" 'PNJ
                                '       g.FillPolygon(Brushes.Pink, New Point() { .MapListeCelluleLeft(Pair.SubItems(0).Text), .MapListeCelluleTop(Pair.SubItems(0).Text), .MapListeCelluleRight(Pair.SubItems(0).Text), .MapListeCelluleDown(Pair.SubItems(0).Text)})
                                '   Case "Gray" ' Prisme
                                '       g.FillPolygon(Brushes.Gray, New Point() { .MapListeCelluleLeft(Pair.SubItems(0).Text), .MapListeCelluleTop(Pair.SubItems(0).Text), .MapListeCelluleRight(Pair.SubItems(0).Text), .MapListeCelluleDown(Pair.SubItems(0).Text)})
                        End Select

                    End If

                Next

                .Map.Map_Viewer_BitMap_All = b.Clone
                ' .FrmUser.PictureBox_MapViewer.Image = b.Clone

                g.Dispose()
                b.Dispose()

            End If

        End With

    End Sub


#End Region

End Module

#Region "Class"

Public Class CMap

    Public Largeur As Integer
    Public Hauteur As Integer
    Public Handler(1280) As Cell
    Public PathTotal As String
    Public ID As Integer
    Public StopDeplacement As Boolean
    Public Haut, Bas, Gauche, Droite As Integer
    Public EnDeplacement As Boolean
    Public Coordonnees As String
    Public Spectateur As Boolean
    Public Map_Viewer_BitMap_All, Map_Viewer_BitMap As New Bitmap(1000, 1000)
    Public MapListeCelluleLeft(1024), MapListeCelluleTop(1024), MapListeCelluleRight(1024), MapListeCelluleDown(1024) As Point
    Public Entite As New Dictionary(Of Integer, CEntite)
    Public Objet As New Dictionary(Of Integer, CMapObjet)
    Public Interaction As New Dictionary(Of Integer, CInteraction)
    Public Bloque As Threading.ManualResetEvent = New Threading.ManualResetEvent(False)

End Class

Public Class CMapObjet

    Public Cellule As Integer
    Public IdUnique As Integer
    Public Nom As String
    Public ResistanceMinimum As Integer
    Public Id As String
    Public ResistanceMaximum As Integer

End Class

Public Class CEntite

    Public IDCategorie As Integer
    Public IDUnique As Integer
    Public Cellule As Integer
    Public Nom As String
    Public Niveau As String
    Public ID As String
    Public Etoile As Integer
    Public Classe As String
    Public Sexe As String
    Public Guilde As String
    Public ModeMarchand As Boolean
    Public Alignement As String
    Public Orientation As Boolean

End Class

#End Region