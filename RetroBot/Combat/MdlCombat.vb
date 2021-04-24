Module MdlCombat


    'Gc+- 55             ; 4 | -55 ; 306     ; 0 ; -1 |-56;335;1;-1
    'Gc+- Id de mon épée ; ? | id  ; cellule ; ? ; ?  | Ennemi

    Sub GiCombatEntrer(index As Integer, data As String)

        With Comptes(index)

            Try

                ' GA ; 905 ; 3101697   ;
                ' GA ; Id  ; Id Joueur ; ?

                Dim separateData As String() = Split(data, ";")

                If separateData(2) = .Personnage.ID Then

                    EcritureMessage(index, "[Combat]", "Vous êtes entré en combat.", Color.Sienna)

                    .Combat.EnCombat = True

                    .Map.Bloque.Set()

                    .Map.Entite.Clear()

                    InterfaceResetMap(index, data)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatEntrer", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiCombatFin(index As Integer, data As String)

        With Comptes(index)

            Try

                'GE29226|2521478|0|2;2521478  ;Linaculer;60     ;0               ;11655000;11712633  ;12450000;93; ; ;388~1,393~1,394~1    ;12         |joueur suivant
                '                   ;ID UNIQUE;Nom      ;niveau ;0 = win 1 = lose;Exp Min ;Exp Actuel;Xp Max  ;? ;?;?;ID Objet+Quantité,etc;Kamas dropé

                .Combat = New CCombat

                With .Defi

                    .EnInvitation = False
                    .EnDefi = False
                    .IdLanceur = Nothing
                    .Bloque.Set()

                End With

                Dim separateData As String() = Split(data, "|")

                For i = 3 To separateData.Count - 1

                    Dim separate As String() = Split(separateData(i), ";")

                    If separate(1) = separateData(1) Then

                        Select Case separate(4)

                            Case "0"



                            Case "1"

                        End Select

                    End If

                Next

                .Map.Entite.Clear()

                InterfaceResetMap(index, data)

                If .MITM = False Then
                    .Send("GC1")
                End If

                EcritureMessage(index, "[Dofus]", "Fin du combat.", Color.Sienna)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatFin", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Private Sub InterfaceResetMap(index As Integer, data As String)

        With Comptes(index).FrmUser

            Try

                If .InvokeRequired Then

                    .Invoke(New DlgPlayer(AddressOf InterfaceResetMap), index, data)

                Else

                    .DataGridView_Map_Joueur.Rows.Clear()
                    .DataGridView_Map_Mobs.Rows.Clear()

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

    Sub GiCombatLancer(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                'Indique l'épée et le tas de monstre (se qu'il contient)

                ' Gt -42                     | + 1234567   ; Linaculer ; 60 
                ' Gt -43                     | + -1        ; 63        ; 3      |+-2;63;2 
                ' Gt Id Unique Epee/Tas Mobs | + Id Unique ; Nom/ID    ; Niveau | Suivant

                Dim separateData As String() = Split(data, "|")
                Dim id As Integer = Mid(separateData(0), 3, separateData(0).Length)

                For i = 1 To separateData.Count - 1

                    Dim separate As String() = Split(separateData(i), ";")
                    Dim newMapCombatLancer As New CCombatLancer

                    With newMapCombatLancer

                        .idUnique = separate(0)
                        .Id = If(IsNumeric(separate(1)), separate(1), 0)
                        .Nom = If(IsNumeric(separate(1)), VarMobs(CInt(separate(1)))(0).Nom, separate(1))
                        .Niveau = separate(2)

                    End With

                    If .Combat.Lancer.ContainsKey(id) Then

                        .Combat.Lancer(id).Add(newMapCombatLancer)

                    Else

                        .Combat.Lancer.Add(id, New List(Of CCombatLancer) From {newMapCombatLancer})

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatLancer", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiCombatImpossible(index As Integer, data As String)

        With Comptes(index)

            Try

                'GA ; 903 ; 1234567    ; f
                'GA ; 903 ; Id Lanceur ; Cadenas/Groupe

                Dim separateData As String() = Split(data, ";")

                Select Case separateData(3)

                    Case "f"

                        EcritureMessage(index, "[Dofus]", "Impossible de rejoindre le combat car l'équipe est fermée (ou limitée au groupe du joueur principal).", Color.Red)

                    Case "p"

                        EcritureMessage(index, "[Dofus]", "Action impossible sur cette carte.", Color.Red)

                    Case Else

                        ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMapAgressionEchec", data)

                End Select

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatImpossible", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#Region "Sort"

    Public Sub GiCombatSortNormal(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                'GA ; 300 ; 1234567    ; 61      , 148     , 902 , 5 , 31 , 2 , 1 
                'GA ; 300 ; Id Lanceur ; Id Sort , Cellule , ?   , ? , ?  , ? , ?

                .Combat.Echec = False

                Dim separateData As String() = Split(data, ";")
                Dim separateInfo As String() = Split(separateData(3), ",")

                If VarSort.ContainsKey(CInt(separateInfo(0))) Then

                    EcritureMessage(index, "[Dofus]", .Map.Entite(separateData(2)).Nom & " lance le sort : " & VarSort(separateInfo(0)).Values(0).Nom &
                                                       " sur la cellule : " & separateInfo(1), Color.Sienna)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatSortNormal", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiCombatSortCoupCritique(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' GA ; 301 ; 1234567    ; 161
                ' GA ; 301 ; ID_Lanceur ; ID_Sort

                .Combat.Echec = False
                EcritureMessage(index, "[Combat]", "Coup Critique !", Color.Sienna)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatSortCoupCritique", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "Challenge"

    Sub GiCombatChallengeRecu(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try
                ' Gd 2            ; 0 ;   ; 25 ; 5         ; 25    ; 5 
                ' Gd ID Challenge ; ? ; ? ; Xp ; Xp Groupe ; Butin ; Butin Groupe

                Dim separateData As String() = Split(Mid(data, 3), ";")

                Dim newCombat As New CCombatChallenge

                With newCombat

                    Select Case separateData(0)

                        Case "2"

                            .Nom = "Statue"

                        Case "4"

                            .Nom = "Sursis"

                        Case "17"

                            .Nom = "Intouchable"

                        Case "24"

                            .Nom = "Borné"

                        Case "31"

                            .Nom = "Statue"

                        Case "38"

                            .Nom = "Blitzkrieg"

                        Case Else

                            .Nom = "Inconnu"

                    End Select

                    .ID = separateData(0)
                    .Raté = False
                    .Xp = separateData(3)
                    .XpGroupe = separateData(4)
                    .Butin = separateData(5)
                    .ButinGroupe = separateData(6)

                End With

                If .Combat.Challenge.ContainsKey(separateData(0)) Then

                    .Combat.Challenge(separateData(0)) = newCombat

                Else

                    .Combat.Challenge.Add(separateData(0), newCombat)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatChallengeReçu", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiCombatChallengeReussi(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try
                ' GdOK 41
                ' GdOK id Challenge

                If .Combat.Challenge.ContainsKey(Mid(data, 5)) Then

                    EcritureMessage(index, "[Dofus]", "Vous avez réussi le challenge : " & .Combat.Challenge(Mid(data, 5)).Nom, Color.Sienna)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatChallengeRéussi", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiCombatChallengeEchoue(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try
                ' GdKO 23
                ' GdKO id Challenge

                If .Combat.Challenge.ContainsKey(Mid(data, 3)) Then

                    .Combat.Challenge(Mid(data, 3)).Raté = True

                    EcritureMessage(index, "[Dofus]", "Challenge raté : " & .Combat.Challenge(Mid(data, 3)).Nom, Color.Sienna)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatChallengeEchoue", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "Information en combat"

    Sub GiCombatMortJoueurMobs(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            ' GA ; 103     ; 01234567 ; -5 
            ' GA ; ID Info ; ID Tueur ; ID Tué

            .Combat.Pause += 1800

            Dim separateData As String() = Split(data, ";")

            If .Combat.Entite.ContainsKey(separateData(3)) Then

                .Combat.Entite(separateData(3)).Vivant = False

                EcritureMessage(index, "[Combat]", .Map.Entite(separateData(3)).Nom & " est mort.", Color.Sienna)

            End If

        End With

    End Sub

    Sub GiCombatRetirePdv(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' GA ; 100 ; 1234567    ; 7654321  , 50
                ' GA ; 100 ; Id Lanceur ; Id Cible , Quantité

                Dim separateData As String() = Split(data, ";")
                Dim separateInfo As String() = Split(separateData(3), ",")

                If .Combat.Entite.ContainsKey(separateInfo(0)) Then

                    If CInt(separateInfo(1)) < 0 Then

                        EcritureMessage(index, "[Dofus]", .Map.Entite(separateInfo(0)).Nom & " perd " & separateInfo(1) & " points de vie.", Color.Sienna)
                        .Combat.Entite(separateInfo(0)).Vitalite = CInt(.Combat.Entite(separateInfo(0)).Vitalite + separateInfo(1))

                    Else

                        EcritureMessage(index, "[Dofus]", .Map.Entite(separateInfo(0)).Nom & " n'a rien subi.", Color.Sienna)

                    End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatRetirePdv", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiCombatTourActuel(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' GTS 1234567   | 29000 
                ' GTS Id Unique | Temps restant

                data = Mid(data, 4)

                Dim separateData As String() = Split(data, "|")

                If .Personnage.ID = separateData(0) Then

                    .Combat.MonTour = True

                End If

                If .Combat.Entite.ContainsKey(separateData(0)) Then

                .Combat.Entite(separateData(0)).NumeroTour += 1

            End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatTourActuel", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiCombatTourPasse(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' GTR -1
                ' GTR Id Joueur/mobs

                .Send("GT")

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatTourPassé", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiCombatTourPasseEntite(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' GTF -1
                ' GTF Id Joueur/mobs

                If Mid(data, 4) = .Personnage.ID Then

                    .Combat.MonTour = False

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatTourPasseEntite", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub


    Sub GiCombatAction(ByVal index As Integer, ByVal data As String)

        With Comptes(index)  'GAF0|01234567 ou GAF14|01234567 etc...

            Try

                Dim separateData() As String = Split(data, "|")

                If .MITM = False Then

                    Task.Delay(.Combat.Pause).Wait()

                End If

                .Combat.Pause = 0

                If .MITM = False Then

                    .Send("GKK" & Mid(separateData(0), 4))

                End If

                If separateData(1) = .Personnage.ID Then

                .Combat.Bloque.Set()

            End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatAction", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiCombatInformationTour(ByVal index As Integer, data As String)

        With Comptes(index)

            Try

                ' GTM |-1         ; 0               ; 45         ; 5  ; 3  ; 330     ;   ; 45      | 1234567;0;145;6;3;309;;145  
                ' GTM | ID Unique ; Vivant=0/Mort=1 ; Pdv actuel ; PA ; PM ; Cellule ; ? ; Pdv Max | Next

                Dim separateData As String() = Split(data, "|")

                For i = 1 To separateData.Count - 1

                    Dim separate As String() = Split(separateData(i), ";")

                    If .Combat.Entite.ContainsKey(separate(0)) Then

                        With .Combat.Entite(separate(0))

                            Select Case separate(1)

                                Case 0 ' Vivant

                                    .Vitalite = separate(2)
                                    .PA = separate(3)
                                    .PM = separate(4)

                                    .Vivant = True

                                Case 1 ' Mort

                                    .Vivant = False

                            End Select

                        End With

                    End If

                    If .Map.Entite.ContainsKey(separate(0)) Then

                        .Map.Entite(separate(0)).Cellule = separate(5)

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatInformationTour", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiCombatOrdreTour(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' GTL | 1234567   | -1 
                ' GTL | Id Unique | Next

                Dim separateData As String() = Split(data, "|")

                For i = 1 To separateData.Count - 1

                    If .Combat.Entite.ContainsKey(separateData(i)) Then

                        .Combat.Entite(separateData(i)).OrdreTour = i

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatOrdreTour", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "PM"

    Sub GiCombatPMPerdu(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try
                'GA;129;ID_Lanceur;ID_Cible,Quantité  

                Dim separateData As String() = Split(data, ";")
                Dim separateInfo As String() = Split(separateData(3), ",")

                If .Map.Entite.ContainsKey(separateInfo(0)) Then

                    EcritureMessage(index, "[Dofus]", .Map.Entite(separateInfo(0)).Nom & " utilise " & separateInfo(1) & " PM.", Color.Sienna)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatPMPerdu", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "PO"

    Sub GiCombatPOGagne(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try
                ' GA ; 117 ; 1234567    ; 7654321  , 1        , 2
                ' GA ; 117 ; ID_Lanceur ; ID_Cible , Quantité , Nbr tour

                Dim separateData As String() = Split(data, ";")
                Dim separateInfo As String() = Split(separateData(3), ",")

                If .Map.Entite.ContainsKey(separateInfo(0)) Then

                    EcritureMessage(index, "[Dofus]", .Map.Entite(separateInfo(0)).Nom & " gagne " & separateInfo(1) & " PO pendant " & separateInfo(2) & " tours.", Color.Sienna)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatPOGagné", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiCombatPOPerdu(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' GA ; 116 ; 1234567    ; -1       , 1        , 1
                ' GA ; 116 ; ID_Lanceur ; ID_Cible , Quantité , Nbr tour

                Dim separateData As String() = Split(data, ";")
                Dim separate As String() = Split(separateData(3), ",")

                If .Map.Entite.ContainsKey(separate(0)) Then

                    EcritureMessage(index, "[Dofus]", .Map.Entite(separate(0)).Nom & " : " & separate(1) & " à la portée (" & separate(2) & " tour).", Color.Green)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatPOPerdu", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "PA"

    Sub GiCombatPAUtilise(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' GA ; 102 ; -1         ; -1       , -5
                ' GA ; 102 ; ID_Lanceur ; ID_Cible , Quantité

                Dim separateData As String() = Split(data, ";")
                Dim separateInfo As String() = Split(separateData(3), ",")

                If .Map.Entite.ContainsKey(separateInfo(0)) Then

                    EcritureMessage(index, "[Dofus]", .Map.Entite(separateInfo(0)).Nom & " utilise " & separateInfo(1) & " PA.", Color.Sienna)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatPAUtilisé", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "Etat"

    Sub GiCombatEtat(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' GA ; 950 ; 7654321    ; 1234567  , 7       , 0
                ' GA ; 950 ; id Lanceur ; ID_Cible , Id Etat , nbr de tour ? 

                Dim separateData As String() = Split(data, ";")
                Dim separateInfo As String() = Split(separateData(3), ",")

                If .Combat.Entite.ContainsKey(separateInfo(0)) Then

                    Select Case separateInfo(1)

                        Case "3"

                        Case "7"

                            .Combat.Entite(separateInfo(0)).Etat = "Pesanteur"
                            EcritureMessage(index, "[Dofus]", .Map.Entite(separateInfo(0)).Nom & " entre dans l'état Pesanteur.", Color.Sienna)

                        Case "8"

                            .Combat.Entite(separateInfo(0)).Etat = ""

                    End Select

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "CombatEtat", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "Préparation"

    Sub GiCombatPhasePlacement(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' GIC | 1234567   ; 207     ; 1 
                ' GIC | Id unique ; Cellule ; Numéro equipe

                Dim separateData As String() = Split(data, "|")

                separateData = Split(separateData(1), ";")

                If .Map.Entite.ContainsKey(separateData(0)) Then

                    .Map.Entite(separateData(0)).Cellule = separateData(1)
                    .Combat.Entite(separateData(0)).Equipe = separateData(2)

                End If

                If separateData(0) = .Personnage.ID Then

                    .Combat.Bloque.Set()

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatPhasePlacement", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiCombatTempsPreparation(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            ' GJK2 | 0 | 1 | 0 | 30000                                      | 4 
            ' GJK2 | ? | ? | ? | Temps restant avant que le combat se lance | ?

            Dim separateData As String() = Split(data, "|")

            With .Combat

                .EnCombat = True
                .EnPreparation = If(CInt(separateData(4)) <= 1, True, False)
                .Entite.Clear()

            End With

            .Map.Entite.Clear()

            EcritureMessage(index, "[Dofus]", "Il reste " & separateData(4) & " millisecondes avant que le combat se lance automatiquement.", Color.Sienna)

        End With

    End Sub

    Sub GiCombatPret(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' GR1 3107486
                ' GR1 Id Unique

                Dim id As Integer = Mid(data, 4)

                If .Combat.Entite.ContainsKey(id) Then

                    .Combat.Entite(id).Pret = True

                End If

                If id = .Personnage.ID Then

                .Combat.Bloque.Set()

            End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatPret", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiCombatPlacementCase(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' GP bfbubBbPbYcbcfct  | fBfPfXf1f_gdgOg2  | 0 
                ' GP Cellules Equipe 1 | Cellules equipé 2 | Indique l'équipe dans laquel vous êtes (couleur des cases)

                Dim separateData As String() = Split(Mid(data, 3), "|")

                With .Combat

                    .PlacementCellule.Clear()
                    .EnPlacement = True

                    For i = 0 To 1

                        For a = 1 To separateData(i).Length Step 2

                            If .PlacementCellule.ContainsKey(i) Then

                                .PlacementCellule(i).Add(ReturnLastCell(Mid(separateData(separateData(2)), a, 2)))

                            Else

                                .PlacementCellule.Add(i, New List(Of Integer)(ReturnLastCell(Mid(separateData(separateData(2)), a, 2))))

                            End If

                        Next

                    Next

                End With

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatPlacementCase", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

End Module


#Region "Class"

Public Class CCombat

    Public MonTour As Boolean
    Public EnPreparation As Boolean
    Public EnPlacement As Boolean
    Public EnCombat As Boolean
    Public EnDefi As Boolean
    Public Spectateur As Boolean
    Public Cadenas As Boolean
    Public Aide As Boolean
    Public Groupe As Boolean
    Public Echec As Boolean
    Public Bloque As Threading.ManualResetEvent = New Threading.ManualResetEvent(False)
    Public Pause As Integer
    Public DefiIdLanceur As Integer
    Public Lancer As New Dictionary(Of Integer, List(Of CCombatLancer))
    Public Entite As New Dictionary(Of Integer, CCombatEntite)
    Public Challenge As New Dictionary(Of Integer, CCombatChallenge)
    Public PlacementCellule As New Dictionary(Of Integer, List(Of Integer))

End Class

Public Class CCombatLancer

    Public idUnique As Integer
    Public Id As Integer
    Public Nom As String
    Public Niveau As Integer

End Class

Public Class CCombatChallenge

    Public ID As Integer
    Public Nom As String
    Public Raté As Boolean
    Public Xp As Integer
    Public XpGroupe As Integer
    Public Butin As Integer
    Public ButinGroupe As Integer

End Class

Public Class CCombatEntite

    Public Pret As Boolean
    Public OrdreTour As Integer
    Public Vivant As Boolean
    Public Vitalite As Integer
    Public PA As Integer
    Public PM As Integer
    Public ResistanceEau As Integer
    Public ResistanceFeu As Integer
    Public ResistanceTerre As Integer
    Public ResistanceAir As Integer
    Public ResistanceNeutre As Integer
    Public EsquivePA As Integer
    Public EsquivePM As Integer
    Public NumeroTour As Integer
    Public Etat As String
    Public Equipe As Integer

End Class
#End Region
