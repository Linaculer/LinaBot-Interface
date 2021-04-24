Partial Class Player

    'TOUT METTRE DANS GESTION POUR LAISSE LE RESTE AU PROPRE.

    Private ReadOnly GestionInfo As New Gestion
    'Faire 2 facon si rien indiquer bot auto
    ' si indiquer faire comme voulu par luser
    Public Sub GestionReception(message As Object, e As Socket_EventArgs)

        With Comptes(Index).GestionInfo

            Try

                If e.Message <> "" Then

                    Select Case e.Message(0)

                        Case "A"

                            Select Case e.Message(1)

                                Case "N"

                                    .UpSort(Index)
                                    .UpCaracteristique(Index)

                                Case "s"

                                    .InterfaceGiCaracteristique(Index, e.Message)

                                Case "S"

                                    Select Case e.Message(2)

                                        Case "K"

                                            .Connecter(Index, e.Message)

                                            '   Task.Run(Sub() .GroupeInvite(Index))

                                    End Select

                            End Select

                        Case "G"

                            Select Case e.Message(1)

                                Case "D"

                                    Select Case e.Message(2)

                                        Case "M"

                                            .InterfaceGiMapData(Index, e.Message)
                                            .InterfaceChargementDivers(Index, Map.Handler)

                                    End Select

                                Case "E"

                                    .GiCombatFin(Index, e.Message)

                                Case "J"

                                    Select Case e.Message(2)

                                        Case "K"

                                            Select Case e.Message(3)

                                                Case "K"

                                            End Select

                                    End Select

                                Case "M"

                                    Select Case e.Message(2)

                                        Case "|"

                                            Select Case e.Message(3)

                                                Case "+"

                                                    .InterfaceGiMapAjouteEntite(Index, e.Message)

                                                Case "-"

                                                    .InterfaceGiMapSupprimeEntite(Index, e.Message)

                                            End Select

                                    End Select

                                Case "T"

                                    Select Case e.Message(2)
                                        Case "S"

                                            Dim separate As String() = Split(Mid(e.Message, 4), "|")

                                            If separate(0) = Comptes(Index).Personnage.ID.ToString Then

                                                .ThreadCombat = New Threading.Thread(Sub() IAGestion(Index)) With {.IsBackground = True}
                                                .ThreadCombat.Start()

                                            End If

                                    End Select

                                Case "t"

                                    If .ThreadCombat IsNot Nothing AndAlso .ThreadCombat.IsAlive Then

                                        Return

                                    Else


                                    End If

                                Case "P"

                                    Task.Run(Sub() .CombatPlacementCase(Index, e.Message))

                            End Select

                        Case "H"

                            Select Case e.Message(1)

                                Case "G"

                                    Comptes(Index).FrmUser.Button_Connexion.BackgroundImage = My.Resources.Connecter
                                    Comptes(Index).FrmUser.Button_Connexion.FlatAppearance.BorderColor = Color.Lime

                                    Comptes(Index).FrmUser.LabelStatut.Text = "Connecté"
                                    Comptes(Index).FrmUser.LabelStatut.ForeColor = Color.Lime

                            End Select

                        Case "O"

                            Select Case e.Message(1)

                                Case "A"

                                    Select Case e.Message(2)

                                        Case "K"

                                            Select Case e.Message(3)

                                                Case "O"

                                                    Task.Run(Sub() .Supprime(Index))

                                            End Select

                                    End Select

                                Case "Q"

                                    Task.Run(Sub() .Supprime(Index))

                                Case "w"



                            End Select

                        Case "P"

                            Select Case e.Message(1)

                                Case "I" ' PI

                                    Select Case e.Message(2)

                                        Case "K" ' PIK

                                            '   Task.Run(Sub() .GroupeAccepte(Index, e.Message))

                                    End Select

                                Case "M" ' PM

                                    Select Case e.Message(2)

                                        Case "-" ' PM-

                                            Task.Run(Sub() .GroupeQuitte(Index))

                                    End Select

                            End Select

                    End Select

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

End Class

Class Gestion

    Private Delegate Sub dlgMap(index As Integer, spritesHandler() As Cell)
    Private Delegate Function dlgFGestion()

    Public ThreadCombat As Threading.Thread

#Region "Interface"

    Sub InterfaceGiCaracteristique(index As Integer, data As String)

        With Comptes(index)

            Try

                If .FrmUser.InvokeRequired Then

                    .FrmUser.Invoke(New DlgPlayer(AddressOf InterfaceGiCaracteristique), index, data)

                Else


                    Dim separateData As String() = Split(Mid(data, 3), "|")
                    Dim separate As String()

                    With .FrmUser

                        'Experience
                        separate = Split(separateData(0), ",")

                        With .ProgressBar_Experience

                            .Text = Math.Round(separate(0) / separate(2) * 100) & "%"
                            .Maximum = separate(2)
                            .Value = separate(0)

                        End With

                        .ToolTip1.SetToolTip(.ProgressBar_Experience, separate(0) & " / " & separate(2))

                        'Kamas
                        .Label_Kamas.Text = ChiffreSeparation(separateData(1), ".")

                        'Capital Caracteristique
                        .GroupBox_Caracteristique_Capital.Text = "Caracteristique (Capital : " & separateData(2) & ")"

                        'Capital Sort
                        .GroupBox_Sort_Capital.Text = "Sorts (Capital : " & separateData(3) & ")"

                        'Point de Vie
                        separate = Split(separateData(5), ",")

                        With .ProgressBar_Vitaliter

                            .Text = Math.Round(CInt(separate(0)) / CInt(separate(1)) * 100) & "%"
                            .Maximum = separate(1)
                            .Value = separate(0)

                        End With

                        .ToolTip1.SetToolTip(.ProgressBar_Vitaliter, separate(0) & " / " & separate(1))

                        'Regeneration
                        Comptes(index).Personnage.Regeneration = CInt(separate(0)) / CInt(separate(1)) * 100

                        'Energie
                        separate = Split(separateData(6), ",")

                        With .ProgressBar_Energie

                            .Text = Math.Round((separate(0) / separate(1)) * 100) & "%"
                            .Maximum = separate(1)
                            .Value = separate(0)

                        End With

                        .ToolTip1.SetToolTip(.ProgressBar_Energie, separate(0) & " / " & separate(1))

                        'Initiative
                        .Label_Caracteristique_Initiative.Text = separateData(7)

                        'Prospection
                        .Label_Caracteristique_Prospection.Text = separateData(8)

                        'PA
                        .Label_Caracteristique_PA.Text = Split(separateData(9), ",")(4)

                        'PM
                        .Label_Caracteristique_PM.Text = Split(separateData(10), ",")(4)

                        'Vitaliter
                        .LabelVitaliter.Text = Split(separateData(12), ",")(0) & " (" & Split(separateData(12), ",")(1) & ")"

                        'Sagesse
                        .LabelSagesse.Text = Split(separateData(13), ",")(0) & " (" & Split(separateData(13), ",")(1) & ")"

                        'Force
                        .LabelForce.Text = Split(separateData(11), ",")(0) & " (" & Split(separateData(11), ",")(1) & ")"

                        'Intelligence
                        .LabelIntelligence.Text = Split(separateData(16), ",")(0) & " (" & Split(separateData(16), ",")(1) & ")"

                        'Chance
                        .LabelChance.Text = Split(separateData(14), ",")(0) & " (" & Split(separateData(14), ",")(1) & ")"

                        'Agilité
                        .LabelAgiliter.Text = Split(separateData(15), ",")(0) & " (" & Split(separateData(15), ",")(1) & ")"

                    End With

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

    Sub InterfaceGiMapData(index As Integer, data As String)

        With Comptes(index).FrmUser

            Try

                If .InvokeRequired Then

                    .Invoke(New DlgPlayer(AddressOf InterfaceGiMapData), index, data)

                Else

                    .DataGridView_Map_Interaction.Rows.Clear()
                    .DataGridView_Map_Joueur.Rows.Clear()
                    .DataGridView_Map_Mobs.Rows.Clear()
                    .DataGridView_Map_Pnj.Rows.Clear()

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

    Sub InterfaceChargementDivers(Index As Integer, spritesHandler() As Cell)

        With Comptes(Index)

            Try

                If .FrmUser.InvokeRequired Then

                    .FrmUser.Invoke(New dlgMap(AddressOf InterfaceChargementDivers), Index, spritesHandler)

                Else

                    ' id sprite | nom action | nom item , id action

                    For i As Integer = 1 To 1000

                        If VarInteraction.ContainsKey(spritesHandler(i).layerObject2Num) Then

                            With .FrmUser.DataGridView_Map_Interaction

                                .Rows.Add(False)

                                With .Rows(.Rows.Count - 1)

                                    .Cells(1).Value = i.ToString

                                    .Cells(2).Value = VarInteraction(spritesHandler(i).layerObject2Num).Name.ToLower

                                    .Cells(3).Value = "disponible"

                                End With

                                .RowsDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
                                .RowsDefaultCellStyle.WrapMode = DataGridViewTriState.True

                            End With

                        End If

                    Next

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

    Sub InterfaceGiMapAjouteEntite(index As Integer, data As String)

        With Comptes(index).FrmUser

            Try

                If .InvokeRequired Then

                    .Invoke(New DlgPlayer(AddressOf InterfaceGiMapAjouteEntite), index, data)

                Else

                    Dim separateData As String() = Split(data, "|+")

                    For i = 1 To separateData.Count - 1

                        Dim separate As String() = Split(separateData(i), ";")

                        Select Case separate(5)

                            Case -1 ' Mobs (en combat)

                                ' GM|+ 369     ; 1           ; 0 ; -1        ; 149     ; -2      ; 1571^95 ; 2          ; -1 ; -1 ; -1 ; 0 , 0 , 0 , 0 ; 18       ; 5  ; 3  ; 1 
                                ' GM|+ Cellule ; Orientation ; ? ; id Unique ; Id Mobs ; indice  ; ?       ; Level mobs ; ?  ; ?  ; ?  ; ? , ? , ? , ? ; Vitalité ; PA ; PM ; ? 

                                With .DataGridView_Map_Mobs

                                    .Rows.Add(False)

                                    With .Rows(.Rows.Count - 1)

                                        .Cells(1).Value = separate(3)

                                        .Cells(2).Value = VarMobs(separate(4))(CInt(separate(7) - 1)).Nom

                                        .Cells(3).Value = VarMobs(separate(4))(CInt(separate(7) - 1)).Niveau

                                    End With

                                    .RowsDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
                                    .RowsDefaultCellStyle.WrapMode = DataGridViewTriState.True

                                End With

                            Case -2 ' Mobs en combat

                                ' GM|+ 369     ; 1           ; 0 ; -1        ; 149     ; -2      ; 1571^95 ; 2          ; -1 ; -1 ; -1 ; 0 , 0 , 0 , 0 ; 18       ; 5  ; 3  ; 1 
                                ' GM|+ Cellule ; Orientation ; ? ; id Unique ; Id Mobs ; indice  ; ?       ; Level mobs ; ?  ; ?  ; ?  ; ? , ? , ? , ? ; Vitalité ; PA ; PM ; ? 

                                With .DataGridView_Map_Mobs

                                    .Rows.Add(False)

                                    With .Rows(.Rows.Count - 1)

                                        .Cells(1).Value = separate(3)

                                        .Cells(2).Value = VarMobs(separate(4))(CInt(separate(7) - 1)).Nom

                                        .Cells(3).Value = VarMobs(separate(4))(CInt(separate(7) - 1)).Niveau

                                    End With

                                    .RowsDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
                                    .RowsDefaultCellStyle.WrapMode = DataGridViewTriState.True

                                End With

                            Case -3 ' Mobs (Hors combat)

                                ' GM|+ 439     ; 5           ; 21      ; -2     ; 198     , 241     ; -3     ;1135^110,1138^100 ; 36 , 32 ; -1       , -1       , -1       ;0,0,0,0;-1,-1,-1;0,0,0,0; 
                                ' GM|+ Cellule ; Orientation ; Etoile% ; ID Map ; ID Mobs , Id Mobs ; Entité ;                  ; Lv , Lv ; Couleur1 , Couleur2 , Couleur3 ;?,?,?,?;Couleur1,etc... 

                                With .DataGridView_Map_Mobs

                                    .Rows.Add(False)

                                    With .Rows(.Rows.Count - 1)

                                        .Cells(1).Value = separate(3)

                                        .Cells(2).Value = NomMobs(separate(4))

                                        .Cells(3).Value = separate(7)

                                    End With

                                    .RowsDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
                                    .RowsDefaultCellStyle.WrapMode = DataGridViewTriState.True

                                End With

                            Case -4 ' Pnj-------------------

                                ' GM|+ 152     ; 3           ; 0        ;-1      ; 100    ; -4     ; 9048^100 ; 0  ; -1 ; -1 ; e7b317 ;   ,   ,   ,   ,   ;   ; 0 |
                                ' GM|+ Cellule ; Orientation ; Etoiles% ; ID Map ; ID PNJ ; Entité ; ?        ; Lv ; ?  ; ?  ; ?      ; ? , ? , ? , ? , ? ; ? ; ? | Next PNJ

                                With .DataGridView_Map_Pnj

                                    .Rows.Add(False)

                                    With .Rows(.Rows.Count - 1)

                                        .Cells(1).Value = separate(3)

                                        .Cells(2).Value = separate(4)

                                        .Cells(3).Value = VarPnj(separate(4))

                                    End With

                                    .RowsDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
                                    .RowsDefaultCellStyle.WrapMode = DataGridViewTriState.True

                                End With

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

                                If Comptes(index).Combat.EnCombat Then

                                    calculLevel = Split(separate(9), ",")

                                Else

                                    calculLevel = Split(separate(8), ",")

                                End If

                                With .DataGridView_Map_Joueur

                                    .Rows.Add(False)

                                    With .Rows(.Rows.Count - 1)

                                        .Cells(1).Value = separate(3)

                                        .Cells(2).Value = separate(4)

                                        .Cells(3).Value = If(Comptes(index).Combat.EnCombat, separate(8), CInt(calculLevel(3)) - CInt(separate(3)))

                                        .Cells(4).Value = If(Comptes(index).Combat.EnCombat, "", separate(16))

                                    End With

                                    .RowsDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
                                    .RowsDefaultCellStyle.WrapMode = DataGridViewTriState.True

                                End With

                        End Select

                    Next

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

    Sub InterfaceGiMapSupprimeEntite(index As Integer, data As String)

        With Comptes(index).FrmUser

            Try

                If .InvokeRequired Then

                    .Invoke(New DlgPlayer(AddressOf InterfaceGiMapSupprimeEntite), index, data)

                Else

                    ' GM|- 1234567
                    ' GM|- Id Unique

                    Dim idUnique As String = Mid(data, 5)

                    For Each pair As DataGridViewRow In If(CInt(idUnique) > 0, .DataGridView_Map_Joueur.Rows, .DataGridView_Map_Mobs.Rows)

                        If pair.Cells(1).Value = idUnique Then

                            Select Case CInt(idUnique)

                                Case > 0

                                    .DataGridView_Map_Joueur.Rows.RemoveAt(pair.Index)

                                Case < 0

                                    .DataGridView_Map_Mobs.Rows.RemoveAt(pair.Index)

                            End Select

                            Exit For

                        End If

                    Next

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

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

#End Region

#Region "Groupe"

    Sub GroupeQuitte(index As Integer)

        With Comptes(index)

            Dim newGroupe As New FunctionGroupe
            Task.Run(Function() newGroupe.Quitte(index))

        End With

    End Sub

#End Region

#Region "Combat"



    'sinon execute le code normalement et après l'apelle je fais las task.run de se que je souahite
    'exemple : 
    ' Case "I"
    'GiAmi(index,e.message)
    'task.run(sub() gege())

    Sub CombatPlacementCase(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                If .Groupe.ChefID = 0 Then

                    Dim newCombat1 As New FunctionCombat
                    Task.Run(Function() newCombat1.Pret(index, True))

                ElseIf .Groupe.ChefID > 0 OrElse .Personnage.ID = .Groupe.ChefID Then

                    Task.Delay(5000).Wait()
                    Dim newCombat1 As New FunctionCombat
                    Task.Run(Function() newCombat1.Pret(index, True))
                    Return

                End If
                ' GP bfbubBbPbYcbcfct  | fBfPfXf1f_gdgOg2  | 0 
                ' GP Cellules Equipe 1 | Cellules equipé 2 | Indique l'équipe dans laquel vous êtes (couleur des cases)

                If .Combat.EnPreparation Then

                    Dim separateData As String() = Split(Mid(data, 3), "|")

                    For a = 1 To separateData(separateData(2)).Length Step 2

                        Dim cellule As Integer = ReturnLastCell(Mid(separateData(separateData(2)), a, 2))

                        'Tcheck la cellule correspond à celle voulu
                        's'il veut la cellule proche/éloigné etc....

                        If cellule = "X" Then

                            Dim newCombat As New FunctionCombat
                            Task.Run(Function() newCombat.Placement(index, cellule))

                            Task.Delay(8000).Wait()

                            Task.Run(Function() newCombat.Pret(index, True))

                            Exit For

                        End If

                    Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiCombatPlacementCase", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiCombatFin(index As Integer, data As String)

        With Comptes(index)

            Try

                If .FrmUser.InvokeRequired Then

                    .FrmUser.Invoke(New DlgPlayer(AddressOf GiCombatFin), index, data)

                Else

                    'GE29226|2521478|0|2;2521478  ;Linaculer;60     ;0               ;11655000;11712633  ;12450000;93; ; ;388~1,393~1,394~1    ;12         |joueur suivant
                    '                   ;ID UNIQUE;Nom      ;niveau ;0 = win 1 = lose;Exp Min ;Exp Actuel;Xp Max  ;? ;?;?;ID Objet+Quantité,etc;Kamas dropé

                    Dim separateData As String() = Split(data, "|")

                    For i = 3 To separateData.Count - 1

                        Dim separate As String() = Split(separateData(i), ";")

                        If separate(1) = .Personnage.ID Then

                            Select Case separate(4)

                                Case "0"

                                    .FrmUser.Label_Combat_Gagne.Text = CInt(.FrmUser.Label_Combat_Gagne.Text) + 1

                                Case "1"

                                    .FrmUser.Label_Combat_Perdu.Text = CInt(.FrmUser.Label_Combat_Perdu.Text) + 1

                            End Select

                            .FrmUser.Label_Combat_Kamas.Text = ChiffreSeparation(CInt(.FrmUser.Label_Combat_Kamas.Text.Replace(".", "")) + CInt(separate(12)), ".")

                            Dim separateObjet As String() = Split(separate(11), ",")

                            For a = 0 To separateObjet.Count - 1

                                Dim separateItem As String() = Split(separateObjet(a), "~")

                                If CombatDropExiste(index, separateItem(0), separateItem(1)) = False Then

                                    With .FrmUser.DataGridView_Combat_Drop

                                        .Rows.Add(separateItem(0))

                                        With .Rows(.Rows.Count - 1)

                                            .Cells(1).Value = VarItems(separateItem(0)).Nom

                                            .Cells(2).Value = separateItem(1)

                                        End With

                                        .RowsDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
                                        .RowsDefaultCellStyle.WrapMode = DataGridViewTriState.True

                                    End With

                                End If

                            Next

                        End If

                    Next

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

    Private Function CombatDropExiste(index As Integer, id As String, quantiter As String)

        With Comptes(index)

            For Each pair As DataGridViewRow In .FrmUser.DataGridView_Combat_Drop.Rows

                If pair.Cells(0).Value.ToString = id.ToString Then

                    pair.Cells(2).Value = CInt(pair.Cells(2).Value) + CInt(quantiter)

                    Return True

                End If

            Next

            Return False

        End With

    End Function

#End Region

#Region "Caracteristique"

    Sub UpCaracteristique(index As Integer)

        With Comptes(index)

            Try

                If .OptionCaracteristique <> "" AndAlso CheckBoxCaracteristique(index) Then

                    Dim newCaracteristique As New FunctionCaractéristique

                    While newCaracteristique.Up(index, .OptionCaracteristique)

                        Task.Delay(500).Wait()

                    End While

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

    Private Function CheckBoxCaracteristique(index As Integer) As Boolean

        With Comptes(index).FrmUser

            Try

                If .InvokeRequired Then

                    Return .Invoke(New dlgFGestion(Function() CheckBoxCaracteristique(index)))

                Else


                    Return .CheckBox_Caracteristique_UpAutomatique.Checked

                End If

            Catch ex As Exception

            End Try

            Return False

        End With

    End Function

    Sub UpSort(index As Integer)

        With Comptes(index)

            Try

                If CheckBoxSort(index) Then

                    Dim newSort As New FunctionSort

                    For i = 0 To .OptionSort.Count - 1

                        newSort.Up(index, .OptionSort(i))

                    Next

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

    Private Function CheckBoxSort(index As Integer) As Boolean

        With Comptes(index).FrmUser

            Try

                If .InvokeRequired Then

                    Return .Invoke(New dlgFGestion(Function() CheckBoxSort(index)))

                Else


                    Return .CheckBox_Sort_UpSort.Checked

                End If

            Catch ex As Exception

            End Try

            Return False

        End With

    End Function

#End Region

#Region "Suppression"

    Sub Supprime(index As Integer)

        With Comptes(index)

            Try

                Dim newItem As New FunctionItem

                For Each pair As CItem In CopyItem(index, .Inventaire).Values

                    If .FrmGroupe.Supprime.Contains(pair.IdObjet.ToString) OrElse .FrmGroupe.Supprime.Contains(pair.Nom.ToLower) Then

                        newItem.Supprime(index, pair.IdUnique, pair.Quantiter)

                    End If

                Next

            Catch ex As Exception

            End Try

        End With

    End Sub

#End Region

#Region "Connecté + Boucle"

    Sub Connecter(index As Integer, data As String)

        With Comptes(index)

            If .FrmUser.InvokeRequired Then

                .FrmUser.Invoke(New DlgPlayer(AddressOf Connecter), index, data)

            Else

                .FrmUser.Button_Connexion.BackgroundImage = My.Resources.Connecter
                .FrmUser.Button_Connexion.FlatAppearance.BorderColor = Color.Lime

                .FrmUser.LabelStatut.Text = "Connecté"
                .FrmUser.LabelStatut.ForeColor = Color.Lime

            End If

        End With

    End Sub

#End Region



End Class