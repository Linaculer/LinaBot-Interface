Module mdlInventaire

#Region "Réception Inventaire"

    Sub GiInventaire(index As Integer, data As String)

        With Comptes(index)

            Try

                If .FrmUser.InvokeRequired Then

                    .FrmUser.Invoke(New DlgPlayer(AddressOf GiInventaire), index, data)

                Else

                    ' ASK | 1234567   | Linaculer  | 99    | 9         | 0       | 90            | -1       | -1       | -1       | 262c1bc        ~ 241      ~ 1         ~ 1                 ~ 64#2#4#0#1d3+1  , 7d#1#0#0#0d0+1 ; Next Item
                    ' ASK | ID Joueur | Nom Joueur | Level | Id Classe | Id Sexe | Classe + Sexe | Couleur1 | Couleur2 | Couleur3 | Id Unique Item ~ Id Objet ~ Quantity  ~ Number equipment  ~ Caractéristique , Caract Next    ; Item suivent

                    .FrmUser.LabelStatut.Text = "Connecté"
                    .FrmUser.LabelStatut.ForeColor = Color.Lime

                    Dim separateData As String() = Split(data, "|")

                    ' Information
                    .Personnage.ID = separateData(1)
                    .Personnage.NomDuPersonnage = separateData(2)
                    .Personnage.Niveau = separateData(3)
                    .Personnage.Classe = separateData(4)
                    .Personnage.Sexe = separateData(5)
                    .Personnage.ClasseSexe = separateData(6)

                    ' .Personnage.Couleur1 = "&H" & separateData(7)
                    ' .Personnage.Couleur2 = "&H" & separateData(8)
                    ' .Personnage.Couleur3 = "&H" & separateData(9)
                    ' /Information

                    EcritureMessage(index, "[Dofus]", "Connecté au personnage '" & separateData(2) & "' (Niveau : " & separateData(3) & ")", Color.Green)
                    EcritureMessage(index, "[Dofus]", "Réception de l'inventaire.", Color.Green)

                    .Inventaire.Clear()

                    GiItemAjoute(index, separateData(10), .Inventaire)

                    InterfaceGiInventaire(index, data)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiInventaire", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Private Sub InterfaceGiInventaire(index As Integer, data As String)

        With Comptes(index).FrmUser

            Try

                If .InvokeRequired Then

                    .Invoke(New DlgPlayer(AddressOf InterfaceGiInventaire), index, data)

                Else

                    ' ASK | 1234567   | Linaculer  | 99    | 9         | 0       | 90            | -1       | -1       | -1       | 262c1bc        ~ 241      ~ 1         ~ 1                 ~ 64#2#4#0#1d3+1  , 7d#1#0#0#0d0+1 ; Next Item
                    ' ASK | ID Joueur | Nom Joueur | Level | Id Classe | Id Sexe | Classe + Sexe | Couleur1 | Couleur2 | Couleur3 | Id Unique Item ~ Id Objet ~ Quantity  ~ Number equipment  ~ Caractéristique , Caract Next    ; Item suivent

                    Dim separateData As String() = Split(data, "|")

                    .DataGridView_Inventaire.Rows.Clear()

                    If separateData(10) <> "" Then

                        separateData = Split(separateData(10), ";")

                        For i = 0 To separateData.Count - 2

                            Dim separateItem As String() = Split(separateData(i), "~")

                            Try

                                With .DataGridView_Inventaire

                                    .Rows.Add(False)

                                    With .Rows(.Rows.Count - 1)

                                        'ID
                                        .Cells(1).Value = Convert.ToInt64(separateItem(1), 16)

                                        'ID Unique
                                        .Cells(2).Value = Convert.ToInt64(separateItem(0), 16)

                                        'Nom
                                        .Cells(3).Value = VarItems(Convert.ToInt64(separateItem(1), 16)).Nom

                                        'Quantité
                                        .Cells(4).Value = Convert.ToInt64(separateItem(2), 16)

                                        If separateItem(3) <> "" Then

                                            .DefaultCellStyle.BackColor = Color.Lime

                                        ElseIf VarItems(Convert.ToInt64(separateItem(1), 16)).Catégorie = "24" Then

                                            .DefaultCellStyle.BackColor = Color.Orange

                                        End If

                                    End With

                                    .RowsDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
                                    .RowsDefaultCellStyle.WrapMode = DataGridViewTriState.True

                                End With

                            Catch ex As Exception

                            End Try

                        Next

                    End If

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

#End Region

#Region "Suppression"

    Sub GiInventaireItemSupprime(index As Integer, data As String)

        With Comptes(index)

            Try

                ' OR 55156977
                ' OR id Unique

                If .Inventaire.ContainsKey(Mid(data, 3)) Then

                    .Inventaire.Remove(Mid(data, 3))

                End If

                InterfaceGiInventaireItemSupprime(index, data)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiInventaireItemSupprime", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Private Sub InterfaceGiInventaireItemSupprime(index As Integer, data As String)

        With Comptes(index).FrmUser

            Try

                If .InvokeRequired Then

                    .Invoke(New DlgPlayer(AddressOf InterfaceGiInventaireItemSupprime), index, data)

                Else
                    ' OR 55156977
                    ' OR id Unique

                    data = Mid(data, 3)

                    For Each pair As DataGridViewRow In .DataGridView_Inventaire.Rows

                        If pair.Cells(2).Value = data Then

                            .DataGridView_Inventaire.Rows.RemoveAt(pair.Index)

                            Exit Sub

                        End If

                    Next

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

#End Region

#Region "Quantite"

    Sub GiInventaireQuantite(index As Integer, data As String)

        With Comptes(index)

            Try

                ' OQ 55259212  | 699
                ' OQ Id Unique | Quantité

                Dim separateData As String() = Split(Mid(data, 3), "|")

                If .Inventaire.ContainsKey(separateData(0)) Then

                    .Inventaire(separateData(0)).Quantiter = separateData(1)

                End If

                InterfaceGiInventaireQuantite(index, data)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiInventaireQuantite", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Private Sub InterfaceGiInventaireQuantite(index As Integer, data As String)

        With Comptes(index).FrmUser

            Try

                If .InvokeRequired Then

                    .Invoke(New DlgPlayer(AddressOf InterfaceGiInventaireQuantite), index, data)

                Else

                    ' OQ 55259212  | 699
                    ' OQ Id Unique | Quantité

                    Dim separateData As String() = Split(Mid(data, 3), "|")

                    For Each pair As DataGridViewRow In .DataGridView_Inventaire.Rows

                        If pair.Cells(2).Value = data Then

                            pair.Cells(4).Value = separateData(1)

                            Exit Sub

                        End If

                    Next

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

#End Region

#Region "Equipement"

    Sub GiEquipement(index As Integer, data As String)

        With Comptes(index)

            Try

                ' OM 123515576 | 7
                ' OM id unique | Numéro équipement

                Dim separateData As String() = Split(Mid(data, 3), "|")

                If .Inventaire.ContainsKey(separateData(0)) Then

                    With .Inventaire(separateData(0))

                        If separateData(1) <> Nothing Then

                            .Equipement = separateData(1)

                            EcritureMessage(index, "(Equipement)", "Vous avez équipé l'item : " & .Nom, Color.Gray)

                        Else

                            .Equipement = ""

                            EcritureMessage(index, "(Equipement)", "Vous avez déséquipé l'item : " & .Nom, Color.Gray)

                        End If

                    End With

                End If

                InterfaceGiEquipement(index, data)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEquipement", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Private Sub InterfaceGiEquipement(index As Integer, data As String)

        With Comptes(index)

            Try

                With .FrmUser

                    If .InvokeRequired Then

                        .Invoke(New DlgPlayer(AddressOf InterfaceGiEquipement), index, data)

                    Else

                        ' OM 123515576 | 7
                        ' OM id unique | Numéro équipement

                        Dim separateData As String() = Split(Mid(data, 3), "|")

                        For Each pair As DataGridViewRow In .DataGridView_Inventaire.Rows

                            If pair.Cells(2).Value = separateData(0) Then

                                If separateData(1) <> Nothing Then

                                    pair.DefaultCellStyle.BackColor = Color.Lime

                                Else

                                    pair.DefaultCellStyle.BackColor = Color.FromArgb(43, 44, 48)

                                End If

                            End If

                        Next

                    End If

                End With

            Catch ex As Exception

            End Try

        End With

    End Sub

#End Region

    Sub GiInventaireCaracteristiqueChange(index As Integer, data As String)

        With Comptes(index)

            Try

                ' OCO 4a239fd  ~ 1f40    ~ 1        ~ 8                 ~ 320#5#48#9,328#28a#1f5#466,326#0#0#48,327#0#0#18a,9e#2da#0#0#0d0+730 ; 
                ' OCO idUnique ~ IdObjet ~ Quantité ~ Numéro Equipement ~ Caractéristique                                                      ; Next item

                data = Mid(data, 4)

                Dim separateData As String() = Split(data, ";")

                For i = 0 To separateData.Count - 1

                    If separateData(i) <> "" Then

                        Dim separateItem As String() = Split(separateData(i), "~")

                        Dim IdUnique As String = Convert.ToInt64(separateItem(0), 16)

                        If .Inventaire.ContainsKey(IdUnique) Then

                            With .Inventaire(IdUnique)

                                'quantity
                                .Quantiter = Convert.ToInt64(separateItem(2), 16)

                                'Caractéristique
                                .Caracteristique = ItemCaracteristique(separateItem(4), .IdObjet)
                                .CaracteristiqueBrute = separateItem(4)

                                If separateItem(3) <> "" Then

                                    .Equipement = Convert.ToInt64(separateItem(3), 16)

                                ElseIf VarItems(Convert.ToInt64(separateItem(1), 16)).Catégorie = "24" Then

                                    .Equipement = "24"

                                Else

                                    .Equipement = ""

                                End If

                            End With

                        End If

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiInventaireCaracteristiqueChange", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiEquipementMetier(index As Integer, data As String)

        With Comptes(index)

            Try

                ' OT 28
                ' OT Id Métier

                If data.Length > 2 Then

                    If .Metier.ContainsKey(Mid(data, 3)) Then

                        .Metier(Mid(data, 3)).ItemEquipe = True

                    End If

                Else

                    For Each pair As CMetier In .Metier.Values

                        pair.ItemEquipe = False

                    Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEquipementMetier", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

End Module

#Region "Class"

Public Class CBonusEquipement

    Public NumeroPanoplie As Integer
    Public IDObjet As String()
    Public Caracteristique As CItemCaractéristique
    Public CaracteristiqueBrute As String

End Class

#End Region