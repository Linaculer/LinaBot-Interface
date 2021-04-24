Module MdlSort

    Sub GiSortAjoute(index As Integer, data As String)

        With Comptes(index)

            Try

                'SL 179     ~ 5     ~ b                    ; Next Sort
                'SL Id Sort ~ Level ~ Position Bar de sort ; 

                Dim separateData As String() = Split(Mid(data, 3), ";")

                For i = 0 To separateData.Count - 2

                    Dim separate As String() = Split(separateData(i), "~")

                    Dim newSort As New CSort

                    If VarSort.ContainsKey(separate(0)) AndAlso VarSort(separate(0)).ContainsKey(separate(1)) Then

                        With newSort

                            .ID = separate(0)
                            .Niveau = separate(1)
                            .Nom = VarSort(separate(0))(separate(1)).Nom.ToLower
                            .POMinimum = VarSort(separate(0))(separate(1)).POMinimum
                            .POMaximum = VarSort(separate(0))(separate(1)).POMaximum
                            .PA = VarSort(separate(0))(separate(1)).PA
                            .NombreLancerParTour = VarSort(separate(0))(separate(1)).NombreLancerParTour
                            .NombreLancerParTourParJoueur = VarSort(separate(0))(separate(1)).NombreLancerParTourParJoueur
                            .NombreToursEntreDeuxLancers = VarSort(separate(0))(separate(1)).NombreToursEntreDeuxLancers
                            .POModifiable = VarSort(separate(0))(separate(1)).POModifiable
                            .LigneDeVue = VarSort(separate(0))(separate(1)).LigneDeVue
                            .LancerEnLigne = VarSort(separate(0))(separate(1)).LancerEnLigne
                            .CelluleLibre = VarSort(separate(0))(separate(1)).CelluleLibre
                            .ECFiniTour = VarSort(separate(0))(separate(1)).ECFiniTour
                            .ZoneMinimum = VarSort(separate(0))(separate(1)).ZoneMinimum
                            .ZoneMaximum = VarSort(separate(0))(separate(1)).ZoneMaximum
                            .ZoneEffet = VarSort(separate(0))(separate(1)).ZoneEffet
                            .NiveauRequisUp = VarSort(separate(0))(separate(1)).NiveauRequisUp
                            .SortClasse = VarSort(separate(0))(separate(1)).SortClasse
                            .Definition = VarSort(separate(0))(separate(1)).Definition.ToLower
                            .BarreSort = separate(2)

                        End With

                    End If

                    If .Sort.ContainsKey(newSort.Nom) Then

                        .Sort(newSort.Nom) = newSort

                    Else

                        .Sort.Add(newSort.Nom, newSort)

                    End If

                Next

                InterfaceGiSortAjoute(index, data)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiSortAjoute", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub InterfaceGiSortAjoute(index As Integer, data As String)

        With Comptes(index)

            Try

                If .FrmUser.InvokeRequired Then

                    .FrmUser.Invoke(New DlgPlayer(AddressOf InterfaceGiSortAjoute), index, data)

                Else

                    'SL 179     ~ 5     ~ b                    ; Next Sort
                    'SL Id Sort ~ Level ~ Position Bar de sort ; 

                    data = data.Replace("SL", "").Replace("SUK", "")

                    Dim separateData As String() = Split(data, ";")

                    For i = 0 To separateData.Count - 2

                        Dim separate As String() = Split(separateData(i), "~")

                        If VarSort.ContainsKey(separate(0)) AndAlso VarSort(separate(0)).ContainsKey(separate(1)) Then

                            If SortExiste(index, separateData(i)) = False Then

                                With .FrmUser.DataGridView_Sort

                                    .Rows.Add(False)

                                    With .Rows(.Rows.Count - 1)

                                        'ID
                                        .Cells(1).Value = separate(0)

                                        'Nom
                                        .Cells(2).Value = VarSort(separate(0))(separate(1)).Nom

                                        'Niveau
                                        .Cells(3).Value = separate(1)

                                        'Position
                                        .Cells(4).Value = Convert.ToInt64(separate(1), 16)

                                        'Up
                                        .Cells(5).Value = "Up"

                                    End With

                                    .RowsDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter
                                    .RowsDefaultCellStyle.WrapMode = DataGridViewTriState.True

                                End With

                            End If

                        End If

                    Next

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

    Private Function SortExiste(index As Integer, data As String) As Boolean

        With Comptes(index)

            Try

                If .FrmUser.InvokeRequired Then

                    Return .FrmUser.Invoke(New DlgFPlayer(AddressOf SortExiste), index, data)

                Else

                    '179     ~ 5     ~ b                    ; Next Sort
                    'Id Sort ~ Level ~ Position Bar de sort ; 

                    Dim separate As String() = Split(data, "~")

                    For Each pair As DataGridViewRow In .FrmUser.DataGridView_Sort.Rows

                        If pair.Cells(1).Value = separate(0) Then

                            pair.Cells(2).Value = VarSort(separate(0))(1).Nom
                            pair.Cells(3).Value = separate(1)
                            pair.Cells(4).Value = Convert.ToInt64(separate(2), 16)
                            pair.Cells(5).Value = "Up"

                            Return True

                        End If

                    Next

                End If

            Catch ex As Exception

            End Try

            Return False

        End With

    End Function

    Sub GiSortUp(index As Integer, data As String)

        With Comptes(index)

            ' SUK 142     ~ 4      ~ B
            ' SUK id sort ~ Niveau ~ barre de sort

            Try

                Dim separateData As String() = Split(Mid(data, 4), "~")

                Dim newSort As New CSort

                If VarSort.ContainsKey(separateData(0)) AndAlso VarSort(separateData(0)).ContainsKey(separateData(1)) Then

                    With newSort

                        .ID = separateData(0)
                        .Niveau = separateData(1)
                        .Nom = VarSort(separateData(0))(separateData(1)).Nom
                        .POMinimum = VarSort(separateData(0))(separateData(1)).POMinimum
                        .POMaximum = VarSort(separateData(0))(separateData(1)).POMaximum
                        .PA = VarSort(separateData(0))(separateData(1)).PA
                        .NombreLancerParTour = VarSort(separateData(0))(separateData(1)).NombreLancerParTour
                        .NombreLancerParTourParJoueur = VarSort(separateData(0))(separateData(1)).NombreLancerParTourParJoueur
                        .NombreToursEntreDeuxLancers = VarSort(separateData(0))(separateData(1)).NombreToursEntreDeuxLancers
                        .POModifiable = VarSort(separateData(0))(separateData(1)).POModifiable
                        .LigneDeVue = VarSort(separateData(0))(separateData(1)).LigneDeVue
                        .LancerEnLigne = VarSort(separateData(0))(separateData(1)).LancerEnLigne
                        .CelluleLibre = VarSort(separateData(0))(separateData(1)).CelluleLibre
                        .ECFiniTour = VarSort(separateData(0))(separateData(1)).ECFiniTour
                        .ZoneMinimum = VarSort(separateData(0))(separateData(1)).ZoneMinimum
                        .ZoneMaximum = VarSort(separateData(0))(separateData(1)).ZoneMaximum
                        .ZoneEffet = VarSort(separateData(0))(separateData(1)).ZoneEffet
                        .NiveauRequisUp = VarSort(separateData(0))(separateData(1)).NiveauRequisUp
                        .SortClasse = VarSort(separateData(0))(separateData(1)).SortClasse
                        .Definition = VarSort(separateData(0))(separateData(1)).Definition
                        .BarreSort = ""

                    End With

                End If

                If .Sort.ContainsKey(newSort.Nom) Then

                    .Sort(newSort.Nom) = newSort

                    EcritureMessage(index, "[Dofus]", "Le sort '" & newSort.Nom & " est désormais niveau : " & separateData(1) & ".", Color.Green)

                Else

                    .Sort.Add(newSort.Nom, newSort)

                    EcritureMessage(index, "[Dofus]", "Tu as appris le sort " & newSort.Nom, Color.Green)

                End If

                InterfaceGiSortAjoute(index, data)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiSortUp", ex.Message)

            End Try

        End With

    End Sub


End Module

#Region "Class"

Public Class CSort

    Public ID As Integer
    Public Niveau As Integer
    Public Nom As String
    Public POMinimum As Integer
    Public POMaximum As Integer
    Public PA As Integer
    Public NombreLancerParTour As Integer
    Public NombreLancerParTourParJoueur As Integer
    Public NombreToursEntreDeuxLancers As Integer
    Public POModifiable As Boolean
    Public LigneDeVue As Boolean
    Public LancerEnLigne As Boolean
    Public CelluleLibre As Boolean
    Public ECFiniTour As Boolean
    Public ZoneMinimum As Integer
    Public ZoneMaximum As Integer
    Public ZoneEffet As String
    Public NiveauRequisUp As Integer
    Public SortClasse As String
    Public Definition As String
    Public BarreSort As String

End Class

#End Region
