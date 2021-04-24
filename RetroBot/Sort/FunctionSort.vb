Public Class FunctionSort

    Public Function Up(index As Integer, nomID As String) As Boolean

        With Comptes(index)

            Try

                If IsNumeric(nomID) Then

                    nomID = VarSort(nomID)(0).Nom.ToLower

                End If

                If .Sort.ContainsKey(nomID) Then

                    If .Personnage.Niveau >= .Sort(nomID).NiveauRequisUp Then

                        If .Personnage.CapitalSort >= .Sort(nomID).Niveau Then

                            Return .Send("SB" & .Sort(nomID).ID,
                                        {"SUK"}) ' Le sort à bien Up.

                        End If

                    End If

                End If

                Return False

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionSort_Up", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function [Return](index As Integer, nomID As String, choix As String) As String

        With Comptes(index)

            Try

                If IsNumeric(nomID) Then

                    nomID = VarSort(nomID)(0).Nom.ToLower

                End If

                If .Sort.ContainsKey(nomID.ToLower) Then

                    Select Case choix.ToLower

                        Case "id"

                            Return .Sort(nomID.ToLower).ID

                        Case "niveaux du sort"

                            Return .Sort(nomID.ToLower).Niveau

                        Case "nom"

                            Return .Sort(nomID.ToLower).Nom

                        Case "po minimum"

                            Return .Sort(nomID.ToLower).POMinimum

                        Case "po maximum"

                            Return .Sort(nomID.ToLower).POMaximum

                        Case "pa"

                            Return .Sort(nomID.ToLower).PA

                        Case "nombre de lancers par tour"

                            Return .Sort(nomID.ToLower).NombreLancerParTour

                        Case "nombre de lancers par tour par joueur"

                            Return .Sort(nomID.ToLower).NombreLancerParTourParJoueur

                        Case "nombre de tours entre deux lancers"

                            Return .Sort(nomID.ToLower).NombreToursEntreDeuxLancers

                        Case "po modifiable"

                            Return .Sort(nomID.ToLower).POModifiable

                        Case "ligne de vue"

                            Return .Sort(nomID.ToLower).LigneDeVue

                        Case "lancer en ligne"

                            Return .Sort(nomID.ToLower).LancerEnLigne

                        Case "cellules libres"

                            Return .Sort(nomID.ToLower).CelluleLibre

                        Case "ec fini le tour"

                            Return .Sort(nomID.ToLower).ECFiniTour

                        Case "zone minimum"

                            Return .Sort(nomID.ToLower).ZoneMinimum

                        Case "zone maximum"

                            Return .Sort(nomID.ToLower).ZoneMaximum

                        Case "zone effet"

                            Return .Sort(nomID.ToLower).ZoneEffet

                        Case "niveau requis"

                            Return .Sort(nomID.ToLower).NiveauRequisUp

                        Case "sort classe"

                            Return .Sort(nomID.ToLower).SortClasse

                        Case "definition"

                            Return .Sort(nomID.ToLower).Definition

                        Case "barre sort"

                            Return .Sort(nomID.ToLower).BarreSort

                        Case Else

                            Return "nothing"

                    End Select

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionSort_[Return]", ex.Message)

            End Try

            Return "nothing"

        End With

    End Function

    Public Function Placement(index As Integer, nomID As String, barreSort As String) As Boolean

        With Comptes(index)

            Try

                If IsNumeric(nomID) Then

                    nomID = VarSort(nomID)(0).Nom.ToLower

                End If

                If .Sort.ContainsKey(nomID.ToLower) Then

                    Return .Send("SM" & .Sort(nomID.ToLower).ID & "|" & barreSort,
                                {"BN"}) ' Info reçu

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionSort_Placement", ex.Message)

            End Try

            Return False

        End With

    End Function

End Class
