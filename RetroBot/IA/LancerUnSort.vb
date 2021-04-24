Partial Module IntelligenceArtificielle

    Private Function LancerUnSort(index As Integer, idSort As String) As Boolean

        With Comptes(index)

            Try

                Dim meilleurDistance As Integer = 999
                Dim meilleurCellule As Integer = 0

                For Each pair As CEntite In .Map.Entite.Values

                    If pair.IDUnique < 0 AndAlso .Combat.Entite(pair.IDUnique).Vivant Then

                        If goalDistance(.Map.Entite(.Personnage.ID).Cellule, pair.Cellule, .Map.Largeur) < meilleurDistance Then

                            meilleurDistance = goalDistance(.Map.Entite(.Personnage.ID).Cellule, pair.Cellule, .Map.Largeur)
                                meilleurCellule = pair.Cellule

                        End If

                    End If

                Next

                idSort = ReturnIDSort(index, idSort)

                AddBarreSort(index, idSort)

                If goalDistance(.Map.Entite(.Personnage.ID).Cellule, meilleurCellule, .Map.Largeur) < 11 Then

                    .Combat.Bloque.Reset()

                    .Send("GA300" & idSort & ";" & meilleurCellule)

                    .Combat.Bloque.WaitOne(15000)

                    Task.Delay(1000).Wait()

                End If
            Catch ex As Exception

            End Try

            Return False

        End With

    End Function

    Public Sub passetour(index As Integer)

        With Comptes(index)

            If .Combat.EnCombat Then

                .Send("GT")
                .Send("Gt")

            End If

        End With

    End Sub

    Private Function ReturnIDSort(index As Integer, sort As String) As Integer

        With Comptes(index)

            For Each pair As Dictionary(Of Integer, CSort) In VarSort.Values

                If pair.Values(0).ID.ToString = sort OrElse pair.Values(0).Nom.ToLower = sort.ToLower Then

                    Return pair.Values(0).ID

                End If

            Next

        End With

        Return 0

    End Function

    Private Function AddBarreSort(index As Integer, sort As String) As Boolean

        With Comptes(index)

            For Each pair As KeyValuePair(Of String, CSort) In .Sort

                If pair.Value.Nom.ToLower = sort.ToLower OrElse pair.Value.ID.ToString = sort Then

                    If pair.Value.BarreSort = "_" Then

                        Return .Send("SM" & pair.Value.ID & "|1",
                                    {"BN"})

                    End If

                End If

            Next

        End With

        Return True

    End Function
End Module
