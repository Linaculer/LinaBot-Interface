Partial Module IntelligenceArtificielle

    Private Function Avancer(index As Integer) As Boolean

        'Args(0) = Index

        With Comptes(index)

            Try

                If 3 > 0 Then

                    Dim distanceCellule As Integer = 999
                    Dim mobsProche As Integer

                    For Each pair As CEntite In .Map.Entite.Values

                        If pair.IDCategorie <= 0 Then

                            If goalDistance(.Map.Entite(.Personnage.ID).Cellule, pair.Cellule, .Map.Largeur) < distanceCellule Then

                                distanceCellule = goalDistance(.Map.Entite(.Personnage.ID).Cellule, pair.Cellule, .Map.Largeur)
                                mobsProche = pair.Cellule

                            End If

                        End If

                    Next

                    Dim pather As New Pathfinding(index)
                    Dim path As String = pather.pathing(mobsProche, False, True, 3)

                    If distanceCellule > 1 Then

                        If path <> "" Then


                            .Send("GA001" & path,
 _                                   ' Bonnes informations    
                                     {
                                     "GA0;1;" & .Personnage.ID.ToString, ' En Déplacement' En Déplacement
                                     "GA;1;" & .Personnage.ID.ToString,
                                     "GAF"
                                     },
 _                                   ' Mauvaises informations                 
                                     {
                                     "GA;0" 'Le déplacement a échoué.
                                     })


                            Return .Map.Bloque.WaitOne(30000)
                            Task.Delay(1000).Wait()
                        End If

                    End If

                End If

            Catch ex As Exception

            End Try

            Return False

        End With

    End Function

End Module
