Public Class FunctionZaap

    Public Function Utiliser(index As Integer) As Boolean

        With Comptes(index)

            Try

                For Each pair As CInteraction In CopyInteraction(index, .Map.Interaction).Values

                    If pair.Nom = "Zaap" Then


                        Dim path As New Pathfinding(index)
                        Dim chemin As String = path.pathing(pair.Cellule)

                        If chemin <> "" Then

                            .Send("GA001" & chemin,
                                 {"GA;0", 'Le déplacement a échoué.
                                  "GA0;1;" & .Personnage.ID, 'Indique que je suis en plein déplacement.
                                  "GDM", 'Indique un changement de map.
                                  "GA;905;"}) 'Je suis entré en combat.

                        End If

                        Return .Send("GA500" & pair.Cellule & ";114",
                                    {"WC", ' Liste des zaaps
                                     "Im024"}) ' Tu viens de mémoriser un nouveau zaap.

                        Exit For

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionZaap_Utiliser", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Sauvegarder(index As Integer) As Boolean

        With Comptes(index)

            Try

                For Each pair As CInteraction In CopyInteraction(index, .Map.Interaction).Values

                    If pair.Nom = "Zaap" Then

                        Dim Path As New Pathfinding(index)
                        Dim chemin As String = Path.pathing(pair.Cellule)

                        If chemin <> "" Then

                            .Send("GA001" & chemin,
                                 {"GA;0", 'Le déplacement a échoué.
                                  "GA0;1;" & .Personnage.ID, 'Indique que je suis en plein déplacement.
                                  "GDM", 'Indique un changement de map.
                                  "GA;905;"}) 'Je suis entré en combat.

                        End If

                        Return .Send("GA500" & pair.Cellule & ";44",
                                    {"Im06"}) ' Position sauvegardée.

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionZaap_Sauvegarder", ex.Message)

            End Try

            Return False

        End With


    End Function

    Public Function Destination(index As Integer, map As String) As Boolean

        With Comptes(index)

            Try

                If .Personnage.EnInteraction Then

                    If Not IsNumeric(map) Then

                        For Each pair As KeyValuePair(Of Integer, String) In VarMap

                            If pair.Value = map Then

                                If .ZaapI.ContainsKey(pair.Key) Then

                                    map = pair.Key

                                    Exit For

                                End If

                            End If

                        Next

                    End If

                    If .ZaapI.ContainsKey(map) Then

                        If .Personnage.Kamas >= .ZaapI(map) Then

                            Return .Send("WU" & map,
                                        {"GDM", ' Change de Map.
                                         "WV"}) ' Fin d'utilisation du zaap.

                        Else

                            EcritureMessage(index, "(Bot)", "Vous avez pas asse de kamas pour utiliser le zaap, kamas requis : " & .ZaapI(map), Color.Red)

                        End If

                    Else

                        EcritureMessage(index, "(Bot)", "Le bot n'a pas trouvé la map voulu dans les maps enregistré du Zaap.", Color.Red)

                    End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionZaap_Destination", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Quitte(index As Integer) As Boolean

        With Comptes(index)

            Try

                If .Personnage.EnInteraction Then

                    Return .Send("WV",
                                {"WV"}) ' Fin d'utilisation du zaap.

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionZaap_Quitte", ex.Message)

            End Try

            Return False

        End With

    End Function

End Class
