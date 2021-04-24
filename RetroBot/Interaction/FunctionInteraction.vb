Public Class FunctionInteraction

    ''' <summary>
    ''' Permet de faire une interaction avec un élément de la map.
    ''' </summary>
    ''' <param name="index">Le numéro du bot.</param>
    ''' <param name="nomInteraction">Lr nom de l'interaction, exemple : "Statue de Classe"</param>
    ''' <param name="action">Le nom de l'action à faire , exemple : "Se teleporter à Incarnam"</param>
    ''' <param name="cellule">La cellule visé, si rien, le bot prend l'interaction voulu qu'il trouve en premier.</param>
    ''' <returns>
    ''' True = Le bot a bien effectué l'interaction voulu. <br/>
    ''' False = Le bot n'a pas réussi à faire l'interaction demandé.
    ''' </returns>
    Public Function Interaction(index As String, nomInteraction As String, action As String, Optional cellule As String = "") As Boolean

        With Comptes(index)

            Try

                For Each Pair As CInteraction In CopyInteraction(index, .Map.Interaction).Values

                    If Pair.Nom.ToLower = nomInteraction.ToLower Then

                        If Pair.Information = "disponible" Then

                            For Each PairValue As KeyValuePair(Of String, Integer) In VarInteraction(Pair.Sprite).DicoInteraction

                                If PairValue.Key.ToLower = action.ToLower Then

                                    If cellule = "" Then cellule = Pair.Cellule

                                    Return .Send("GA500" & cellule & ";" & PairValue.Value,
 _                                               'Bonnes informations
                                                 {
                                                 "GA1;500;" & .Personnage.ID.ToString, ' Recolte
                                                 "GA0;500;" & .Personnage.ID.ToString, ' Recolte
                                                 "GDF|" & cellule & ";2;0", ' En Utilisation (Utile pour recolte)          
                                                 "Wc", ' Zaapi
                                                 "WC", ' Zaap
                                                 "Autre"
                                                 },
 _                                               'Mauvaises informations
                                                 {
                                                 "GDF|" & cellule & ";3;0", ' Indisponible (Utile pour recolte)
                                                 "GDF|" & cellule & ";4;0", ' Indisponible (Utile pour recolte)
                                                 "BN",
                                                 "GA;0"})

                                End If

                            Next

                        End If

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionInteraction_Interaction", ex.Message)

            End Try

            Return False

        End With

    End Function


    ''' <summary>
    ''' Quitte l'interaction en cours.
    ''' </summary>
    ''' <param name="index">Le numéro du bot.</param>
    ''' <param name="choix">Le nom de l'interaction à quitter, exemple : "Zaap"</param>
    ''' <returns>
    ''' True = Le bot a bien quitté l'interaction <br/>
    ''' False = Le bot n'a pas réussie à quitter l'interaction en cours.
    ''' </returns>
    Public Function Quitte(index As Integer, choix As String) As Boolean

        With Comptes(index)

            Try

                Select Case choix.ToLower

                    Case Else

                        Return .Send("EV",
                                    {"EV"}) ' Bonnes informations

                End Select

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionInteraction_Quitte", ex.Message)

            End Try

            Return False

        End With

    End Function

End Class
