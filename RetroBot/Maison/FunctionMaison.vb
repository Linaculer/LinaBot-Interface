Public Class FunctionMaison

    Public Function CoffreOuvrir(ByVal index As Integer)

        With Comptes(index)

            For Each Pair As CInteraction In CopyInteraction(index, .Map.Interaction).Values

                If Pair.Nom.ToLower = "coffre" Then

                    For Each PairValue As KeyValuePair(Of String, Integer) In VarInteraction(Pair.Sprite).DicoInteraction

                        If PairValue.Key.ToLower = "ouvrir" Then

                            EcritureMessage(index, "[Intéraction]", "Le bot interagit avec '" & Pair.Nom & "' et effectue l'action : " & "ouvrir", Color.Orange)

                            ._Send = "GA500" & Pair.Cellule & ";" & PairValue.Value

                            .Personnage.InteractionCellule = Pair.Cellule

                            .Map.Bloque.Reset()

                            Dim newMap As New FunctionMap
                            newMap.Deplacement(index, Pair.Cellule)

                            Return .Map.Bloque.WaitOne(15000)

                        End If

                    Next

                End If

            Next

            Return False

        End With

    End Function

    Public Function CoffreFermer(ByVal index As Integer) As Boolean

        With Comptes(index)

            If .Echange.EnEchange Then

                .Map.Bloque.Reset()

                .Send("EV")

                Return .Map.Bloque.WaitOne(15000)

            End If

        End With

    End Function

    Public Function VerificationAllMaison(index As Integer) As Boolean

        With Comptes(index)

            For Each pair As CMaison In .MaisonMap.Values

                If pair.EnVente Then
                    If pair.Cellule > 0 Then

                        ._Send = "GA500" & pair.Cellule & ";97"

                    Dim newMap As New FunctionMap
                    newMap.Deplacement(index, pair.Cellule)

                    Task.Delay(3000).Wait()

                    .Send("hV", {"hV"})

                    Task.Delay(5000).Wait()

                    End If
                End If

            Next

            Return True

        End With

    End Function

    Public Function Acheter(index As Integer, prix As String) As Boolean

        With Comptes(index)

            Try

                For Each pair As KeyValuePair(Of Integer, CMaison) In CopyMaisonMap(index, .MaisonMap)

                    If pair.Value.Prix <= CInt(prix) AndAlso .Personnage.Kamas >= pair.Value.Prix Then

                        Return .Send("hB" & pair.Value.Prix,
 _                                   ' Bonnes informations
                                     {"hP" & pair.Key & "|" & .Personnage.Pseudo, ' Indique sur la map à qui est la maison.
                                      "hL+" & pair.Key, ' Ma maison
                                      "hBK" & pair.Key}) ' Achat + prix de la maison 

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionMaison_Acheter", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Vendre(index As Integer, prix As String) As Boolean

        With Comptes(index)

            Try


                Return .Send("hS" & prix,
 _                           ' Bonnes informations
                            {"hSK"}) ' maison en vente 


            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionMaison_Vendre", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function ChangeCode(index As Integer, code As String) As Boolean

        With Comptes(index)

            Try

                'Indique aucun code
                If code = "" Then

                    code = "-"

                End If

                Return .Send("KK1|" & code,
 _                           ' Bonnes informations
                            {"KKK"}) ' Code changé 


            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionMaison_ChangeCode", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function ParametreMaisonGuilde(index As Integer) As Boolean

        With Comptes(index)

            Try

                Return .Send("hG",
 _                           ' Bonnes informations
                            {"hG" & .Maison.ID}) ' Code changé 


            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionMaison_ParametreMaisonGuilde", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function ParametreMaisonGuildeGestion(index As Integer, active As Boolean) As Boolean

        With Comptes(index)

            Try

                Return .Send("hG" & If(active = True, "+", "-"),
 _                           ' Bonnes informations
                            {"hG" & .Maison.ID & ";" & .Guilde.Nom}) ' Code changé 


            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionMaison_ParametreMaisonGuildeGestion", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function ParametreMaisonGuildeGestionDroits(index As Integer, droits As String) As Boolean

        With Comptes(index)

            Try

                Dim resultat As Integer = 0

                Dim separate As String() = Split(droits, "|")

                For i = 0 To separate.Count - 1

                    Dim separateDroits As String() = Split(separate(i), " = ")

                    Select Case separateDroits(0).ToLower

                        Case "pour les membres de la guilde"

                            resultat += 2

                        Case "pour les autres"

                            resultat += 4

                        Case "autoriser l'acces aux membres de la guilde sans code (maison)"

                            resultat += 8

                        Case "interdire l'acces aux non-membres de la guilde (maison)"

                            resultat += 16

                        Case "autoriser l'acces aux membres de la guilde sans code (coffre)"

                            resultat += 32

                        Case "interdire l'acces aux non-membres de la guilde (coffre)"

                            resultat += 64

                        Case "autoriser les membres de la guilde a se teleporter dans la maison"

                            resultat += 128

                        Case "autoriser les membres de la guilde a se reposer dans la maison"

                            resultat += 256

                    End Select

                Next

                Return .Send("hG" & resultat,
                            {"BN"})


            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionMaison_ParametreMaisonGuildeGestionDroits", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function CodePorteCoffre(index As Integer, code As String) As Boolean

        With Comptes(index)

            Try

                If .Maison.EnCode Then

                    Return .Send("KK0|" & code,
 _                           'Bonnes Informations
                            {"GDM",
                             "ECK5"},
 _                           ' Mauvaises informations
                            {"KKE", ' Code erroné
                             "Im120"}) 'Cet emplacement de stockage est déjà utilisé.

                End If


            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionMaison_CodePorteCoffre", ex.Message)

            End Try

            Return False

        End With

    End Function


End Class
