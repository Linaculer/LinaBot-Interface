Public Module Tchat

    Public Function CanalActiveDesactive(index As Integer, canal As String, choix As String) As Boolean

        With Comptes(index)

            Try

                If .Connecté Then

                    Dim envoyer As String = "cC" & If(CBool(choix) = True, "+", "-")

                    Select Case canal.ToLower

                        Case "information"

                            Return .Send(envoyer & "i",
 _                                       ' Bonnes Informations
                                         {If(envoyer = "cC+", "cC+i", "cC-i")})

                        Case "communs", "commun"

                            Return .Send(envoyer & "*",
 _                                       ' Bonnes Informations
                                         {If(envoyer = "cC+", "cC+*", "cC-*")})

                        Case "groupe"

                            Return .Send(envoyer & "#$p",
 _                                       ' Bonnes Informations
                                         {If(envoyer = "cC+", "cC+#$p", "cC-#$p")})

                        Case "guilde"

                            Return .Send(envoyer & "%",
 _                                       ' Bonnes Informations
                                         {If(envoyer = "cC+", "cC+%", "cC-%")})

                        Case "alignement"

                            Return .Send(envoyer & "!",
 _                                       ' Bonnes Informations
                                         {If(envoyer = "cC+", "cC+!", "cC-!")})

                        Case "recrutement"

                            Return .Send(envoyer & "?",
 _                                       ' Bonnes Informations
                                         {If(envoyer = "cC+", "cC+?", "cC-?")})

                        Case "commerce"

                            Return .Send(envoyer & ":",
 _                                       ' Bonnes Informations
                                         {If(envoyer = "cC+", "cC+:", "cC-:")})

                        Case Else

                            Return False

                    End Select

                End If

            Catch ex As Exception

            End Try

            Return False

        End With

    End Function



    Public Function CanalEnvoieMessage(index As Integer, message As String) As Boolean

        With Comptes(index)

            Try

                If .Connecté Then

                    If message <> "" Then

                        Dim canal As String = Split(message, " ")(0)

                        message = message.Replace(canal & " ", "")

                        Select Case canal.ToLower

                            Case "/c" ' Communs

                                Return .Send("BM*|" & message & "|",
 _                                           ' Bonnes Informations
                                             {"cMK|" & .Personnage.ID},
 _                                           ' Mauvaises informations
                                             {"BN"})

                            Case "/w" ' Message Privée

                                Dim joueur As String = Split(message, " ")(0)
                                Return .Send("BM" & joueur & "|" & message.Replace(joueur & " ", "") & "|",
 _                                           ' Bonnes Informations
                                             {"cMKT" & .Personnage.ID},
 _                                           ' Mauvaises informations
                                             {"BN",
                                              "cMEf"}) ' Le joueur X n'est pas connecté.

                            Case "/p" ' Groupe

                                Return .Send("BM$|" & message & "|",
 _                                           ' Bonnes Informations
                                             {"cMK$" & .Personnage.ID},
 _                                           ' Mauvaises informations
                                             {"BN"})

                            Case "/g" ' Guilde

                                Return .Send("BM%|" & message & "|",
 _                                           ' Bonnes Informations
                                             {"cMK%" & .Personnage.ID},
 _                                           ' Mauvaises informations
                                             {"BN"})

                            Case "/r" ' Recrutement

                                Return .Send("BM?|" & message & "|",
 _                                           ' Bonnes Informations
                                             {"cMK?" & .Personnage.ID},
 _                                           ' Mauvaises informations
                                             {"BN"})

                            Case "/b" ' Commerce

                                Return .Send("BM:|" & message & "|",
 _                                           ' Bonnes Informations
                                             {"cMK:" & .Personnage.ID},
 _                                           ' Mauvaises informations
                                             {"BN"})

                            Case "/a" ' Alignement

                                Return .Send("BM!|" & message & "|",
 _                                           ' Bonnes Informations
                                             {"cMK!" & .Personnage.ID},
 _                                           ' Mauvaises informations
                                             {"BN",
                                              "cMEA", ' Impossible d'utilise ce canal
                                              "Im0106", 'Alignement pas asse important.
                                              "Im0115;"}) ' Il faut attendre avant de pouvoir remettre un message.

                            Case Else

                                Return False

                        End Select

                    End If

                End If

            Catch ex As Exception

            End Try

            Return False

        End With

    End Function

End Module
