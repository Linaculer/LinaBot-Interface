Public Class FunctionCaractéristique

    Private Delegate Function dlgCharacteristic()

    ''' <summary>
    ''' Permet de Up la caractéristique voulu.
    ''' </summary>
    ''' <param name="index">Indique le compte.</param>
    ''' <param name="caracteristique">Indique la caractéristique. Exemple : Vitalité, Sagesse, etc...</param>
    ''' <returns>Retourne 'True' s'il a Up, sinon 'False'</returns>
    Public Function Up(index As String, caracteristique As String) As Boolean

        With Comptes(index)

            Try

                If CaracteristiqueUpPossible(index, caracteristique) Then

                    Dim caracteristiqueAvant As Integer = .Caracteristique.Caracteristique(caracteristique.ToLower).Base

                    Select Case caracteristique.ToLower

                        Case "vitaliter"

                            .Send("AB11",
                                 {"BN", ' Info reçu serveur
                                  "As", ' Caractéristique reçu
                                  "ABE"}) ' Impossible de Up la caractéristique.

                        Case "sagesse"

                            .Send("AB12",
                                 {"BN", ' Info reçu serveur
                                  "As", ' Caractéristique reçu
                                  "ABE"}) ' Impossible de Up la caractéristique.

                        Case "force"

                            .Send("AB10",
                                 {"BN", ' Info reçu serveur
                                  "As", ' Caractéristique reçu
                                  "ABE"}) ' Impossible de Up la caractéristique.

                        Case "chance"

                            .Send("AB13",
                                 {"BN", ' Info reçu serveur
                                  "As", ' Caractéristique reçu
                                  "ABE"}) ' Impossible de Up la caractéristique.

                        Case "intelligence"

                            .Send("AB15",
                                 {"BN", ' Info reçu serveur
                                  "As", ' Caractéristique reçu
                                  "ABE"}) ' Impossible de Up la caractéristique.

                        Case "agilité", "agiliter"

                            .Send("AB14",
                                 {"BN", ' Info reçu serveur
                                  "As", ' Caractéristique reçu
                                  "ABE"}) ' Impossible de Up la caractéristique.

                    End Select

                    If caracteristiqueAvant < .Caracteristique.Caracteristique(caracteristique.ToLower).Base Then

                        Return True ' Le up a réussi.

                    Else

                        Return False ' Le up a échoué.

                    End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionCaracteristiqueUp", caracteristique & vbCrLf & ex.Message)

            End Try

            Return False

        End With

    End Function


    ''' <summary>
    ''' Vérifier si le personnage a assé de point pour up la caractéristique.
    ''' </summary>
    ''' <param name="index">Indique le compte.</param>
    ''' <param name="caracteristique">Indique la caractéristique à vérifier.</param>
    ''' <returns>Retourne 'True' s'il peut Up la caractéristique, sinon 'False'</returns>
    Private Function CaracteristiqueUpPossible(index As Integer, caracteristique As String) As Boolean

        With Comptes(index)

            Try

                If .FrmUser.InvokeRequired Then

                    Return .FrmUser.Invoke(New dlgCharacteristic(Function() CaracteristiqueUpPossible(index, caracteristique)))

                Else

                    If .Personnage.ClasseSexe <> Nothing Then

                        For Each pair As KeyValuePair(Of String, String()) In VarCaractéristique(VarPersonnage(CInt(.Personnage.ClasseSexe)).Nom)

                            For i = 2 To pair.Value.Count - 1

                                If Not IsNothing(caracteristique) AndAlso caracteristique.ToLower = pair.Key.ToLower Then

                                    If .Caracteristique.Caracteristique(caracteristique).Base >= Split(pair.Value(i), ">")(0) AndAlso .Caracteristique.Caracteristique(caracteristique).Base <= Split(pair.Value(i), ">")(1) Then

                                        If .Caracteristique.Capital >= Split(pair.Value(i), ">")(2) Then

                                            Return True

                                        End If

                                    End If

                                End If

                            Next

                        Next

                    End If

                    Return False

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionCaracteristiqueUpPossible", caracteristique & vbCrLf & ex.Message)

            End Try

            Return False

        End With

    End Function


    ''' <summary>
    ''' Retourne la valeur indiqué selon la caractéristique et le choix voulu.
    ''' </summary>
    ''' <param name="index">Indique le compte</param>
    ''' <param name="caracteristique">Indique la caractéristique. Exemple : Vitalité, Sagesse, etc...</param>
    ''' <param name="choix">Indique le choix : 'Base' , 'Equipement' , 'Dons' , 'Boosts' , 'Total'</param>
    ''' <returns>Retourne la valeur qui correspond à la caractéristique et au choix, sinon retourne '0'</returns>
    Public Function [Return](index As String, caracteristique As String, choix As String) As String

        With Comptes(index)

            Try

                If .FrmUser.InvokeRequired Then

                    Return .FrmUser.Invoke(New dlgCharacteristic(Function() [Return](index, caracteristique, choix)))

                Else

                    With .Caracteristique.Caracteristique(caracteristique.ToLower)

                        Select Case choix.ToLower

                            Case "base"

                                Return .Base

                            Case "equipement", "équipement"

                                Return .Equipement

                            Case "don", "dons"

                                Return .Dons

                            Case "boost", "boosts"

                                Return .Boost

                            Case "total", "totals"

                                Return .Total

                            Case Else

                                Return "0"

                        End Select

                    End With

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionCaracteristiqueReturn", caracteristique & vbCrLf & choix & vbCrLf & ex.Message)

            End Try

            Return "0"

        End With

    End Function


    ''' <summary>
    ''' Retourne l'energie du personnage.
    ''' </summary>
    ''' <param name="index">Indique le compte.</param>
    ''' <param name="choix">'Actuelle' , 'Maximum' , 'Pr' (Pr = Pourcentage)</param>
    ''' <returns>Retourne la valeur selon le choix indiqué, sinon retourne '0'.</returns>
    Public Function Energie(index As String, choix As String) As String

        With Comptes(index)

            Try

                If .FrmUser.InvokeRequired Then

                    Return .FrmUser.Invoke(New dlgCharacteristic(Function() Energie(index, choix)))

                Else

                    With .Personnage.Energie

                        Select Case choix.ToLower

                            Case "actuelle", "actuel"

                                Return .Actuelle

                            Case "maximum"

                                Return .Maximum

                            Case "pr"

                                Return .Pourcentage

                            Case Else

                                Return "0"

                        End Select

                    End With

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionCaracteristiqueEnergie", choix & vbCrLf & ex.Message)

            End Try

            Return "0"

        End With

    End Function


    ''' <summary>
    ''' Retourne le niveau du personnage.
    ''' </summary>
    ''' <param name="index">Indique le compte.</param>
    ''' <returns>Retourne le niveau actuelle du personnage, sinon retourne 0.</returns>
    Public Function Niveau(index As String) As String

        With Comptes(index)

            Try

                If .FrmUser.InvokeRequired Then

                    Return .FrmUser.Invoke(New dlgCharacteristic(Function() Niveau(index)))

                Else

                    Return .Personnage.Niveau

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionCaracteristiqueNiveau", ex.Message)

            End Try

            Return "0"

        End With

    End Function


    ''' <summary>
    ''' Retourne l'expèrience du personnage.
    ''' </summary>
    ''' <param name="index">Indique le compte.</param>
    ''' <param name="choix">'Actuelle' , 'Maximum' , 'Minimum' , 'Pr' (Pr = Pourcentage)</param>
    ''' <returns>Retourne la valeur selon le choix indiqué, sinon retourne '0'.</returns>
    Public Function Experience(index As String, choix As String) As String

        With Comptes(index)

            Try

                If .FrmUser.InvokeRequired Then

                    Return .FrmUser.Invoke(New dlgCharacteristic(Function() Experience(index, choix)))

                Else

                    With .Personnage.Experience

                        Select Case choix.ToLower

                            Case "minimum"

                                Return .Minimum

                            Case "actuelle", "actuel"

                                Return .Actuelle

                            Case "maximum"

                                Return .Maximum

                            Case "pr"

                                Return .Pourcentage

                            Case Else

                                Return "0"

                        End Select

                    End With

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, choix & vbCrLf & "FunctionCaracteristiqueExperience", ex.Message)

            End Try

            Return "0"

        End With

    End Function


    ''' <summary>
    ''' Permet d'avoir le nombre de point de vie du personnage.
    ''' </summary>
    ''' <param name="index">Indique le compte.</param>
    ''' <param name="choix">'Actuelle' , 'Maximum' , 'Pr'</param>
    ''' <returns>Retourne la valeur selon le choix indiqué, sinon retourne '0'.</returns>
    Public Function PointDeVie(index As String, choix As String) As String

        With Comptes(index)

            Try

                If .FrmUser.InvokeRequired Then

                    Return .FrmUser.Invoke(New dlgCharacteristic(Function() PointDeVie(index, choix)))

                Else

                    With .Personnage.Vitaliter

                        Select Case choix.ToLower

                            Case "actuelle", "actuel"

                                Return .Actuelle

                            Case "maximum"

                                Return .Maximum

                            Case "pr"

                                Return .Pourcentage

                            Case Else

                                Return "0"

                        End Select

                    End With

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, choix & vbCrLf & "FunctionCaracteristiquePointDeVie", choix & vbCrLf & ex.Message)

            End Try

            Return "0"

        End With

    End Function

End Class
