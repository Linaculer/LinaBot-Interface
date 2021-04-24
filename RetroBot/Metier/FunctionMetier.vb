Public Class FunctionMetier

    ''' <summary>
    ''' Change les options des métiers, payant, gratuit, etc.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="nomID">Le nom ou l'ID du métier.</param>
    ''' <param name="payant">True = payant. <br/> False = non payant.</param>
    ''' <param name="gratuitSurEchec">True = Gratuit si échec. <br/> False = Payant même si échec.</param>
    ''' <param name="neFournitAucuneRessource">True = Fournit aucune ressource pour le craft. <br/> False = peut fournir des ressources pour le craft..</param>
    ''' <param name="nombreIngredientMinimum">Le nombre d'ingrédient minimum pour effectuer un craft.</param>
    ''' <returns>
    ''' True = Option bien changé. <br/>
    ''' False = Option non changé.
    ''' </returns>
    Public Function [Option](index As Integer, nomID As String, payant As Boolean, gratuitSurEchec As Boolean, neFournitAucuneRessource As Boolean, nombreIngredientMinimum As Integer) As Boolean

        With Comptes(index)

            Try

                Dim numeroMetier As Integer = 0
                Dim NbrOption As Integer

                For Each pair As CMetier In .Metier.Values

                    If pair.Nom.ToLower = nomID.ToLower OrElse pair.ID = nomID Then

                        If payant Then

                            NbrOption += 1

                        End If

                        If gratuitSurEchec Then

                            NbrOption += 2

                        End If

                        If neFournitAucuneRessource Then

                            NbrOption += 4

                        End If

                        Return .Send("JO" & numeroMetier & "|" & NbrOption & "|" & nombreIngredientMinimum,
                                    {"JO" & numeroMetier}) ' Changement des options du métier.

                    End If

                    numeroMetier += 1

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionMetier_[Option]", ex.Message)

            End Try

            Return False

        End With

    End Function



    ''' <summary>
    ''' Active/Désactive le mode public.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="activer">
    ''' True = Activer le mode Public (tout le monde peut vous voir). <br/> 
    ''' False = Désactive le mode public (personne vous voit).
    ''' </param>
    ''' <returns>
    ''' True = l'action a réussie. <br/>
    ''' False = l'action a échoué.
    ''' </returns>
    Public Function [Public](index As Integer, activer As Boolean) As Boolean

        With Comptes(index)

            Try

                If activer Then

                    Return .Send("EW+",
                                {"EW+"}) ' Mode public activé.

                Else

                    Return .Send("EW-",
                                {"EW-"}) ' Mode public désactivé.

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionMetier_Public", ex.Message)

            End Try

            Return False

        End With

    End Function



    Public Function Existe(index As Integer, nomID As String) As Boolean

        With Comptes(index)

            Try

                For Each pair As CMetier In .Metier.Values

                    If pair.Nom.ToLower = nomID.ToLower OrElse pair.ID = nomID Then

                        Return True

                    End If

                Next

            Catch ex As Exception

            End Try

            Return False

        End With

    End Function



    Public Function Niveau(index As Integer, nomID As String) As Integer

        With Comptes(index)

            Try

                For Each pair As CMetier In .Metier.Values

                If pair.Nom.ToLower = nomID.ToLower OrElse pair.ID = nomID Then

                    Return pair.Niveau

                End If

            Next

            Catch ex As Exception

            End Try

            Return 0

        End With

    End Function

End Class
