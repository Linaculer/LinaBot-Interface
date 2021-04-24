
Public Class FunctionItem

    ''' <summary>
    ''' Permet la suppression d'un item contenue dans l'inventaire du joueur.
    ''' </summary>
    ''' <param name="index">Indique le numéro bot.</param>
    ''' <param name="nomID">Nom/ID de l'item.</param>
    ''' <param name="quantite">La quantité à supprimer (de base 999.999).</param>
    ''' <param name="caracteristique">Les caractéristiques que doit avoir l'item pour être supprimer.</param>
    ''' <returns>
    ''' True = Le bot a bien supprimé l'item. <br/>
    ''' False = Le bot n'a pas réussi à supprimer l'item.
    ''' </returns>
    Public Function Supprime(index As String, nomID As String, Optional quantite As String = "999999", Optional caracteristique As String = Nothing) As Boolean

        With Comptes(index)

            Try

                For Each Pair As CItem In CopyItem(index, .Inventaire).Values

                    If Pair.Nom.ToLower = nomID.ToLower OrElse Pair.IdObjet.ToString = nomID OrElse Pair.IdUnique.ToString = nomID Then

                        If Pair.Equipement = "" Then

                            If IsNothing(caracteristique) OrElse ComparateurCaractéristiqueObjets(Pair.Caracteristique, caracteristique) Then

                                If quantite > Pair.Quantiter Then quantite = Pair.Quantiter

                                EcritureMessage(index, "(Bot)", "Suppression de l'item " & Pair.Nom & " x " & quantite, Color.Gray)

                                Return .Send("Od" & Pair.IdUnique & "|" & quantite,
                                            {"OR", ' Item supprimé.
                                             "OQ"}) ' Quantité supprimé.

                            End If

                        End If

                    End If

                Next

            Catch ex As Exception

                EcritureMessage(index, "(Bot)", "Erreur lors de la suppression d'un item." & vbCrLf &
                                                "Nom : " & nomID, Color.Red)

            End Try

            Return False

        End With

    End Function



    ''' <summary>
    ''' Permet de retirer un item contenue dans la banque/coffre/échange/HDV/Etc...
    ''' </summary>
    ''' <param name="index">Indique le numéro bot.</param>
    ''' <param name="nomID">Nom/ID de l'item.</param>
    ''' <param name="quantite">La quantité à retirer (de base 999.999).</param>
    ''' <param name="caracteristique">Les caractéristiques que doit avoir l'item pour être retiré.</param>
    ''' <returns>
    ''' True = Le bot a bien retiré l'item. <br/>
    ''' False = Le bot n'a pas réussi à retirer l'item.
    ''' </returns>
    Public Function Retire(index As String, nomID As String, Optional quantite As String = "999999", Optional caracteristique As String = Nothing) As Boolean

        With Comptes(index)

            Try

                For Each Pair As CItem In .Echange.Moi.Inventaire.Values

                    If Pair.Nom.ToLower = nomID.ToLower OrElse Pair.IdObjet.ToString = nomID OrElse Pair.IdUnique.ToString = nomID Then

                        If Pair.Equipement = "" Then

                            If IsNothing(caracteristique) OrElse ComparateurCaractéristiqueObjets(Pair.Caracteristique, caracteristique) Then

                                If quantite > Pair.Quantiter Then quantite = Pair.Quantiter

                                EcritureMessage(index, "(Bot)", "Retire l'item " & Pair.Nom & " x " & quantite, Color.Gray)

                                Return .Send("EMO-" & Pair.IdUnique & "|" & quantite,
                                            {"OQ", ' Quantité changé.
                                             "EsKO-", ' supprime item de l'échange (Banque,etc...).
                                             "EMKO-", ' Supprime item de l'échange (mon inventaire).
                                             "OAKO"}) ' Ajoute un item.

                            End If

                        End If

                    End If

                Next

            Catch ex As Exception

                EcritureMessage(index, "(Bot)", "Erreur Pour retirer l'item." & vbCrLf &
                                         "Nom : " & nomID, Color.Red)

            End Try

            Return False

        End With

    End Function



    ''' <summary>
    ''' Permet de déposer un item en Banque/Echange/Coffre/Etc...
    ''' </summary>
    ''' <param name="index">Indique le numéro bot.</param>
    ''' <param name="nomID">Nom/ID de l'item.</param>
    ''' <param name="quantite">La quantité à déposer (de base 999.999).</param>
    ''' <param name="caracteristique">Les caractéristiques que doit avoir l'item pour être déposé.</param>
    ''' <returns>
    ''' True = Le bot a bien déposé l'item. <br/>
    ''' False = Le bot n'a pas réussi à déposer l'item.
    ''' </returns>
    Public Function Depose(index As String, nomID As String, Optional quantite As String = "999999", Optional caracteristique As String = Nothing) As Boolean

        With Comptes(index)

            Try

                For Each Pair As CItem In CopyItem(index, .Inventaire).Values

                    If Pair.Nom.ToLower = nomID.ToLower OrElse Pair.IdObjet.ToString = nomID OrElse Pair.IdUnique.ToString = nomID OrElse nomID.ToLower = "all" Then

                        If Pair.Equipement = "" Then

                            If IsNothing(caracteristique) OrElse ComparateurCaractéristiqueObjets(Pair.Caracteristique, caracteristique) Then

                                If quantite > CInt(Pair.Quantiter) Then quantite = Pair.Quantiter

                                EcritureMessage(index, "(Bot)", "Dépose l'item " & Pair.Nom & " x " & quantite, Color.Gray)

                                Return .Send("EMO+" & Pair.IdUnique & "|" & quantite,
                                            {"OR", ' Supprime item inventaire.
                                             "OQ", ' Change quantité inventaire.
                                             "EMKO+", ' Item déposé en échange.
                                             "EsKO+"}) ' Dépose item (banque/coffre)

                            End If

                        End If

                    End If

                Next

            Catch ex As Exception

                EcritureMessage(index, "(Bot)", "Erreur pour déposer l'item." & vbCrLf &
                                "Nom : " & nomID, Color.Red)

            End Try

            Return False

        End With

    End Function



    ''' <summary>
    ''' Retourne un boolean si l'item existe ou non.
    ''' </summary>
    ''' <param name="index">Indique le numéro bot.</param>
    ''' <param name="nomID">Nom/ID de l'item.</param>
    ''' <param name="caracteristique">Les caractéristiques que doit avoir l'item.</param>
    ''' <returns>
    ''' True = L'item existe dans l'inventaire. <br/>
    ''' False = L'item n'existe pas dans l'inventaire.
    ''' </returns>
    Public Function Existe(index As String, nomID As String, Optional caracteristique As String = Nothing) As Boolean

        With Comptes(index)

            Try

                For Each Pair As CItem In CopyItem(index, .Inventaire).Values

                    If Pair.Nom.ToLower = nomID.ToLower OrElse Pair.IdObjet.ToString = nomID OrElse Pair.IdUnique.ToString = nomID Then

                        If IsNothing(caracteristique) OrElse ComparateurCaractéristiqueObjets(Pair.Caracteristique, caracteristique) Then

                            Return True

                        End If

                    End If

                Next

            Catch ex As Exception

                EcritureMessage(index, "(Bot)", "Erreur pour vérifier si l'item existe en inventaire." & vbCrLf &
                        "Nom : " & nomID, Color.Red)

            End Try

            Return False

        End With

    End Function



    ''' <summary>
    ''' Equipe une item selon le nom/ID et les caractéristiques.
    ''' </summary>
    ''' <param name="index">Indique le numéro bot.</param>
    ''' <param name="nomID">Nom/ID de l'item.</param>
    ''' <param name="caracteristique">Les caractéristiques que doit avoir l'item pour être equipé.</param>
    ''' <returns>
    ''' True = L'item est équipé. <br/>
    ''' False = L'item n'est pas équipé.
    ''' </returns>
    Public Function Equipe(index As String, nomID As String, Optional caracteristique As String = Nothing) As Boolean

        With Comptes(index)

            Try

                For Each Pair As CItem In CopyItem(index, .Inventaire).Values

                    If Pair.Nom.ToLower = nomID.ToLower OrElse Pair.IdUnique.ToString = nomID OrElse Pair.IdObjet.ToString = nomID Then

                        If IsNothing(caracteristique) OrElse ComparateurCaractéristiqueObjets(Pair.Caracteristique, caracteristique) Then

                            If Pair.Equipement = "" Then

                                Return .Send("OM" & Pair.IdUnique & "|" & RetourneEquipementCatégorie(index, Pair.Categorie),
                                            {"OM", ' Un objet équipé.
                                             "OCO"})

                            Else

                                Return True

                            End If

                        End If

                    End If

                Next

            Catch ex As Exception

                EcritureMessage(index, "(Bot)", "Erreur pour équiper l'item voulu." & vbCrLf &
                        "Nom : " & nomID, Color.Red)

            End Try

            Return False

        End With

    End Function



    ''' <summary>
    ''' Retourne le numéro pour savoir ou est équipé l'item.
    ''' </summary>
    ''' <param name="index">Indique le numéro bot.</param>
    ''' <param name="Categorie">La catégorie de l'item (amulette, anneaux, bottes, etc...)</param>
    ''' <returns>
    ''' Retourne le numéro pour savoir où est équipé l'item, si rien, retourne -1.
    ''' </returns>
    Private Function RetourneEquipementCatégorie(index As String, Categorie As Integer) As Integer

        With Comptes(index)

            Try

                Select Case Categorie

                    Case 1 'Amulette

                        Return 0

                    Case 5, 19, 8, 22, 7, 3, 4, 6, 20, 21, 83 'Arme

                        Return 1

                    Case 18 'Familier 

                        Return 8

                    Case 10 'Ceinture 

                        Return 3

                    Case 11 'Botte  

                        Return 5

                    Case 16 'Coiffe 

                        Return 6

                    Case 17, 81 'Cape/Sac

                        Return 7

                    Case 9 'Anneaux

                        Dim anneauxSuivant As Integer = 6

                        For Each Pair As CItem In CopyItem(index, .Inventaire).Values

                            If Pair.Categorie = 9 Then

                                If Pair.Equipement = "4" OrElse Pair.Equipement = "2" Then

                                    anneauxSuivant -= (Pair.Equipement)

                                    If anneauxSuivant = 0 Then Exit For

                                End If

                            End If

                        Next

                        Return anneauxSuivant

                    Case 23 'Dofus

                        Dim dofusSuivant As Integer() = {9, 10, 11, 12, 13, 14}

                        For Each Pair As CItem In CopyItem(index, .Inventaire).Values

                            If Pair.Categorie = 23 Then

                                If Pair.Equipement = "9" Then

                                    dofusSuivant(0) = 0

                                End If

                                If Pair.Equipement = "10" Then

                                    dofusSuivant(1) = 0

                                End If

                                If Pair.Equipement = "11" Then

                                    dofusSuivant(2) = 0

                                End If

                                If Pair.Equipement = "12" Then

                                    dofusSuivant(3) = 0

                                End If

                                If Pair.Equipement = "13" Then

                                    dofusSuivant(4) = 0

                                End If

                                If Pair.Equipement = "14" Then

                                    dofusSuivant(5) = 0

                                End If

                            End If

                        Next

                        For i = 0 To dofusSuivant.Count - 1

                            If dofusSuivant(i) > 0 Then

                                Return dofusSuivant(i)

                            End If

                        Next

                    Case Else

                        Return 8

                End Select

            Catch ex As Exception
            End Try

            Return -1

        End With

    End Function



    ''' <summary>
    ''' Déséquipe un item.
    ''' </summary>
    ''' <param name="index">Indique le numéro bot.</param>
    ''' <param name="nomID">Nom/ID de l'item.</param>
    ''' <param name="caracteristique">Les caractéristiques que doit avoir l'item pour être déséquipé.</param>
    ''' <returns>
    ''' True = L'item est déséquipé. <br/>
    ''' False = L'item n'est pas déséquipé.
    ''' </returns>
    Public Function Desequipe(index As String, nomID As String, Optional caracteristique As String = Nothing) As Boolean

        With Comptes(index)

            Try

                For Each Pair As CItem In CopyItem(index, .Inventaire).Values

                    If Pair.Nom.ToLower = nomID.ToLower OrElse Pair.IdUnique.ToString = nomID OrElse Pair.IdObjet.ToString = nomID Then

                        If Pair.Equipement <> "" Then

                            If IsNothing(caracteristique) OrElse ComparateurCaractéristiqueObjets(Pair.Caracteristique, caracteristique) Then

                                EcritureMessage(index, "(Bot)", "Déséquipe item : " & Pair.Nom, Color.Gray)

                                Return .Send("OM" & Pair.IdUnique & "|-1",
                                            {"OM"}) ' Item Déséquipé.

                            End If

                        End If

                    End If

                Next

            Catch ex As Exception

                EcritureMessage(index, "(Bot)", "Erreur pour déséquiper l'item voulu." & vbCrLf &
                        "Nom : " & nomID, Color.Red)

            End Try

            Return False

        End With

    End Function



    ''' <summary>
    ''' Jétte un objet au sol.
    ''' </summary>
    ''' <param name="index">Indique le numéro bot.</param>
    ''' <param name="nomID">Nom/ID de l'item.</param>
    ''' <param name="quantité">La quantité à jeter (de base 999.999).</param>
    ''' <param name="caractéristique">Les caractéristiques que doit avoir l'item pour être jeté.</param>
    ''' <returns>
    ''' True = Le bot a bien jeté l'item. <br/>
    ''' False = Le bot n'a pas réussi à jeter l'item.
    ''' </returns>
    Public Function Jette(index As String, nomID As String, Optional quantité As String = "999999", Optional caractéristique As String = Nothing) As Boolean

        With Comptes(index)

            Try

                For Each Pair As CItem In CopyItem(index, .Inventaire).Values

                    If Pair.Nom.ToLower = nomID.ToLower OrElse Pair.IdObjet.ToString = nomID OrElse Pair.IdUnique.ToString = nomID Then

                        If Pair.Equipement = "" Then

                            If IsNothing(caractéristique) OrElse ComparateurCaractéristiqueObjets(Pair.Caracteristique, caractéristique) Then

                                If quantité > Pair.Quantiter Then quantité = Pair.Quantiter

                                EcritureMessage(index, "(Bot)", "Jette l'item " & Pair.Nom & " x " & quantité, Color.Gray)

                                Return .Send("OD" & Pair.IdUnique & "|" & quantité,
                                            {"OR", ' Supprime item.
                                             "OQ", ' Change quantité.
                                             "GDO+"}) ' Dépose au sol.

                            End If

                        End If

                    End If

                Next

            Catch ex As Exception

                EcritureMessage(index, "(Bot)", "Erreur pour jeter l'item voulu." & vbCrLf &
                        "Nom : " & nomID, Color.Red)

            End Try

            Return False

        End With

    End Function



    ''' <summary>
    ''' Utilise un objet de l'inventaire.
    ''' </summary>
    ''' <param name="index">Indique le numéro bot.</param>
    ''' <param name="nomID">Nom/ID de l'item.</param>
    ''' <returns>
    ''' True = Le bot a bien utilisé l'item. <br/>
    ''' False = Le bot n'a pas réussi à utiliser l'item.
    ''' </returns>
    Public Function Utilise(index As String, nomID As String) As Boolean

        With Comptes(index)

            Try

                For Each Pair As CItem In CopyItem(index, .Inventaire).Values

                    If Pair.Nom.ToLower = nomID.ToLower OrElse Pair.IdObjet.ToString = nomID OrElse Pair.IdUnique.ToString = nomID Then

                        EcritureMessage(index, "(Bot)", "Utilise l'item " & Pair.Nom, Color.Gray)

                        Return .Send("OU" & Pair.IdUnique & "|",
                                    {"OQ", ' change quantité.
                                     "OR", ' Suppime item
                                     "GDM"}) ' Change Map

                    End If

                Next

            Catch ex As Exception

                EcritureMessage(index, "(Bot)", "Erreur pour utiliser l'item voulu." & vbCrLf &
                        "Nom : " & nomID, Color.Red)

            End Try

            Return False

        End With

    End Function

End Class
