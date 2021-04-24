Public Class FunctionPnj

#Region "Parler"


    ''' <summary>
    ''' Parler a un pnj présent sur la map.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="nomID">Le nom ou l'ID du Pnj a parler.</param>
    ''' <returns>
    ''' True = Si le bot est en dialogue. <br/>
    ''' False = Le bot n'a pas réussi à parler au Pnj.
    ''' </returns>
    Public Function Parler(index As String, nomID As String) As Boolean

        With Comptes(index)

            Try

                If .Pnj.EnParler = False Then
                    For Each Pair As CEntite In CopyMap(index, .Map.Entite).Values

                        If Pair.Nom.ToLower = nomID.ToLower OrElse Pair.ID = nomID Then

                            .Send("DC" & Pair.IDUnique,
                                 {"DCK", ' En dialogue
                                  "DCE"}) ' Déjà en dialogue.

                            Exit For

                        End If

                    Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Parler", ex.Message)

            End Try

            Return .Pnj.EnParler

        End With

    End Function


    ''' <summary>
    ''' Donne une reponse au Pnj.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="phrase">La phrase à donner au Pnj pour lui répondre.</param>
    ''' <returns>
    ''' True = Le bot a bien réussi a répondre au Pnj. <br/>
    ''' False = Le bot n'a pas réussi a répondre au pnj.
    ''' </returns>
    Public Function Reponse(index As String, phrase As String) As Boolean

        With Comptes(index)

            Try

                If .Pnj.EnParler Then

                    If phrase.ToLower = "terminer la discussion." Then

                        Return QuitteDialogue(index)

                    End If

                    If .Pnj.Reponse.Count > 0 Then

                        For i = 0 To .Pnj.Reponse.Count - 1

                            If phrase.ToLower = VarPnjRéponse(.Pnj.Reponse(i)).ToLower Then

                                EcritureMessage(index, "(Bot)", "Réponse : " & VarPnjRéponse(.Pnj.Reponse(i)), Color.Orange)

                                Return .Send("DR" & .Pnj.IdReponse & "|" & .Pnj.Reponse(i),
                                            {"DQ", ' Reçoit des nouvelles réponses disponible.
                                             "DV",' Fin du dialogue avec le Pnj.
                                             "DR"}) ' ?

                            End If

                        Next

                    Else

                        Return QuitteDialogue(index)

                    End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Pnj_Reponse", ex.Message)

            End Try

            Return False

        End With

    End Function


    ''' <summary>
    ''' Quitte le dialogue en cours avec le pnj.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <returns>
    ''' True = Le bot a bien quitter le dialogue en cours. <br/>
    ''' False = Le bot n'a pas réussi à quitter le dialogue en cours.
    ''' </returns>
    Public Function QuitteDialogue(index As String) As Boolean

        With Comptes(index)

            Try

                If .Pnj.EnParler Then

                    .Send("DV",
                         {"DV"}) 'Fin du dialogue

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "QuitteDialogue", ex.Message)

            End Try

            Return Not .Pnj.EnParler

        End With

    End Function


#End Region

#Region "Acheter/Vendre"

    ''' <summary>
    ''' selectionne le choix "Acheter/Vendre" du Pnj indiqué.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="nomID">Le nom ou l'ID du Pnj avec qui intéragir.</param>
    ''' <returns>
    ''' True = Le bot est bien en "Acheter/Vendre" avec le Pnj. <br/>
    ''' False = Le bot n'a pas réussi à choisir le choix "Acheter/Vendre".
    ''' </returns>
    Public Function AcheterVendre(index As Integer, nomID As String) As Boolean

        With Comptes(index)

            Try

                If .Pnj.EnAcheterVendre = False Then

                    For Each Pair As CEntite In CopyMap(index, .Map.Entite).Values

                        If Pair.Nom.ToLower = nomID.ToLower OrElse Pair.ID = nomID Then

                            .Send("ER0|" & Pair.IDUnique,
                                 {"ECK10"}) ' Reçoit les informations de l'HDV

                            Return .Pnj.EnAcheterVendre

                        End If

                    Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "PnjAcheterVendre", ex.Message)

            End Try

            Return False

        End With

    End Function


    ''' <summary>
    ''' Quitte le mode "Acheter/Vendre" du pnj.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <returns>
    ''' True = Le bot a bien quitté le mode "Acheter/Vendre" du pnj. <br/>
    ''' False = Le bot n'a pas réussi à quitter le mode "Acheter/Vendre" du Pnj.
    ''' </returns>
    Public Function QuitteAcheterVendre(index As Integer) As Boolean

        With Comptes(index)

            Try

                If .Pnj.EnAcheterVendre Then

                    .Send("EV",
                         {"EV"}) ' a quitté le mode "acheter/vendre" du Pnj.

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "QuitteAcheterVendre", ex.Message)

            End Try

            Return Not .Pnj.EnAcheterVendre

        End With

    End Function

    ''' <summary>
    ''' Vend un item au Pnj.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="nomID">Le nom ou l'ID de l'item à vendre.</param>
    ''' <param name="quantite">La quantité.</param>
    ''' <param name="caracteristique">Les caractéristique que l'item doit avoir pour être vendu.</param>
    ''' <returns>
    ''' True = Le bot a vendu l'item <br/>
    ''' False = Le bot n'a pas réussi à vendre l'item.
    ''' </returns>
    Public Function AcheterVendreVendItem(index As Integer, nomID As String, quantite As Integer, Optional caracteristique As String = Nothing) As Boolean

        With Comptes(index)

            Try

                If .Pnj.EnAcheterVendre Then

                    For Each pair As CItem In CopyItem(index, .Inventaire).Values

                        If pair.Nom.ToLower = nomID.ToLower OrElse pair.IdObjet = nomID OrElse nomID.ToLower = "all" Then

                            If IsNothing(caracteristique) OrElse ComparateurCaractéristiqueObjets(pair.Caracteristique, caracteristique) Then

                                If quantite > pair.Quantiter Then quantite = pair.Quantiter

                                Return .Send("ES" & pair.IdUnique & "|" & quantite,
                                            {"ESK"}) ' Item Vendu.

                            End If

                        End If

                    Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "AcheterVendreVendItem", ex.Message)

            End Try

            Return False

        End With

    End Function


    ''' <summary>
    ''' Achète un item au Pnj.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="nomID">Le nom ou l'ID de l'item à acheter.</param>
    ''' <param name="quantite">La quantité.</param>
    ''' <param name="caracteristique">Les caractéristique que l'item doit avoir pour être acheté.</param>
    ''' <returns>
    ''' True = Le bot a acheté l'item <br/>
    ''' False = Le bot n'a pas réussi à acheter l'item.
    ''' </returns>
    Public Function AcheterVendreAcheteItem(ByVal index As Integer, ByVal nomID As String, ByVal quantite As Integer, Optional ByVal caracteristique As String = Nothing) As Boolean

        With Comptes(index)

            Try

                If .Pnj.EnAcheterVendre Then

                    For Each pair As KeyValuePair(Of Integer, CPnjAcheterVendre) In .Pnj.AcheterVendre

                        If pair.Value.ID = nomID OrElse VarItems(pair.Key).Nom.ToLower = nomID.ToLower Then

                            If IsNothing(caracteristique) OrElse ComparateurCaractéristiqueObjets(pair.Value.Caracteristique, caracteristique) Then

                                .Pnj.Bloque.Reset()

                                .Send("EB" & pair.Key & "|" & quantite)

                                Return .Pnj.Bloque.WaitOne(15000)

                            End If

                        End If

                    Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "AcheterVendreAcheteItem", ex.Message)

            End Try

            Return False

        End With

    End Function

#End Region

#Region "Echanger"

    ''' <summary>
    ''' Permet de faire une échange avec un pnj.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="nomID">Le nom ou l'ID du Pnj avec qui faire l'échange.</param>
    ''' <returns>
    ''' True = Le bot est en échange avec le npj. <br/>
    ''' False = Le bot n'a pas réussi à lancer un échange avec le pnj.
    ''' </returns>
    Public Function Echanger(ByVal index As Integer, ByVal nomID As String)

        With Comptes(index)

            Try

                If .Pnj.EnEchange = False Then

                    For Each Pair As CEntite In CopyMap(index, .Map.Entite).Values

                        If Pair.Nom.ToLower = nomID.ToLower OrElse Pair.ID = nomID Then

                            .Pnj.Bloque.Reset()

                            .Send("ER2|" & Pair.IDUnique)

                            .Pnj.Bloque.WaitOne(15000)

                            Return .Pnj.EnEchange

                        End If

                    Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "PnjEchanger", ex.Message)

            End Try

            Return False

        End With

    End Function

#End Region

#Region "Vendre"

    ''' <summary>
    ''' Permet de vendre des items dans l'hôtel de vente.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="nomID">Le nom ou l'ID du Pnj avec qui faire de la vente en hôtel.</param>
    ''' <returns>
    ''' True = Le bot est en Hôtel de vente avec le npj. <br/>
    ''' False = Le bot n'a pas réussi à ouvrir l'hôtel de vente avec le pnj.
    ''' </returns>
    Public Function Vendre(ByVal index As Integer, ByVal nomID As String)

        With Comptes(index)

            Try

                If .Pnj.EnVendre = False Then

                    For Each Pair As CEntite In CopyMap(index, .Map.Entite).Values

                        If Pair.Nom.ToLower = nomID.ToLower OrElse Pair.ID = nomID Then

                            .Pnj.Bloque.Reset()

                            .Send("ER10|" & Pair.IDUnique)

                            .Pnj.Bloque.WaitOne(15000)

                            Return .Pnj.EnVendre

                        End If

                    Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "PnjVendre", ex.Message)

            End Try

            Return False

        End With

    End Function

    ''' <summary>
    ''' Quitte le mode "Vendre" du pnj.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <returns>
    ''' True = Le bot a bien quitté le mode "Vendre" du pnj. <br/>
    ''' False = Le bot n'a pas réussi à quitter le mode "Vendre" du Pnj.
    ''' </returns>
    Public Function QuitteVendre(ByVal index As Integer) As Boolean

        With Comptes(index)

            Try

                If .Pnj.EnVendre Then

                    .Pnj.Bloque.Reset()

                    .Send("EV")

                    .Pnj.Bloque.WaitOne(15000)

                End If

                Return .Pnj.EnVendre

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "QuitteVendre", ex.Message)

            End Try

            Return False

        End With

    End Function

    ''' <summary>
    ''' Vend un item dans l'hôtel de vente.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="nomID">Le nom ou l'ID du Pnj avec qui faire de la vente en hôtel.</param>
    ''' <param name="quantite">La quantiter à vendre : 1, 10, 100</param>
    ''' <param name="prix">Le prix à l'UNITER.</param>
    ''' <param name="caracteristique">Les caractéristiques que doit avoir l'item pour être vendu.</param>
    ''' <returns>
    ''' True = L'objet est mis en vente. <br/>
    ''' False = l'item n'a pas pu être mis en vente.
    ''' </returns>
    Public Function VendreItem(ByVal index As Integer, ByVal nomID As String, ByVal quantite As Integer, ByVal prix As Integer, Optional ByVal caracteristique As String = Nothing) As Boolean

        With Comptes(index)

            Try

                For Each Pair As CItem In CopyItem(index, .Inventaire).Values

                    If Pair.Nom.ToLower = nomID.ToLower OrElse Pair.IdObjet.ToString = nomID Then

                        If Pair.Equipement = "" Then

                            If IsNothing(caracteristique) OrElse ComparateurCaractéristiqueObjets(Pair.Caracteristique, caracteristique) Then

                                If quantite > CInt(Pair.Quantiter) Then

                                    EcritureMessage(index, "(Bot)", "Vous n'avez pas asse de quantiter pour vendre.", Color.Red)

                                    Return False

                                End If

                                prix = quantite * prix

                                If ((prix / 100) * .Pnj.Vendre.Taxe) > .Personnage.Kamas Then

                                    EcritureMessage(index, "(Bot)", "Vous n'avez pas asse de kamas pour payer la taxe !", Color.Gray)

                                    Return False

                                End If

                                EcritureMessage(index, "(Bot)", "Vente de l'item " & Pair.Nom & " x " & quantite & "au prix de : " & (prix * quantite), Color.Gray)

                                .BloqueItem.Reset()

                                Select Case quantite

                                    Case 1

                                        quantite = 1

                                    Case 10

                                        quantite = 2

                                    Case 100

                                        quantite = 3

                                End Select

                                .Send("EMO+" & Pair.IdUnique & "|" & quantite & "|" & prix)

                                Return .BloqueItem.WaitOne(15000)

                            End If

                        End If

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "PnjVendreItem", ex.Message)

            End Try

            Return False

        End With

    End Function

    ''' <summary>
    ''' Permet de retirer un item contenue dans l'Hôtel de vente.
    ''' </summary>
    ''' <param name="index">Indique le numéro bot.</param>
    ''' <param name="nomID">Nom/ID de l'item.</param>
    ''' <param name="quantite">La quantité à retirer (de base le maximum trouvable).</param>
    ''' <param name="caracteristique">Les caractéristiques que doit avoir l'item pour être retiré.</param>
    ''' <returns>
    ''' True = Le bot a bien retiré l'item. <br/>
    ''' False = Le bot n'a pas réussi à retirer l'item.
    ''' </returns>
    Public Function Retirer(ByVal index As String, ByVal nomID As String, ByVal quantite As String, Optional ByVal caracteristique As String = Nothing) As Boolean

        With Comptes(index)

            Try

                For Each Pair As CPnjHdvItem In .Pnj.Vendre.ListeItem.Values

                    If Pair.Nom.ToLower = nomID.ToLower OrElse Pair.IdObjet.ToString = nomID Then

                        If IsNothing(caracteristique) OrElse ComparateurCaractéristiqueObjets(Pair.Caracteristique, caracteristique) Then

                            If quantite > CInt(Pair.Quantiter) Then quantite = Pair.Quantiter

                            EcritureMessage(index, "(Bot)", "Retire l'item " & Pair.Nom & " x " & quantite, Color.Gray)

                            .Pnj.Bloque.Reset()

                            .Send("EMO-" & Pair.IDUnique & "|" & quantite)

                            Return .Pnj.Bloque.WaitOne(15000)

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
    ''' Permet de sélectionner un item contenue dans l'Hôtel de vente.
    ''' </summary>
    ''' <param name="index">Indique le numéro bot.</param>
    ''' <param name="nomID">Nom/ID de l'item.</param>
    ''' <param name="caracteristique">Les caractéristiques que doit avoir l'item pour être retiré.</param>
    ''' <returns>
    ''' True = Le bot a bien sélectionné l'item. <br/>
    ''' False = Le bot n'a pas réussi à sélectionner l'item.
    ''' </returns>
    Public Function Selectionne(ByVal index As Integer, ByVal nomID As String, Optional ByVal caracteristique As String = Nothing) As Boolean

        With Comptes(index)

            Try

                For Each Pair As CPnjHdvItem In .Pnj.Vendre.ListeItem.Values

                    If Pair.Nom.ToLower = nomID.ToLower OrElse Pair.IdObjet.ToString = nomID Then

                        If IsNothing(caracteristique) OrElse ComparateurCaractéristiqueObjets(Pair.Caracteristique, caracteristique) Then

                            EcritureMessage(index, "(Bot)", "Sélection de l'item " & Pair.Nom, Color.Gray)

                            .Pnj.Bloque.Reset()

                            .Send("EHP" & Pair.IdObjet)

                            Return .Pnj.Bloque.WaitOne(15000)

                        End If

                    End If

                Next


            Catch ex As Exception

                EcritureMessage(index, "(Bot)", "Erreur pour sélectionner l'item." & vbCrLf &
                                         "Nom : " & nomID, Color.Red)

            End Try

        End With

    End Function

#End Region

#Region "Acheter"

    ''' <summary>
    ''' Selectionne l'action "Acheter" du Pnj.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="nomID">Le nom ou l'ID du Pnj avec qui faire des achats en hôtel.</param>
    ''' <returns>
    ''' True = Le bot est en hôtel de vente avec le npj. <br/>
    ''' False = Le bot n'a pas réussi à ouvrir l'hôtel de vente avec le pnj.
    ''' </returns>
    Public Function Acheter(ByVal index As Integer, ByVal nomID As String) As Boolean

        With Comptes(index)

            Try

                If .Pnj.EnAcheter = False Then

                    For Each Pair As CEntite In CopyMap(index, .Map.Entite).Values

                        If Pair.Nom.ToLower = nomID.ToLower OrElse Pair.ID = nomID Then

                            .Pnj.Bloque.Reset()

                            .Send("ER11|" & Pair.IDUnique)

                            .Pnj.Bloque.WaitOne(15000)

                            Return .Pnj.EnAcheter

                        End If

                    Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "PnjAcheter", ex.Message)

            End Try

            Return False

        End With

    End Function

    ''' <summary>
    ''' Quitte le mode "Acheter" du pnj.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <returns>
    ''' True = Le bot a bien quitté le mode "Acheter" du pnj. <br/>
    ''' False = Le bot n'a pas réussi à quitter le mode "Acheter" du Pnj.
    ''' </returns>
    Public Function QuitteAcheter(ByVal index As Integer) As Boolean

        With Comptes(index)

            Try

                If .Pnj.EnAcheter Then

                    .Pnj.Bloque.Reset()

                    .Send("EV")

                    .Pnj.Bloque.WaitOne(15000)

                End If

                Return .Pnj.EnAcheter

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "PnjQuitteAcheter", ex.Message)

            End Try

            Return False

        End With

    End Function

    ''' <summary>
    ''' Recherche un item dans l'hôtel de vente (Acheter).
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="nomID">Le nom ou l'ID de l'item.</param>
    ''' <returns>
    ''' True = le bot à trouvé l'item. <br/>
    ''' False = Le bot n'a pas trouvé l'item.
    ''' </returns>
    Public Function Recherche(ByVal index As Integer, ByVal nomID As String) As Boolean

        With Comptes(index)

            Try

                If .Pnj.EnAcheter Then

                    For Each pair As sItems In VarItems.Values

                    If pair.ID.ToString = nomID OrElse pair.Nom.ToLower = nomID.ToLower Then

                        .Pnj.Bloque.Reset()

                        .Send("EHS" & pair.Catégorie & "|" & pair.ID)

                        If .Pnj.Bloque.WaitOne(15000) = True Then

                            .Pnj.Bloque.Reset()

                            .Send("EHP" & pair.ID)

                            Return .Pnj.Bloque.WaitOne(15000)

                        End If

                        Return False

                    End If

                Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "PnjRecherche", ex.Message)

            End Try

            Return False

        End With

    End Function

    ''' <summary>
    ''' Achète un item dans l'hôtel de vente.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="nomID">Le nom ou l'ID du Pnj avec qui faire de l'achat en hôtel.</param>
    ''' <param name="quantite">La quantiter à acheter : 1, 10, 100</param>
    ''' <param name="prix">Le prix à l'UNITER.</param>
    ''' <param name="caracteristique">Les caractéristiques que doit avoir l'item pour être acheté.</param>
    ''' <returns>
    ''' True = L'objet est acheté. <br/>
    ''' False = l'item n'a pas pu être acheter.
    ''' </returns>
    Public Function AcheterItem(ByVal index As Integer, ByVal nomID As String, ByVal quantite As Integer, ByVal prix As Integer, Optional ByVal caracteristique As String = Nothing) As Boolean

        With Comptes(index)

            Try

                For Each pair As CPnjHdvItem In .Pnj.Acheter.ListeItem.Values

                    If pair.Nom.ToLower = nomID.ToLower OrElse pair.IdObjet = nomID Then

                        If IsNothing(caracteristique) OrElse ComparateurCaractéristiqueObjets(pair.Caracteristique, caracteristique) Then

                            If quantite >= 100 Then

                                If .Personnage.Kamas >= pair.Prix100 Then

                                    .Pnj.Bloque.Reset()

                                    .Send("EHB" & pair.IDUnique & "|3|" & pair.Prix100)

                                    Return .Pnj.Bloque.WaitOne(15000)

                                End If

                            End If

                            If quantite >= 10 Then

                                If .Personnage.Kamas >= pair.Prix10 Then

                                    .Pnj.Bloque.Reset()

                                    .Send("EHB" & pair.IDUnique & "|2|" & pair.Prix10)

                                    Return .Pnj.Bloque.WaitOne(15000)

                                End If

                            End If

                            If quantite >= 1 Then

                                If .Personnage.Kamas >= pair.Prix1 Then

                                    .Pnj.Bloque.Reset()

                                    .Send("EHB" & pair.IDUnique & "|1|" & pair.Prix1)

                                    Return .Pnj.Bloque.WaitOne(15000)

                                End If

                            End If

                        End If

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "PnjAcheterItem", ex.Message)

            End Try

            Return False

        End With

    End Function

    ''' <summary>
    ''' Sélectionne un item de l'hôtel de vente (Acheter).
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="nomID">Le nom ou l'ID de l'item.</param>
    ''' <returns>
    ''' True = Le bot à sélectionner l'item indiqué. <br/>
    ''' False = Le bot n'a pas réussie à sélectionner l'item indiqué.
    ''' </returns>
    Public Function SelectionneItem(ByVal index As Integer, ByVal nomID As String) As Boolean

        With Comptes(index)

            Try

                If .Pnj.EnAcheter Then

                    For Each pair As sItems In VarItems.Values

                    If pair.ID.ToString = nomID OrElse pair.Nom.ToLower = nomID.ToLower Then

                        .Pnj.Bloque.Reset()

                        .Send("EHP" & pair.Catégorie)

                        If .Pnj.Bloque.WaitOne(15000) = True Then

                            .Pnj.Bloque.Reset()

                            .Send("EHl" & pair.Catégorie)

                            Return .Pnj.Bloque.WaitOne(15000)

                        End If

                        Return False

                    End If

                Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "PnjSelectionneItem", ex.Message)

            End Try

            Return False

        End With

    End Function

    ''' <summary>
    ''' Selectionne une catégorie de l'hôtel de vente (Acheter).
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="nomID">Le nom ou l'ID de l'item.</param>
    ''' <returns>
    ''' True = Le bot à sélectionné la catégorie voulu. <br/>
    ''' False = Le bot n'à pas sélectionné la catégorie voulu. <br/>
    ''' </returns>
    Public Function SelectionneCategorie(ByVal index As Integer, ByVal nomID As String) As Boolean

        With Comptes(index)

            Try

                If .Pnj.EnAcheter Then

                    For Each pair As sItems In VarItems.Values

                        If pair.ID.ToString = nomID OrElse pair.Nom.ToLower = nomID.ToLower Then

                            .Pnj.Bloque.Reset()

                            .Send("EHT" & RetourneItemNomIdCategorie(index, pair.ID, "categorie"))

                            Return .Pnj.Bloque.WaitOne(15000)

                        End If

                    Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "PnjSelectionneCategorie", ex.Message)

            End Try

            Return False

        End With

    End Function

#End Region

End Class
