Module mdlPnj

#Region "Parler"

    Sub GiPnjParlerDialogue(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' DCK -2  
                ' DCK ID sur la map

                .Pnj.EnParler = True

                'J'affiche le nom du PNJ auquel je parle.
                Dim idUnique As String = Mid(data, 4)

                If .Map.Entite.ContainsKey(idUnique) Then

                    EcritureMessage(index, "[Dofus]", "Je parle actuellement avec " & .Map.Entite(idUnique).Nom, Color.Orange)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiPnjParlerDialogue", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiPnjParlerQuestionReponse(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' DQ 318        ; 449                                           |   259     ;    329    ;
                ' DQ ID Réponse ; Information à mettre dans le dialogue de base | Réponse 1 ; Réponse 2 ; etc....

                .Pnj.Reponse.Clear()
                .Pnj.IdReponse = 0

                data = Mid(data, 3)

                Dim separateData As String() = Split(data, "|")
                Dim separate As String() = Split(separateData(0), ";")

                .Pnj.IdReponse = separate(0)

                If data.Contains("|") Then

                    separateData = Split(separateData(1), ";")

                    For i = 0 To separateData.Count - 1

                        .Pnj.Reponse.Add(separateData(i))

                        EcritureMessage(index, "[Dofus - Réponse]", i + 1 & ") " & VarPnjRéponse(separateData(i)), Color.Green)

                    Next

                Else

                    EcritureMessage(index, "(Bot)", "Il n'y a plus aucune réponse disponible pour ce Pnj.", Color.Green)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiPnjParlerQuestionReponse", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiPnjParlerFinDialogue(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' DV

                With .Pnj

                    .EnParler = False
                    .Reponse.Clear()
                    .IdReponse = 0

                End With

                EcritureMessage(index, "[Dofus]", "Fin du dialogue avec le Pnj.", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiPnjParlerFinDialogue", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiPnjParlerDejaEnDialogue(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' DCE

                With .Pnj

                    .EnParler = True
                    .Bloque.Set()

                End With

                EcritureMessage(index, "[Dofus]", "Vous êtes déjà en dialogue.", Color.Red)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiPnjParlerDejaEnDialogue", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "Acheter/Vendre"

    Sub GiPnjAcheterVendreItemEnVente(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' EL 596      ; 60#1#4##1d4+0   | 1860;60#1#14##1d20+0 
                ' EL Id Objet ; Caractéristique | Item Suivant

                Dim separateData As String() = Split(Mid(data, 3), "|")

                For i = 0 To separateData.Count - 1

                    Dim separate As String() = Split(separateData(i), ";")

                    Dim newPnjAcheterVendre As New CPnjAcheterVendre

                    With newPnjAcheterVendre

                        .ID = separate(0)
                        .Caracteristique = ItemCaracteristique(separate(1), separate(0))
                        .CaracteristiqueBrute = separate(1)
                        .Prix = 0

                    End With

                    .Pnj.AcheterVendre.Add(separate(0), newPnjAcheterVendre)

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiPnjAcheterVendreItemEnVente", data & vbCrLf & ex.Message)

            End Try

            .Pnj.Bloque.Set()

        End With

    End Sub

    Sub GiPnjAcheterVendreAchatReussi(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' EBK

                EcritureMessage(index, "[Dofus]", "Achat effectué", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiPnjAcheterVendreAchatReussi", data & vbCrLf & ex.Message)

            End Try

            .Pnj.Bloque.Set()

        End With

    End Sub

    Sub GiPnjAcheterVendreVenteReussi(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' ESK

                EcritureMessage(index, "[Dofus]", "Vente effectuée", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiPnjAcheterVendreVenteReussi", data & vbCrLf & ex.Message)

            End Try

            .Pnj.Bloque.Set()

        End With

    End Sub

#End Region

    Sub GiPnjHotelDeVenteAcheterVendre(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' ECK10 | 1            , 10            , 100            ; 2            , 3            , 4            ; 2.0      ; 1000            ; 20                ; -1 ; 350
                ' ECK10 | Quantité * 1 , Quantité * 10 , Quantité * 100 ; Id Catégorie , Id Catégorie , Id Catégorie ; Taxe (%) ; Niveau Max Item ; Nbr item vendable ; ?  ; Nbr heure Max


                Dim separateData As String() = Split(data, "|")
                Dim separateInfo As String() = Split(separateData(1), ";")

                If separateData(0) = "ECK10" Then

                    .Pnj.EnVendre = True
                    EcritureMessage(index, "[Dofus]", "Vous êtes en vente avec le PNJ.", Color.Green)

                Else

                    .Pnj.EnAcheter = True
                    EcritureMessage(index, "[Dofus]", "Vous êtes en achat avec le PNJ.", Color.Green)

                End If

                With If(.Pnj.EnVendre, .Pnj.Vendre, .Pnj.Acheter)

                    Dim separate As String() = Split(separateInfo(0), ",")

                    .Quantiter1 = separate(0)
                    .Quantiter10 = separate(1)
                    .Quantiter100 = separate(2)

                    separate = Split(separateInfo(1), ",")

                    .ListeIdCategorie.Clear()

                    For i = 0 To separate.Count - 1

                        .ListeIdCategorie.Add(separate(i))

                    Next

                    .Taxe = separateInfo(2)
                    .NiveauMax = separateInfo(3)
                    .StockEnMagasin = separateInfo(4)
                    .HeureMax = separateInfo(6)

                End With

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiPnjHotelDeVenteAcheterVendre", data & vbCrLf & ex.Message)

            End Try

            .Pnj.Bloque.Set()

        End With

    End Sub

    Sub GiPnjHotelDeVentePrixMoyen(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' EHP 180      | 4836469
                ' EHP Id Objet | Prix Moyen

                Dim separateData As String() = Split(data, "|")

                If .Pnj.EnVendre Then

                    .Pnj.Vendre.PrixMoyen = separateData(1)

                Else

                    .Pnj.Acheter.PrixMoyen = separateData(1)

                End If

                EcritureMessage(index, "[Dofus]", "Prix Moyen constaté dans cet hôtel : " & separateData(1) & " kamas/u.", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiHotelDeVentePrixMoyen", data & vbCrLf & ex.Message)

            End Try

            .Pnj.Bloque.Set()

        End With

    End Sub

#Region "Vendre"

    Sub GiPnjHotelDeVenteItemEnVente(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' EL 11759152  ; 100      ; 304      ;                 ; 10000 ; 1800          | 11803628;100;304;;10000;1800  
                ' EL Id Unique ; Quantité ; Id Objet ; Caractéristique ; Prix  ; Temps restant | Suivant

                Dim separateData As String() = Split(Mid(data, 3), "|")

                With .Pnj.Vendre.ListeItem

                    .Clear()

                    For i = 1 To separateData.Count - 1

                        Dim separate As String() = Split(separateData(i), ";")

                        Dim newHDV As New CPnjHdvItem

                        With newHDV

                            .IDUnique = separate(0)
                            .Quantiter = separate(1)
                            .IdObjet = separate(2)
                            .Nom = VarItems(separate(2)).Nom
                            .Caracteristique = ItemCaracteristique(separate(3), separate(2))
                            .Prix = separate(4)
                            .TempsRestant = separate(5)

                        End With

                        .Add(separate(0), newHDV)

                    Next

                End With

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiPnjHotelDeVenteItemEnVente", data & vbCrLf & ex.Message)

            End Try

            .Pnj.Bloque.Set()

        End With

    End Sub

    Sub GiPnjHotelDeVenteItemMisEnVente(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' EmK+ 11759152  | 100      | 304      |          | 10000 | 1800 
                ' EmK+ id unique | quantité | id objet | caract ? | Prix  | Temps Restant

                With .Pnj.Vendre.ListeItem

                    Dim separateData As String() = Split(Mid(data, 5), "|")

                    Dim newHDV As New CPnjHdvItem

                    With newHDV

                        .IDUnique = separateData(0)
                        .Quantiter = separateData(1)
                        .IdObjet = separateData(2)
                        .Nom = VarItems(separateData(2)).Nom
                        .Caracteristique = ItemCaracteristique(separateData(3), separateData(2))
                        .Prix = separateData(4)
                        .TempsRestant = separateData(5)

                    End With

                    .Add(separateData(0), newHDV)

                End With

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiPnjHotelDeVenteItemMisEnVente", data & vbCrLf & ex.Message)

            End Try

            .Pnj.Bloque.Set()

        End With

    End Sub

    Sub GiPnjHotelDeVenteRetireItemMisEnVente(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' EmK- 11791321
                ' EmK- Id Unique

                If .Pnj.Vendre.ListeItem.ContainsKey(Mid(data, 5)) Then

                    .Pnj.Vendre.ListeItem.Remove(Mid(data, 5))

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiPnjHotelDeVenteRetireItemMisEnVente", data & vbCrLf & ex.Message)

            End Try

            .Pnj.Bloque.Set()

        End With

    End Sub

#End Region

#Region "Acheter"

    Sub GiPnjHotelDeVenteAcheterItemID(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' EHL 2         | 829    ; 1351    ; etc...
                ' EHL Categorie | IdItem ; Id Item ; etc...

                Dim separateData As String() = Split(data, "|")
                EcritureMessage(index, "[Dofus]", "Vous avez sélectionner la catégorie : " & VarItemsCategorieNom(Mid(separateData(0), 4, separateData(0).Length)), Color.Green)

                separateData = Split(separateData(1), ";")

                With .Pnj.Acheter

                    .ListeIdItem.Clear()

                    For i = 0 To separateData.Count - 1

                        .ListeIdItem.Add(separateData(i))

                    Next

                End With

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiHotelDeVenteItemID", data & vbCrLf & ex.Message)

            End Try

            .Pnj.Bloque.Set()

        End With

    End Sub

    Sub GiPnjHotelDeVenteAcheterItemChoisi(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' EHl 180      | 7335643   ; 6f#1###0d0+1,64#10#14##1d5+15 ; 4150000  ;           ;            | Suivant
                ' EHl ID Objet | Id Unique ; Caracteristique               ; Prix * 1 ; Prix * 10 ; Prix * 100 | Suivant

                Dim separateData As String() = Split(data, "|")

                With .Pnj.Acheter.ListeItem

                    .Clear()

                    For i = 1 To separateData.Count - 1

                        Dim separate As String() = Split(separateData(i), ";")

                        Dim newHDV As New CPnjHdvItem

                        With newHDV

                            .IDUnique = separate(0)
                            .Caracteristique = ItemCaracteristique(separate(1), Mid(separateData(0), 4))
                            .IdObjet = Mid(separateData(0), 4)
                            .Nom = VarItems(Mid(separateData(0), 4)).Nom
                            .Prix1 = separate(2)
                            .Prix10 = separate(3)
                            .Prix100 = separate(4)

                        End With

                        .Add(separate(0), newHDV)

                    Next

                End With

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiPnjHotelDeVenteAcheterItemChoisi", data & vbCrLf & ex.Message)

            End Try

            .Pnj.Bloque.Set()

        End With

    End Sub

    Sub GiPnjHotelDeVenteAcheterRechercheEchoue(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                EcritureMessage(index, "[Dofus]", "L'objet recherché n'est pas en vente dans cet hôtel des ventes.", Color.Red)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiPnjHotelDeVenteAcheterRechercheEchoue", data & vbCrLf & ex.Message)

            End Try

            .Pnj.Bloque.Set()

        End With

    End Sub

#End Region

End Module

#Region "Class"

Public Class CPnj

    Public EnParler, EnAcheter, EnVendre, EnAcheterVendre, EnEchange As Boolean
    Public Bloque As Threading.ManualResetEvent = New Threading.ManualResetEvent(False)
    Public Reponse As New List(Of Integer)
    Public IdReponse As Integer
    Public Acheter As New CPnjHdvAcheterVendre
    Public Vendre As New CPnjHdvAcheterVendre
    Public AcheterVendre As New Dictionary(Of Integer, CPnjAcheterVendre)

End Class

Public Class CPnjAcheterVendre

    Public ID As Integer
    Public Caracteristique As New CItemCaractéristique
    Public CaracteristiqueBrute As String
    Public Prix As Integer

End Class

Public Class CPnjHdvAcheterVendre

    Public Quantiter1, Quantiter10, Quantiter100 As Boolean
    Public ListeIdCategorie As New List(Of Integer)
    Public ListeIdItem As New List(Of Integer)
    Public Taxe As Integer
    Public NiveauMax As Integer
    Public StockEnMagasin As Integer
    Public HeureMax As Integer
    Public PrixMoyen As Integer
    Public ListeItem As New Dictionary(Of Integer, CPnjHdvItem)

End Class

Public Class CPnjHdvItem

    Public IDUnique As Integer
    Public Nom As String
    Public Caracteristique As New CItemCaractéristique
    Public Prix1, Prix10, Prix100 As Integer
    Public IdObjet As Integer
    Public TempsRestant As Integer
    Public Prix As Integer
    Public Quantiter As Integer

End Class

#End Region