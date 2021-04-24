Module mdlEchange

    Sub GiEchangeRecu(index As Integer, data As String)

        With Comptes(index)

            Try

                ' ERK 1234567    | 7654321     | 1
                ' ERK Id Lanceur | Id Receveur | ?

                Dim separateData As String() = Split(Mid(data, 4), "|")

                .Echange.EnInvitation = True

                If separateData(0) <> .Personnage.ID Then

                    .Echange.IdJoueur = separateData(0)
                    EcritureMessage(index, "[Dofus]", .Map.Entite(separateData(0)).Nom & " te propose de faire un échange. acceptes-tu ?", Color.Green)

                Else

                    EcritureMessage(index, "[Dofus]", "En Attente de la réponse de " & .Map.Entite(separateData(1)).Nom & " pour un échange...", Color.Green)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEchangeRecu", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiEchangeDejaEnEchange(index As Integer, data As String)

        With Comptes(index)

            Try

                ' EREO

                EcritureMessage(index, "[Dofus]", "Ce joueur est déjà en échange.", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEchangeDejaEnEchange", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiEchangeAccepter(index As Integer, data As String)

        With Comptes(index)

            Try

                ' ECK1

                With .Echange

                    .EnEchange = True
                    .EnInvitation = False
                    .Numero = 1
                    .IdJoueur = ""

                End With

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEchangeAccepter", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiEchangeAnnuler(index As Integer, data As String)

        With Comptes(index)

            Try

                ' EV

                With .Echange

                    .EnEchange = False
                    .EnInvitation = False
                    .Lui.Inventaire.Clear()
                    .Lui.Kamas = 0
                    .Lui.Valider = False
                    .Moi.Inventaire.Clear()
                    .Moi.Kamas = 0
                    .Moi.Valider = False
                    .Numero = 0
                    .IdJoueur = ""

                End With

                EcritureMessage(index, "[Dofus]", "Echange annulé", Color.Red)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEchangeAnnuler", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiEchangeEffectuer(index As Integer, data As String)

        With Comptes(index)

            Try

                ' EVa

                With .Echange

                    .EnEchange = False
                    .EnInvitation = False
                    .Numero = 0
                    .IdJoueur = ""

                    With .Lui

                        .Inventaire.Clear()
                        .Kamas = 0
                        .Valider = False

                    End With

                    With .Moi

                        .Inventaire.Clear()
                        .Kamas = 0
                        .Valider = False

                    End With

                End With

                EcritureMessage(index, "[Dofus]", "Echange effectué", Color.Red)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEchangeEffectuer", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiEchangeValideInvalide(index As Integer, data As String)

        With Comptes(index)

            Try

                ' EK 1      1234567
                ' EK 1 ou 0 Id perso

                Dim id As Integer = Mid(data, 4)

                Select Case Mid(data, 3, 1)

                    Case "0"

                        If id = .Personnage.ID Then

                            .Echange.Moi.Valider = False

                        Else

                            .Echange.Lui.Valider = False

                        End If

                    Case "1"

                        If id = .Personnage.ID Then

                            .Echange.Moi.Valider = True

                        Else

                            .Echange.Lui.Valider = True

                        End If

                End Select

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEchangeValideInvalide", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#Region "Lui"

    Sub GiEchangeAjouteItemLui(index As Integer, data As String)

        With Comptes(index)

            Try

                ' EmKO+ 40514824  | 1        | 7659     |
                ' EmKO+ Id Unique | Quantité | Id Objet | Caractéristique

                Dim separateData As String() = Split(Mid(data, 6), "|")

                If .Echange.Lui.Inventaire.ContainsKey(separateData(0)) Then

                    .Echange.Lui.Inventaire(separateData(0)).Quantiter = separateData(1)

                Else

                    Dim newItem As New CItem

                    With newItem

                        .IdObjet = separateData(2)

                        .IdUnique = separateData(0)

                        .Nom = VarItems(Convert.ToInt64(separateData(2))).Nom

                        .Quantiter = separateData(1)

                        .Caracteristique = ItemCaracteristique(separateData(3), separateData(2))

                        .CaracteristiqueBrute = separateData(3)

                        .Categorie = VarItems(.IdObjet).Catégorie

                        .Equipement = ""

                    End With

                    If .Echange.Lui.Inventaire.ContainsKey(newItem.IdUnique) Then

                        .Echange.Lui.Inventaire(newItem.IdUnique) = newItem

                    Else

                        .Echange.Lui.Inventaire.Add(newItem.IdUnique, newItem)

                    End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEchangeAjouteItemLui", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiEchangeSupprimeItemLui(index As Integer, data As String)

        With Comptes(index)

            Try

                ' EmKO- 40420233
                ' EmKO- Id Unique

                Dim idUnique As String = Mid(data, 6)

                If .Echange.Lui.Inventaire.ContainsKey(idUnique) Then

                    .Echange.Lui.Inventaire.Remove(idUnique)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEchangeSupprimeItemLui", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "Moi"

    Sub GiEchangeAjouteItemMoiEchange(index As Integer, data As String)

        With Comptes(index)

            Try

                ' EsKO+ 78415959  | 2        | 393      |
                ' EsKO+ Id Unique | Quantité | Id Objet | Caracteristique

                Dim separateData As String() = Split(Mid(data, 6), "|")

                If .Echange.Moi.Inventaire.ContainsKey(separateData(0)) Then

                    .Echange.Moi.Inventaire(separateData(0)).Quantiter = separateData(1)

                Else

                    Dim newItem As New CItem

                    With newItem

                        .IdObjet = separateData(2)

                        .IdUnique = separateData(0)

                        .Nom = VarItems(separateData(2)).Nom

                        .Quantiter = separateData(1)

                        .Caracteristique = ItemCaracteristique(separateData(3), separateData(2))

                        .CaracteristiqueBrute = separateData(3)

                        .Categorie = VarItems(.IdObjet).Catégorie

                        .Equipement = ""

                    End With

                    .Echange.Moi.Inventaire.Add(separateData(0), newItem)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEchangeAjouteItemMoiEchange", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiEchangeSupprimeItemMoiEchange(index As Integer, data As String)

        With Comptes(index)

            Try

                ' EsKO- 78415959 
                ' EsKO- Id Unique 

                Dim idUnique As String = Mid(data, 6)

                If .Echange.Moi.Inventaire.ContainsKey(idUnique) Then

                    .Echange.Moi.Inventaire.Remove(idUnique)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEchangeSupprimeItemMoiEchange", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiEchangeAjouteItemMoi(index As Integer, data As String)

        With Comptes(index)

            Try

                ' EMKO+ 40420233  | 20 
                ' EMKO+ Id Unique | Quantité
                ' EsKO+ 78415959  |2        | 393 |

                Dim separateData As String() = Split(Mid(data, 6), "|")

                If .Echange.Moi.Inventaire.ContainsKey(separateData(0)) Then

                    .Echange.Moi.Inventaire(separateData(0)).Quantiter = separateData(1)

                Else

                    Dim newItem As CItem = .Inventaire(separateData(0))

                    newItem.Quantiter = separateData(1)

                    .Echange.Moi.Inventaire.Add(separateData(0), newItem)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEchangeAjouteItemMoi", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiEchangeSupprimeItemMoi(index As Integer, data As String)

        With Comptes(index)

            Try

                ' EMKO- 40420233
                ' EMKO- Id Unique

                Dim idUnique As String = Mid(data, 6)

                If .Echange.Moi.Inventaire.ContainsKey(idUnique) Then

                    .Echange.Moi.Inventaire.Remove(idUnique)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEchangeSupprimeItemMoi", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "Kamas"

    Sub GiEchangeKamasLui(index As Integer, data As String)

        With Comptes(index)

            Try

                ' EmKG 5
                ' EmKG Kamas

                .Echange.Lui.Kamas = Mid(data, 5)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEchangeKamasLui", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiEchangeKamasMoi(index As Integer, data As String)

        With Comptes(index)

            Try

                ' Echange
                ' EMKG 5
                ' EMKG kamas

                ' Banque/Coffre
                ' EsKG 5
                ' EsKG kamas

                .Echange.Moi.Kamas = Mid(data, 5)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiEchangeKamasMoi", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

End Module

#Region "Echange"

Public Class CEchange

    Public Numero As Integer
    Public EnEchange, EnInvitation As Boolean
    Public IdJoueur As String
    Public Moi As New CEchangeAll
    Public Lui As New CEchangeAll

End Class

Public Class CEchangeAll

    Public Inventaire As New Dictionary(Of Integer, CItem)
    Public Kamas As Integer
    Public Valider As Boolean

End Class

#End Region