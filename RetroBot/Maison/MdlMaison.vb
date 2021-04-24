Module MdlMaison

    Public Sub GiMaisonAucunCodePorte(index As Integer, data As String)

        With Comptes(index)

            Try

                ' hX 999 | 0
                ' hX Id  | Vérouillé (oui ou non)

                Dim separateData As String() = Split(Mid(data, 3), "|")

                If .MaisonMap.ContainsKey(separateData(0)) Then

                    .MaisonMap(separateData(0)).Verouiller = separateData(1) IsNot "0"

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMaisonChangeCodeReussie", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiMaisonAucunCodeCoffre(index As Integer, data As String)

        With Comptes(index)

            Try

                ' sX 9999   _ 999     | 0
                ' sX Id map _ cellule | Vérouillé (oui ou non)

                Dim separateData As String() = Split(Mid(data, 3), "|")

                If .Maison.Coffre.ContainsKey(separateData(0)) Then

                    .Maison.Coffre(separateData(0)).Verouiller = separateData(1) IsNot "0"

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMaisonAucunCodeCoffre", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiMaMaison(index As Integer, data As String)

        With Comptes(index)

            Try

                ' hL + 999       ; 1          ; 0        ; 0
                ' hL + Id Maison ; Verouiller ; En Vente ; En Guilde

                data = Mid(data, 4)

                Dim separate As String() = Split(data, ";")

                With .Maison

                    .ID = separate(0)
                    .Verouiller = separate(1)
                    .EnVente = separate(2)
                    .EnGuilde = separate(3)
                    .Cellule = VarMaison(.ID).CellulePorte
                    .MapID = VarMaison(.ID).MapId
                    .Coordonnees = VarMaison(.ID).Map
                    .Prix = 0
                    .Code = 0

                End With

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMaMaison", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiMaMaisonSupprime(index As Integer, data As String)

        With Comptes(index)

            Try

                ' hL -
                ' hL -

                .Maison = New CMaison

                EcritureMessage(index, "[Dofus]", "Vous n'avez plus de maison.", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMaMaisonSupprime", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiMaMaisonVendu(index As Integer, data As String)

        With Comptes(index)

            Try

                ' M15 | 1000 ; slider 
                ' M15 | Prix ; Pseudo 

                Dim separateData As String() = Split(data, "|")
                separateData = Split(separateData(1), ";")

                EcritureMessage(index, "[Dofus]", "L'une de vos maisons vient d'être achetée " & separateData(0) & " kamas par " & separateData(1) & ". La somme a été placée sur votre compte en banque.", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMaMaisonVendu", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiMesCoffres(index As Integer, data As String)

        With Comptes(index)

            Try

                ' sL + 9999   _ 114     ; 1          | Next
                ' sL + Id Map _ Cellule ; Verouiller | Next

                data = Mid(data, 4)

                Dim separateData As String() = Split(data, "|")

                For i = 0 To separateData.Count - 1

                    Dim separate As String() = Split(separateData(i), ";")

                    Dim Vérouiller As Boolean = separate(1)

                    separate = Split(separate(0), "_")

                    Dim varCoffre As New CCoffre

                    With varCoffre

                        .Coordonnees = VarMap(separate(0))
                        .MapID = separate(0)
                        .Cellule = separate(1)
                        .Verouiller = Vérouiller
                        .Code = Nothing

                    End With

                    If .Maison.Coffre.ContainsKey(separate(0) & "_" & separate(1)) Then

                        .Maison.Coffre(separate(0) & "_" & separate(1)) = varCoffre

                    Else

                        .Maison.Coffre.Add(separate(0) & "_" & separate(1), varCoffre)

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMesCoffres", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiMaisonMap(index As Integer, data As String)

        With Comptes(index)

            Try

                ' hP 444 | Linacu ; 0            ; Lenculeur lourd ; a     , 0    ,i     ,9drge
                ' hP Id  | Pseudo ; pas en vente ; Nom guilde      ; blason,blason,blason,blason

                data = Mid(data, 3)

                Dim separateData As String() = Split(data, "|")

                Dim id As Integer = separateData(0)

                separateData = Split(separateData(1), ";")

                If Not VarMaison.ContainsKey(id) Then

                    MaisonAjouteInformation(index, id)

                End If

                Dim newMaison As New CMaison

                With newMaison

                    .ID = id
                    .Verouiller = False
                    .EnVente = separateData(1)
                    .EnGuilde = False
                    .Proprietaire = separateData(0)
                    .Cellule = VarMaison(id).CellulePorte
                    .MapID = Comptes(index).Map.ID
                    .Coordonnees = Comptes(index).Map.Coordonnees
                    .Prix = 0
                    .Code = 0

                    If separateData.Length > 2 Then

                        .GuildeNom = separateData(2)

                    End If

                End With

                If .MaisonMap.ContainsKey(id) Then

                    .MaisonMap(id) = newMaison

                Else

                    .MaisonMap.Add(id, newMaison)

                End If

                If VarMaison(id).Map = "[X,Y]" OrElse VarMaison(id).MapId = "0" Then

                    MaisonChangeInformation(index, id)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMaisonMap", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Private Sub MaisonChangeInformation(ByVal index As Integer, ByVal id As String)

        With Comptes(index)

            'Mise à jour de la version automatiquement (celui du fichier)
            Dim swLecture As New IO.StreamReader(Application.StartupPath & "\Data/Maison.txt")

            Dim ligneFinal As String = ""

            Do Until swLecture.EndOfStream

                Dim Ligne As String = swLecture.ReadLine

                If Ligne <> "" Then

                    Dim separate As String() = Split(Ligne, " | ")

                    If separate(0) = "hP : " & id Then

                        ligneFinal &= "hP : " & id & " | Porte : " & VarMaison(id).CellulePorte & " | Map : " & .Map.Coordonnees & " | Mapid : " & .Map.ID & " | Nom : " & VarMaison(id).Nom & vbCrLf

                    Else

                        ligneFinal &= Ligne & vbCrLf

                    End If

                End If

            Loop

            'Puis je ferme le fichier.
            swLecture.Close()

            'J'ouvre le fichier pour y écrire se que je souhaite
            Dim Sw_Ecriture As New IO.StreamWriter(Application.StartupPath + "\Data/Maison.txt")

            'J'écris dedans avant de le fermer.
            Sw_Ecriture.Write(ligneFinal)

            'Puis je le ferme.
            Sw_Ecriture.Close()

            ChargeMaison()

        End With

    End Sub

    Private Sub MaisonAjouteInformation(ByVal index As Integer, ByVal id As String)

        With Comptes(index)

            'Mise à jour de la version automatiquement (celui du fichier)
            Dim swLecture As New IO.StreamReader(Application.StartupPath & "\Data/Maison.txt")

            Dim ligneFinal As String = ""

            Do Until swLecture.EndOfStream

                Dim ligne As String = swLecture.ReadLine

                If ligne <> "" Then

                    ligneFinal &= ligne & vbCrLf

                End If

            Loop

            'Puis je ferme le fichier.
            swLecture.Close()

            ligneFinal &= "hP : " & id & " | Porte : 0 | Map : " & .Map.Coordonnees & " | Mapid : " & .Map.ID & " | Nom : Maison"

            'J'ouvre le fichier pour y écrire se que je souhaite
            Dim Sw_Ecriture As New IO.StreamWriter(Application.StartupPath + "\Data/Maison.txt")

            'J'écris dedans avant de le fermer.
            Sw_Ecriture.Write(ligneFinal)

            'Puis je le ferme.
            Sw_Ecriture.Close()

            ChargeMaison()

        End With

    End Sub

    Public Sub GiMaisonQuitteAchat(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' hV

                .Personnage.EnInteraction = False

                EcritureMessage(index, "[Dofus]", "Le bot n'est plus en achat de maison.", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMaisonQuitteAchat", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiMaisonPrix(index As Integer, data As String)

        With Comptes(index)

            Try

                ' hCK 607 | 8999999
                ' hCK Id  | Prix

                Dim separateData As String() = Split(Mid(data, 4), "|")

                If .MaisonMap.ContainsKey(separateData(0)) Then

                    .MaisonMap(separateData(0)).Prix = separateData(1)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMaisonQuitteAchat", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiMaisonAchete(index As Integer, data As String)

        With Comptes(index)

            Try

                ' hBK 999 | 10000000 
                ' hBK ID  | Prix 

                Dim separateData As String() = Split(Mid(data, 4), "|")

                EcritureMessage(index, "[Dofus]", "Tu viens d'acheter 'Maison' pour " & separateData(1) & " kamas.", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMaisonAchete", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiMaisonMisEnVente(index As Integer, data As String)

        With Comptes(index)

            Try

                ' hSK 999 |10000000 
                ' hSK ID  | Prix 

                Dim separateData As String() = Split(Mid(data, 4), "|")

                EcritureMessage(index, "[Dofus]", "'Maison' est mise en vente au prix de " & separateData(1) & " kamas.", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMaisonAchete", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiMaisonCode(index As Integer, data As String)

        With Comptes(index)

            Try

                ' KCK 1 | 8 
                ' KCK 1 | lenombre de chiffre maximum  

                .Maison.EnCode = True

                Dim separateData As String() = Split(Mid(data, 4), "|")

                Select Case separateData(0)

                    Case "0"

                        EcritureMessage(index, "[Dofus]", "Veuillez saisir le code.", Color.Green)

                    Case "1"

                        EcritureMessage(index, "[Dofus]", "Veuillez saisir le nouveau code (maximum de " & separateData(1) & " chiffres).", Color.Green)

                End Select

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMaisonChangeCode", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiMaisonChangeCodeReussie(index As Integer, data As String)

        With Comptes(index)

            Try

                ' KKK

                EcritureMessage(index, "[Dofus]", "Code changé", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMaisonChangeCodeReussie", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiMaisonGuilde(index As Integer, data As String)

        With Comptes(index)

            Try

                ' hG 999
                ' hG ID

                EcritureMessage(index, "[Dofus]", "Vous pouvez changer les droits de la maison de guilde.", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMaisonChangeCodeReussie", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiMaisonCodeEchec(index As Integer, data As String)

        With Comptes(index)

            Try

                ' KKE

                EcritureMessage(index, "[Dofus]", "Code erroné", Color.Red)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMaisonChangeCodeReussie", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiMaisonEnCodeFin(index As Integer, data As String)

        With Comptes(index)

            Try

                ' KV

                .Maison.EnCode = False

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiMaisonEnCodeFin", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Private Delegate Function dlgFMaisonMap()
    Public Function CopyMaisonMap(index As Integer, dico As Dictionary(Of Integer, CMaison)) As Dictionary(Of Integer, CMaison)

        With Comptes(index)

            If .FrmUser.InvokeRequired Then

                Return .FrmUser.Invoke(New dlgFMaisonMap(Function() CopyMaisonMap(index, dico)))

            Else

                Dim newDico As New Dictionary(Of Integer, CMaison)

                For Each pair As KeyValuePair(Of Integer, CMaison) In dico

                    newDico.Add(pair.Key, pair.Value)

                Next

                Return newDico

            End If

        End With

    End Function

End Module


#Region "Class"

Public Class CMaison

    Public Proprietaire As String
    Public ID As Integer
    Public Verouiller As Boolean
    Public EnVente As Boolean
    Public EnGuilde As Boolean
    Public GuildeNom As String
    Public Cellule As Integer
    Public MapID As Integer
    Public Coordonnees As String
    Public Prix As Integer
    Public Code As Integer
    Public Coffre As New Dictionary(Of String, CCoffre)

    Public EnCode As Boolean
End Class

Public Class CCoffre

    Public Verouiller As Boolean
    Public Cellule As Integer
    Public MapID As Integer
    Public Coordonnees As String
    Public Code As Integer

End Class

#End Region