Module AmiInformation

    Sub GiAmiEnnemi(index As Integer, data As String)

        With Comptes(index)

            Try

                'FL | Leroienculeurné ; 1      ; Linaculers   ; 1     ; 0           ; 7         ; 1    ; 71 
                'FL | Pseudo          ; en ami ; Nom          ; Level ; alignement  ; id classe ; Sexe ; classe + sexe

                Dim separateData As String() = Split(data, "|")

                Select Case separateData(0)

                    Case "FL"

                        .Ami.Ami.Clear()

                    Case "iL"

                        .Ami.Ennemi.Clear()

                End Select

                If separateData.Length > 1 Then

                    For i = 1 To separateData.Count - 1

                        Dim separate As String() = Split(separateData(i), ";")

                        Dim newFriend As New CAmiInformation

                        With newFriend

                            .Pseudo = separate(0)

                            If separate.Length > 1 Then

                                .Connecte = True
                                .Nom = separate(2)
                                .Niveau = separate(3)

                                Select Case separate(4)

                                    Case "0"

                                        .Alignement = "Neutre"

                                    Case "1"

                                        .Alignement = "Bontarien"

                                    Case "2"

                                        .Alignement = "Brakmarien"

                                    Case Else

                                        .Alignement = "Inconnu"

                                End Select

                                Select Case separate(5)

                                    Case "1"

                                        .Classe = "Feca"

                                    Case "2"

                                        .Classe = "Osamodas"

                                    Case "3"

                                        .Classe = "Enutrof"

                                    Case "4"

                                        .Classe = "Sram"

                                    Case "5"

                                        .Classe = "Xelor"

                                    Case "6"

                                        .Classe = "Ecaflip"

                                    Case "7"

                                        .Classe = "Eniripsa"

                                    Case "8"

                                        .Classe = "Iop"

                                    Case "9"

                                        .Classe = "Cra"

                                    Case "10"

                                        .Classe = "Sadida"

                                    Case "11"

                                        .Classe = "Sacrieur"

                                    Case "12"

                                        .Classe = "Pandawa"

                                End Select

                                Select Case separate(6)

                                    Case "0"

                                        .Sex = "Male"

                                    Case "1"

                                        .Sex = "Femelle"

                                End Select

                                .ClasseSex = separate(7)

                            Else

                                .Connecte = False
                                .Nom = ""
                                .Niveau = ""
                                .Sex = ""
                                .Classe = ""
                                .ClasseSex = ""
                                .Alignement = ""

                            End If

                        End With

                        Select Case separateData(0)

                            Case "FL"

                                .Ami.Ami.Add(separate(0), newFriend)

                            Case "iL"

                                .Ami.Ennemi.Add(separate(0), newFriend)

                        End Select

                    Next

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiAmiEnnemi", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiAmiEnnemiAjoute(index As Integer, data As String)

        With Comptes(index)

            Try

                ' FAK Linacu ; 2 ; Linaculer ; 99     ; 9      ; 0    ; 90 
                ' FAK Pseudo ; ? ; Nom       ; Niveau ; Classe ; Sexe ; Classe + Sexe

                Dim separateData As String() = Split(Mid(data, 4), ";")

                Select Case Mid(separateData(0), 1, 3)

                    Case "FAK"

                        EcritureMessage(index, "[Dofus]", "(" & separateData(0) & ") " & separateData(2) & " a été ajouté à votre liste d'ami.", Color.Green)

                    Case "iAK"

                        EcritureMessage(index, "[Dofus]", "(" & separateData(0) & ") " & separateData(2) & " a été ajouté à votre liste d'ennemi", Color.Green)

                    Case "FAEa"

                        EcritureMessage(index, "[Dofus]", "Déjà dans ta liste d'amis.", Color.Red)

                    Case "iAEa"

                        EcritureMessage(index, "[Dofus]", "Déjà dans ta liste d'ennemis.", Color.Red)

                End Select

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiAmiEnnemiAjoute", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiAmiEnnemiInformation(index As Integer, data As String)

        With Comptes(index)

            Try

                ' BWK Linaculer | 1 | Lenculé | 7
                ' BWK Pseudo    | ? | Nom     | Zone

                data = Mid(data, 4)

                Dim separateData As String() = Split(data, "|")

                Dim phrase As String = separateData(2) & " (" & separateData(1) & ") se trouve en "

                Select Case separateData(3)

                    Case "-1"

                        phrase &= "zone inconnue."

                    Case "7"

                        phrase &= "bonta."

                    Case "11"

                        phrase &= "Brakmar"

                    Case "18"

                        phrase &= "Astrub"

                End Select

                With .Ami.Information

                    .Pseudo = separateData(0)
                    .Nom = separateData(2)
                    .Zone = separateData(3)

                End With

                EcritureMessage(index, "[Dofus]", phrase, Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiAmiEnnemiInformation", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiAmiEnnemiInformationEchec(index As Integer, data As String)

        With Comptes(index)

            Try

                ' BWE Linaculer 
                ' BWE Pseudo    

                .Ami.Information = New CAmiInformation

                EcritureMessage(index, "[Dofus]", Mid(data, 4) & " n'est pas connecté ou n'existe pas.", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiAmiEnnemiInformationEchec", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiAmiEnnemiSupprimer(index As Integer, data As String)

        With Comptes(index)

            Try

                'FDK 
                'iDK
                'iAEf
                'FAEf

                Select Case data

                    Case "FDK"

                        EcritureMessage(index, "[Dofus]", "Tu viens de perdre un ami.", Color.Green)

                    Case "iDK"

                        EcritureMessage(index, "[Dofus]", "L'ennemi a été effacé, la paix gagne une bataille.", Color.Green)

                    Case "iAEf", "FAEf"

                        EcritureMessage(index, "[Dofus]", "Impossible, ce perso ou compte n'existe pas ou n'est pas connecté.", Color.Red)

                End Select

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiAmiEnnemiSupprimer", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiAmiEnnemiAvertie(index As Integer, data As String)

        With Comptes(index)

            Try

                ' FO - ou +
                ' FO - ou +

                Select Case Mid(data, 3)

                    Case "-"

                        .Ami.Avertie = False

                        EcritureMessage(index, "[Dofus]", "Vous serai pas avertie lors de la connexion d'un ami.", Color.Green)

                    Case "+"

                        .Ami.Avertie = True

                        EcritureMessage(index, "[Dofus]", "Vous serai avertie lors de la connexion d'un ami.", Color.Green)

                End Select

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiAmiEnnemiAvertie", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

End Module

#Region "Class"

Public Class CAmi

    Public Ami As New Dictionary(Of String, CAmiInformation)
    Public Ennemi As New Dictionary(Of String, CAmiInformation)
    Public Ignore As New Dictionary(Of String, CAmiInformation)
    Public Information As New CAmiInformation
    Public Avertie As Boolean

End Class

Public Class CAmiInformation

    Public Pseudo As String
    Public Connecte As Boolean
    Public Nom As String
    Public Niveau As String
    Public Alignement As String
    Public Classe As String
    Public Sex As String
    Public ClasseSex As String
    Public Zone As Integer

End Class

#End Region
