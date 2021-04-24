
Module MdlConnexion

#Region "Connexion au serveur."

    Sub GiConnexionServeurAuthentification(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                'HC trkzqijwpzvunfezdxdhhlmmgxxgsbqm 
                'HC Clef Crypt Mdp 

                .EnConnexion = True

                EcritureMessage(index, "(Bot)", "Connecté au serveur d'authentification.", Color.Lime)

                Dim clefCrypt As String = Mid(data, 3)

                If .MITM = False Then

                    .Send(VarServeur("Authentification").ID)
                    .Send(.Personnage.NomDeCompte & Chr(10) & PassEnc(.Personnage.MotDePasse, clefCrypt))
                    .Send("Af")

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "ConnexionServeurAuthentification", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Private Function PassEnc(ByVal pwd As String, ByVal key As String) As String
        Dim l1, l2, l3, l4, l5 As Integer, l7 As String = "#1"
        Dim hash() As String = {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "-", "_"}
        Dim v1, v2 As String
        For l1 = 1 To Len(pwd)
            l2 = Asc(Mid(pwd, l1, 1))
            l3 = Asc(Mid(key, l1, 1))
            l5 = Fix(l2 / 16)
            l4 = l2 Mod 16
            v1 = hash(((l5 + l3) Mod (UBound(hash) + 1)) Mod (UBound(hash) + 1))
            v2 = hash(((l4 + l3) Mod (UBound(hash) + 1)) Mod (UBound(hash) + 1))
            l7 = l7 + v1 + v2
        Next
        Return l7
    End Function  ' Fonction du cryptage du mot de passe   'Provient de Maxoubot

    Sub GiConnexionServeurJeu(ByVal index As Integer)

        With Comptes(index)

            Try

                'HG 

                EcritureMessage(index, "[Dofus]", "Connecté au serveur de jeu, envoie du ticket.", Color.Green)

                .Send("AT" & .Personnage.Ticket)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiConnexionServeurJeu", ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "File d'attente"

    Async Sub GiFileAttenteAuthentification(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' Af 82              | 272            | 0 |   | -1 
                ' Af Position actuel | sur X personne | ? | ? | ?

                data = Mid(data, 3)

                Dim separate As String() = Split(data, "|")

                'Si la file d'attente est supérieur à 1, je refresh la file d'attente. toutes les 5 secondes.

                If CInt(separate(0)) > 10 Then
                    EcritureMessage(index, "[Dofus]", "En attente de connexion sur le serveur..." & vbCrLf &
                                                  "Position dans la file d'attente : " & separate(0), Color.Green)
                    If .MITM = False Then

                        Await Task.Delay(5000)

                        .Send("Af")

                    End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FileAttenteAuthentification", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Async Sub GiFileAttenteJeu(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                'Aq 1
                'Aq Position dans la queu.

                Dim position As Integer = Mid(data, 3)

                EcritureMessage(index, "[Dofus]", "Connexion au serveur... ( Position dans la file d'attente : " & position & " )", Color.Green)

                'Si la file d'attente est supérieur à 1, je refresh la file d'attente. toutes les 5 secondes.
                If position > 1 Then

                    If .MITM = False Then

                        Await Task.Delay(5000)

                        .Send("Af")

                    End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiFileAttenteJeu", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

    Sub GiPseudo(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' Ad Linaculer
                ' Ad Pseudo du compte

                .Personnage.Pseudo = Mid(data, 3)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Pseudo", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Async Sub GiReçoisServeur(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                'AH 601        ; 1            ; 75      ; 1       | 602;1;75;1
                'AH ID_Serveur ; Etat_Serveur ; Inconnu ; Inconnu | Next

                data = Mid(data, 3) 'Je prend les infos seulement après le "AH"

                Dim separateData As String() = Split(data, "|") ' 601;1;75;1|602;1;75;1

                For i = 0 To separateData.Count - 1 ' 601;1;75;1

                    Dim separate As String() = Split(separateData(i), ";")

                    If separate(0) = VarServeur(.Personnage.Serveur).ID Then ' ID = ID

                        Select Case separate(1)

                            Case "0"

                                EcritureMessage(index, "[Dofus]", "Serveur en Maintenance ! Déconnexion.", Color.Red)

                                If .MITM Then

                                    .Client.Close()

                                End If

                                .Socket_Authentification.Connexion_Game(False)

                            Case "1"

                            Case "2" ' En sauvegarde

                                EcritureMessage(index, "[Dofus]", "Serveur en sauvegarde !" & vbCrLf &
                                                                  "Refresh des serveurs dans 10 secondes.", Color.Red)

                                If .MITM = False Then

                                    Await Task.Delay(10000)

                                    ' Refresh la demande des serveurs  
                                    .Send("Ax")

                                    Return

                                End If

                            Case Else

                                ErreurFichier(index, .Personnage.NomDuPersonnage, "ReçoitServer", data)

                        End Select

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "ReçoitServeur", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Sub GiQuestionSecréte(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' AQ Quel+est+mon+mod%C3%A8le+de+voiture+pr%C3%A9f%C3%A9r%C3%A9+%3F
                ' AQ Question secréte

                If data.Length > 2 Then

                    'Je prend tout se qui se trouve après le "AQ"
                    data = Mid(data, 3)

                    'Je remplace les "+" par un espace.
                    data = data.Replace("+", " ")

                    .Personnage.QuestionSecréte = AsciiDecoder(data)

                    EcritureMessage(index, "[Dofus]", "Question secréte : " & .Personnage.QuestionSecréte, Color.Green)

                    If .MITM = False Then

                        '  a REFAIRE
                        .Send("Ap443")
                        .Send("Ai" & HasardMono(index) & vbCrLf)
                        .Send("Ax")

                    End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "QuestionSecréte", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Private Function HasardMono(index As Integer) As String

        Dim Minuscule As String() = {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"}
        Dim Majuscule As String() = {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"}
        Dim Number As Integer() = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
        Dim Rand As New Random

        Dim Resultat As String = index

        For i = 0 To 195

            Select Case Rand.Next(1, 3)

                Case 1

                    Resultat &= Minuscule(Rand.Next(0, 25))

                Case 2

                    Resultat &= Majuscule(Rand.Next(0, 25))

                Case 3

                    Resultat &= Number(Rand.Next(0, 9))

            End Select

        Next

        Return Resultat

    End Function

    Sub GiSelectionServeur(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                ' AxK 758257142                  | 601        , 5                    |
                ' AxK Abonnement en milliseconde | Id Serveur , Nombre de personnage | Next

                Dim separateData As String() = Split(Mid(data, 4), "|")

                .Personnage.Abonnement = DateAdd("s", separateData(0) \ 1000, Date.Now) 's = seconde

                If separateData.Length > 1 Then

                    For i = 1 To separateData.Count - 1

                        Dim separateServeur As String() = Split(separateData(i), ",")

                        If VarServeur(.Personnage.Serveur).ID = separateServeur(0) Then

                            EcritureMessage(index, "[Dofus]", "Connexion au serveur : " & VarServeur(.Personnage.Serveur).Nom, Color.Green)

                            .Send("AX" & VarServeur(.Personnage.Serveur).ID)

                            Exit Sub

                        End If

                    Next

                    EcritureMessage(index, "(Bot)", "Le serveur demander est introuvable, vérifier d'avoir bien créer un personnage en jeu avant de lancer le bot.", Color.Red)

                Else

                    EcritureMessage(index, "(Bot)", "Aucun serveur détecté, déconnexion du bot.", Color.Red)

                    If .MITM Then

                        .Client.Close()

                    End If

                    .Socket_Authentification.Connexion_Game(False)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "SelectionServeur", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiReceptionPersonnage(ByVal index As Integer, ByVal data As String)

        With Comptes(index)


            'ALK 25487456210      | 4              | 1234567       ; Linaculer      ; 101    ; 90     ; -1       ; -1       ; -1       ; 48a , 1bea   , 1b0f , 1f40     , Bouclier ; 0 ; 601        ;   ;   ;   | Next personnage
            'ALK Abonnement_Dofus | Nbr_Personnage | ID_Personnage ; Nom_Personnage ; Niveau ; Classe ; Couleur1 ; Couleur2 ; Couleur3 ; Cac , Coiffe , Cape , Familier , Bouclier ; ? ; ID_Serveur ; ? ; ? ; ? | Next

            Try

                Dim separateData As String() = Split(data, "|")

                'Abonnement
                .Personnage.Abonnement = DateAdd("s", Mid(separateData(0), 4, separateData(0).Length) \ 1000, Date.Now) 's = seconde

                If separateData(1) <> "0" Then

                    EcritureMessage(index, "(Bot)", "Réception des personnages. (" & separateData(1) & ")", Color.Lime)

                    For i = 2 To separateData.Count - 1

                        Dim separate() As String = Split(separateData(i), ";")

                        'Je regarde si le nom du personnage correspond à celui voulu par l'utilisateur. (Je mets en majuscule par sécurité, si la personne oublie une majuscule ou autre)
                        If separate(1).ToUpper = .Personnage.NomDuPersonnage.ToUpper Then

                            .Personnage.ID = separate(0)

                            .Personnage.NomDuPersonnage = separate(1)

                            .Personnage.Niveau = separate(2)

                            .Personnage.ClasseSexe = separate(3)

                            'Pour obtenir la couleur sur une var 'Color' = ColorTranslator.FromOle(.Couleur1)
                            ' .Couleur1 = "&H" & separate(4)
                            ' .Couleur2 = "&H" & separate(5)
                            ' .Couleur3 = "&H" & separate(6)

                            Dim separateItem As String() = Split(separate(7), ",")

                            If separate(7) <> "null" Then

                                Dim idCaC As Integer = If(separateItem(0) = "", 0, Convert.ToInt64(separateItem(0), 16))
                                Dim idCoiffe, coiffeLv, coiffeForme As Integer
                                Dim idCape, capeLv, capeForm As Integer

                                If separateItem(1).Contains("~") Then

                                    Dim separateObvijevan As String() = Split(separateItem(1), "~")

                                    idCoiffe = Convert.ToInt64(separateObvijevan(0), 16)
                                    coiffeLv = Convert.ToInt64(separateObvijevan(1), 16)
                                    coiffeForme = Convert.ToInt64(separateObvijevan(2), 16)

                                Else

                                    If separateItem(1) <> Nothing Then

                                        idCoiffe = Convert.ToInt64(separateItem(1), 16)

                                    End If

                                End If

                                If separateItem(2).Contains("~") Then

                                    Dim separateObvijevan As String() = Split(separateItem(2), "~")

                                    idCape = Convert.ToInt64(separateObvijevan(0), 16)
                                    capeLv = Convert.ToInt64(separateObvijevan(1), 16)
                                    capeForm = Convert.ToInt64(separateObvijevan(2), 16)

                                Else

                                    If separateItem(2) <> Nothing Then

                                        idCape = Convert.ToInt64(separateItem(2), 16)

                                    End If

                                End If

                                Dim idFamilier As Integer = If(separateItem(3) = "", 0, Convert.ToInt64(separateItem(3), 16))
                                Dim idBouclier As Integer = If(separateItem(4) = "", 0, Convert.ToInt64(separateItem(4), 16))

                            End If

                            .Personnage.IDServeur = separate(9)

                            EcritureMessage(index, "(Bot)", "Connexion au personnage : " & .Personnage.NomDuPersonnage, Color.Lime)

                            'J'envoie l'ID du personnage sur lequel je souhaite me connecter.
                            .Send("AS" & .Personnage.ID)
                            .Send("Af")

                            Exit For

                        End If

                    Next

                Else

                    '    If .NomPersonnage = "Aléatoire" Then

                    '       .Socket.Envoyer("AP")

                    '  Else

                    '     .Socket.Envoyer("AA" & .NomPersonnage & "|" & .Classe & "|" & .Sexe & "|" & .Couleur1 & "|" & .Couleur2 & "|" & .Couleur3)

                    ' End If

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiReceptionPersonnage", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#Region "Ip port"

    Public Sub GiServeurIpPortTicket(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            Try

                'AXK 98752:98 gr5   32tr9f
                'AXK IP       Port  Ticket 

                ' AYK eratz.ankama-games.com ; 0123f45
                ' AYK Ip et Port             ; Ticket
                .EnAuthentification = False
                EcritureMessage(index, "(Bot)", "Récuperation de l'IP, Port et du Ticket.", Color.Lime)

                Select Case Mid(data, 1, 3)

                    Case "AXK"

                        .Personnage.Ticket = Mid(data, 15)

                        Dim ip As String = DécryptIP(Mid(data, 4, 8))
                        Dim port As Integer = DécryptPort(Mid(data, 12, 3))

                        If ip <> VarServeur(.Personnage.Serveur).IP OrElse port <> VarServeur(.Personnage.Serveur).Port Then

                            ReplaceIpPort(index, .Personnage.Serveur, ip, port)

                        End If

                        If .MITM Then

                            .Socket = If(.Proxy.Active, New All_CallBack(ip, 443, .Proxy.Active, .Proxy.IP, .Proxy.Port, .Proxy.Identifiant, .Proxy.MotDePasse), New All_CallBack(ip, 443))

                            AddHandler .Socket.Deconnexion, AddressOf .e_Deconnexion
                            AddHandler .Socket.Envoie, AddressOf .e_Envoi
                            AddHandler .Socket.Reception, AddressOf .E_Reception
                            AddHandler .Socket.Reception, AddressOf .GestionReception

                        Else

                            .CreateSocketServeurJeu(ip, port)

                        End If

                    Case "AYK"

                        data = Mid(data, 4)

                        Dim separateData As String() = Split(data, ";")

                        .Personnage.Ticket = separateData(1)

                        If data.Contains(".com") Then

                            Dim ip As String = HostNameIP(separateData(0))
                            Dim port As String = VarServeur(.Personnage.Serveur).Port

                            If ip <> VarServeur(.Personnage.Serveur).IP Then

                                ReplaceIpPort(index, .Personnage.Serveur, ip, VarServeur(.Personnage.Serveur).Port)

                            End If

                            If .MITM Then

                                .Socket = If(.Proxy.Active, New All_CallBack(ip, 443, .Proxy.Active, .Proxy.IP, .Proxy.Port, .Proxy.Identifiant, .Proxy.MotDePasse), New All_CallBack(ip, 443))

                                AddHandler .Socket.Deconnexion, AddressOf .e_Deconnexion
                                AddHandler .Socket.Envoie, AddressOf .e_Envoi
                                AddHandler .Socket.Reception, AddressOf .E_Reception
                                AddHandler .Socket.Reception, AddressOf .GestionReception

                            Else

                                .CreateSocketServeurJeu(ip, port)

                            End If

                        Else

                            If .MITM Then

                                .Socket = If(.Proxy.Active, New All_CallBack(separateData(0), 443, .Proxy.Active, .Proxy.IP, .Proxy.Port, .Proxy.Identifiant, .Proxy.MotDePasse), New All_CallBack(separateData(0), 443))

                                AddHandler .Socket.Deconnexion, AddressOf .e_Deconnexion
                                AddHandler .Socket.Envoie, AddressOf .e_Envoi
                                AddHandler .Socket.Reception, AddressOf .E_Reception
                                AddHandler .Socket.Reception, AddressOf .GestionReception

                            Else

                                .CreateSocketServeurJeu(separateData(0), VarServeur(.Personnage.Serveur).Port)

                            End If

                        End If

                End Select

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiServeurIpPortTicket", ex.Message)

            End Try

        End With

    End Sub

    Private Function HostNameIP(ByVal hostname As String) As String

        Try

            Dim hostname2 As System.Net.IPHostEntry = System.Net.Dns.GetHostEntry(hostname)
            Dim ip As System.Net.IPAddress() = hostname2.AddressList
            Return ip(0).ToString()

        Catch ex As Exception

            ErreurFichier(0, "All", "HostNameIP", ex.Message)

        End Try

        Return 0

    End Function

    Private Sub ReplaceIpPort(ByVal index As Integer, ByVal _serveur As String, ByVal ip As String, ByVal port As String)

        With Comptes(index)

            Try

                'Je lis le fichier.
                Dim swLecture As New IO.StreamReader(Application.StartupPath + "\Data/Serveur.txt")

                Dim ligneFinal As String = ""

                Do Until swLecture.EndOfStream

                    Dim Ligne As String = swLecture.ReadLine

                    If Ligne <> "" Then

                        Dim separate As String() = Split(Ligne, "|")

                        If separate(0) = _serveur Then

                            ligneFinal &= _serveur & "|" & ip & "|" & port & "|" & VarServeur(_serveur).ID & vbCrLf

                        Else

                            ligneFinal &= Ligne & vbCrLf

                        End If

                    End If

                Loop

                'Puis je ferme le fichier.
                swLecture.Close()

                Dim swEcriture As New IO.StreamWriter(Application.StartupPath + "\Data/Serveur.txt")

                swEcriture.Write(ligneFinal)

                swEcriture.Close()

                ChargeServeur()

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "ReplaceIpPort", ex.Message)

            End Try

        End With

    End Sub

#Region "Cryptage/Décryptage"

    'Maxoubot
    Public Function DécryptIP(ByVal IP_Crypt As String) As String

        ' Fonction de cryptage serveur      

        Dim i As Long = 0
        Dim fois As Long = 0
        Dim ipServeurJeu As String = ""

        While (i < 8)

            i += 1
            fois += 1

            Dim dat1 As Integer = Asc(Mid(IP_Crypt, i, 1)) - 48

            i += 1

            Dim dat2 As Integer = Asc(Mid(IP_Crypt, i, 1)) - 48
            Dim Dat3 As String = Str((dat1 And 15) << 4 Or dat2 And 15)

            If fois > 1 Then

                ipServeurJeu += Mid(Dat3, 2)

            Else

                ipServeurJeu += Dat3

            End If

            If i < 8 Then

                ipServeurJeu += "."

            End If

        End While

        Return ipServeurJeu.Replace(" ", "")

    End Function

    'Salesprendes
    Dim caracteres_array As Char() = New Char() {"a"c, "b"c, "c"c, "d"c, "e"c, "f"c, "g"c, "h"c, "i"c, "j"c, "k"c, "l"c, "m"c, "n"c, "o"c, "p"c, "q"c, "r"c, "s"c, "t"c, "u"c, "v"c, "w"c, "x"c, "y"c, "z"c, "A"c, "B"c, "C"c, "D"c, "E"c, "F"c, "G"c, "H"c, "I"c, "J"c, "K"c, "L"c, "M"c, "N"c, "O"c, "P"c, "Q"c, "R"c, "S"c, "T"c, "U"c, "V"c, "W"c, "X"c, "Y"c, "Z"c, "0"c, "1"c, "2"c, "3"c, "4"c, "5"c, "6"c, "7"c, "8"c, "9"c, "-"c, "_"c}

    Public Function DécryptPort(ByVal chars As Char()) As Integer

        If chars.Length <> 3 Then Throw New ArgumentOutOfRangeException("Le port doit contenir au minimum 3 caractéres.")

        Dim port As Integer = 0

        For i As Integer = 0 To 2 - 1
            port += CInt((Math.Pow(64, 2 - i) * get_Hash(chars(i))))
        Next

        port += get_Hash(chars(2))

        Return port

    End Function

    Public Function get_Hash(ByVal ch As Char) As Short

        For i As Short = 0 To caracteres_array.Length - 1

            If caracteres_array(i) = ch Then Return i

        Next

        Return 0

    End Function

#End Region

#End Region

    Public Sub GiVersion(ByVal index As Integer, ByVal data As String)

        With Comptes(index)

            ' AlEv 1.30.1 
            ' AlEv Version

            'Mise à jour de la version automatiquement (celui du fichier)
            Dim swLecture As New IO.StreamReader(Application.StartupPath & "\Data/Serveur.txt")

            Dim ligneFinal As String = ""

            Do Until swLecture.EndOfStream

                Dim ligne As String = swLecture.ReadLine

                If ligne <> "" Then

                    Dim separate As String() = Split(ligne, "|")

                    If separate(0) = "Authentification" Then

                        ligneFinal &= ligne.Replace(separate(3), Mid(data, 5) & "e") & vbCrLf

                    Else

                        ligneFinal &= ligne & vbCrLf

                    End If

                End If

            Loop

            swLecture.Close()

            'J'ouvre le fichier pour y écrire se que je souhaite
            Dim swEcriture As New IO.StreamWriter(Application.StartupPath + "\Data/Serveur.txt")

            'J'écris dedans avant de le fermer.
            swEcriture.WriteLine(ligneFinal)

            'Puis je le ferme.
            swEcriture.Close()

            ChargeServeur()

        End With

    End Sub

End Module
