
Module mdlGuilde

    Private Delegate Function dlgGuilde()

#Region "Guilde Base"

    Public Sub GiGuildeInformation(index As Integer, data As String)

        With Comptes(index)

            Try

                ' gSLinaculerBot|a|0|i|8fsdgj|0 = La plupart à part le nom = Inconnu

                Dim separateData As String() = Split(data, "|")

                .Guilde.EnGuilde = True

                .Guilde.Nom = Mid(separateData(0), 3)

                If .Guilde.Invitation Then

                    .Guilde.Invitation = False

                    EcritureMessage(index, "[Dofus]", "Tu viens d'intégrer la guilde " & Mid(separateData(0), 3), Color.DarkViolet)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGuildeInformation", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiGuildeExp(index As Integer, data As String)

        With Comptes(index)

            Try

                ' gIG 0 | 5      | 28000   | 30271        | 48000
                ' gIG ? | Niveau | exp min | exp actuelle | exp max

                Dim separate As String() = Split(data, "|")

                With .Guilde

                    .Niveau = separate(1)
                    .ExpMinimum = separate(2)
                    .ExpActuelle = separate(3)
                    .ExpMaximum = separate(4)

                End With

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGuildeExp", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiGuildeInvitationEnCours(index As Integer, data As String)

        With Comptes(index)

            Try

                ' gJR Linaculer
                ' gJR Nom

                .Guilde.Invitation = True

                .Guilde.JoueurQuiInvite = .Personnage.ID

                EcritureMessage(index, "[Dofus]", "Tu invites " & Mid(data, 4) & " à rejoindre ta guilde...", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGuildeInvitationEnCours", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiGuildeInvitationEnCoursInformation(index As Integer, data As String)

        With Comptes(index)

            Try

                ' gJr 1234567 | Linaculer | [Ankama]
                ' gJr ID      | Nom       | Nom Guilde

                Dim separateData As String() = Split(Mid(data, 4), "|")

                .Guilde.Invitation = True

                .Guilde.JoueurQuiInvite = separateData(0)

                EcritureMessage(index, "[Dofus]", separateData(1) & " t'invite à rejoindre sa guilde (" & separateData(2) & ") acceptes-tu ?", Color.Green)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGuildeInvitationEnCoursInformation", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiGuildeInvitationEnCoursRefuse(index As Integer, data As String)

        With Comptes(index)

            Try

                ' gJE 1234567
                ' gJE id

                .Guilde.Invitation = False
                .Guilde.JoueurQuiInvite = ""
                .Guilde.JoueurQuiEstInvite = ""

                EcritureMessage(index, "[Dofus]", Mid(data, 4) & " refuse d'intégrer ta guilde.", Color.Red)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGuildeInvitationEnCoursRefuse", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiGuildeInvitationEnCoursAccepte(index As Integer, data As String)

        With Comptes(index)

            Try

                ' gJKa Linaculer
                ' gJKa nom

                .Guilde.Invitation = False
                .Guilde.JoueurQuiEstInvite = ""
                .Guilde.JoueurQuiInvite = ""

                EcritureMessage(index, "[Dofus]", Mid(data, 4) & " refuse d'intégrer ta guilde.", Color.Red)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGuildeInvitationEnCoursAccepte", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "Guilde Membres"

    Public Sub GiGuildeJoueurExclut(index As Integer, data As String)

        With Comptes(index)

            Try

                ' gKK

                Dim separateData As String() = Split(Mid(data, 4), "|")

                If separateData(0).ToLower = .Personnage.NomDuPersonnage.ToLower Then

                    EcritureMessage(index, "[Dofus]", "Tu as banni " & separateData(1) & " de ta guilde.", Color.Green)

                Else

                    EcritureMessage(index, "[Dofus]", "", Color.Green)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGuildeJoueurExclut", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GuildeMembresAjoute(index As Integer, data As String)

        With Comptes(index)

            Try

                ' gIM+ 1234567  ; Linaculer ; 60     ; 81     ; 2    ; 0        ; 0   ; 29694 ; 1        ; 0          ; 0                  | Next
                ' gIM+ IdUnique ; Nom       ; Niveau ; Classe ; Rang ; XpGagnée , %Xp ; Droit ; Connecté ; Alignement ; Dernière connexion | 
                Dim rangActuel() As String = {"A l'essai", "Meneur", "Bras Droit", "Trésorier", "Protecteur", "Artisan", "Réserviste", "Gardien", "Eclaireur", "Espion", "Diplomate", "Secrétaire",
            "Tueur de familiers", "Braconnier", "Chercheur de trésor", "Voleur", "Initié", "Assassin", "Gouverneur", "Muse", "Conseiller", "Elu", "Guide", "Mentor", "Recruteur",
            "Eleveur", "Marchand", "Apprenti", "Bourreau", "Mascotte", "Pénitent", "Tueur de Percepteurs", "Déserteur", "Traître", "Boulet", "Larbin", "A l'essai"}

                With .Guilde

                    .Membre.Clear()

                    Dim separateData As String() = Split(data, "|")

                    For i = 0 To separateData.Count - 1

                        Dim separate As String() = Split(separateData(i), ";")

                        Dim NewJoueur As New CGuildeJoueur

                        With NewJoueur

                            .IdUnique = separate(0)
                            .Nom = separate(1)
                            .Niveau = separate(2)
                            .Classe = GuildeClasseAlignement(separate(3))
                            .Rang = rangActuel(separate(4))
                            .RangChiffre = separate(4)
                            .XpGagnée = separate(5)
                            .PrXp = separate(6)
                            .Connecté = separate(8)
                            .Alignement = GuildeClasseAlignement(separate(9))
                            .DernièreConnection = If(separate(10) = -1, "Dernière connexion il y a moins d'un jour", "")

                            Dim NewDroit As New CGuildeDroit

                            NewDroit = GuildeDroits(index, separate(1), separate(7))

                            .Droit = NewDroit
                            .DroitChiffre = separate(7)

                        End With

                        .Membre.Add(separate(1), NewJoueur)

                    Next

                End With

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GuildeMembresAjoute", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Function GuildeClasseAlignement(Valeur As Integer) As String

        Try

            Select Case Valeur
                Case 0 : Return "Neutre" 'Neutre
                Case 1 : Return "Brakmarien"
                Case 2 : Return "Bontarien"
                Case 10, 11 : Return "Feca"
                Case 20, 21 : Return "Osamodas"
                Case 30, 31 : Return "Enutrof"
                Case 40, 41 : Return "Sram"
                Case 50, 51 : Return "Xelor"
                Case 60, 61 : Return "Ecaflip"
                Case 70, 71 : Return "Eniripsa"
                Case 80, 81 : Return "Iop"
                Case 90, 91 : Return "Cra"
                Case 100, 101 : Return "Sadida"
                Case 110, 111 : Return "Sacrieur"
                Case 120, 121 : Return "Pandawa"
                Case Else
                    Return Valeur
            End Select

        Catch ex As Exception

            ErreurFichier(0, "unknow", "", ex.Message)

        End Try

        Return Valeur

    End Function

    Public Function GuildeDroits(Index As Integer, Name_Joueur As String, Information As Integer) As CGuildeDroit

        With Comptes(Index)

            Dim newRights As New CGuildeDroit
            Dim Valeur() As Integer = {16384, 8192, 4096, 512, 256, 128, 64, 32, 16, 8, 4, 2}

            Try

                With newRights

                    If Information = 1 Then

                        .GererLesBoosts = True
                        .GererLesDroits = True
                        .InviterDeNouveauxMembres = True
                        .Bannir = True
                        .GererLesRepartitionsXP = True
                        .GererSaRepartitionXP = True
                        .GererLesRangs = True
                        .PoserUnPercepteur = True
                        .CollecterSurUnPercepteur = True
                        .UtiliserLesEnclos = True
                        .AmenagerLesEnclos = True
                        .GererLesMonturesDesAutresMembres = True

                    Else

                        For i = 0 To 11

                            If Information >= Valeur(i) Then

                                Select Case Information

                                    Case 16384

                                        .GererLesBoosts = True

                                    Case 8192

                                        .GererLesDroits = True

                                    Case 4096

                                        .InviterDeNouveauxMembres = True

                                    Case 512

                                        .Bannir = True

                                    Case 256

                                        .GererLesRepartitionsXP = True

                                    Case 128

                                        .GererSaRepartitionXP = True

                                    Case 64

                                        .GererLesRangs = True

                                    Case 32

                                        .PoserUnPercepteur = True

                                    Case 16

                                        .CollecterSurUnPercepteur = True

                                    Case 8

                                        .UtiliserLesEnclos = True

                                    Case 4

                                        .AmenagerLesEnclos = True

                                    Case 2

                                        .GererLesMonturesDesAutresMembres = True

                                End Select

                                Information -= Valeur(i)

                            End If

                        Next

                    End If

                End With

            Catch ex As Exception

                ErreurFichier(Index, .Personnage.NomDuPersonnage, "GuildeDroits", ex.Message)

            End Try

            Return newRights

        End With

    End Function

    Public Sub GuildeMembreSupprime(index As Integer, data As Integer)

        With Comptes(index)

            Try

                ' gIM- Linaculer
                ' gIM- nom du joueur

                Dim nom As String = Mid(data, 5)

                If .Guilde.Membre.ContainsKey(nom) Then

                    .Guilde.Membre.Remove(nom)

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GuildeMembreSupprime", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Function CopyGuildeMembre(index As Integer, dico As Dictionary(Of String, CGuildeJoueur)) As Dictionary(Of String, CGuildeJoueur)

        With Comptes(index)

            Dim newDico As New Dictionary(Of String, CGuildeJoueur)

            Try

                If .FrmUser.InvokeRequired Then

                    Return .FrmUser.Invoke(New dlgGuilde(Function() CopyGuildeMembre(index, dico)))

                Else

                    For Each pair As KeyValuePair(Of String, CGuildeJoueur) In dico

                        newDico.Add(pair.Key, pair.Value)

                    Next

                    Return newDico

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "CopyGuildeMembre", ex.Message)

            End Try

            Return newDico

        End With

    End Function

#End Region

#Region "Guilde Personnalisation"

    Public Sub GiGuildePersonnalisation(index As Integer, data As String)

        With Comptes(index)

            Try

                ' gIB1 |0|200|2|1000|100|0|1|5|1020|462;0|461;0|460;0|459;0|458;0|457;0|456;0|455;0|454;0|453;0|452;0|451;0
                ' gIB1 |                   ?

                Dim NewGuildePercepteur As New CGuildePercepteur

                Dim separateData As String() = Split(data, "|")

                With NewGuildePercepteur

                    .NombreDePercepteur = separateData(0)
                    .ActuellementPercepteur = separateData(1)
                    .PointsDeVie = separateData(2)
                    .BonusAuxDommages = separateData(3)
                    .Pods = separateData(4)
                    .Prospection = separateData(5)
                    .Sagesse = separateData(6)

                    .ResteARepartir = separateData(8)
                    .CoûtPourPoserPercepteur = separateData(9)

                    .ArmureAqueuse = Split(separateData(21), ";")(0)
                    .ArmureIncandescente = Split(separateData(20), ";")(0)
                    .ArmureTerrestre = Split(separateData(19), ";")(0)
                    .ArmureVenteuse = Split(separateData(18), ";")(0)
                    .Flamme = Split(separateData(17), ";")(0)
                    .Cyclone = Split(separateData(16), ";")(0)
                    .Vague = Split(separateData(15), ";")(0)
                    .Rocher = Split(separateData(14), ";")(0)
                    .MotSoignant = Split(separateData(13), ";")(0)
                    .Desenvoutement = Split(separateData(12), ";")(0)
                    .CompulsionDeMasse = Split(separateData(11), ";")(0)
                    .Destabilisation = Split(separateData(10), ";")(0)

                End With

                .Guilde.Percepteur = NewGuildePercepteur

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGuildePersonnalisation", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

#End Region

#Region "Guilde Percepteurs"

    Public Sub GiGuildePercepteurPosee(index As Integer, data As String)

        With Comptes(index)

            Try

                'gTS12,28|8840|-15|6|Name
                '12,28 = ?

                Dim separateData As String() = Split(data, "|")

                EcritureMessage(index, "[Guilde - Information]", "Le percepteur [Name] a été posé en (" & separateData(2) & ", " & separateData(3) & ") par " & separateData(4), Color.DarkViolet)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGuildePercepteurPosee", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiGuildePercepteur(index As Integer, data As String)

        With Comptes(index)

            Try

                ' gITM+ 2ki ; b , 28 , Linaculer1    , 1516232486517 ,   , 0 , 0 ; 2ki ; 0 
                ' gITM+     ; ? , ?  , Nom du poseur , ?             , ? , ? , ? ; ?   ; ?

                'Inconnu actuellement

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGuildePercepteurPosee", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiGuildePercepteurPoseEchec(index As Integer, data As String)

        With Comptes(index)

            Try

                ' gHEy

                EcritureMessage(index, "[Dofus]", "Impossible de poser le percepteur maintenant, il doit se reposer.", Color.Red)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGuildePercepteurPoseEchec", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Sub GiGuildePercepteurRetire(index As Integer, data As String)

        With Comptes(index)

            Try

                'gTRm,f|8858|-17|7|Name

                Dim separateData As String() = Split(data, "|")

                EcritureMessage(index, "[Guilde - Information]", "Le percepteur [Name] en (" & separateData(2) & ", " & separateData(3) & ") a été retiré par " & separateData(4), Color.DarkViolet)

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGuildePercepteurRetire", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Function CopyGuildePercepteur(index As Integer, dico As Dictionary(Of String, CGuildePercepteur)) As Dictionary(Of String, CGuildePercepteur)

        With Comptes(index)

            Dim newDico As New Dictionary(Of String, CGuildePercepteur)

            Try

                If .FrmUser.InvokeRequired Then

                    Return .FrmUser.Invoke(New dlgGuilde(Function() CopyGuildePercepteur(index, dico)))

                Else

                    For Each pair As KeyValuePair(Of String, CGuildePercepteur) In dico

                        newDico.Add(pair.Key, pair.Value)

                    Next

                    Return newDico

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "CopyGuildePercepteur", ex.Message)

            End Try

            Return newDico

        End With

    End Function

#End Region

#Region "Guilde Enclos"

    Public Sub GiGuildeEnclos(index As Integer, data As String)

        With Comptes(index)

            Try

                ' gIF3 | 9999 ; 2 ; 2 ; 
                ' gIF3 | 9999 ; 2 ; 2 ; 

                .Guilde.Enclos.Clear()

                Dim separateData As String() = Split(data, "|")

                For i = 1 To separateData.Count - 1

                    Dim separate As String() = Split(separateData(i), ";")

                    Dim newEnclos As New CGuildeEnclos

                    With newEnclos

                        .MapID = separate(0)
                        .Position = VarMap(separate(0))
                        .DragodindeActuelle = separate(1)
                        .DragodindeActuelle = separate(2)

                    End With

                    .Guilde.Enclos.Add(separate(0), newEnclos)

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGuildeEnclos", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Function CopyGuildeEnclos(index As Integer, dico As Dictionary(Of String, CGuildeEnclos)) As Dictionary(Of String, CGuildeEnclos)

        With Comptes(index)

            Dim newDico As New Dictionary(Of String, CGuildeEnclos)

            Try

                If .FrmUser.InvokeRequired Then

                Return .FrmUser.Invoke(New dlgGuilde(Function() CopyGuildeEnclos(index, dico)))

            Else

                    For Each pair As KeyValuePair(Of String, CGuildeEnclos) In dico

                    newDico.Add(pair.Key, pair.Value)

                Next

                Return newDico

            End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "CopyGuildeEnclos", ex.Message)

            End Try

            Return newDico

        End With

    End Function

#End Region

#Region "Guilde Maisons"

    Public Sub GiGuildeMaison(index As Integer, data As String)

        With Comptes(index)

            Try

                'gIH+ 999 ; Linaculer   ; 666,66 ;            ; 499    |
                '     ID  ; proriétaire ; Pos    ; compétence ; Droits | Next

                .Guilde.Maison.Clear()

                Dim separateData As String() = Split(Mid(data, 5), "|")

                For i = 0 To separateData.Count - 1

                    Dim separate As String() = Split(separateData(i), ";")

                    Dim Valeur As Integer() = {256, 128, 64, 32, 16, 8, 4, 2, 1}

                    Dim newMaison As New CGuildeMaison

                    With newMaison

                        For a = 0 To 8

                            If CInt(separate(4)) >= Valeur(a) Then

                                Select Case Valeur(a)

                                    Case 256 '256 =  Repos autorisé aux membres de la guilde dans cette maison

                                        .ReposAutoriser = True

                                    Case 128 '128 = Téléportation autorisée vers cette maison

                                        .TeleportationAutoriser = True

                                    Case 64 '64 = Accès aux coffres interdit aux non-membres de la guilde

                                        .AccesCoffresInterditNonMembreGuilde = True

                                    Case 32 '32 = Accès aux coffres autorisé aux membres de la guilde

                                        .AccesCoffresAutoriseMembreGuilde = True

                                    Case 16 '16 = Accès interdit aux non-membres de la guilde

                                        .AccesInterditNonMembreGuilde = True

                                    Case 8 '8 = Accès autorisé aux membres de la guilde

                                        .AccesAutoriserMembreGuilde = True

                                    Case 4 '4 = Blason Visible pour tout le monde

                                        .BlasonVisiblePourToutMonde = True

                                    Case 2 '2 = Blason Visible pour la guilde

                                        .BlasonVisiblePourGuilde = True

                                    Case 1 '1 = Maison Visible pour la guilde.

                                        .MaisonVisiblePourGuilde = True

                                End Select

                                separate(4) = CInt(separate(4)) - Valeur(a)

                            End If

                        Next

                        .ID = separate(0)
                        .Proriétaire = separate(1)
                        .Position = separate(2)
                        .Compétence = separate(3)

                    End With

                    .Guilde.Maison.Add(separate(1), newMaison)

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "GiGuildeMaison", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

    Public Function CopyGuildeMaison(index As Integer, dico As Dictionary(Of String, CGuildeMaison)) As Dictionary(Of String, CGuildeMaison)

        With Comptes(index)

            Dim newDico As New Dictionary(Of String, CGuildeMaison)

            Try

                If .FrmUser.InvokeRequired Then

                    Return .FrmUser.Invoke(New dlgGuilde(Function() CopyGuildeMaison(index, dico)))

                Else

                    For Each pair As KeyValuePair(Of String, CGuildeMaison) In dico

                        newDico.Add(pair.Key, pair.Value)

                    Next

                    Return newDico

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "CopyGuildeMaison", ex.Message)

            End Try

            Return newDico

        End With

    End Function

#End Region

End Module

#Region "Class"

Public Class CGuilde

    Public Bloque As Threading.ManualResetEvent = New Threading.ManualResetEvent(False)
    Public Niveau As String
    Public ExpMinimum As String
    Public ExpMaximum As String
    Public ExpActuelle As String
    Public EnGuilde As Boolean
    Public Invitation As Boolean
    Public Nom As String
    Public JoueurQuiInvite, JoueurQuiEstInvite As String
    Public Membre As New Dictionary(Of String, CGuildeJoueur)
    Public Percepteur As CGuildePercepteur
    Public Enclos As New Dictionary(Of String, CGuildeEnclos)
    Public Maison As New Dictionary(Of String, CGuildeMaison)

End Class

Public Class CGuildeJoueur

    Public Classe As String
    Public Nom As String
    Public IdUnique As String
    Public Rang As String
    Public RangChiffre As Integer
    Public Niveau As String
    Public PrXp As String
    Public Connecté As Boolean
    Public XpGagnée As String
    Public Alignement As String
    Public Droit As CGuildeDroit
    Public DroitChiffre As Integer
    Public DernièreConnection As String

End Class

Public Class CGuildeDroit

    Public GererLesBoosts As Boolean
    Public GererLesDroits As Boolean
    Public InviterDeNouveauxMembres As Boolean
    Public Bannir As Boolean
    Public GererLesRepartitionsXP As Boolean
    Public GererSaRepartitionXP As Boolean
    Public GererLesRangs As Boolean
    Public PoserUnPercepteur As Boolean
    Public CollecterSurUnPercepteur As Boolean
    Public UtiliserLesEnclos As Boolean
    Public AmenagerLesEnclos As Boolean
    Public GererLesMonturesDesAutresMembres As Boolean

End Class

Public Class CGuildePercepteur

    Public PointsDeVie As String
    Public BonusAuxDommages As String
    Public Prospection As String
    Public Sagesse As String
    Public Pods As String
    Public NombreDePercepteur As String
    Public ResteARepartir As String
    Public ActuellementPercepteur As String
    Public CoûtPourPoserPercepteur As String

    'Spell
    Public ArmureAqueuse As String
    Public ArmureIncandescente As String
    Public ArmureTerrestre As String
    Public ArmureVenteuse As String
    Public Flamme As String
    Public Cyclone As String
    Public Vague As String
    Public Rocher As String
    Public MotSoignant As String
    Public Desenvoutement As String
    Public CompulsionDeMasse As String
    Public Destabilisation As String

End Class

Public Class CGuildeEnclos

    Public MapID As Integer
    Public Position As String
    Public DragodindeActuelle As Integer
    Public DragodindeMaximum As Integer

End Class

Public Class CGuildeMaison

    Public MaisonVisiblePourGuilde As Boolean = False
    Public BlasonVisiblePourGuilde As Boolean = False
    Public BlasonVisiblePourToutMonde As Boolean = False
    Public AccesAutoriserMembreGuilde As Boolean = False
    Public AccesInterditNonMembreGuilde As Boolean = False
    Public AccesCoffresAutoriseMembreGuilde As Boolean = False
    Public AccesCoffresInterditNonMembreGuilde As Boolean = False
    Public TeleportationAutoriser As Boolean = False
    Public ReposAutoriser As Boolean = False

    Public ID As Integer
    Public Proriétaire As String
    Public Position As String
    Public Compétence As String

End Class
#End Region
