Public Class FunctionGuilde


#Region "Ouverture"

    Public Function Ouvre(index As String) As Boolean

        With Comptes(index)

            Try

                Return .Send("gIG",
                            {"gIG", ' Xp de la guilde.
                             "gS"}) ' Information de la guilde

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_Ouvre", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Membres(index As String) As Boolean

        With Comptes(index)

            Try

                Return .Send("gIM",
                            {"gIM"}) ' Reçoit les membres de guilde.

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_Membres", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Personnalisation(index As String) As Boolean

        With Comptes(index)

            Try

                .Send("gIB",
                     {"gIB"}) ' Personnalisation percepteur.

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_Personnalisation", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Percepteurs(index As String) As Boolean

        With Comptes(index)

            Try

                Return .Send("gIT",
                            {"gIT"}) ' Pas sûr ?

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "Guilde_Percepteurs", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Enclos(index As String) As Boolean

        With Comptes(index)

            Try

                .Send("gITV")

                Return .Send("gIF",
                            {"gIF"}) ' Les enclos actuelle.

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_Enclos", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Maisons(index As String) As Boolean

        With Comptes(index)

            Try

                Return .Send("gIH",
                            {"gIH"}) ' Maison reçu.

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_Maisons", ex.Message)

            End Try

            Return False

        End With

    End Function

#End Region

#Region "Membres"

    Public Function Exclure(index As String, nom As String) As Boolean

        With Comptes(index)

            Try

                Return .Send("gK" & nom,
                            {"gKK"}) ' Joueur exlut.

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_Exclure", ex.Message)

            End Try

        End With

    End Function

    Public Function Invite(index As String, nom As String) As Boolean

        With Comptes(index)

            Try

                .Guilde.JoueurQuiEstInvite = nom

                Return .Send("gJR" & nom,
                            {"gJR"}) ' J'invite un joueur.

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_Invite", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Refuse(index As String) As Boolean

        With Comptes(index)

            Try

                Return .Send("gJE" & .Guilde.JoueurQuiInvite,
                            {"gJE"}) ' Refuse l'invitation.

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_Refuse", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Rang(index As String, membre As String, leRang As String) As Boolean

        With Comptes(index)

            Try

                Dim rangActuel() As String = {"a l'essai", "meneur", "bras droit", "tresorier", "protecteur", "artisan", "reserviste",
                "gardien", "eclaireur", "espion", "diplomate", "secretaire", "tueur de familiers", "braconnier", "chercheur de tresor",
                "voleur", "initie", "assassin", "gouverneur", "muse", "conseiller", "elu", "guide", "mentor", "recruteur",
                "eleveur", "marchand", "apprenti", "bourreau", "mascotte", "penitent", "tueur de percepteurs", "deserteur", "traitre",
                "boulet", "larbin", "a l'essai"}

                For i = 0 To rangActuel.Count - 1

                    If rangActuel(i) = leRang.ToLower Then

                        For Each pair As CGuildeJoueur In .Guilde.Membre.Values

                            If pair.Nom.ToLower = membre.ToLower Then

                                .Send("gP" & pair.IdUnique & "|" & i & "|" & pair.PrXp & "|" & pair.DroitChiffre)

                                Return True

                            End If

                        Next

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_Rang", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Experience(index As String, membre As String, valeur As String) As Boolean

        With Comptes(index)

            Try

                For Each pair As CGuildeJoueur In .Guilde.Membre.Values

                If pair.Nom.ToLower = membre.ToLower Then

                    .Send("gP" & pair.IdUnique & "|" & pair.RangChiffre & "|" & valeur & "|" & pair.DroitChiffre)

                    Return True

                End If

            Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_Experience", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function Droits(index As String, membre As String, lesDroits As String, Active As Boolean) As Boolean

        With Comptes(index)

            Try

                For Each pair As CGuildeJoueur In .Guilde.Membre.Values

                    If pair.Nom.ToLower = membre.ToLower Then

                        .Send("gP" & pair.IdUnique & "|" & pair.RangChiffre & "|" & pair.PrXp & "|" & ReturnDroit(pair.Droit, lesDroits, Active))

                        Return True

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_Droits", ex.Message)

            End Try

            Return False

        End With

    End Function

    Private Function ReturnDroit(membre As CGuildeDroit, lesDroits As String, Active As Boolean) As Integer

        Dim valeur As Integer = 0

        Try

            With membre

                valeur += If(.GererLesBoosts = True, 16384, 0)
                valeur += If(.GererLesDroits = True, 8192, 0)
                valeur += If(.InviterDeNouveauxMembres = True, 4096, 0)
                valeur += If(.Bannir = True, 512, 0)
                valeur += If(.GererLesRepartitionsXP = True, 256, 0)
                valeur += If(.GererSaRepartitionXP = True, 128, 0)
                valeur += If(.GererLesRangs = True, 64, 0)
                valeur += If(.PoserUnPercepteur = True, 32, 0)
                valeur += If(.CollecterSurUnPercepteur = True, 16, 0)
                valeur += If(.UtiliserLesEnclos = True, 8, 0)
                valeur += If(.AmenagerLesEnclos = True, 4, 0)
                valeur += If(.GererLesMonturesDesAutresMembres = True, 2, 0)

                Dim separate As String() = Split(lesDroits, " | ")

                For i = 0 To separate.Count - 1

                    Select Case separate(i).ToLower

                        Case "gerer les boosts"

                            valeur += If(Active = True, If(.GererLesBoosts = True, 0, 16384), If(.GererLesBoosts = True, -16384, -0))

                        Case "gerer les droits"

                            valeur += If(Active = True, If(.GererLesDroits = True, 0, 8192), If(.GererLesDroits = True, -8192, -0))

                        Case "inviter de nouveaux membres"

                            valeur += If(Active = True, If(.InviterDeNouveauxMembres = True, 0, 4096), If(.InviterDeNouveauxMembres = True, -4096, -0))

                        Case "bannir"

                            valeur += If(Active = True, If(.Bannir = True, 0, 512), If(.Bannir = True, -512, -0))

                        Case "gerer les repartitions d'xp"

                            valeur += If(Active = True, If(.GererLesRepartitionsXP = True, 0, 256), If(.GererLesRepartitionsXP = True, -256, -0))

                        Case "gerer sa repartition d'xp"

                            valeur += If(Active = True, If(.GererSaRepartitionXP = True, 0, 128), If(.GererSaRepartitionXP = True, -128, -0))

                        Case "gerer les rangs"

                            valeur += If(Active = True, If(.GererLesRangs = True, 0, 64), If(.GererLesRangs = True, -64, -0))

                        Case "poser un percepteur"

                            valeur += If(Active = True, If(.PoserUnPercepteur = True, 0, 32), If(.PoserUnPercepteur = True, -32, -0))

                        Case "collecter sur un percepteur"

                            valeur += If(Active = True, If(.CollecterSurUnPercepteur = True, 0, 16), If(.CollecterSurUnPercepteur = True, -16, -0))

                        Case "utiliser les enclos"

                            valeur += If(Active = True, If(.UtiliserLesEnclos = True, 0, 8), If(.UtiliserLesEnclos = True, -8, -0))

                        Case "amenager les enclos"

                            valeur += If(Active = True, If(.AmenagerLesEnclos = True, 0, 4), If(.AmenagerLesEnclos = True, -4, -0))

                        Case "gerer les montures des autres membres"

                            valeur += If(Active = True, If(.GererLesMonturesDesAutresMembres = True, 0, 2), If(.GererLesMonturesDesAutresMembres = True, -2, -0))

                    End Select

                Next

            End With

        Catch ex As Exception

        End Try

        Return valeur

    End Function

#End Region

#Region "Personnalisation"

    Public Function Up(index As String, choix As String, quantiter As String) As Boolean

        With Comptes(index)

            Try

                For i = 0 To CInt(quantiter)

                    Select Case choix.ToLower

                        Case "prospection"

                            If CInt(.Guilde.Percepteur.Prospection) < 500 Then

                                If CInt(.Guilde.Percepteur.ResteARepartir) >= 1 Then

                                    .Send("gBp",
                                         {"gIB"}) ' Personnalisation.

                                End If

                            End If

                        Case "sagesse"

                            If CInt(.Guilde.Percepteur.Sagesse) < 400 Then

                                If CInt(.Guilde.Percepteur.ResteARepartir) >= 1 Then

                                    .Send("gBx",
                                         {"gIB"}) ' Personnalisation.

                                End If

                            End If

                        Case "pods"

                            If CInt(.Guilde.Percepteur.Pods) < 5000 Then

                                If CInt(.Guilde.Percepteur.ResteARepartir) >= 1 Then

                                    .Send("gBo",
                                         {"gIB"}) ' Personnalisation.

                                End If

                            End If

                        Case "nombre de percepteur"

                            If CInt(.Guilde.Percepteur.NombreDePercepteur) < 50 Then

                                If CInt(.Guilde.Percepteur.ResteARepartir) >= 10 Then

                                    .Send("gBk",
                                         {"gIB"}) ' Personnalisation.

                                End If

                            End If

                        Case "armure aqueuse"

                            If CInt(.Guilde.Percepteur.ArmureAqueuse) < 5 Then

                                If CInt(.Guilde.Percepteur.ResteARepartir) >= 5 Then

                                    .Send("gB451",
                                         {"gIB"}) ' Personnalisation.

                                End If

                            End If

                        Case "armure incandescente"

                            If CInt(.Guilde.Percepteur.ArmureIncandescente) < 5 Then

                                If CInt(.Guilde.Percepteur.ResteARepartir) >= 5 Then

                                    .Send("gB452",
                                         {"gIB"}) ' Personnalisation.

                                End If

                            End If

                        Case "armure terrestre"

                            If CInt(.Guilde.Percepteur.ArmureTerrestre) < 5 Then

                                If CInt(.Guilde.Percepteur.ResteARepartir) >= 5 Then

                                    .Send("gB453",
                                         {"gIB"}) ' Personnalisation.

                                End If

                            End If

                        Case "armure venteuse"

                            If CInt(.Guilde.Percepteur.ArmureVenteuse) < 5 Then

                                If CInt(.Guilde.Percepteur.ResteARepartir) >= 5 Then

                                    .Send("gB454",
                                         {"gIB"}) ' Personnalisation.

                                End If

                            End If

                        Case "flamme"

                            If CInt(.Guilde.Percepteur.Flamme) < 5 Then

                                If CInt(.Guilde.Percepteur.ResteARepartir) >= 5 Then

                                    .Send("gB455",
                                         {"gIB"}) ' Personnalisation.

                                End If

                            End If

                        Case "cyclone"

                            If CInt(.Guilde.Percepteur.Cyclone) < 5 Then

                                If CInt(.Guilde.Percepteur.ResteARepartir) >= 5 Then

                                    .Send("gB456",
                                         {"gIB"}) ' Personnalisation.

                                End If

                            End If

                        Case "vague"

                            If CInt(.Guilde.Percepteur.Vague) < 5 Then

                                If CInt(.Guilde.Percepteur.ResteARepartir) >= 5 Then

                                    .Send("gB457",
                                         {"gIB"}) ' Personnalisation.

                                End If

                            End If

                        Case "rocher"

                            If CInt(.Guilde.Percepteur.Rocher) < 5 Then

                                If CInt(.Guilde.Percepteur.ResteARepartir) >= 5 Then

                                    .Send("gB458",
                                         {"gIB"}) ' Personnalisation.

                                End If

                            End If

                        Case "mot soignant"

                            If CInt(.Guilde.Percepteur.MotSoignant) < 5 Then

                                If CInt(.Guilde.Percepteur.ResteARepartir) >= 5 Then

                                    .Send("gB459",
                                         {"gIB"}) ' Personnalisation.

                                End If

                            End If

                        Case "desenvoutement"

                            If CInt(.Guilde.Percepteur.Desenvoutement) < 5 Then

                                If CInt(.Guilde.Percepteur.ResteARepartir) >= 5 Then

                                    .Send("gB460",
                                         {"gIB"}) ' Personnalisation.

                                End If

                            End If

                        Case "compulsion de masse"

                            If CInt(.Guilde.Percepteur.CompulsionDeMasse) < 5 Then

                                If CInt(.Guilde.Percepteur.ResteARepartir) >= 5 Then

                                    .Send("gB461",
                                         {"gIB"}) ' Personnalisation.

                                End If

                            End If

                        Case "destabilisation"

                            If CInt(.Guilde.Percepteur.Destabilisation) < 5 Then

                                If CInt(.Guilde.Percepteur.ResteARepartir) >= 5 Then

                                    .Send("gB462",
                                         {"gIB"}) ' Personnalisation.

                                End If

                            End If

                    End Select

                Next

                Return True

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function PoserPercepteur(index As String) As Boolean

        With Comptes(index)

            Try

                If .Personnage.Kamas >= CInt(.Guilde.Percepteur.CoûtPourPoserPercepteur) Then

                    Return .Send("gH",
                                {"gTS"}) ' Pose un percepteur.

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_PoserPercepteur", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function RetirerPercepteur(index As String) As Boolean

        With Comptes(index)

            Try

                For Each pair As CEntite In .Map.Entite.Values

                    If pair.Classe = "-6" Then

                        Return .Send("gF" & pair.IDUnique,
                                    {"gTR"}) ' Percepteur Retiré.

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_RetirerPercepteur", ex.Message)

            End Try

            Return False

        End With

    End Function

    Public Function ReleverPercepteur(index As String) As Boolean

        With Comptes(index)

            Try

                For Each pair As CEntite In .Map.Entite.Values

                    If pair.Classe = "-6" Then

                        Return .Send("ER8|-88" & pair.IDUnique,
                                    {"gTR"}) ' Percepteur retiré.

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_ReleverPercepteur", ex.Message)

            End Try

            Return False

        End With

    End Function

#End Region

#Region "Percepteurs"

#End Region

#Region "Enclos"

    Public Function EnclosTeleporter(index As Integer, enclos As String) As Boolean

        With Comptes(index)

            Try

                Return .Send("gf" & enclos,
                            {"GDM"}) ' Change de map.

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_EnclosTeleporter", ex.Message)

            End Try

            Return False

        End With

    End Function

#End Region

#Region "Maisons"

    Public Function MaisonsTeleporter(index As Integer, maisons As String) As Boolean

        With Comptes(index)

            Try

                For Each pair As CGuildeMaison In .Guilde.Maison.Values

                    If pair.Proriétaire.ToLower = maisons.ToLower Then

                        Return .Send("gh" & pair.ID,
                                    {"GDM"}) ' Change de map.

                    End If

                Next

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDuPersonnage, "FunctionGuilde_MaisonsTeleporter", ex.Message)

            End Try

            Return False

        End With

    End Function

#End Region

End Class
