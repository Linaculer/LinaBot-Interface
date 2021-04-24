Module TrajetExecution

    Public Sub TrajetLecture(index As Integer, balise As String)

        With Comptes(index)

            Try

                While True

                    If RetourneEnBanque(index) Then

                        TrajetEnCours(index, "banque()", True)

                    Else

                        TrajetEnCours(index, balise)

                    End If

                End While

            Catch ex As Exception

            End Try

        End With

    End Sub

    Public Function RetourneEnBanque(index As Integer) As Boolean

        With Comptes(index)

            Try

                For Each bot As Player In Comptes

                    If bot.FrmGroupe Is .FrmGroupe Then

                        If bot.Personnage.Pods.Pourcentage >= .FrmGroupe.Pods Then

                            Return True

                        End If

                    End If

                Next

            Catch ex As Exception

            End Try

            Return False

        End With

    End Function

    Public Function TrajetEnCours(index As Integer, balise As String, Optional banque As Boolean = False) As Boolean

        With Comptes(index)

            Try

                Dim SearchEndLine As String = ""
                Dim nextLine As Boolean = True
                Dim whileLine As Integer = 0
                Dim SelectCase As String = ""

                For a = 0 To .Trajet(balise).Count - 1

                    With .Trajet(balise)

                        If .Item(a) <> "" AndAlso Mid(.Item(a), 1, 1) <> "'" Then

                            Dim separateAction As String() = Split(.Item(a), " : ")

                            For i = 0 To separateAction.Count - 1

                                If banque = False AndAlso RetourneEnBanque(index) Then

                                    Return True

                                End If

                                Dim separatePair As String() = Split(separateAction(i), " ")

                                Select Case separatePair(0).ToLower

                                    Case "if"

                                        nextLine = IfReturn(index, .Item(a))

                                        If nextLine Then

                                            SearchEndLine = "end if"

                                        End If

                                    Case "elseif"

                                        If SearchEndLine <> "end if" Then

                                            nextLine = IfReturn(index, .Item(a))

                                        End If

                                        If nextLine Then

                                            SearchEndLine = "end if"

                                        End If

                                    Case "else"

                                        If SearchEndLine <> "end if" Then

                                            If nextLine Then nextLine = False

                                        Else

                                            SearchEndLine = "end if"
                                            nextLine = True

                                        End If

                                    Case "end"

                                        SearchEndLine = ""

                                        Select Case separatePair(1).ToLower

                                            Case "if"

                                                nextLine = True

                                            Case "while"

                                                If nextLine Then

                                                    a = whileLine - 1

                                                Else

                                                    nextLine = True
                                                    whileLine = 0

                                                End If

                                            Case "sub"

                                            Case "select"

                                        End Select

                                    Case "while"

                                        If WhileReturn(index, .Item(a)) Then

                                            nextLine = True
                                            whileLine = a

                                        Else

                                            nextLine = False
                                            whileLine = 0

                                        End If

                                    Case "call"

                                        If nextLine Then
                                            nextLine = TrajetEnCours(index, separatePair(1).ToLower, banque)
                                        End If

                                    'Select case
                                    Case "select"

                                        If SearchEndLine = "" Then SelectCase = SelectReturn(index, .Item(a))

                                    Case "case"

                                        Select Case separatePair(1).ToLower

                                            Case "else"

                                                If SearchEndLine = "end select" Then

                                                    If nextLine Then nextLine = False

                                                Else

                                                    SearchEndLine = "end select"
                                                    nextLine = True

                                                End If

                                            Case Else

                                                If SearchEndLine <> "end select" AndAlso SearchEndLine <> "end if" Then

                                                    nextLine = SelectCaseReturn(index, .Item(a), SelectCase)

                                                End If

                                                If nextLine Then

                                                    SearchEndLine = "end select"

                                                End If

                                        End Select
                                    '/Select case

                                    Case "return"

                                        Dim retourneValeur As String = separateAction(i).ToLower
                                        Return Action(index, Split(retourneValeur, "return ")(1))

                                    Case Else

                                        If nextLine Then

                                            Task.Delay(100).Wait()

                                            If Action(index, separateAction(i)) = False Then

                                                Exit For

                                            End If

                                        End If

                                End Select

                                '   Bloque(index)

                            Next

                        End If

                    End With

                    Task.Delay(1).Wait()

                Next

            Catch ex As Exception

            End Try

        End With

        Return True

    End Function

    Private Sub Bloque(index As Integer)

        With Comptes(index)

            Try

                If .Combat.EnCombat Then

                    While .Combat.EnCombat

                        Task.Delay(1000).Wait()

                    End While

                    Task.Delay(2000).Wait()

                End If

                If .Personnage.Regeneration < .FrmGroupe.Regeneration Then

                    Dim pause As Integer = (.Personnage.Vitaliter.Maximum * .FrmGroupe.Regeneration) / 100
                    pause = pause - .Personnage.Vitaliter.Actuelle
                    If pause > 0 Then

                        .Send("eU1",
                     {"ILF",
                      "ILS",
                      "eUK" & .Personnage.ID})
                        Task.Delay(pause * 1000).Wait()

                    End If
                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

    Public Function ReturnParametre(index As Integer, laligne As String) As Object

        Dim separateFunctionParamétre As String()

        If laligne.Contains(" = ") Then

            separateFunctionParamétre = Split(laligne, " = ")

            If separateFunctionParamétre(1).Contains("(") Then

                separateFunctionParamétre = Split(separateFunctionParamétre(1).Replace("""", ""), "(")
                separateFunctionParamétre = Split(separateFunctionParamétre(1), ")")
                separateFunctionParamétre = Split(separateFunctionParamétre(0), " , ")

            Else

                separateFunctionParamétre = Nothing

            End If

        Else

            If laligne.Contains("(") Then

                If laligne.Contains("""") Then

                    separateFunctionParamétre = Split(laligne.Replace("""", ""), "(")
                    separateFunctionParamétre = Split(separateFunctionParamétre(1), ")")
                    separateFunctionParamétre = Split(separateFunctionParamétre(0), " , ")

                Else

                    separateFunctionParamétre = Nothing

                End If

            Else

                separateFunctionParamétre = Nothing

            End If

        End If

        'Dim functionParamétre(If(separateFunctionParamétre Is Nothing, 0, separateFunctionParamétre.count)) As Object
        Dim functionParamétre(If(separateFunctionParamétre Is Nothing, 0, separateFunctionParamétre.Count)) As Object
        functionParamétre(0) = index

        If Not separateFunctionParamétre Is Nothing Then

            For i = 0 To separateFunctionParamétre.Count - 1

                If separateFunctionParamétre(i).StartsWith(" ") Then
                    separateFunctionParamétre(i) = Mid(separateFunctionParamétre(i), 2)
                End If
                If separateFunctionParamétre(i).EndsWith(" ") Then
                    separateFunctionParamétre(i) = Mid(separateFunctionParamétre(i), 1, separateFunctionParamétre(i).Length - 1)
                End If

                functionParamétre(i + 1) = separateFunctionParamétre(i)

            Next

        End If

        Return functionParamétre

    End Function


#Region "Action"

    Public Function Action(index As Integer, laLigne As String)

        With Comptes(index)

            Try

                'Ici je dois avoir seulement l'action.
                'Exemple : Map.Interaction("Statue de Classe" , "Monter a Incarnoob")

                Dim separateLigne As String() = Split(laLigne.Replace(vbTab, ""), "(")

                Dim separate As String() = Split(separateLigne(0), ".")

                Dim Parametre As Object = ReturnParametre(index, laLigne)

                Select Case separate(0).ToLower

                    Case "ami" ' FINI

                        Select Case separate(1).ToLower

                            Case "ouvre"

                                Return Ami_Ouvre(Parametre(0), Parametre(1))

                            Case "supprime"

                                Return Ami_Supprime(Parametre(0), Parametre(1), Parametre(2))

                            Case "ajoute"

                                Return Ami_Ajoute(Parametre(0), Parametre(1), Parametre(2))

                            Case "information"

                                Return Ami_Information(Parametre(0), Parametre(1), Parametre(2))

                            Case "rejoindre"

                                Return Ami_Rejoindre(Parametre(0), Parametre(1))

                            Case "avertie"

                                Return Ami_Avertie(Parametre(0), Parametre(1))

                            Case "exist", "existe"

                                Return Ami_Exist(Parametre(0), Parametre(1), Parametre(2))

                            Case Else

                                MsgBox("Information inconnu : " & laLigne)

                        End Select

                    Case "familier"

                        Dim newFamilier As New FunctionFamilier

                        Select Case separate(1).ToLower

                            Case "retire"

                                newFamilier.FamilierRetire(Parametre(0), Parametre(1), If(Parametre.Length > 2, Parametre(2), Nothing))

                            Case "nourrit"

                                Return newFamilier.Nourrit(index)

                            Case "reconnexion"

                                Task.Delay(newFamilier.reconnexion(index)).Wait()
                                Task.Delay(60000).Wait()
                            Case Else
                                Return True
                        End Select

                    Case "map"

                        Select Case separate(1).ToLower

                            Case "id"

                                Return Map_ID(Parametre(0), Parametre(1))

                            Case "coordonnees"

                                Return Map_Coordonnees(Parametre(0), Parametre(1))

                            Case "deplacement"

                                Return Map_Deplacement(Parametre(0), Parametre(1))

                            Case "attaquer"

                                Dim newMap As New FunctionMap
                                Return newMap.Attaquer(index)

                        End Select

                    Case "caracteristique", "caracteristiques"

                        Dim newCaracteristique As New FunctionCaractéristique

                        With newCaracteristique

                            Select Case separate(1).ToLower

                                Case "up"

                                    Return .Up(index, Parametre(1))

                                Case "return", "retourne"

                                    Return .Return(index, Parametre(1), Parametre(2))

                                Case "energie"

                                    Return .Energie(index, Parametre(1))

                                Case "niveau", "niveaux"

                                    Return .Niveau(index)

                                Case "experience"

                                    Return .Experience(index, Parametre(1))

                                Case "pointdevie", "pdv"

                                    Return .PointDeVie(index, Parametre(1))

                                Case Else

                                    MsgBox("Information inconnu : " & laLigne)

                            End Select

                        End With

                    Case "echange" ' FINI

                        Dim newEchange As New FunctionEchange

                        Select Case separate(1).ToLower

                            Case "invite"

                                Return newEchange.Invite(index, Parametre(1))

                            Case "refuse"

                                Return newEchange.Refuse(index)

                            Case "arrete"

                                Return newEchange.Arrete(index)

                            Case "accepte"

                                Return newEchange.Accepte(index)

                            Case "kamas"

                                Return newEchange.Kamas(index, Parametre(1))

                            Case "valide"

                                Return newEchange.Valide(index)

                            Case "verification"

                            'Return newEchange.Verification(index)

                            Case "enechange"

                                Return .Echange.EnEchange

                        End Select

                    Case "defi" ' FINI

                        Dim newDefi As New FunctionDefi

                        Select Case separate(1).ToLower

                            Case "invite"

                                Return newDefi.Invite(index, Parametre(1))

                            Case "accepte"

                                Return newDefi.Accepte(index)

                            Case "refuse"

                                Return newDefi.Refuse(index)

                            Case "abandonne"

                                Return newDefi.Abandonne(index)

                            Case "annule"

                                Return newDefi.Annule(index)

                        End Select

                    Case "item" ' FINI

                        Select Case separate(1).ToLower

                            Case "supprime", "suprime"

                                Return Item_Supprime(Parametre(0), Parametre(1), If(Parametre.Length > 2, Parametre(2), 999999), If(Parametre.Length > 3, Parametre(3), Nothing))

                            Case "retire"

                                Return Item_Retire(Parametre(0), Parametre(1), If(Parametre.Length > 2, Parametre(2), 999999), If(Parametre.Length > 3, Parametre(3), Nothing))

                            Case "depose"

                                Return Item_Depose(Parametre(0), Parametre(1), If(Parametre.Length > 2, Parametre(2), 999999), If(Parametre.Length > 3, Parametre(3), Nothing))

                            Case "existe"

                                Return Item_Existe(Parametre(0), Parametre(1), If(Parametre.Length > 2, Parametre(2), ""))

                            Case "equipe"

                                Return Item_Equipe(Parametre(0), Parametre(1), If(Parametre.Length > 3, Parametre(3), Nothing))

                            Case "desequipe"

                                Return Item_Desequipe(Parametre(0), Parametre(1), If(Parametre.Length > 3, Parametre(3), Nothing))

                            Case "jette", "jete", "jet"

                                Return Item_Jette(Parametre(0), Parametre(1), If(Parametre.Length > 2, Parametre(2), 999999), If(Parametre.Length > 3, Parametre(3), Nothing))

                            Case "utilise"

                                Return Item_Utilise(Parametre(0), Parametre(1))

                            Case Else

                                MsgBox("Action inconnu, vérifier bien d'avoir les mots correctement orthographié et que la function existe." & vbCrLf &
                                   laLigne)

                        End Select

                    Case "groupe" ' FINI

                        Dim newGroupe As New FunctionGroupe

                        Select Case separate(1).ToLower

                            Case "invite"

                                Return newGroupe.Invite(index, Parametre(1))

                            Case "refuse"

                                Return newGroupe.RefuseArrete(index)

                            Case "arrete"

                                Return newGroupe.RefuseArrete(index)

                            Case "accepte"

                                Return newGroupe.Accepte(index)

                            Case "quitte"

                                Return newGroupe.Quitte(index)

                            Case "suivezmoitous"

                                Return newGroupe.SuivezMoiTous(index)

                            Case "arreteztousdemesuivre"

                                Return newGroupe.ArretezTousDeMeSuivre(index)

                            Case "suivreledeplacement"

                                Return newGroupe.SuivreLeDeplacement(index, Parametre(1))

                            Case "neplussuivreledeplacement"

                                Return newGroupe.NePlusSuivreLeDeplacement(index, Parametre(1))

                            Case "suivezletous"

                                Return newGroupe.SuivezLeTous(index, Parametre(1))

                            Case "arreteztousdelesuivre"

                                Return newGroupe.ArretezTousDeLeSuivre(index, Parametre(1))

                            Case "exclure"

                                Return newGroupe.Exclure(index, Parametre(1))

                            Case Else

                                MsgBox("La ligne n'est pas connu : " & laLigne)

                        End Select

                    Case "guilde" ' FINI

                        Dim newGuilde As New FunctionGuilde

                        Select Case separate(1).ToLower

                            Case "ouvre"

                                newGuilde.Ouvre(index)

                                Select Case separate(2).ToLower

                                    Case "membre", "membres"

                                        Return newGuilde.Membres(index)

                                    Case "personnalisation"

                                        Return newGuilde.Personnalisation(index)

                                    Case "percepteur", "percepteurs"

                                        Return newGuilde.Percepteurs(index)

                                    Case "enclos", "enclo"

                                        Return newGuilde.Enclos(index)

                                    Case "maisons", "maison"

                                        Return newGuilde.Maisons(index)

                                    Case Else

                                        MsgBox("Ligne inconnu : " & laLigne)

                                End Select

                            Case "membre", "membres"

                                Select Case separate(2).ToLower

                                    Case "exclure"

                                        Return newGuilde.Exclure(index, Parametre(1))

                                    Case "invite"

                                        Return newGuilde.Invite(index, Parametre(1))

                                    Case "refuse"

                                        Return newGuilde.Refuse(index)

                                    Case "rang"

                                        Return newGuilde.Rang(index, Parametre(1), Parametre(2))

                                    Case "droits", "droit"

                                        Return newGuilde.Droits(index, Parametre(1), Parametre(2), Parametre(3))

                                    Case "experience"

                                        Return newGuilde.Experience(index, Parametre(1), Parametre(2))

                                    Case Else

                                        MsgBox("Ligne inconnu : " & laLigne)

                                End Select

                            Case "personnalisation"

                                Select Case separate(2).ToLower

                                    Case "up"

                                        Return newGuilde.Up(index, Parametre(1), Parametre(2))

                                    Case "poserunpercepteur"

                                        Return newGuilde.PoserPercepteur(index)

                                    Case "retirerunpercepteur"

                                        Return newGuilde.RetirerPercepteur(index)

                                    Case "releverunpercepteur"

                                        Return newGuilde.ReleverPercepteur(index)

                                    Case Else

                                        MsgBox("Ligne inconnu : " & laLigne)

                                End Select

                            Case "enclos", "enclo"

                                Select Case separate(2).ToLower

                                    Case "teleporter"

                                        Return newGuilde.EnclosTeleporter(index, Parametre(1))

                                    Case Else

                                        MsgBox("Ligne inconnu : " & laLigne)

                                End Select

                            Case "maisons", "maison"

                                Select Case separate(2).ToLower

                                    Case "teleporter"

                                        Return newGuilde.MaisonsTeleporter(index, Parametre(1))

                                    Case Else

                                        MsgBox("Ligne inconnu : " & laLigne)

                                End Select

                            Case Else

                                MsgBox("Ligne inconnu : " & laLigne)

                        End Select

                    Case "metier" ' FINI

                        Dim newMetier As New FunctionMetier

                        Select Case separate(1).ToLower

                            Case "existe", "exist"

                                Return newMetier.Existe(index, Parametre(1))

                            Case "public"

                                Return newMetier.Public(index, Parametre(1))

                            Case "option"

                                Return newMetier.Option(index, Parametre(1), Parametre(2), Parametre(3), Parametre(4), Parametre(5))

                            Case "niveau"

                                Return newMetier.Niveau(index, Parametre(1))

                        End Select

                    Case "chargement"

                        Select Case separate(1).ToLower

                            Case "trajet"

                                Task.Run(Sub() TrajetLoad(index, Parametre(1))).Wait()

                        End Select

                    Case "connexion"

                        Return Connexion(Parametre(0), Parametre(1), Parametre(2), Parametre(3), Parametre(4))

                    Case "zaap", "zaapi"

                        Dim newZaap As New FunctionZaap

                        Select Case separate(1).ToLower

                            Case "utiliser"

                                Return newZaap.Utiliser(index)

                            Case "sauvegarder"

                                Return newZaap.Sauvegarder(index)

                            Case "destination"

                                Return newZaap.Destination(index, Parametre(1))

                            Case "quitte"

                                Return newZaap.Quitte(index)

                            Case Else

                                MsgBox("Ligne inconnu : " & laLigne)

                        End Select

                    Case "pnj"

                        Dim newPnj As New FunctionPnj

                        Select Case separate(1).ToLower

                            Case "parler"

                                Return Pnj_Parler(Parametre(0), Parametre(1))

                            Case "quitte"

                                Select Case separate(2).ToLower

                                    Case "dialogue"

                                        '    Return Groupe_Pnj_Quitte_Dialogue(index)

                                End Select

                            Case "reponse"

                                Return Pnj_Reponse(Parametre(0), Parametre(1))

                            Case "achetervendre"

                                Select Case separate(2).ToLower

                                    Case "parler"

                                        Return newPnj.AcheterVendre(index, Parametre(1))

                                    Case "vendre"

                                        Return newPnj.AcheterVendreVendItem(index, Parametre(1), Parametre(2), Parametre(3))

                                End Select

                            Case "acheter"

                                If separate.Count > 2 Then

                                    '  While AcheterItem(index, Parametre(1), Parametre(2), Parametre(3))

                                    Task.Delay(500).Wait()

                                    '  End While

                                Else

                                    'Return Acheter(index, Parametre(1))

                                End If

                            Case "recherche"

                                '   Return Recherche(index, Parametre(1))

                        End Select

                    Case "maison", "maisons"

                        Dim newMaison As New FunctionMaison

                        Select Case separate(1).ToLower

                            Case "acheter"

                                Return newMaison.VerificationAllMaison(index)

                        End Select

                    Case "pause"

                        Dim rand As New Random
                        Task.Delay(rand.Next(Parametre(1), Parametre(2))).Wait()

                        Return True

                    Case "personnage"

                        Select Case separate(1).ToLower

                            Case "niveau"

                                Return .Personnage.Niveau

                        End Select

                    Case "recolte"

                        Dim newfunc As New FunctionRecolte

                        While newfunc.Recolte(index)

                            Task.Delay(500).Wait()

                        End While

                        Return True

                    Case "recolte1"

                        Dim newRecolte As New FunctionRecolte


                        Return True

                    Case "mobs"

                        Select Case separate(1).ToLower

                            Case "proche"

                                ' IAMobs(inde)

                        End Select

                    Case "msgbox"

                        MsgBox(Parametre(1))

                End Select

            Catch ex As Exception

                EcritureMessage(index, "(ERREUR - Script)", "La méthode demander n'existe pas" & vbCrLf & laLigne, Color.Red)

            End Try

        End With

        Return False

    End Function

#End Region

#Region "Spe"

    Public Function WhileReturn(index As Integer, laLigne As String) As Boolean

        With Comptes(index)

            Try

                ' While Map.ID("5465") = True

                Dim ligneConstruction As String = Mid(laLigne, 7)

                Dim Operateur As String = ""
                Dim monAction As String = ""
                Dim aComparer As String = ""

                If ligneConstruction.Contains(") = ") Then

                    Operateur = "="

                ElseIf ligneConstruction.Contains(") <= ") Then

                    Operateur = "<="

                ElseIf ligneConstruction.Contains(") >= ") Then

                    Operateur = ">="

                ElseIf ligneConstruction.Contains(") <> ") Then

                    Operateur = "<>"

                End If

                monAction = Split(ligneConstruction, " " & Operateur & " ")(0)
                aComparer = Split(ligneConstruction, " " & Operateur & " ")(1)

                Dim resultat As String = Action(index, monAction)

                If IsNumeric(aComparer) Then

                    Select Case Operateur

                        Case "<="

                            If CInt(resultat) <= CInt(aComparer) Then

                                Return True

                            Else

                                Return False

                            End If

                        Case ">="

                            If CInt(resultat) >= CInt(aComparer) Then

                                Return True

                            Else

                                Return False

                            End If

                        Case "="

                            If CInt(resultat) = CInt(aComparer) Then

                                Return True

                            Else

                                Return False

                            End If

                    End Select

                ElseIf aComparer.ToLower = "true" OrElse aComparer.ToLower = "false" Then

                    If CBool(resultat) = CBool(aComparer) Then

                        Return True

                    Else

                        Return False

                    End If

                Else

                    Select Case Operateur

                        Case "<>"

                            If resultat.ToLower <> aComparer.ToLower Then

                                Return True

                            Else

                                Return False

                            End If

                        Case "="

                            If resultat.ToLower = aComparer.ToLower Then

                                Return True

                            Else

                                Return False

                            End If

                    End Select

                End If


            Catch ex As Exception

            End Try

            Return False

        End With

    End Function

    Public Function IfReturn(ByVal index As Integer, ByVal laLigne As String) As Boolean

        With Comptes(index)

            Try

                Dim ligneConstruction As String = Mid(laLigne, 4)
                ligneConstruction = Mid(ligneConstruction, 1, ligneConstruction.Length - 5)

                Dim Operateur As String = ""
                Dim monAction As String = ""
                Dim aComparer As String = ""

                If ligneConstruction.Contains(") = ") Then

                    Operateur = "="

                ElseIf ligneConstruction.Contains(") <= ") Then

                    Operateur = "<="

                ElseIf ligneConstruction.Contains(") >= ") Then

                    Operateur = ">="

                ElseIf ligneConstruction.Contains(") <> ") Then

                    Operateur = "<>"

                End If

                monAction = Split(ligneConstruction, " " & Operateur & " ")(0)
                aComparer = Split(ligneConstruction, " " & Operateur & " ")(1)

                Dim resultat As String = Action(index, monAction)

                If IsNumeric(aComparer) Then

                    Select Case Operateur

                        Case "<="

                            If CInt(resultat) <= CInt(aComparer) Then

                                Return True

                            Else

                                Return False

                            End If

                        Case ">="

                            If CInt(resultat) >= CInt(aComparer) Then

                                Return True

                            Else

                                Return False

                            End If

                        Case "="

                            If CInt(resultat) = CInt(aComparer) Then

                                Return True

                            Else

                                Return False

                            End If

                    End Select

                ElseIf aComparer.ToLower = "true" OrElse aComparer.ToLower = "false" Then

                    If CBool(resultat) = CBool(aComparer) Then

                        Return True

                    Else

                        Return False

                    End If

                Else

                    Select Case Operateur

                        Case "<>"

                            If resultat.ToLower <> aComparer.ToLower Then

                                Return True

                            Else

                                Return False

                            End If

                        Case "="

                            If resultat.ToLower = aComparer.ToLower Then

                                Return True

                            Else

                                Return False

                            End If

                    End Select

                End If

            Catch ex As Exception

            End Try

            Return False

        End With

    End Function

#End Region

#Region "Select Case"

    Public Function SelectReturn(ByVal index As Integer, ByVal laLigne As String) As String

        With Comptes(index)

            Dim separateLigne As String() = Split(laLigne, " ")

            Dim nomFunction As String = ReturnNomFunction(laLigne)
            Dim ParametreFunction As String() = ReturnParametreFunction(index, laLigne)

            ' Return LuaScript.GetFunction(nomFunction.ToLower).Call(ParametreFunction).First

        End With

    End Function

    Public Function SelectCaseReturn(ByVal index As Integer, ByVal laLigne As String, ByVal resultat As String) As Boolean

        With Comptes(index)

            Dim separate As String() = Split(laLigne, " ")

            Select Case separate(1)

                Case "Integer"

                    Select Case separate(2)

                        Case "<"

                            If CInt(resultat) < CInt(separate(3)) Then

                                Return True

                            Else

                                Return False

                            End If

                        Case ">"

                            If CInt(resultat) > CInt(separate(3)) Then

                                Return True

                            Else

                                Return False

                            End If

                        Case "="

                            If CInt(resultat) = CInt(separate(3)) Then

                                Return True

                            Else

                                Return False

                            End If

                    End Select

                Case "Boolean"

                Case "String"

            End Select

        End With

    End Function

#End Region

#Region "Function"

    Private Function ReturnNomFunction2(ByVal laligne As String) As String

        Dim separate As String() = Split(laligne, " ")

        Return separate(1)

        If laligne.Contains(" = ") Then

            separate = Split(laligne, " = ")
            separate = Split(separate(1), "(")

        Else

            If laligne.Contains("Select") Then

                separate = Split(laligne, " ")
                separate = Split(separate(2), "(")

            Else

                separate = Split(laligne, "(")

            End If

        End If

        Return separate(0)

    End Function

    Private Function ReturnNomFunction(ByVal laligne As String) As String

        Dim separate As String() = Split(laligne, " ")

        Return separate(1)
        If laligne.Contains(" = ") Then

            separate = Split(laligne, " = ")
            separate = Split(separate(1), "(")

        Else

            If laligne.Contains("Select") Then

                separate = Split(laligne, " ")
                separate = Split(separate(2), "(")

            Else

                separate = Split(laligne, "(")

            End If

        End If

        Return separate(0)

    End Function

    Private Function ReturnParametreFunction(ByVal index As Integer, ByVal laligne As String)

        Dim separateFunctionParamétre As String()

        If laligne.Contains(" = ") Then

            separateFunctionParamétre = Split(laligne, " = ")

            If separateFunctionParamétre(1).Contains("(") Then

                separateFunctionParamétre = Split(separateFunctionParamétre(1).Replace("""", ""), "(")
                separateFunctionParamétre = Split(separateFunctionParamétre(1), ")")
                separateFunctionParamétre = Split(separateFunctionParamétre(0), If(separateFunctionParamétre(0).Contains(", "), ", ", " , "))

            Else

                separateFunctionParamétre = Nothing

            End If

        Else

            If laligne.Contains("(") Then

                If laligne.Contains("""") Then

                    separateFunctionParamétre = Split(laligne.Replace("""", ""), "(")
                    separateFunctionParamétre = Split(separateFunctionParamétre(1), ")")
                    separateFunctionParamétre = Split(separateFunctionParamétre(0), If(separateFunctionParamétre(0).Contains(", "), ", ", " , "))

                Else

                    separateFunctionParamétre = Nothing

                End If

            Else

                separateFunctionParamétre = Nothing

            End If

        End If

        Dim functionParamétre(If(separateFunctionParamétre Is Nothing, 0, separateFunctionParamétre.Count)) As String
        functionParamétre(0) = index

        If Not separateFunctionParamétre Is Nothing Then

            For i = 0 To separateFunctionParamétre.Count - 1

                functionParamétre(i + 1) = separateFunctionParamétre(i)

            Next

        End If

        Return functionParamétre

    End Function

#End Region

End Module
