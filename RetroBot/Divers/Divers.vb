Imports System.Net.Sockets, System.Net, Org.Mentalis.Network.ProxySocket

Public Module Divers

    Private Delegate Sub dlgDivers()
    Private Delegate Function dlgFDivers()





    Public Function ChiffreSeparation(chiffre As String, separateur As String) As String

        Dim resultat As String = ""
        chiffre = StrReverse(chiffre)

        Try

            For i = 1 To chiffre.Count Step 3

                resultat &= Mid(chiffre, i, 3) & separateur

            Next

        Catch ex As Exception

        End Try

        Return Mid(StrReverse(resultat), 2)

    End Function

    Public Sub Initialiser(index As Integer, frmGroupe As FrmGroupe)

        With Comptes(index)

            frmGroupe.TabControl1.Controls.Add(.TabPage_Bot)

            .TabPage_Bot.Controls.Add(.FrmUser)

            .TabPage_Bot.Text = .Personnage.NomDuPersonnage

            .TabPage_Bot.BackColor = Color.FromArgb(43, 44, 48)

            .FrmUser.Index = index

            ChargeOptionBot(index, Application.StartupPath + "\Compte\Options\" & .Personnage.NomDeCompte & "_" & .Personnage.NomDuPersonnage & ".txt")

        End With

    End Sub

    Public Sub ChargeOptionBot(index As Integer, chemin As String)

        With Comptes(index).FrmUser

            Try

                If .InvokeRequired Then

                    .Invoke(New dlgDivers(Sub() ChargeOptionBot(index, chemin)))

                Else

                    'J'ouvre et je lis le fichier.
                    Dim swLecture As New IO.StreamReader(chemin)

                    Try

                        Do Until swLecture.EndOfStream

                            Dim Ligne As String = swLecture.ReadLine

                            If Ligne <> "" Then

                                Dim separateLigne As String() = Split(Ligne, "|")

                                For i = 1 To separateLigne.Count - 1

                                    Dim separate As String() = Split(separateLigne(i), " = ")

                                    Select Case separateLigne(0).ToLower

                                        Case "caracteristique"

                                            Select Case separate(0).ToLower

                                                Case "up automatique"

                                                    .CheckBox_Caracteristique_UpAutomatique.Checked = CBool(separate(1))

                                                Case "caracteristique a up"

                                                    For Each ctl As Control In .GroupBox2.Controls

                                                        If TypeOf (ctl) Is RadioButton Then

                                                            Dim rdb As RadioButton = TryCast(ctl, RadioButton)

                                                            If Split(rdb.Name, "_")(2).ToLower = separate(1).ToLower Then

                                                                rdb.Checked = True

                                                                Exit For

                                                            End If

                                                        End If

                                                    Next

                                            End Select

                                        Case "sort"

                                            Select Case separate(0).ToLower

                                                Case "up automatique"

                                                    .CheckBox_Sort_UpSort.Checked = CBool(separate(1))

                                                Case "ia automatique"

                                                    .CheckBox_Sort_IAAutomatique.Checked = CBool(separate(1))

                                                Case "id sort"

                                                    InterfaceGiSortAjoute(index, separate(1) & "~1~_;")

                                                    For Each pair As DataGridViewRow In .DataGridView_Sort.Rows

                                                        If pair.Cells(1).Value = separate(1) Then

                                                            pair.Cells(0).Value = True

                                                            Exit For

                                                        End If

                                                    Next

                                            End Select

                                        Case "proxy"

                                            With Comptes(index).Proxy

                                                Select Case separate(0).ToLower

                                                    Case "active"

                                                        .Active = CBool(separate(1))

                                                        Comptes(index).FrmUser.CheckBox_Proxy.Checked = CBool(separate(1))

                                                    Case "ip"

                                                        .IP = separate(1)

                                                        Comptes(index).FrmUser.TextBox_Proxy_IP.Text = separate(1)

                                                    Case "port"

                                                        .Port = separate(1)

                                                        Comptes(index).FrmUser.TextBox_Proxy_Port.Text = separate(1)

                                                    Case "identifiant"

                                                        .Identifiant = separate(1)

                                                        Comptes(index).FrmUser.TextBox_Proxy_Identifiant.Text = separate(1)

                                                    Case "mot de passe"

                                                        .MotDePasse = separate(1)

                                                        Comptes(index).FrmUser.TextBox_Proxy_MotDePasse.Text = separate(1)

                                                End Select

                                            End With

                                        Case "planning"

                                            With Comptes(index)

                                                Dim separatePlanning As String() = Split(separate(1), ":")

                                                Select Case separate(0).ToLower

                                                    Case "active planning"

                                                        .Option.PlanningActive = CBool(separate(1))

                                                        .FrmUser.CheckBox_Planning.Checked = CBool(separate(1))

                                                    Case "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"

                                                        For Each pair As KeyValuePair(Of String, Boolean()) In .Option.Planning

                                                            If pair.Key.ToLower = separate(0).ToLower Then

                                                                For e = 0 To pair.Value.Count - 1

                                                                    pair.Value(e) = CBool(separatePlanning(e + 1))

                                                                Next

                                                            End If

                                                        Next

                                                End Select

                                                If Date.Now.ToString("dddd") = separate(0).ToLower Then

                                                    .FrmUser.ComboBox_Planning.Text = separate(0)

                                                End If

                                            End With

                                        Case "ia"

                                            .Label_IA.Name = separate(0)
                                            Dim nom As String() = Split(separate(0), "\")
                                            .Label_IA.Text = nom(nom.Count - 1)
                                            .Button_IA.BackgroundImage = My.Resources.Ampoule_On
                                            .Button_IA.FlatAppearance.BorderColor = Color.Lime
                                            Comptes(index).ThreadIA = New Threading.Thread(Sub() IALoad(index, separate(0))) With {.IsBackground = True}
                                            Comptes(index).ThreadIA.Start()

                                    End Select

                                Next

                            End If

                        Loop

                    Catch ex As Exception

                    End Try

                    'Puis je ferme le fichier.
                    swLecture.Close()

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

    Public Sub ErreurFichier(ByVal index As Integer, ByVal nomJoueur As String, ByVal nomErreur As String, ByVal erreur As String)

        Try

            Comptes(index).monerreur = erreur

            EcritureMessage(index, "[Erreur]", "Une erreur est survenue, veuillez envoyez les fichiers qui se trouve dans le dossier 'Erreur' à l'administrateur.", Color.Red)

            'Si le dossier erreur n'existe pas, alors je le créer
            If Not IO.Directory.Exists(Application.StartupPath & "\AllErreur") Then IO.Directory.CreateDirectory(Application.StartupPath & "\AllErreur")

            'J'ouvre le fichier pour y écrire se que je souhaite
            Dim swEcriture As New IO.StreamWriter(Application.StartupPath + "\AllErreur/" & nomJoueur & "_" & nomErreur & ".txt")

            swEcriture.WriteLine(erreur)

            'Puis je le ferme.
            swEcriture.Close()

        Catch ex As Exception

        End Try

    End Sub

    Public Sub EcritureMessage(index As Integer, indice As String, message As String, couleur As Color)

        With Comptes(index).FrmUser

            Try

                If .InvokeRequired Then

                    .Invoke(New dlgDivers(Sub() EcritureMessage(index, indice, message, couleur)))

                Else

                    .RichTextBoxTchat.SelectionColor = couleur
                    .RichTextBoxTchat.AppendText("[" & TimeOfDay & "] " & indice & " " & message & vbCrLf)
                    .RichTextBoxTchat.ScrollToCaret()

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

    Public Sub EcritureMessageSocket(index As Integer, indice As String, message As String, couleur As Color)

        With Comptes(index).FrmUser


            Try

                If .InvokeRequired Then

                    .Invoke(New dlgDivers(Sub() EcritureMessageSocket(index, indice, message, couleur)))

                Else

                    .RichTextBoxSocket.SelectionColor = couleur
                    .RichTextBoxSocket.AppendText("[" & TimeOfDay & "] " & indice & " " & message & vbCrLf)
                    .RichTextBoxSocket.ScrollToCaret()

                End If

            Catch ex As Exception

            End Try

        End With

    End Sub

    Public Function AsciiDecoder(msg As String) As String
        Dim msgFinal As String = ""
        Try

            Dim iMax As Integer = msg.Length
            Dim i As Integer = 0
            While (i < iMax)
                Dim caractC As Char = msg(i)
                Dim caractI As Integer = Asc(caractC)
                Dim nbLettreCode As Integer = 1
                If (caractI And 128) = 0 Then
                    msgFinal &= ChrW(caractI)
                Else
                    Dim temp As Integer = 64
                    Dim codecPremLettre As Integer = 63
                    While (caractI And temp)
                        temp >>= 1
                        codecPremLettre = codecPremLettre Xor temp
                        nbLettreCode += 1
                    End While
                    codecPremLettre = codecPremLettre And 255
                    Dim caractIFinal As Integer = caractI And codecPremLettre
                    nbLettreCode -= 1
                    i += 1
                    While (nbLettreCode <> 0)
                        caractC = msg(i)
                        caractI = Asc(caractC)
                        caractIFinal <<= 6
                        caractIFinal = caractIFinal Or (caractI And 63)
                        nbLettreCode -= 1
                        i += 1
                    End While
                    msgFinal &= ChrW(caractIFinal)
                End If
                i += nbLettreCode
            End While
        Catch ex As Exception

        End Try


        Return msgFinal.Replace("%27", "'").Replace("%C3%A9", "é").Replace("%2C", ",").Replace("%3F", "?").Replace("%C3%A8", "é").Replace("%29", "]").Replace("%28", "[")
    End Function 'Provient de Maxoubot.

    Public Function ProxySocketUtilisateur(ipAnkama As String, portAnkama As Integer, proxyIP As String, proxyPort As Integer, nomUtilisateur As String, motDePasse As String) As Socket

        Dim monProxy As ProxySocket = New ProxySocket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)

        monProxy.ProxyEndPoint = New IPEndPoint(IPAddress.Parse(proxyIP), proxyPort)

        If nomUtilisateur <> "" Then

            monProxy.ProxyUser = nomUtilisateur
            monProxy.ProxyPass = motDePasse

        End If

        monProxy.ProxyType = ProxyTypes.Socks5

        monProxy.Connect(ipAnkama, portAnkama)

        Return monProxy

    End Function

    Public Function CopyDatagridView(ByVal Index As Integer, ByVal row As DataGridView) As DataGridView

        With Comptes(Index)

            Try

                If .FrmUser.InvokeRequired Then

                    Return .FrmUser.Invoke(New dlgFDivers(Function() CopyDatagridView(Index, row)))

                Else

                    Dim Data As New DataGridView With {.AllowUserToAddRows = False}

                    For Each Col As DataGridViewColumn In row.Columns
                        Data.Columns.Add(DirectCast(Col.Clone, DataGridViewColumn))
                    Next

                    For rowIndex As Integer = 0 To (row.Rows.Count - 1)

                        Data.Rows.Add(row.Rows(rowIndex).Cells.Cast(Of DataGridViewCell).Select(Function(c) c.Value).ToArray)

                        For cellIndex As Integer = 0 To row.Rows(rowIndex).Cells.Count - 1
                            Data.Rows(rowIndex).Cells(cellIndex).ToolTipText = row.Rows(rowIndex).Cells(cellIndex).ToolTipText

                        Next

                        Data.Rows(rowIndex).DefaultCellStyle.BackColor = row.Rows(rowIndex).DefaultCellStyle.BackColor

                    Next

                    Return Data

                End If

            Catch ex As Exception

            End Try

        End With

        Return Nothing

    End Function

    Public Function CopyItem(ByVal index As Integer, ByVal dico As Dictionary(Of Integer, CItem)) As Dictionary(Of Integer, CItem)

        With Comptes(index)

            If .FrmUser.InvokeRequired Then

                Return .FrmUser.Invoke(New dlgFDivers(Function() CopyItem(index, dico)))

            Else

                Dim newDico As New Dictionary(Of Integer, CItem)

                For Each pair As KeyValuePair(Of Integer, CItem) In dico

                    newDico.Add(pair.Key, pair.Value)

                Next

                Return newDico

            End If

        End With

    End Function
    Public Function CopyMap(ByVal index As Integer, ByVal dico As Dictionary(Of Integer, CEntite)) As Dictionary(Of Integer, CEntite)

        With Comptes(index)

            If .FrmUser.InvokeRequired Then

                Return .FrmUser.Invoke(New dlgFDivers(Function() CopyMap(index, dico)))

            Else

                Dim newDico As New Dictionary(Of Integer, CEntite)

                For Each pair As KeyValuePair(Of Integer, CEntite) In dico

                    newDico.Add(pair.Key, pair.Value)

                Next

                Return newDico

            End If

        End With

    End Function

    Public Function CopyAmi(ByVal index As Integer, ByVal dico As Dictionary(Of String, CAmiInformation)) As Dictionary(Of String, CAmiInformation)

        With Comptes(index)

            If .FrmUser.InvokeRequired Then

                Return .FrmUser.Invoke(New dlgFDivers(Function() CopyAmi(index, dico)))

            Else

                Dim newDico As New Dictionary(Of String, CAmiInformation)

                For Each pair As KeyValuePair(Of String, CAmiInformation) In dico

                    newDico.Add(pair.Key, pair.Value)

                Next

                Return newDico

            End If

        End With

    End Function

    Public Function CopyInteraction(ByVal index As Integer, ByVal dico As Dictionary(Of Integer, CInteraction)) As Dictionary(Of Integer, CInteraction)

        With Comptes(index)

            If .FrmUser.InvokeRequired Then

                Return .FrmUser.Invoke(New dlgFDivers(Function() CopyInteraction(index, dico)))

            Else

                Dim newDico As New Dictionary(Of Integer, CInteraction)

                For Each pair As KeyValuePair(Of Integer, CInteraction) In dico

                    newDico.Add(pair.Key, pair.Value)

                Next

                Return newDico

            End If

        End With

    End Function
    ''' <summary>
    ''' Retourne l'ID ou la categorie de l'item.
    ''' </summary>
    ''' <param name="index">Indique le numéro du bot.</param>
    ''' <param name="nomID">Le nom de l'item ou son ID.</param>
    ''' <param name="choix">l'un des choix suivant : <br/>
    ''' ID = Retourne l'ID de l'item.
    ''' Categorie = Retourne la categorie de l'item.</param>
    ''' <returns>
    ''' Retourne l'ID ou la categorie selon le nom ou l'ID de l'item.
    ''' </returns>
    Public Function RetourneItemNomIdCategorie(ByVal index As Integer, ByVal nomID As String, ByVal choix As String) As String

        With Comptes(index)

            For Each pair As sItems In VarItems.Values

                If pair.Nom.ToLower = nomID.ToLower OrElse pair.ID = nomID Then

                    Select Case choix.ToLower

                        Case "id"

                            Return pair.ID

                        Case "categorie", "categori"

                            Return pair.Catégorie

                    End Select

                End If

            Next

        End With

        Return ""

    End Function

End Module
