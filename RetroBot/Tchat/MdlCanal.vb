Module MdlCanal

    Sub GiCanalDofus(index As Integer, data As String)

        With Comptes(index)

            Try

                If .FrmUser.InvokeRequired Then

                    .FrmUser.Invoke(New DlgPlayer(AddressOf GiCanalDofus), index, data)

                Else

                    'cC +                *#%!$:?pi^
                    'cC Active/Désactive Les canaux

                    'Le "+" ou le "-" indique si les canaux suivant l'opérateur sont activé ou non.
                    Dim checked As Boolean = If(Mid(data, 3, 1) = "+", True, False)

                    With .Tchat.Canal

                        'Puis je tcheck lettre par lettre les infos après le "+" ou le "-".
                        For i = 3 To data.Length - 1

                            Select Case data(i)

                                Case "i" 'Information

                                    .Information = checked

                                Case "*" 'Communs/Défaut

                                    .General = checked

                                Case "#", "$", "p" 'groupe/privée/équipe

                                    .GroupePriveeEquipe = checked

                                Case "%" 'guilde

                                    .Guilde = checked

                                Case "!" 'alignement

                                    .Alignement = checked

                                Case "?" 'recrutement

                                    .Recrutement = checked

                                Case ":" 'Commerce

                                    .Commerce = checked

                            End Select

                        Next

                    End With

                End If

                InterfaceGiCanalDofus(index, data)

            Catch ex As Exception

                ErreurFichier(index, Comptes(index).Personnage.NomDuPersonnage, "GiCanalDofus", ex.Message)

            End Try

        End With

    End Sub

    Private Sub InterfaceGiCanalDofus(index As Integer, data As String)

        With Comptes(index)

            Try

                If .FrmUser.InvokeRequired Then

                    .FrmUser.Invoke(New DlgPlayer(AddressOf InterfaceGiCanalDofus), index, data)

                Else

                    'cC +                *#%!$:?pi^
                    'cC Active/Désactive Les canaux

                    'Le "+" ou le "-" indique si les canaux suivant l'opérateur sont activé ou non.
                    Dim checked As Boolean = If(Mid(data, 3, 1) = "+", True, False)

                    With .FrmUser

                        'Puis je tcheck lettre par lettre les infos après le "+" ou le "-".
                        For i = 3 To data.Length - 1

                            Select Case data(i)

                                Case "i" 'Information

                                    .CheckBox_Tchat_Information.Checked = checked

                                Case "*" 'Communs/Défaut

                                    .CheckBox_Tchat_Communs.Checked = checked

                                Case "#", "$", "p" 'groupe/privée/équipe

                                    .CheckBox_Tchat_Groupe.Checked = checked

                                Case "%" 'guilde

                                    .CheckBox_Tchat_Guilde.Checked = checked

                                Case "!" 'alignement

                                    .CheckBox_Tchat_Alignement.Checked = checked

                                Case "?" 'recrutement

                                    .CheckBox_Tchat_Recrutement.Checked = checked

                                Case ":" 'Commerce

                                    .CheckBox_Tchat_Commerce.Checked = checked

                            End Select

                        Next

                    End With

                End If

                .Tchat.BloqueTchat.Set()

            Catch ex As Exception

                ErreurFichier(index, Comptes(index).Personnage.NomDuPersonnage, "GiCanalDofus", ex.Message)

            End Try

        End With

    End Sub


    Public Sub GiDofusInformation(index As Integer, data As String)

        With Comptes(index)

            'Im 1165
            'Im Numéro du texte a affiché

            Try

                'Je prend toutes les informations après le "Im"
                data = Mid(data, 3)

                'Je sélectionne selon si y'a une information à afficher ou non via le signe ";"
                If data.Contains(";") Then

                    Dim Separation() As String = Split(data, ";")

                    Select Case Separation(0)

                        Case "1202" 'Im1201;[Seydlex] 

                            EcritureMessage(index, "[Modérateur]", "Attention un modérateur vous surveille : " & Separation(1) & ".", Color.Red)

                            'Modérateur_Ban(Index)

                        Case "1184" 'Im1184;Linaculer

                            EcritureMessage(index, "[Combat]", Separation(1) & " vient de se reconnecter en combat.", Color.Red)

                            .Combat.EnCombat = True

                        Case "1171" 'Im1171;1~9~19
                            Separation = Split(Separation(1), "~")
                            EcritureMessage(index, "[Combat]", "Impossible de lancer ce sort : Vous avez une portée de " & Separation(0) & " à " & Separation(1) & " et vous visez à " & Separation(2) & " !", Color.Red)

                            .Combat.Bloque.Set()

                        Case "1170" 'Im1170;0~4

                            'Je sépare les informations via ce signe "~"
                            Separation = Split(Separation(1), "~")
                            EcritureMessage(index, "[Combat]", "Vous avez '" & Separation(0) & "' PA, hors il vous en faut minimum '" & Separation(1) & "' PA pour lancer ce sort.", Color.Red)
                            .Combat.Bloque.Set()
                        Case "1168" 'Im1168;1

                            EcritureMessage(index, "[Dofus]", "Vous ne pouvez pas poser plus de " & Separation(1) & " percepteur(s) par zone.", Color.Red)

                        Case "1167" 'Im1167;54

                            EcritureMessage(index, "[Dofus]", "Vous ne pouvez pas poser de percepteur ici avant " & Separation(1) & " minutes.", Color.Red)

                        Case "1139" 'Im1139;5

                            EcritureMessage(index, "[Percepteur]", "Attention, la fenêtre d'échange se fermera automatiquement dans " & Separation(1) & " minutes.", Color.Red)
                            .BloqueGuilde.Set()

                        Case "1111" 'Im1111;3

                            EcritureMessage(index, "[Dragodinde]", "A peine entrée dans l'étable, votre monture s'accroupit et commence à mettre bas. Après quelques instants, vous pouvez constater que tout s'est bien passé. Vous voilà responsable de " & Separation(1) & " nouvelle(s) monture(s).", Color.Violet)

                        Case "0188" '"Im0188;player"

                            EcritureMessage(index, "[Dofus]", "Et comme d'habitude, c'est à " & Separation(1) & " que l'on doit cet exploit...", Color.Maroon)

                        Case "0157" ' Im0157;6

                            EcritureMessage(index, "[Dofus]", "Ce canal n'est accessible en diffusion aux abonnés qu'à partir du niveau " & Separation(1), Color.Green)

                        Case "0153" 'Im0153;xx.xxx.xx.xx

                            EcritureMessage(index, "[Dofus]", "Votre adresse IP actuelle est : " & Separation(1) & ".", Color.Green)

                        Case "0152"

                            'Im0152; 2019  ~ 06   ~ 27   ~ 7     ~ 19     ~ xx.xxx.xx.xx
                            'Im0152; Année ~ Mois ~ Jour ~ Heure ~ Minute ~ IP

                            'Je sépare les informations qui se trouve dans la separation(1) par ce signe "~"
                            Separation = Split(Separation(1), "~")

                            EcritureMessage(index, "[Dofus]", "Précédente connexion sur votre compte effectuée le : " &
                                                Separation(2) & "/" & Separation(1) & "/" & Separation(0) & " à " & Separation(3) & ":" & Separation(4) &
                                                " via l'adresse IP  : " & Separation(5), Color.Green)

                        Case "0143" ' Im0143;Linaculer (<b><a href="asfunction:onHref,ShowPlayerPopupMenu,Linacular">Linaculeur</a></b>) 

                            Separation = Split(Separation(1), " (<b><a href=""""asfunction: onHref, ShowPlayerPopupMenu, Linacular"""">")
                            Dim V_Nom_De_Compte As String = Separation(0)
                            Separation = Split(Separation(1), "</a></b>)")
                            EcritureMessage(index, "[Dofus]", "Le joueur : " & V_Nom_De_Compte & "(" & Separation(0) & ") vient de se connecter.", Color.Green)

                        Case "0115"

                            EcritureMessage(index, "[Dofus]", "Ce canal est restreint pour améliorer sa lisibilité. Vous pourrez envoyer un nouveau message dans " & Separation(1) & " secondes. Ceci ne vous autorise cependant pas pour autant à surcharger ce canal.", Color.Red)

                        Case "128" 'Im128;Linaculer

                            EcritureMessage(index, "[Combat]", "En attente du joueur " & Separation(1) & "...", Color.Red)

                        Case "120"

                            EcritureMessage(index, "[Dofus]", "Cet emplacement de stockage est déjà utilisé.", Color.Red)

                        Case "116" 'Im116;[Seydlex]~Bot tchatJoueur

                            'Je sépare les informations via ce signe "~"
                            Separation = Split(Separation(1), "~")

                            EcritureMessage(index, "[Modérateur]", "Vous avez été banni par " & Separation(0) & ". Motif : " & Separation(2), Color.Red)
                            'Modérateur_Ban(Index)

                        Case "115"

                            ' Im115;16 heures, 43 minutes, 43 secondes
                            EcritureMessage(index, "[Dofus]", "Pour des raisons de maintenances, le serveur va être redémarré dans " & Separation(1), Color.Red)


                        Case "092" 'Im092;50

                            EcritureMessage(index, "[Dofus]", "Vous avez récupéré " & Separation(1) & " points d'énergie en vous reposant.", Color.Green)

                        Case "065"

                            'Im065; 300         ~ 2598     ~ 2598     ~ 1
                            'Im065; Kamas gagné ~ ID Objet ~ ID Objet ~ Quantité

                            'Je sépare les informations via ce signe "~"
                            Separation = Split(Separation(1), "~")

                            EcritureMessage(index, "[Dofus]", "Votre compte en banque a été crédité de " & Separation(0) & " kamas suite à la vente de '" & VarItems(Separation(1)).Nom & "' (x " & Separation(3) & ").", Color.Green)

                        Case "056"

                            EcritureMessage(index, "[Dofus]", "Quête terminée : " & VarQuête(Separation(1)), Color.Green)

                        Case "055"

                            EcritureMessage(index, "[Dofus]", "Quête mise à jour : " & VarQuête(Separation(1)), Color.Green)

                        Case "054"

                            EcritureMessage(index, "[Dofus]", "Nouvelle quête : " & VarQuête(Separation(1)), Color.Green)

                           ' If Not .Quête.ContainsKey(Separation(1)) Then

                            '    Dim newQuête As New ClassQuête With {.Nom = VarQuête(Separation(1)), .ID = Separation(1)}
                            '    .Quête.Add(VarQuête(Separation(1)), newQuête)

                          '  End If

                        Case "053" 'Im053;Linaculer

                            EcritureMessage(index, "[Groupe]", Separation(1) & " ne suit plus votre déplacement.", Color.Green)

                        Case "052" 'Im052;Linaculer

                            EcritureMessage(index, "[Groupe]", Separation(1) & " suit votre déplacement.", Color.Green)

                        Case "045" ' Im045;50

                            EcritureMessage(index, "[Dofus]", "Tu as gagné " & Separation(1) & " kamas.", Color.Green)

                        Case "036" 'Im036;Linaculer

                            EcritureMessage(index, "[Dofus]", Separation(1) & " vient de rejoindre le combat en spectateur.", Color.Green) '/away

                          '  Dim newCombat As New Class_Combat

                          '  Task.Run(Function() newCombat.Spectateur(index, .FrmGroupe.Spectateur))

                        Case "034" 'Im034;60 

                            EcritureMessage(index, "[Familier]", "Tu as perdu " & Separation(1) & " points d'énergie.", Color.Red)
                            .Combat.EnCombat = False

                        Case "022" 'Im022;1~1568

                            'Je sépare les informations via ce signe "~"
                            Separation = Split(Separation(1), "~")

                            EcritureMessage(index, "[Dofus]", "Tu as perdu " & Separation(0) & " '" & VarItems(Separation(1)).Nom & "'.", Color.Red)

                        Case "020" ' Im022;481

                            EcritureMessage(index, "[Dofus]", "Tu as dû donner " & Separation(1) & " kamas pour pouvoir accéder à ce coffre.", Color.Lime)

                        Case "08" 'Im08;17293

                            EcritureMessage(index, "[Dofus]", "Tu as gagné " & Separation(1) & " points d'expérience.", Color.Green)

                        Case "01" 'Im01;100

                            EcritureMessage(index, "[Dofus]", "Tu as récupéré " & Separation(1) & " points de vie.", Color.Green)

                        Case Else

                            '  InformationInconnu(Comptes(Index).NomPersonnage, "Message_Dofus", Information)


                    End Select

                Else

                    Select Case data

                        Case "1183"

                            EcritureMessage(index, "[Dofus]", "La zone 'Incarnam' fonctionne sur plusieurs instances, pour éviter qu'un trop grand nombre de joueurs soient présent dans cette zone de petite taille. Ceci signifie qu'il existe plusieurs 'Incarnam' en parallèle, afin qu'il n'y ait pas plus d'un certain nombre de joueurs dans la même instance. Vous pouvez donc ne pas être dans le même 'Incarnam' que vos amis, pour les rejoindre, vous pouvez utiliser la liste d'amis, et vous retrouver instantanément à leurs côtés, à conditions qu'ils soient eux aussi dans Incarnam en dehors des grottes et donjons.", Color.Red)

                        Case "1182"

                        Case "1177"

                            EcritureMessage(index, "[Dofus]", "Vous avez trop d'objets dans votre inventaire, vous ne pouvez pas les voir tous. (1000 objets maximum)", Color.Red)

                        Case "1175"

                            EcritureMessage(index, "[Combat]", "Impossible de lancer ce sort actuellement.", Color.Red)
                            .Combat.Bloque.Set()
                        Case "1174"

                            EcritureMessage(index, "[Combat]", "Un obstacle géne le passage.", Color.Red)
                            .Combat.Bloque.Set()
                        Case "1165"

                            EcritureMessage(index, "[Dofus]", "La sauvegarde du serveur est terminée. L'accès au serveur est de nouveau possible. Merci de votre compréhension.", Color.Red)

                        Case "1164"

                            EcritureMessage(index, "[Dofus]", "Une sauvegarde du serveur est en cours... Vous pouvez continuer de jouer, mais l'accès au serveur est temporairement bloqué. La connexion sera de nouveau possible d'ici quelques instants. Merci de votre patience.", Color.Red)

                        Case "1159"

                            EcritureMessage(index, "[Dofus]", "Vous êtes à court de potion d'enclos de guilde.", Color.Red)
                            .BloqueGuilde.Set()

                        Case "1127"

                            EcritureMessage(index, "[Dofus]", "Incarnam ne vous est plus accessible désormais, votre expérience fait de vous un aventurier apte à parcourir le monde sans continuer dans cette zone...", Color.Red)

                        Case "1120"

                            EcritureMessage(index, "[Dofus]", "Impossible d'interagir avec votre percepteur sur la carte même où vous vous êtes connecté.", Color.Red)

                        Case "1117"

                            .Dragodinde.Bloque.Set()
                            EcritureMessage(index, "[Dofus]", "Impossible d'être sur une monture à l'intérieur d'une maison.", Color.Red)

                        Case "1105"

                            EcritureMessage(index, "[Dragodinde]", "L'étable est pleine. Vous ne pouvez conserver que 100 montures maximum.", Color.Violet)

                        Case "1104"

                            EcritureMessage(index, "[Dragodinde]", "Monture désignée invalide, trop de monture dans l'étable", Color.Violet)

                        Case "1102"

                            EcritureMessage(index, "[Dragodinde]", "Cellule cible invalide", Color.Violet)

                        Case "0194"

                            EcritureMessage(index, "[Forgemagie]", "La magie n'a pas parfaitement fonctionné, une des caractéristiques de l'objet a baissé en puissance.", Color.Red)

                        Case "0183"

                            EcritureMessage(index, "[Forgemagie]", "Malgré vos talents, la magie n'opère pas et vous sentez l'échec de la transformation.", Color.Red)

                        Case "0144"

                            EcritureMessage(index, "[Récolte]", "Votre inventaire est plein. Votre récolte est perdue...", Color.Red)

                       '     .FullPods = True

                        Case "0143"

                            EcritureMessage(index, "[Dofus]", "Le joueur : " & data & " vient de se connecter.", Color.Green)

                        Case "0118"

                            EcritureMessage(index, "[Craft]", "Vous n'arrivez pas à assembler correctement les ingrédients, et vous n'arrivez pas à concevoir quoi que ce soit d'utilisable cette fois.", Color.Red)

                        Case "0117"

                            EcritureMessage(index, "[Forgemagie]", "Malgré vos talents, la magie n'opère pas et vous sentez l'échec de la transformation, ainsi que la diminution de la puissance de l'objet..", Color.Red)

                        Case "0106" ' Im0106

                            EcritureMessage(index, "[Dofus]", "Pour utiliser le canal d'alignement vous devez développer vos ailes à 3 ou plus, ou encore avoir choisi une spécialisation par les quêtes d'alignement (niveau de quêtes à partir de 20)", Color.Red)

                        Case "0104"

                            EcritureMessage(index, "[Dofus]", "Demande d'aide annulée...", Color.Green)

                            .Combat.Aide = False

                        Case "0103"

                            EcritureMessage(index, "[Dofus]", "Demande d'aide signalée...", Color.Green)

                            .Combat.Aide = True

                        Case "189"

                            EcritureMessage(index, "[Dofus]", "Bienvenue sur Dofus, dans le Monde des douze !" & vbCrLf &
                            "Rappel : prenez garde, il est interdit de transmettre votre identifiant de connexion ainsi que votre mot de passe.", Color.Red)

                        Case "172"

                            EcritureMessage(index, "[Hôtel de Vente]", "Cet objet n'est plus disponible à ce prix. Quelqu'un a été plus rapide...", Color.Red)

                        Case "167"

                            EcritureMessage(index, "[Hôtel de Vente]", "Vous ne pouvez pas mettre plus d'objets en vente actuellement...", Color.Red)

                        Case "165"

                            EcritureMessage(index, "[Hôtel de vente]", "Vous ne disposez pas d'assez de kamas pour acquitter la taxe de mise en vente...", Color.Red)

                        Case "120"

                            EcritureMessage(index, "[Maison]", "Cet emplacement de stockage est déjà utilisé.", Color.Red)

                        Case "118" 'Im188

                            EcritureMessage(index, "[Dofus]", "Votre familier ne peut vous suivre tant que vous êtes sur votre monture...", Color.Red)

                        Case "113" ' Im113

                            EcritureMessage(index, "[Dofus]", "Cette action n'est pas autorisée sur cette carte.", Color.Red)

                           ' .BloqueAmi.Set()

                        Case "112"

                            EcritureMessage(index, "[Dofus]", "Vous êtes trop chargé. Jetez quelques objets afin de pouvoir bouger.", Color.Red)
                          '  .FullPods = True

                        Case "096"

                            EcritureMessage(index, "[Combat]", "L'équipe accepte de nouveau des personnages supplémentaires.", Color.Red)

                            .Combat.Cadenas = False

                        Case "095"

                            EcritureMessage(index, "[Combat]", "L'équipe n'accepte plus de personnages supplémentaires.", Color.Red)

                            .Combat.Cadenas = True

                        Case "094"

                            EcritureMessage(index, "[Combat]", "L'équipe accepte les membres de tous les groupes.", Color.Red)

                            .Combat.Groupe = False

                        Case "093"

                            EcritureMessage(index, "[Combat]", "L'équipe n'accepte désormais que les membres du groupe du personnage principal.", Color.Red)

                            .Combat.Groupe = True

                        Case "073"

                        Case "068"

                            .Pnj.Bloque.Set()

                            EcritureMessage(index, "[Dofus]", "Lot acheté.", Color.Green)

                        Case "040"

                            EcritureMessage(index, "[Combat]", "Le mode 'spectateur' est désactivé.", Color.Red)

                            .Combat.Spectateur = True

                        Case "039"

                            EcritureMessage(index, "[Combat]", "Le mode 'spectateur' est activé.", Color.Red)

                            .Combat.Spectateur = False

                        Case "037"

                            EcritureMessage(index, "[Dofus]", "Vous êtes désormais considéré comme absent.", Color.Red) '/away
                             'FAMILIER

                        Case "032"

                            EcritureMessage(index, "[Familier]", "Votre familier apprécie le repas.", Color.Green)
                            .BloqueItem.Set()

                        Case "031"

                            EcritureMessage(index, "[Familier]", "Vous donnez à manger à votre familier famélique qui traînait comme un zombi. Il se force à manger mais la nourriture qu'il avale fait 3 fois son estomac et il se tord de douleur. Au moins il a mangé.", Color.Red)
                            .BloqueItem.Set()

                        Case "029"

                            EcritureMessage(index, "[Familier]", "Vous donnez à manger à votre familier. Il semble qu'il avait très faim.", Color.Green)
                            .BloqueItem.Set()

                        Case "027"

                            EcritureMessage(index, "[Familier]", "Vous donnez à manger à répétition à votre familier déjà obèse. Il avale quand même la ressource et fait une indigestion.", Color.Red)
                            .BloqueItem.Set()

                        Case "026"

                            EcritureMessage(index, "[Familier]", "Vous donnez à manger à votre familier alors qu'il n'avait plus faim. Il se force pour vous faire plaisir.", Color.Red)
                            .BloqueItem.Set()

                        Case "025"

                            EcritureMessage(index, "[Familier]", "Votre familier vous fait la fête !", Color.Green)

                        Case "153"

                            EcritureMessage(index, "[Familier]", "Votre familier prend la ressource, la renifle un peu, ne semble pas convaincu et vous la rend.", Color.Red)
                            .BloqueItem.Set()

                        Case "024"

                            EcritureMessage(index, "[Dofus]", "Tu viens de mémoriser un nouveau zaap.", Color.Green)

                        Case "06"

                            EcritureMessage(index, "[Dofus]", "Position sauvegardée.", Color.Green)

                            '  .EnInteraction = False
                            '  .BloqueInteraction.Set()

                        Case Else

                            'InformationInconnu(Comptes(Index).NomPersonnage, "Message_Dofus", Information)

                    End Select

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDeCompte, "GiDofusInformation", data)

            End Try

        End With

    End Sub

    Sub GiDofusTchat(index As Integer, data As String)

        With Comptes(index)

            Try

                'cMK%|1234567|Linaculer|salut tout le monde|

                data = AsciiDecoder(data)

                Dim separateData As String() = Split(data, "|")

                Select Case Mid(data, 4, 1)

                    Case "|"

                        EcritureMessage(index, "[General]", "[" & separateData(1) & "] " & separateData(2) & " : " & separateData(3), Color.White)  'General

                    Case "$"

                        EcritureMessage(index, "[Groupe]", "[" & separateData(1) & "] " & separateData(2) & " : " & separateData(3), Color.Cyan)  'Groupe

                    Case "F"

                        EcritureMessage(index, "[Privée de]", "[" & separateData(1) & "] " & separateData(2) & " : " & separateData(3), Color.Blue) 'Privée de 

                    Case "f" ' cMEf

                        EcritureMessage(index, "[Alignement]", "Le joueur " & Mid(data, 5) & " n'est pas connecté.", Color.Red)  'Commerce

                    Case "T"

                        EcritureMessage(index, "[Privée à]", "[" & separateData(1) & "] " & separateData(2) & " : " & separateData(3), Color.Blue)  'Privée à

                    Case "%"

                        EcritureMessage(index, "[Guilde]", "[" & separateData(1) & "] " & separateData(2) & " : " & separateData(3), Color.Violet) 'Guilde

                    Case "!"

                        EcritureMessage(index, "[Alignement]", "[" & separateData(1) & "] " & separateData(2) & " : " & separateData(3), Color.Orange)  'Alignement

                    Case "?"

                        EcritureMessage(index, "[Recrutement]", "[" & separateData(1) & "] " & separateData(2) & " : " & separateData(3), Color.Gray)  'Recrutement

                    Case ":"

                        EcritureMessage(index, "[Commerce]", "[" & separateData(1) & "] " & separateData(2) & " : " & separateData(3), Color.Sienna)  'Commerce

                    Case "A" ' cMEA

                        EcritureMessage(index, "[Alignement]", "Impossible d'utilise ce canal.", Color.Red)  'Commerce

                End Select

                If separateData(1) = .Personnage.ID Then

                    .Tchat.BloqueTchat.Set()

                End If

            Catch ex As Exception

                ErreurFichier(index, .Personnage.NomDeCompte, "GiDofusTchat", data & vbCrLf & ex.Message)

            End Try

        End With

    End Sub

End Module

#Region "Class"

Public Class CTchat

    Public Canal As New CTchatCanal
    Public Tchat As New Dictionary(Of String, Color)
    Public BloqueTchat As Threading.ManualResetEvent = New Threading.ManualResetEvent(False)

End Class

Public Class CTchatCanal

    Public Information As Boolean
    Public General As Boolean
    Public GroupePriveeEquipe As Boolean
    Public Guilde As Boolean
    Public Alignement As Boolean
    Public Recrutement As Boolean
    Public Commerce As Boolean

End Class

#End Region