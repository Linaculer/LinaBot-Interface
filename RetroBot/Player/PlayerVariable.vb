
Partial Class Player

    Public Proxy As New cProxy

#Region "MITM"

    Public Client As Net.Sockets.Socket
    Public Listener As Net.Sockets.Socket
    Public BufferClient(65536) As Byte
    Public MITM As Boolean
    Public AppMITM As Integer

#End Region

    Public Index As Integer
    Public FrmGroupe As New FrmGroupe
    Public FrmUser As New UcBot
    Public Personnage As New ClassPersonnage
    Public TabPage_Bot As New TabPage

    'Connexion
    Public Connecté, EnConnexion As Boolean
    Public EnAuthentification As Boolean

    'Les Sockets
    Public Socket_Authentification, Socket As All_CallBack
    Public SockSendRecv As New Dictionary(Of String, Color)

    'Envoie
    Public _Send As String

    'DLL
    Public ThreadDll As Threading.Thread

    'Familier
    Public Familier As New Dictionary(Of String, String)

    'Caractéristique
    Public Caracteristique As New CCaracteristique
    Public OptionCaracteristique As String

    'Sort
    Public Sort As New Dictionary(Of String, CSort)
    Public OptionSort As New List(Of String)
    Public BloqueSort As New Threading.ManualResetEvent(False)

    'Combat
    Public Combat As New CCombat

    'IA
    Public ThreadIA As Threading.Thread
    Public IntelligenceArtificielle As New List(Of String)

    'Map
    Public Map As New CMap

    'Métier
    Public Metier As New Dictionary(Of Integer, CMetier)
    Public Recolte As New CRecolte

    'Maison
    Public Maison As New CMaison
    Public MaisonMap As New Dictionary(Of Integer, CMaison)

    'Guilde
    Public BloqueGuilde As Threading.ManualResetEvent = New Threading.ManualResetEvent(False)

    'Inventaire
    Public Inventaire As New Dictionary(Of Integer, CItem)
    Public BloqueItem As Threading.ManualResetEvent = New Threading.ManualResetEvent(False)
    Public BonusEquipement As New Dictionary(Of Integer, CBonusEquipement)

    'Dragodinde
    Public Dragodinde As New CDragodinde



    'Guilde
    Public Guilde As New CGuilde

    'Pnj
    Public Pnj As New CPnj

    'Echange
    Public Echange As New CEchange

    'Ami
    Public Ami As New CAmi

    'Tchat
    Public Tchat As New CTchat
    ' Public FrmTchat As New FrmTchat(Index)



    'Groupe
    Public Groupe As New CGroupe

    'Defi
    Public Defi As New CDefi

    'Zaap
    Public ZaapI As New Dictionary(Of Integer, Integer)

    'Enclos
    Public Enclos As New CEnclos

    'Option total
    Public [Option] As New COption

    'Trajet
    '  Public CreateurTrajetBot As New CreateurTrajet(Index)
    Public ThreadTrajet As Threading.Thread
    Public Trajet As New Dictionary(Of String, List(Of String))

End Class
