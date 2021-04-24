Friend Class ClassServeur

    Public Nom As String
    Public IP As String
    Public Port As Integer
    Public ID As String

End Class

#Region "Personnage"

Public Class ClassPersonnage

    Public NomDeCompte As String
    Public MotDePasse As String
    Public NomDuPersonnage As String
    Public Serveur As String
    Public Alignement As String = ""
    Public ID As Integer
    Public Pseudo As String = ""
    Public QuestionSecréte As String = ""
    Public Abonnement As Date = TimeOfDay
    Public Ticket As String
    Public Niveau As Integer
    Public ClasseSexe As Integer
    Public Classe As Integer
    Public Sexe As Integer
    Public IDServeur As Integer
    Public Kamas As Integer
    Public Regeneration As Integer
    Public CapitalSort As Integer
    Public Vivant As Boolean
    Public EnInteraction As Boolean
    Public Experience As New ClassExpérience
    Public Pods As New ClassPods
    Public Energie As New ClassEnergie
    Public Vitaliter As New ClassVitalité

    Public InteractionCellule As Integer

End Class


Public Class ClassExpérience

    Public Minimum As Integer
    Public Maximum As Integer
    Public Actuelle As Integer
    Public Pourcentage As Integer

End Class

Public Class ClassVitalité

    Public Maximum As Integer = 55
    Public Actuelle As Integer = 0
    Public Pourcentage As Integer = 0

End Class

Public Class ClassEnergie

    Public Maximum As Integer
    Public Actuelle As Integer
    Public Pourcentage As Integer

End Class

Public Class ClassPods

    Public Maximum As Integer
    Public Actuelle As Integer
    Public Pourcentage As Integer

End Class

#End Region

Public Class ClassQuête

    Public Nom As String
    Public ID As Integer

End Class

Public Class cProxy

    Public Active As Boolean
    Public IP As String
    Public Port As String
    Public Identifiant As String
    Public MotDePasse As String

End Class