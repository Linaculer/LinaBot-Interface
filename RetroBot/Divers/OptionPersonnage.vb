Public Class COption

    Public PlanningActive As Boolean
    Public Planning As New Dictionary(Of String, Boolean()) From
        {
            {"Lundi", New Boolean() {True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True}},
            {"Mardi", New Boolean() {True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True}},
            {"Mercredi", New Boolean() {True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True}},
            {"Jeudi", New Boolean() {True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True}},
            {"Vendredi", New Boolean() {True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True}},
            {"Samedi", New Boolean() {True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True}},
            {"Dimanche", New Boolean() {True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True}}
        }

End Class


