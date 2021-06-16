Friend Structure PropertyInfo
    Public IsReadOnly As Boolean
    Public IsKey As Boolean

    Dim _name As String
    Public Property Name As String
        Get
            Return _name
        End Get
        Set(value As String)
            _name = value
            camelCaseName = _name(0).ToString.ToLower + _name.Substring(1)
        End Set
    End Property

    Public AddOverrides As Boolean
    Public InheritanceModifier As String
    Public camelCaseName As String
    Public Type As String
    Public DefaultValue As String
End Structure
