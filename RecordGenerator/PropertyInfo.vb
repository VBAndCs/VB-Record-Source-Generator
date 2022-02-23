Public Class PropertyInfo
    Public IsReadOnly As Boolean
    Public IsKey As Boolean

    Dim _name As String
    Public Property Name As String
        Get
            Return _name
        End Get
        Set(value As String)
            _name = value
            If _name = "" Then Return

            camelCaseName = _name(0).ToString.ToLower + If(_name.Length = 1, "", _name.Substring(1))
        End Set
    End Property

    Public InheritanceModifier As String
    Public camelCaseName As String
    Public Type As String
    Public DefaultValue As String
    Public LiteralDefVal As Boolean
    Public Attrs As String
    Public IsValueType As Boolean
    Public IsField As Boolean
    Public [Implements] As String
    Friend IsPrivateImplementation As Boolean
    Friend PublicPropName As String
End Class
