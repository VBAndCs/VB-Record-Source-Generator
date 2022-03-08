Module Module1
    Sub MySub(s As String)
        Console.WriteLine(s)
    End Sub
End Module

Public Class Test
    Public Property [Date] As Date
End Class

Public Structure MyStruct
    Public Property Value As Integer
End Structure

Public Class MyAttrAttribute
    Inherits Attribute

End Class

Public Interface ITest(Of T)
    ReadOnly Property Name As String
    Default Property Item(i As Integer) As T
    Property Age As T
    Property Data(x As Integer) As T
    Sub Move(x As Integer)
    Sub Move(x As Integer, y As Integer)
    Function GetText() As String
    Function Concat(s1 As String, s2 As String, n As Integer) As String
End Interface
