Imports System.Reflection
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic

Module Helper
    Public Function GetCompilation(source As String) As Compilation

        Dim syntaxTree = VisualBasicSyntaxTree.ParseText(source)

        Dim references = New List(Of MetadataReference)
        Dim assemblies As Assembly() = AppDomain.CurrentDomain.GetAssemblies()
        For Each assembly As Assembly In assemblies
            If Not assembly.IsDynamic Then
                references.Add(MetadataReference.CreateFromFile(assembly.Location))
            End If
        Next

        Return VisualBasicCompilation.Create("__SOURCE_GENERATOR_TEST__", New SyntaxTree() {syntaxTree}, references, New VisualBasicCompilationOptions(OutputKind.DynamicallyLinkedLibrary))
    End Function

    Public TestSourceCode As String = <![CDATA[
Module Program
    Public Const MyValue As Integer = 1
End Module

Module Module1
   Public Sub MySub(s As String)
        Console.WriteLine(s)
    End Sub
End Module

Public Class Test
    Public Property [Date] As Date
End Class

Public Class DateInfo
   Public [Date] As Date
End Class

Public Structure MyStruct
    Public Property [Date] As Date
End Structure

Public Class MyAttrAttribute
    Inherits Attribute

End Class

Public Interface ITest(Of T)
       Property Name As String
       Default Property Item(i As Integer) As T
       Property Age As T
       Property Data(x As Integer) As T
       Sub Move(x As Integer)
       Sub Move(x As Integer, y As Integer)
       Function GetText() As String
       Function Concat(s1 As String, s2 As String, n As Integer)
End InterFace
]]>.Value

End Module


Public Class MyAdditionalText
    Inherits AdditionalText

    Dim _path, _text As String
    Public Sub New(path As String, text As String)
        _path = path
        _text = text
    End Sub

    Public Overrides ReadOnly Property Path As String
        Get
            Return _path
        End Get
    End Property

    Public Overrides Function GetText(Optional cancellationToken As CancellationToken = Nothing) As SourceText
        Return SourceText.From(_text)
    End Function
End Class
