Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Text

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
