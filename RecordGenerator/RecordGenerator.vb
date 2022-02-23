' Created By, Mohammad Hamdy Ghanem, 
' Egypt, 2021

Imports System.Text
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic

<Generator(LanguageNames.VisualBasic)>
Public Class RecGen
    Implements ISourceGenerator

    Public Sub Initialize(context As GeneratorInitializationContext) Implements ISourceGenerator.Initialize

    End Sub

    Public Sub Execute(context As GeneratorExecutionContext) Implements ISourceGenerator.Execute
        Dim errList As New List(Of (RecordName As String, Code As String, Exception As Exception))

        Dim recFiles = From file In context.AdditionalFiles
                       Where file.Path.ToLower().EndsWith(".rec")

        If Not recFiles.Any Then Return

        Try
            context.AddSource(NameOf(OptionalStruct), SourceText.From(OptionalStruct, Encoding.UTF8))
            context.AddSource(NameOf(HelperClass), SourceText.From(HelperClass, Encoding.UTF8))

            RecordParser.CurrentCompilation = context.Compilation.AddSyntaxTrees(
                    SyntaxFactory.ParseSyntaxTree(OptionalStruct),
                    SyntaxFactory.ParseSyntaxTree(HelperClass)
            )

        Catch ex As Exception
            If context.Compilation.AssemblyName <> "" Then
                Throw ex
            End If

        End Try

        For Each recFile In recFiles
            Try
                RecordParser.Generate(context, recFile.GetText().ToString())

            Catch ex As Exception
                If recFile.Path = "__Just_For_Test__.rec" Then
                    Throw ex ' To make the test fail
                Else
                    errList.Add((IO.Path.GetFileNameWithoutExtension(recFile.Path), recFile.GetText().ToString(), ex))
                End If
            End Try
        Next

        For Each errDetails In errList
            context.AddSource("_Error_" & errDetails.RecordName, SourceText.From($"
Module {errDetails.RecordName}_ErrorDetails
     ' The following record:
     ' {errDetails.Code.Replace(vbLf, vbLf & "'")}
     ' cuased this error:
     ' {errDetails.Exception.Message.Replace(vbLf, vbLf & "'")}
     ' {errDetails.Exception.StackTrace?.Replace(vbLf, vbLf & "'")}
End Module", Encoding.UTF8))
        Next
    End Sub


End Class

