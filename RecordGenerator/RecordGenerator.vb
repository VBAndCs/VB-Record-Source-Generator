' Created By, Mohammad Hamdy Ghanem, 
' Egypt, 2021

Imports System.Text
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<Generator(LanguageNames.VisualBasic)>
Public Class RecordGenerator
    Implements ISourceGenerator

    Public Sub Initialize(context As GeneratorInitializationContext) Implements ISourceGenerator.Initialize

    End Sub

    Public Sub Execute(context As GeneratorExecutionContext) Implements ISourceGenerator.Execute
        Dim errMsg = ""
        Try
            Dim recFiles = From file In context.AdditionalFiles
                           Where file.Path.ToLower().EndsWith(".rec")

            If Not recFiles.Any Then Return

            Dim Namespaces = context.Compilation

            context.AddSource(NameOf(OptionalStruct), SourceText.From(OptionalStruct, Encoding.UTF8))
            context.AddSource(NameOf(HelperClass), SourceText.From(HelperClass, Encoding.UTF8))

            RecordParser.CurrentCompilation = context.Compilation.AddSyntaxTrees(
                    SyntaxFactory.ParseSyntaxTree(OptionalStruct),
                    SyntaxFactory.ParseSyntaxTree(HelperClass)
            )

            For Each recFile In recFiles
                RecordParser.Generate(context, recFile.GetText().ToString())
            Next
        Catch ex As Exception
            errMsg = "'" & ex.Message
        End Try

        If errMsg <> "" Then
            context.AddSource("Error", SourceText.From(errMsg, Encoding.UTF8))
        End If


    End Sub


End Class

