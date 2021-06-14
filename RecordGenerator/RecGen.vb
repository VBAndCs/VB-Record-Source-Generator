' Created By, Mohammad Hamdy Ghanem, 
' Egypt, 2021

Imports System.Text
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Text


<Generator(LanguageNames.VisualBasic)>
Class RecordGenerator
    Implements ISourceGenerator

    Dim context As GeneratorExecutionContext
    Public Sub Initialize(context As GeneratorInitializationContext) Implements ISourceGenerator.Initialize

    End Sub

    Public Sub Execute(context As GeneratorExecutionContext) Implements ISourceGenerator.Execute
        Console.WriteLine(context.AdditionalFiles.Length)
        Try
            Dim recFiles = From file In context.AdditionalFiles
                           Where file.Path.ToLower().EndsWith(".rec")

            If Not recFiles.Any Then Return

            context.AddSource(NameOf(DefaultOfStruct), SourceText.From(DefaultOfStruct, Encoding.UTF8))
            context.AddSource(NameOf(DefaultOfTStruct), SourceText.From(DefaultOfTStruct, Encoding.UTF8))
            context.AddSource(NameOf(DefaultStruct), SourceText.From(DefaultStruct, Encoding.UTF8))
            context.AddSource(NameOf(OptionalStruct), SourceText.From(OptionalStruct, Encoding.UTF8))
            Me.context = context

            For Each recFile In recFiles
                Dim generatorDocuments = New List(Of (documentName As String, source As String))
                RecordParser.Parse(context, recFile.GetText().ToString())
            Next
        Catch ex As Exception
            Console.WriteLine(ex.Message)
        End Try
    End Sub


End Class

