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
        Try
            Dim recFiles = From file In context.AdditionalFiles
                           Where file.Path.ToLower().EndsWith(".rec")

            If Not recFiles.Any Then Return

            context.AddSource(NameOf(DefaultOfStruct), SourceText.From(DefaultOfStruct, Encoding.UTF8))
            context.AddSource(NameOf(DefaultOfTStruct), SourceText.From(DefaultOfTStruct, Encoding.UTF8))
            context.AddSource(NameOf(DefaultStruct), SourceText.From(DefaultStruct, Encoding.UTF8))
            context.AddSource(NameOf(OptionalStruct), SourceText.From(OptionalStruct, Encoding.UTF8))
            context.AddSource(NameOf(HelperClass), SourceText.From(HelperClass, Encoding.UTF8))

            Dim comp = context.Compilation
            RecordParser.AddReferences(comp.References)
            RecordParser.AddReferences(comp.ExternalReferences)
            RecordParser.AddReferences(comp.DirectiveReferences)

            For Each recFile In recFiles
                RecordParser.Generate(context, recFile.GetText().ToString())
            Next
        Catch ex As Exception
            Console.WriteLine(ex.Message)
        End Try
    End Sub


End Class

