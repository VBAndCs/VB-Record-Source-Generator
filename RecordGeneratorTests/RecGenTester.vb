Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports System.Diagnostics.CodeAnalysis
Imports System.Reflection
Imports System.Runtime.InteropServices
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.VisualStudio.TestTools.UnitTesting

Namespace RecordGeneratorTests
    <TestClass>
    Public Class RecGenTester

        <TestMethod>
        Public Sub Execute()
            Dim rec As String = <![CDATA[
Imports System.Text, System.IO
Imports System.Collections


Public Class TestEnums(
    State = TriState.False,
    List = new List(Of Integer),
    Value = MyValue
 )

Public Class Person(
	ID = 0, 
	Name = "", 
    Address = (City := "", Street := "", No := 0)
) Inherits Test

<Record>
Public Class Student(
    Name As String,
    ClassRoom = 0,
	Grades As double, 
    Print = Function()
                     return Name & Grades
                End Function
) Inherits Person

Public Class UniStudent(
    University As String,
    Collage As String,
    Print = Function() $"{Name}, {University}, {Collage}"
) Inherits Student
   
]]>.Value

            Dim source As String = <![CDATA[
Module Program
    Public Const MyValue As Integer = 1
End Module

Public Class Test
    Public Property [Date] As Date
End Class
]]>.Value


            Dim result = GetGeneratedOutput(source, rec)
            Stop

            If result.Diagnostics.Length > 0 Then
                Console.WriteLine("Diagnostics:")
                For Each diag In result.Diagnostics
                    Console.WriteLine("   " & diag.ToString())
                Next
                Console.WriteLine()
                Console.WriteLine("Output:")
            End If

            Console.WriteLine(result.Output)

        End Sub

        Private Function GetGeneratedOutput(source As String, additionalFile As String) As (Diagnostics As ImmutableArray(Of Diagnostic), Output As String)

            Dim syntaxTree = VisualBasicSyntaxTree.ParseText(source)

            Dim references As List(Of MetadataReference) = New List(Of MetadataReference)
            Dim assemblies As Assembly() = AppDomain.CurrentDomain.GetAssemblies()
            For Each assembly As Assembly In assemblies
                If Not assembly.IsDynamic Then
                    references.Add(MetadataReference.CreateFromFile(assembly.Location))
                End If
            Next



            Dim compilation = VisualBasicCompilation.Create("Foo", New SyntaxTree() {syntaxTree}, references, New VisualBasicCompilationOptions(OutputKind.DynamicallyLinkedLibrary))

            Dim generator1 As ISourceGenerator = New RecordGenerator.RecordGenerator()

            Dim iaGenerator = {generator1}.ToImmutableArray

            Dim driver = VisualBasicGeneratorDriver.Create(iaGenerator,
                                                       {CType(New MyAdditionalText("JustFotTest.rec", additionalFile), AdditionalText)}.ToImmutableArray,
                                                       Nothing,
                                                       Nothing)

            Dim outputCompilation As Compilation = Nothing
            Dim generateDiagnostics As ImmutableArray(Of Diagnostic) = Nothing
            driver.RunGeneratorsAndUpdateCompilation(compilation, outputCompilation, generateDiagnostics)

            Return (generateDiagnostics, outputCompilation.SyntaxTrees.Last().ToString())

        End Function

    End Class
End Namespace