Option Explicit On
Option Strict On
Option Infer On

Imports System.Collections.Immutable
Imports System.Reflection
Imports System.Security.Cryptography
Imports System.Text
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.VisualStudio.TestTools.UnitTesting

Namespace RecordGeneratorTests
    <TestClass>
    Public Class RecGenTester

        Private Function GetGeneratedOutput(source As String, additionalFile As String) As (Diagnostics As ImmutableArray(Of Diagnostic), Output As String)

            Dim compilation = GetCompilation(source)

            Dim generator1 As ISourceGenerator = New RecordGenerator.RecordGenerator()

            Dim iaGenerator = {generator1}.ToImmutableArray

            Dim driver = VisualBasicGeneratorDriver.Create(iaGenerator,
                                                       {CType(New MyAdditionalText("__Just_For_Test__.rec", additionalFile), AdditionalText)}.ToImmutableArray,
                                                       Nothing,
                                                       Nothing)

            Dim outputCompilation As Compilation = Nothing
            Dim generateDiagnostics As ImmutableArray(Of Diagnostic) = Nothing
            driver.RunGeneratorsAndUpdateCompilation(compilation, outputCompilation, generateDiagnostics)

            Return (generateDiagnostics, outputCompilation.SyntaxTrees.Last().ToString())

        End Function


        Function GetHash(sourceStr As String) As String
            Dim b = ASCIIEncoding.ASCII.GetBytes(sourceStr)
            Dim hash = New MD5CryptoServiceProvider().ComputeHash(b)
            Dim sb As New StringBuilder(hash.Length)
            For i = 0 To hash.Length - 1
                sb.Append(hash(i).ToString("X2"))
            Next
            Return sb.ToString()
        End Function

        <TestMethod>
        Public Sub NameValue()
            Dim TestRecord = <![CDATA[Public Record NameValue(Name ="", Value = 0.0)]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "23EA3418815F4B00DDDDFAB7FAE8B8CB")
        End Sub

        <TestMethod>
        Public Sub Info()
            Dim TestRecord = <![CDATA[Record Info(Of T1, T2, T3 As New)(A As T1, B As T2, C As T3)]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "042C45F43F5EB37FB23EF4CC654B0385")
        End Sub

        <TestMethod>
        Public Sub ROStruct()
            Dim TestRecord = <![CDATA[Public ReadOnly Structure ROStruct(X$, Y%, Z@)]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "D7EA876A72D6310B941A5BC53DB79720")
        End Sub

        <TestMethod>
        Public Sub ROClass()
            Dim TestRecord = <![CDATA[Friend ReadOnly Class ROClass(A As Integer, B As Integer)]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "23D3E71CA11B1B810AE87A4D7953EF2A")
        End Sub

        <TestMethod>
        Public Sub Author()
            Dim TestRecord = <![CDATA[<MyAttr>
Public Key Class Author(
	 ReadOnly Key ID = 0, 
	ReadOnly Name = "",	
    Books As List(Of Book)
)]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "379C904DBE64C47E4DE9EAF5B6BAC276")
        End Sub

        <TestMethod>
        Public Sub Book()
            Dim TestRecord = <![CDATA[
Public Class Book(
	ReadOnly Key ID%, 
	<MyAttr>ReadOnly Name As String,	
    AuthorID As Integer
) ]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "BD0B74EF5A197005C806FAC65D8CC256")
        End Sub

        <TestMethod>
        Public Sub Person()
            Dim TestRecord = <![CDATA[
Imports System.Text, System.IO
Imports System.Collections 

Public Class Person(
	Key ID = 0, 
	Name = "", 
    <MyAttr>Address = (City := "", Street := "", No := 0)
) Inherits Test

]]>.Value

            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "887CA26CBCA0BDEBB9B821BBD497AFE7")
        End Sub

        <TestMethod>
        Public Sub Student()
            Dim TestRecord = <![CDATA[<MyAttr>
Public Record Student(
    Name As String,
    ClassRoom = 0,
	Grades As double, 
    Print = Function()
                     return Name & Grades
                End Function
) Inherits Person
]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "85C5A7486FC88663E88388715DD7DBDA")
        End Sub

        <TestMethod>
        Public Sub UniStudent()
            Dim TestRecord = <![CDATA[<MyAttr>Public Class UniStudent(
    University As String,
    Collage As String,
    Print = Function() $"{Name}, {University}, {Collage}"
) Inherits Student]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "238DCFCB9E7435E37542F6C650733EE5")

        End Sub

        <TestMethod>
        Public Sub EnumsAndConsts()
            Dim TestRecord = <![CDATA[
Public Class TestEnums(
    Immutable State = TriState.False,
    ImmutableKey List = new List(Of Integer),
    Immutable Key Value = MyValue
 )]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "868ADB786CA37DDB75B281A9B231DA68")
        End Sub

        <TestMethod>
        Public Sub ThreeSingleLineRecords()
            Dim TestRecord = <![CDATA[Public Record NameValue(Name ="", Value = 0.0)

Public ReadOnly Structure ROStruct(X$, Y%, Z@)

Friend ReadOnly Class ROClass(A As Integer, B As Integer)

]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
        End Sub

        <TestMethod>
        Public Sub ThreeMultiLineRecords()
            Dim TestRecord = <![CDATA[Imports System.Text, System.IO
Imports System.Collections 

Public Class Person(
	Key ID = 0, 
	Name = "", 
    <MyAttr>Address = (City := "", Street := "", No := 0)
) Inherits Test

<MyAttr>
Public Record Student(
    Name As String,
    ClassRoom = 0,
	Grades As double, 
    Print = Function()
                     return Name & Grades
                End Function
) Inherits Person

<MyAttr>Public Class UniStudent(
    University As String,
    Collage As String,
    Print = Function() $"{Name}, {University}, {Collage}"
) Inherits Student
]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
        End Sub

        <TestMethod>
        Public Sub TwoMultiLineRecords()
            Dim TestRecord = <![CDATA[<MyAttr>
Public Key Class Author(
	 ReadOnly Key ID = 0, 
	ReadOnly Name = "",	
    Books As List(Of Book)
)

Public Class Book(
	ReadOnly Key ID%, 
	<MyAttr>ReadOnly Name As String,	
    AuthorID As Integer
) 
]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next

        End Sub

    End Class

End Namespace