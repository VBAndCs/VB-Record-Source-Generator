
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

            Dim generator1 As ISourceGenerator = New RecordGenerator.RecGen()

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
            Dim b = Encoding.ASCII.GetBytes(sourceStr)
            Dim hash = New MD5CryptoServiceProvider().ComputeHash(b)
            Dim sb As New StringBuilder(hash.Length)
            For i = 0 To hash.Length - 1
                sb.Append(hash(i).ToString("X2"))
            Next
            Return sb.ToString()
        End Function

        <TestMethod>
        Public Sub SB()
            Dim TestRecord = <![CDATA[
Imports System.Text
Record TestSB(SB As StringBuilder)
]]>.Value

            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "F32F7CDCC5555D85878C33DA12615269")
        End Sub

        <TestMethod>
        Public Sub XMLComments()
            Dim TestRecord = <![CDATA[
''' <summary>
''' Person record type
''' </summary>
''' <param name="FirstName">First Name</param>
''' <param name="LastName">Last Name</param>
''' <remarks>
''' The person type is a positional record containing the
''' properties for the first and last names.
''' </remarks>
Public Record Test(
    FirstName$,
    LastName$
)
]]>.Value

            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "4D586F5D0EBA04910436FAD4BB8D39F0")
        End Sub

        <TestMethod>
        Public Sub ImpInterface()
            Dim TestRecord = <![CDATA[
Record Test(
    Name$,
    Pos As (X%, Y%),
    Age%,
    Move = Sub(x%) _Pos.X = x,
    Move = Function(x%, y%) 
                       _Pos = (x, y)
                       Return True
                  End Function,
    GetText = Function(name$) "Hello " & name,
    GetText = Function( ) "Hello",
    Concat = Function(x$, y$) x & y,
    Concat = Fn(x$, y$, n%) => x & y & n.ToString()
) Implements ITest(Of Integer) , Inherits DateInfo
]]>.Value

            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "364633B62E23ECB43560D74ACCD72A3C")
        End Sub

        <TestMethod>
        Public Sub Lists()
            Dim TestRecord = <![CDATA[
Record Test(
    A As List(Of Byte),
    B As List(Of Byte) = New List(Of Byte)(),
    C = New List(Of Byte)(),
    D As New List(Of Byte) From {1, 2, 3}
)
]]>.Value

            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "8693C1959E637740DA20C0C953EA0A5C")
        End Sub


        <TestMethod>
        Public Sub PropertyLess()
            Dim TestRecord = <![CDATA[
  
   Namespace MyApp.Tests

Class Zero()
]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "4D79620A60F86AC4E76D626C2C685C12")
        End Sub

        <TestMethod>
        Public Sub AsNew()
            Dim TestRecord = <![CDATA[
Public Record Test(Of T1 As New, T2 As New)(
      Items As New List(Of Integer)
)
]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "0EE681E6FCE930FF2675BBE0A10A8970")
        End Sub


        <TestMethod>
        Public Sub Nameless()
            Dim TestRecord = <![CDATA[Public Record (Name ="", Value = 0.0)]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            ' Must Fail
            Assert.AreEqual(result.Diagnostics(0).Id, "BC42502", result.Diagnostics(0).ToString())
        End Sub

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
            Assert.AreEqual(GetHash(result.Output), "9A15CC9284CE0B1EE3C382F59755DDFA")
        End Sub

        <TestMethod>
        Public Sub Student()
            Dim TestRecord = <![CDATA[
Public Record Human(
	ID = 0, 
	Name = "", 
    Address As (City$, Street$, No%)
) Inherits DateInfo

<MyAttr>
Public Record Student(
    Name As String,
    ClassRoom = 0,
	Grades As double, 
    Print = Function()
                     return Name & Grades
                End Function
) Inherits Human
]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next
            Assert.AreEqual(GetHash(result.Output), "C444D0FB2A09413B7298CA531D708114")
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
            Assert.AreEqual(GetHash(result.Output), "049409585AB16F64E8E2C41387FB8C08")
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

            Assert.AreEqual(GetHash(result.Output), "23D3E71CA11B1B810AE87A4D7953EF2A")

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

            Assert.AreEqual(GetHash(result.Output), "39340A9FACC31B27029A47137C660244")

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

            Assert.AreEqual(GetHash(result.Output), "BD0B74EF5A197005C806FAC65D8CC256")

        End Sub

        <TestMethod>
        Public Sub TestValueTypes()
            Dim TestRecord = <![CDATA[
Imports Microsoft.VisualBasic

Record TestStructs(
    A As MyStruct,
    B As MyStruct?,
    C  As MyStruct = new MyStruct(),
    D = New MyStruct() With {.Value = 1}
)

Record TestEums(
    A As TriState,
    B As TriState?,
    C = TriState.True,
    D  As TriState = TriState.True,
)

Record TestTuples(
    A As System.ValueTuple,
    B As System.ValueTuple?,
    C As (Integer, String) = (1, "abc"),
    D = (1, "abc"),
    E = (ID:= 1, Value:= "abc")
)
]]>.Value
            Dim result = GetGeneratedOutput(TestSourceCode, TestRecord)
            For Each diag In result.Diagnostics
                Assert.AreNotEqual(diag.Id, "BC42502", diag.ToString())
            Next

            Assert.AreEqual(GetHash(result.Output), "02CD51357CD42ABEF09809F0AD07E19B")

        End Sub

    End Class
End Namespace