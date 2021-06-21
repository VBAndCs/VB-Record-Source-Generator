Imports System.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports RecordGenerator

<TestClass>
Public Class RecordParserTester

    <TestMethod>
    Public Sub Lower()
        ' 1
        Dim testRecord = <![CDATA[Public Record NameValue(Name ="", Value = 0.0)]]>.Value
        Dim expected = <![CDATA[Public Record NameValue(Name ="", Value = 0.0)]]>.Value
        Dim result = RecordParser.Lower(testRecord, False)
        Assert.AreEqual(result, expected)

        expected = <![CDATA[<Record>Public Class NameValue(Name ="", Value = 0.0)]]>.Value
        result = RecordParser.Lower(result, True)
        Assert.AreEqual(result, expected)


        ' 2
        testRecord = <![CDATA[<Serializable>Public Record NameValue(Key Name ="", <Key>ReadOnly Value = 0.0)]]>.Value
        expected = <![CDATA[<Serializable>Public Record NameValue(<Key>Name ="", <Key><ReadOnly>Value = 0.0)]]>.Value
        result = RecordParser.Lower(testRecord, False)
        Assert.AreEqual(result, expected)

        expected = <![CDATA[<Record><Serializable>Public Class NameValue(<Key>Name ="", <Key><ReadOnly>Value = 0.0)]]>.Value
        result = RecordParser.Lower(result, True)
        Assert.AreEqual(result, expected)

        ' 3
        testRecord = <![CDATA[<Serializable>Public Record [Class](ReadOnly Key  [Class] = 0, Immutable Value = "Record")]]>.Value
        expected = <![CDATA[<Serializable>Public Record [Class](<ReadOnlyKey> [Class] = 0, <ReadOnly>Value = "Record")]]>.Value
        result = RecordParser.Lower(testRecord, False)
        Assert.AreEqual(result, expected)

        expected = <![CDATA[<Record><Serializable>Public Class [Class](<ReadOnlyKey> [Class] = 0, <ReadOnly>Value = "Record")]]>.Value
        result = RecordParser.Lower(result, True)
        Assert.AreEqual(result, expected)

        ' 4
        testRecord = <![CDATA[
<Serializable>
Public readonly Class [Class](
      ReadOnlyKey  [Class] = 0, 
      <ReadOnly>Value = "Class")
]]>.Value

        expected = <![CDATA[
<Serializable>
<ReadOnly>Public Class [Class](
      <ReadOnlyKey> [Class] = 0, 
      <ReadOnly>Value = "Class")
]]>.Value

        result = RecordParser.Lower(testRecord, False)
        Assert.AreEqual(result, expected)

        result = RecordParser.Lower(result, True)
        Assert.AreEqual(result, expected)


        ' 5
        testRecord = <![CDATA[
<Serializable>
Public Key Structure [Record](
      ReadOnly ID As Integer, 
      ReadOnly Value As String)
]]>.Value

        expected = <![CDATA[
<Serializable>
<Key>Public Structure [Record](
      <ReadOnly>ID As Integer, 
      <ReadOnly>Value As String)
]]>.Value

        result = RecordParser.Lower(testRecord, False)
        Assert.AreEqual(result, expected)

        result = RecordParser.Lower(result, True)
        Assert.AreEqual(result, expected)
    End Sub


    <TestMethod>
    Public Sub WriteNameValue()
        Dim code = <![CDATA[(Name ="", Value = 0.0)]]>.Value

        Dim expected = <![CDATA[   <Key>
   <DefaultValue("1= #1/1/0001#")>
   Public Shadows Property [Date] As Date

   <Key>
   <DefaultValue("1=__QUOTE____QUOTE__")>
   Public Property [Name] As String

   <Key>
   <DefaultValue("1= 0.0")>
   Public Property [Value] As Double


]]>.Value

        Dim properties As New List(Of PropertyInfo)
        Dim result = WriteProperties(code, properties, "Inherits Test").Replace(vbCrLf, vbLf)
        Assert.AreEqual(result, expected)


        expected = <![CDATA[    Public Sub New(
                Optional [date] As Date = #1/1/0001#,
                Optional [name] As String ="",
                Optional [value] As Double = 0.0
            )

        Me.Date = [date]
        Me.Name = [name]
        Me.Value = [value]
    End Sub

]]>.Value

        Dim sb As New StringBuilder
        RecordParser.WriteConstructor(properties, sb)
        result = sb.Replace(vbCrLf, vbLf).ToString()
        Assert.AreEqual(result, expected)


        expected = <![CDATA[    Public Function [With](
                Optional [date] As Date? = Nothing,
                Optional [name] As [Optional](Of String) = Nothing,
                Optional [value] As Double? = Nothing
            ) As NameValue

        Return  New NameValue(
            If ([date].HasValue, [date].Value, Me.Date),
            If ([name].HasValue, [name].Value, Me.Name),
            If ([value].HasValue, [value].Value, Me.Value)
        )
    End Function

]]>.Value
        sb.Clear()
        RecordParser.WriteWith("NameValue", "", properties, sb)
        result = sb.Replace(vbCrLf, vbLf).ToString()
        Assert.AreEqual(result, expected)

    End Sub


    <TestMethod>
    Public Sub WriteAuthor()
        Dim code = <![CDATA[(
	 ReadOnly Key ID = 0, 
	 ReadOnly Name = "",	
     Books As List(Of Book)
)]]>.Value

        Dim expected = <![CDATA[   <Key>
   <DefaultValue("1= 0")>
   Public ReadOnly Property [ID] As Integer

   <Key>
   <DefaultValue("1= __QUOTE____QUOTE__")>
   Public ReadOnly Property [Name] As String

   <Key>
   Public Property [Books] As List(Of Book)


]]>.Value

        Dim properties As New List(Of PropertyInfo)
        Dim result = WriteProperties(code, properties).Replace(vbCrLf, vbLf)
        Assert.AreEqual(result, expected)


        expected = <![CDATA[    Public Sub New(
                Optional [iD] As Integer = 0,
                Optional [name] As String = "",
                Optional [books] As [Optional](Of List(Of Book)) = Nothing
            )

        Me.ID = [iD]
        Me.Name = [name]
        If [books].HasValue
            Me.Books = [books].Value
        Else
            Me.Books = Nothing
        End If

    End Sub

]]>.Value
        Dim sb As New StringBuilder
        RecordParser.WriteConstructor(properties, sb)
        result = sb.Replace(vbCrLf, vbLf).ToString()
        Assert.AreEqual(result, expected)


        expected = <![CDATA[    Public Function [With](
                Optional [iD] As Integer? = Nothing,
                Optional [name] As [Optional](Of String) = Nothing,
                Optional [books] As [Optional](Of List(Of Book)) = Nothing
            ) As Author

        Return  New Author(
            If ([iD].HasValue, [iD].Value, Me.ID),
            If ([name].HasValue, [name].Value, Me.Name),
            If ([books].HasValue, [books].Value, Me.Books)
        )
    End Function

]]>.Value
        sb.Clear()
        RecordParser.WriteWith("Author", "", properties, sb)
        result = sb.Replace(vbCrLf, vbLf).ToString()
        Assert.AreEqual(result, expected)

    End Sub


    <TestMethod>
    Public Sub WriteBook()
        Dim code = <![CDATA[(ReadOnly Key ID%, 
	<MyAttr>ReadOnly Name As String,	
    AuthorID As Integer)]]>.Value

        Dim expected = <![CDATA[   <Key>
   <DefaultValue("1= 0")>
   Public ReadOnly Property [ID] As Integer

   <Key>
   <DefaultValue("1= __QUOTE____QUOTE__")>
   <MyAttr>
   Public ReadOnly Property [Name] As String

   <Key>
   <DefaultValue("1= 0")>
   Public Property [AuthorID] As Integer


]]>.Value

        Dim properties As New List(Of PropertyInfo)

        Dim result = WriteProperties(code, properties).Replace(vbCrLf, vbLf)
        Assert.AreEqual(result, expected)


        expected = <![CDATA[    Public Sub New(
                Optional [iD] As Integer = 0,
                Optional [name] As String = "",
                Optional [authorID] As Integer = 0
            )

        Me.ID = [iD]
        Me.Name = [name]
        Me.AuthorID = [authorID]
    End Sub

]]>.Value
        Dim sb As New StringBuilder
        RecordParser.WriteConstructor(properties, sb)
        result = sb.Replace(vbCrLf, vbLf).ToString()
        Assert.AreEqual(result, expected)


        expected = <![CDATA[    Public Function [With](
                Optional [iD] As Integer? = Nothing,
                Optional [name] As [Optional](Of String) = Nothing,
                Optional [authorID] As Integer? = Nothing
            ) As Book

        Return  New Book(
            If ([iD].HasValue, [iD].Value, Me.ID),
            If ([name].HasValue, [name].Value, Me.Name),
            If ([authorID].HasValue, [authorID].Value, Me.AuthorID)
        )
    End Function

]]>.Value
        sb.Clear()
        RecordParser.WriteWith("Book", "", properties, sb)
        result = sb.Replace(vbCrLf, vbLf).ToString()
        Assert.AreEqual(result, expected)

    End Sub


    <TestMethod>
    Public Sub WriteInfo()
        Dim code = <![CDATA[(A As T1, B As T2, C As T3)]]>.Value

        Dim expected = <![CDATA[   <Key>
   Public Property [A] As T1

   <Key>
   Public Property [B] As T2

   <Key>
   Public Property [C] As T3


]]>.Value

        Dim properties As New List(Of PropertyInfo)
        Dim result = WriteProperties(code, properties, "").Replace(vbCrLf, vbLf)
        Assert.AreEqual(result, expected)


        expected = <![CDATA[    Public Sub New(
                Optional [a] As [Optional](Of T1) = Nothing,
                Optional [b] As [Optional](Of T2) = Nothing,
                Optional [c] As [Optional](Of T3) = Nothing
            )

        If [a].HasValue
            Me.A = [a].Value
        Else
            Me.A = Nothing
        End If

        If [b].HasValue
            Me.B = [b].Value
        Else
            Me.B = Nothing
        End If

        If [c].HasValue
            Me.C = [c].Value
        Else
            Me.C = Nothing
        End If

    End Sub

]]>.Value

        Dim sb As New StringBuilder
        RecordParser.WriteConstructor(properties, sb)
        result = sb.Replace(vbCrLf, vbLf).ToString()
        Assert.AreEqual(result, expected)


        expected = <![CDATA[    Public Function [With](
                Optional [a] As [Optional](Of T1) = Nothing,
                Optional [b] As [Optional](Of T2) = Nothing,
                Optional [c] As [Optional](Of T3) = Nothing
            ) As Info(Of T1, T2, T3)

        Return  New Info(Of T1, T2, T3)(
            If ([a].HasValue, [a].Value, Me.A),
            If ([b].HasValue, [b].Value, Me.B),
            If ([c].HasValue, [c].Value, Me.C)
        )
    End Function

]]>.Value
        sb.Clear()
        RecordParser.WriteWith("Info", "(Of T1, T2, T3)", properties, sb)
        result = sb.Replace(vbCrLf, vbLf).ToString()
        Assert.AreEqual(result, expected)

    End Sub

    <TestMethod>
    Public Sub WriteTestEnum()
        Dim code = <![CDATA[(
    State = TriState.False,
    List = new List(Of Integer),
    Value = MyValue
 )]]>.Value

        Dim expected = <![CDATA[   <Key>
   <DefaultValue("1= TriState.False")>
   Public Property [State] As Microsoft.VisualBasic.TriState

   <Key>
   <DefaultValue("0= new List(Of Integer)")>
   Public Property [List] As System.Collections.Generic.List(Of Integer)

   <Key>
   <DefaultValue("1= MyValue")>
   Public Property [Value] As Integer


]]>.Value

        Dim properties As New List(Of PropertyInfo)

        Dim result = WriteProperties(
            code, properties, "", New StringBuilder("
    Imports Microsoft.VisualBasic
    Imports System.Collections.Generic")
            ).Replace(vbCrLf, vbLf)

        Assert.AreEqual(result, expected)


        expected = <![CDATA[    Public Sub New(
                Optional [state] As Microsoft.VisualBasic.TriState = TriState.False,
                Optional [list] As [Optional](Of System.Collections.Generic.List(Of Integer)) = Nothing,
                Optional [value] As Integer = MyValue
            )

        Me.State = [state]
        If [list].HasValue
            Me.List = [list].Value
        Else
            Me.List = new List(Of Integer)
        End If

        Me.Value = [value]
    End Sub

]]>.Value

        Dim sb As New StringBuilder
        RecordParser.WriteConstructor(properties, sb)
        result = sb.Replace(vbCrLf, vbLf).ToString()
        Assert.AreEqual(result, expected)


        expected = <![CDATA[    Public Function [With](
                Optional [state] As Microsoft.VisualBasic.TriState? = Nothing,
                Optional [list] As [Optional](Of System.Collections.Generic.List(Of Integer)) = Nothing,
                Optional [value] As Integer? = Nothing
            ) As TestEnum

        Return  New TestEnum(
            If ([state].HasValue, [state].Value, Me.State),
            If ([list].HasValue, [list].Value, Me.List),
            If ([value].HasValue, [value].Value, Me.Value)
        )
    End Function

]]>.Value

        sb.Clear()
        RecordParser.WriteWith("TestEnum", "", properties, sb)
        result = sb.Replace(vbCrLf, vbLf).ToString()
        Assert.AreEqual(result, expected)

    End Sub


    <TestMethod>
    Public Sub WritePerson()
        Dim code = <![CDATA[(
	Key ID = 0, 
	Name = "", 
    <MyAttr>Address = (City := "", Street := "", No := 0)
)]]>.Value

        Dim expected = <![CDATA[   <Key>
   <DefaultValue("1= 0")>
   Public Property [ID] As Integer

   <Key>
   <DefaultValue("1= __QUOTE____QUOTE__")>
   Public Property [Name] As String

   <Key>
   <DefaultValue("0= (City := __QUOTE____QUOTE__, Street := __QUOTE____QUOTE__, No := 0)")>
   <MyAttr>
   Public Property [Address] As (City As String, Street As String, No As Integer)


]]>.Value

        Dim properties As New List(Of PropertyInfo)
        Dim result = WriteProperties(code, properties, "Inherits Student").Replace(vbCrLf, vbLf)
        Assert.AreEqual(result, expected)


        expected = <![CDATA[    Public Sub New(
                Optional [iD] As Integer = 0,
                Optional [name] As String = "",
                Optional [address] As [Optional](Of (City As String, Street As String, No As Integer)) = Nothing
            )

        Me.ID = [iD]
        Me.Name = [name]
        If [address].HasValue
            Me.Address = [address].Value
        Else
            Me.Address = (City := "", Street := "", No := 0)
        End If

    End Sub

]]>.Value
        Dim sb As New StringBuilder
        RecordParser.WriteConstructor(properties, sb)
        result = sb.Replace(vbCrLf, vbLf).ToString()
        Assert.AreEqual(result, expected)


        expected = <![CDATA[    Public Function [With](
                Optional [iD] As Integer? = Nothing,
                Optional [name] As [Optional](Of String) = Nothing,
                Optional [address] As [Optional](Of (City As String, Street As String, No As Integer)) = Nothing
            ) As Person

        Return  New Person(
            If ([iD].HasValue, [iD].Value, Me.ID),
            If ([name].HasValue, [name].Value, Me.Name),
            If ([address].HasValue, [address].Value, Me.Address)
        )
    End Function

]]>.Value
        sb.Clear()
        RecordParser.WriteWith("Person", "", properties, sb)
        result = sb.Replace(vbCrLf, vbLf).ToString()
        Assert.AreEqual(result, expected)

    End Sub


    <TestMethod>
    Public Sub WriteStudent()
        Dim code = <![CDATA[(
    Name As String,
    ClassRoom = 0,
	Grades As double, 
    Print = Function()
                     return Name & Grades
                End Function
)]]>.Value

        Dim expected = <![CDATA[   <Key>
   <DefaultValue("1= __QUOTE____QUOTE__")>
   Public Property [Name] As String

   <Key>
   <DefaultValue("1= 0")>
   Public Property [ClassRoom] As Integer

   <Key>
   <DefaultValue("1= 0")>
   Public Property [Grades] As double


]]>.Value

        Dim properties As New List(Of PropertyInfo)
        Dim result = WriteProperties(code, properties, "").Replace(vbCrLf, vbLf)
        Assert.AreEqual(result, expected)


        expected = <![CDATA[    Public Sub New(
                Optional [name] As String = "",
                Optional [classRoom] As Integer = 0,
                Optional [grades] As double = 0
            )

        Me.Name = [name]
        Me.ClassRoom = [classRoom]
        Me.Grades = [grades]
    End Sub

]]>.Value
        Dim sb As New StringBuilder
        RecordParser.WriteConstructor(properties, sb)
        result = sb.Replace(vbCrLf, vbLf).ToString()
        Assert.AreEqual(result, expected)


        expected = <![CDATA[    Public Function [With](
                Optional [name] As [Optional](Of String) = Nothing,
                Optional [classRoom] As Integer? = Nothing,
                Optional [grades] As double? = Nothing
            ) As Student

        Return  New Student(
            If ([name].HasValue, [name].Value, Me.Name),
            If ([classRoom].HasValue, [classRoom].Value, Me.ClassRoom),
            If ([grades].HasValue, [grades].Value, Me.Grades)
        )
    End Function

]]>.Value
        sb.Clear()
        RecordParser.WriteWith("Student", "", properties, sb)
        result = sb.Replace(vbCrLf, vbLf).ToString()
        Assert.AreEqual(result, expected)

    End Sub


    <TestMethod>
    Public Sub WriteUniStudent()
        Dim code = <![CDATA[(
    University As String,
    Collage As String,
    Print = Function() $"{Name}, {University}, {Collage}"
)]]>.Value

        Dim expected = <![CDATA[   <Key>
   <DefaultValue("1= __QUOTE____QUOTE__")>
   Public Property [University] As String

   <Key>
   <DefaultValue("1= __QUOTE____QUOTE__")>
   Public Property [Collage] As String


]]>.Value

        Dim properties As New List(Of PropertyInfo)
        Dim result = WriteProperties(code, properties, "").Replace(vbCrLf, vbLf)
        Assert.AreEqual(result, expected)


        expected = <![CDATA[    Public Sub New(
                Optional [university] As String = "",
                Optional [collage] As String = ""
            )

        Me.University = [university]
        Me.Collage = [collage]
    End Sub

]]>.Value
        Dim sb As New StringBuilder
        RecordParser.WriteConstructor(properties, sb)
        result = sb.Replace(vbCrLf, vbLf).ToString()
        Assert.AreEqual(result, expected)


        expected = <![CDATA[    Public Function [With](
                Optional [university] As [Optional](Of String) = Nothing,
                Optional [collage] As [Optional](Of String) = Nothing
            ) As UniStudent

        Return  New UniStudent(
            If ([university].HasValue, [university].Value, Me.University),
            If ([collage].HasValue, [collage].Value, Me.Collage)
        )
    End Function

]]>.Value
        sb.Clear()
        RecordParser.WriteWith("UniStudent", "", properties, sb)
        result = sb.Replace(vbCrLf, vbLf).ToString()
        Assert.AreEqual(result, expected)

    End Sub




    Function WriteProperties(code As String, properties As List(Of PropertyInfo), Optional inheritance As String = "", Optional importsList As StringBuilder = Nothing) As String

        Dim DefaultPropInfo As New PropertyInfo With {.IsKey = True}

        RecordParser.CurrentCompilation = GetCompilation(TestSourceCode)
        RecordParser.importsList = If(importsList, New StringBuilder())

        If inheritance <> "" Then
            RecordParser.AddInheritedPropertiesInfo(inheritance, properties, DefaultPropInfo)
        End If

        Dim basePropCount = properties.Count
        Dim params = SyntaxFactory.ParseParameterList(RecordParser.Lower(code, False)).ChildNodes()

        For Each param As ParameterSyntax In params
            Dim valueExpr = param.Default?.DescendantNodes?(0)

            If TypeOf valueExpr IsNot LambdaExpressionSyntax Then
                RecordParser.AddPropertyInfo(inheritance, New List(Of String), properties, basePropCount, DefaultPropInfo, param)
            End If
        Next

        Return RecordParser.WriteProperties(properties)
    End Function
End Class
