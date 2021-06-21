' Created By, Mohammad Hamdy Ghanem, 
' Egypt, 2021

Imports System.Text
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Class RecordParser
    Private Const StringQuote As String = "__QUOTE__"
    Public Shared CurrentCompilation As Compilation = Nothing
    Public Shared importsList As New StringBuilder()
    Private Shared TypeChars As New Dictionary(Of Char, String) From {
        {"%"c, "As Integer"},
        {"&"c, "As Long"},
        {"@"c, "As Decimal"},
        {"!"c, "As Single"},
        {"#"c, "As Double"},
        {"$"c, "As String"}
    }


    Public Shared Sub Debug(code As String)
        CurrentCompilation = VisualBasicCompilation.Create("Debug")
        Generate(Nothing, code)
    End Sub


    Public Shared Sub Generate(context? As GeneratorExecutionContext, code As String)
        Dim st = 0
        importsList.Clear()
        For Each node In SyntaxFactory.ParseSyntaxTree(code).GetRoot.ChildNodes
            If node.Kind <> SyntaxKind.ImportsStatement Then
                st = node.SpanStart
                Exit For
            End If

            For Each ImportsClause In CType(node, ImportsStatementSyntax).ImportsClauses
                importsList.AppendLine("Imports " & ImportsClause.ToString())
            Next
        Next

        If importsList.Length > 0 Then code = code.Substring(st)
        Parse(context, Lower(code, False))
    End Sub

    Public Shared Function Lower(code As String, isClassHeader As Boolean) As String
        Dim sb As New StringBuilder(code)
        Dim tokens = SyntaxFactory.ParseTokens(code).ToArray
        Dim Token = ""
        Dim PrevToken = tokens(tokens.Length - 1).Text.ToLower()

        For i = tokens.Length - 1 To 0 Step -1
            Token = PrevToken
            PrevToken = If(i = 0, "", tokens(i - 1).Text.ToLower())

            Select Case Token
                Case "key"
                    Dim B4PrevToken = If(i < 2, "", tokens(i - 2).Text).ToLower()
                    If (PrevToken = "readonly" OrElse PrevToken = "immitable") AndAlso B4PrevToken <> "[" AndAlso B4PrevToken <> "<" Then
                        Dim st = If(i = 0, 0, tokens(i - 1).SpanStart)
                        sb.Remove(st, tokens(i).Span.End - st + 1)
                        st = GetStartOfLineIfClassHeader(st, i, tokens)
                        sb.Insert(st, "<ReadOnlyKey>")
                        ' Skip ReadOnly
                        i -= 1
                        PrevToken = B4PrevToken
                    ElseIf PrevToken <> "[" AndAlso PrevToken <> "<" Then
                        Dim st = tokens(i).SpanStart
                        sb.Remove(st, tokens(i).Span.End - st + 1)
                        st = GetStartOfLineIfClassHeader(st, i, tokens)
                        sb.Insert(st, "<Key>")
                    End If

                Case "readonly", "immutable"
                    If PrevToken <> "[" AndAlso PrevToken <> "<" Then
                        Dim st = tokens(i).SpanStart
                        sb.Remove(st, tokens(i).Span.End - st + 1)
                        st = GetStartOfLineIfClassHeader(st, i, tokens)
                        sb.Insert(st, "<ReadOnly>")
                    End If

                Case "readonlykey", "immutablekey"
                    If PrevToken <> "[" AndAlso PrevToken <> "<" Then
                        Dim st = tokens(i).SpanStart
                        sb.Remove(st, tokens(i).Span.End - st + 1)
                        st = GetStartOfLineIfClassHeader(st, i, tokens)
                        sb.Insert(st, "<ReadOnlyKey>")
                    End If

                Case "record"
                    If isClassHeader AndAlso PrevToken <> "[" AndAlso PrevToken <> "<" Then
                        Dim st = tokens(i).SpanStart
                        sb.Remove(st, tokens(i).Span.End - st + 1)
                        sb.Insert(st, "Class ")
                        sb.Insert(0, "<Record>")
                        Exit For
                    End If
            End Select
        Next

        Return sb.ToString()
    End Function

    Private Shared Function GetStartOfLineIfClassHeader(st As Integer, pos As Integer, tokens() As SyntaxToken) As Integer
        Dim found = False
        For i = pos To tokens.Length - 1
            If tokens(i).Kind = SyntaxKind.StatementTerminatorToken Then
                Exit For
            ElseIf (tokens(i).Kind = SyntaxKind.ClassKeyword OrElse tokens(i).Kind = SyntaxKind.StructureKeyword) AndAlso
                (i = 0 OrElse tokens(i - 1).Kind <> SyntaxKind.OpenBraceToken) Then
                found = True
                Exit For
            End If
        Next

        If found Then
            For i = pos To 0 Step -1
                If tokens(i).Kind = SyntaxKind.StatementTerminatorToken Then
                    Return tokens(i).Span.End
                End If
            Next
            Return 0
        End If

        ' Not a class/structure
        Return st
    End Function

    Public Shared Sub Parse(context? As GeneratorExecutionContext, code As String)
        Dim tokens = SyntaxFactory.ParseTokens(code).ToArray


        ' Skip until class/structure token
        Dim st = 0
        Dim L = tokens.Length - 1
        For i = 0 To L
            If tokens(i).Kind = SyntaxKind.ClassKeyword OrElse
                     tokens(i).Kind = SyntaxKind.StructureKeyword OrElse
                     (tokens(i).Kind = SyntaxKind.IdentifierToken AndAlso tokens(i).Text.ToLower() = "record" AndAlso (i = 0 OrElse tokens(i - 1).Kind <> SyntaxKind.LessThanToken)) Then
                st = i + 1
                Exit For
            End If
        Next

        For i = st To L
            Dim token = tokens(i)

            If token.Kind = SyntaxKind.OpenParenToken Then
                If i < L AndAlso tokens(i + 1).Kind = SyntaxKind.OfKeyword Then ' Skip tokens untile reaching "("
                    Dim j = i + 1
                    Do While j < L
                        token = tokens(j)
                        If token.Kind = SyntaxKind.CloseParenToken Then Continue For
                        j += 1
                    Loop
                End If

                Dim definition = ""
                Try
                    definition = Lower(code.Substring(0, token.SpanStart).TrimStart(ChrW(10), ChrW(13)), True)
                Catch ex As Exception
                    Throw New Exception("This record doesn't have a valid definition:" & vbCrLf & definition)
                End Try

                Dim members As ParameterListSyntax
                Try
                    members = SyntaxFactory.ParseParameterList(code.Substring(token.SpanStart))
                Catch ex As Exception
                    Throw New Exception("This record doesn't have a valid memberlist:" & vbCrLf & definition)
                End Try

                Dim pos = token.SpanStart + members.Span.Length
                Dim inheritance = ""

                If pos < code.Length Then
                    Dim nextNode = SyntaxFactory.ParseSyntaxTree(code.Substring(pos)).GetRoot().ChildNodes(0)
                    If nextNode?.Kind = SyntaxKind.InheritsStatement Then
                        inheritance = nextNode.ToString()
                        pos += nextNode.Span.Length + 1
                    End If
                End If

                GenerateRecord(context, inheritance, definition, members)
                If pos < code.Length Then Parse(context, code.Substring(pos))
                Return
            End If
        Next
    End Sub

    Private Shared Sub GenerateRecord(
                       context? As GeneratorExecutionContext,
                       inheritance As String,
                       definition As String,
                       paramList As ParameterListSyntax)

        Dim classStatement = SyntaxFactory.ParseSyntaxTree(definition).GetRoot().ChildNodes(0).ChildNodes(0)
        Dim tokens = classStatement.ChildNodesAndTokens.ToArray()
        Dim DefaultPropInfo As New PropertyInfo

        Dim isClass = (From token In tokens
                       Where token.Kind = SyntaxKind.ClassKeyword).Count > 0

        Dim attributeLists As SyntaxList(Of AttributeListSyntax)

        If isClass Then
            attributeLists = TryCast(classStatement, ClassStatementSyntax)?.AttributeLists
        Else
            attributeLists = TryCast(classStatement, StructureStatementSyntax)?.AttributeLists
        End If

        If attributeLists.Count > 0 Then
            DefaultPropInfo = GetPropertyInfo(attributeLists)
            Dim st = attributeLists(attributeLists.Count - 1).Span.End
            definition = DefaultPropInfo.Attrs & vbCrLf & definition.Substring(st).TrimStart(ChrW(10), ChrW(13))
        End If

        Dim result = (From token In tokens
                      Where token.Kind = SyntaxKind.IdentifierToken)

        If Not result.Any Then
            Throw New Exception("This record doesn't have a name:" & vbCrLf & definition)
        End If

        Dim className = result.First.ToString()
        Dim typeParams = ""
        Dim typeParamLists = classStatement.ChildNodes.OfType(Of TypeParameterListSyntax)
        If typeParamLists.Any Then
            Dim typeParamList = typeParamLists.First
            Dim addSep = False
            typeParams = "(Of "
            For Each t In typeParamList.Parameters
                If addSep Then
                    typeParams += ", "
                Else
                    addSep = True
                End If
                typeParams += t.Identifier.Text
            Next
            typeParams += ")"
        End If

        Dim Properties As New List(Of PropertyInfo)
        Dim Methods As New List(Of String)

        If inheritance <> "" Then AddInheritedPropertiesInfo(inheritance, Properties, DefaultPropInfo)
        Dim basePropCount = Properties.Count

        For Each member As ParameterSyntax In paramList.ChildNodes
            Dim valueExpr = member.Default?.DescendantNodes?(0)

            If TypeOf valueExpr Is LambdaExpressionSyntax Then
                LambdaToMethod(inheritance, Methods, Properties, member, valueExpr)
            Else
                AddPropertyInfo(inheritance, Methods, Properties, basePropCount, DefaultPropInfo, member)
            End If
        Next

        ' ------------------------Generate the record class/struct ----------------------------

        Dim record As New StringBuilder(
"Option Explicit On
Option Strict Off
Option Infer On
Option Compare Binary

")

        record.Append(importsList.ToString())

        If importsList.ToString().ToLower().IndexOf("imports system.componentmodel" & vbCrLf) = -1 Then
            record.AppendLine("Imports System.ComponentModel")
            record.AppendLine()
        End If

        record.AppendLine()

        record.AppendLine(definition)
        record.AppendLine("    " & inheritance)
        record.AppendLine()
        record.AppendLine(WriteProperties(Properties))
        WriteConstructor(Properties, record)
        record.AppendLine(WriteMethods(Methods))
        WriteWith(className, typeParams, Properties, record)
        WriteWithProps(className, typeParams, Properties, record)
        WriteClone(className, typeParams, record)
        WriteToString(className, Properties, record)
        WriteEquals(className, typeParams, Properties, record)
        WriteEqualityOps(className, typeParams, record)
        WriteTuplesOps(className, typeParams, Properties, record)
        record.Append(If(isClass, "End Class", "End Structure"))

        If context.HasValue Then
            Dim rec = record.ToString()
            context.Value.AddSource(className & "Record", SourceText.From(rec, Encoding.UTF8))
            Dim syntx = SyntaxFactory.ParseSyntaxTree(rec)
            CurrentCompilation = CurrentCompilation.AddSyntaxTrees(syntx)
        End If

    End Sub

    Private Shared Sub LambdaToMethod(
                                     inheritance As String,
                                     methods As List(Of String),
                                     properties As List(Of PropertyInfo),
                                     param As ParameterSyntax,
                                     lanbdaExpr As LambdaExpressionSyntax)

        Dim Header = lanbdaExpr.SubOrFunctionHeader
        Dim MethodType = Header.DeclarationKeyword.Text
        Dim isSub = MethodType.ToLower() = "sub"
        Dim AsClause = ""
        If Not isSub Then
            AsClause = If(Header.AsClause?.ToString(), InferType(inheritance, methods, properties, param.Default.ToString()).Type)
        End If
        Dim lambdaBody As String = ""
        If TypeOf lanbdaExpr Is SingleLineLambdaExpressionSyntax Then
            lambdaBody = If(isSub, "", "Return ") & CType(lanbdaExpr, SingleLineLambdaExpressionSyntax).Body.ToString()
        Else
            For Each statement In CType(lanbdaExpr, MultiLineLambdaExpressionSyntax).Statements
                lambdaBody &= statement.ToString() & vbCrLf
            Next
            lambdaBody = lambdaBody.Trim({ChrW(10), ChrW(13)})
        End If

        methods.Add(
$"    Public {MethodType} { param.Identifier}{Header.ParameterList} {AsClause}
        {lambdaBody}
    End {MethodType}")
    End Sub

    Public Shared Sub AddPropertyInfo(
                                      inheritance As String,
                                      methods As List(Of String),
                                      properties As List(Of PropertyInfo),
                                      basePropCount As Integer,
                                      DefaultPropInfo As PropertyInfo,
                                      param As ParameterSyntax
                                  )

        Dim AccessAttr = param.AttributeLists
        Dim prop As PropertyInfo = Nothing
        If AccessAttr.Count > 0 Then prop = GetPropertyInfo(AccessAttr)

        Dim belongsToType = ""
        Dim id = param.Identifier
        prop.Name = id.Identifier.Text
        If prop.Name = "" Then Return

        ' Handle type chars
        Dim n = prop.Name.Length - 1
        Dim c = prop.Name(n)
        Dim typeOfChar As String = ""
        If TypeChars.ContainsKey(c) Then
            prop.Name = prop.Name.Substring(0, n)
            typeOfChar = TypeChars(c)
        End If

        ' I allow ti use multiple symbols after the identifier
        If id.Nullable.Text = "?" Then
            belongsToType = "?"
        End If

        If id.ArrayBounds IsNot Nothing Then
            belongsToType &= "()"
        End If

        If id.ArrayRankSpecifiers.Count > 0 Then
            For Each rank In id.ArrayRankSpecifiers
                belongsToType &= rank.ToString
            Next
        End If


        If inheritance <> "" Then
            For i = 0 To basePropCount - 1
                Dim baseProp = properties(i)
                If baseProp.Name.ToLower() = prop.Name.ToLower() Then
                    prop.IsReadOnly = baseProp.IsReadOnly Or prop.IsReadOnly
                    prop.IsKey = baseProp.IsKey Or prop.IsKey
                    prop.Type = baseProp.Type
                    If param.Default Is Nothing Then
                        prop.DefaultValue = baseProp.DefaultValue
                        prop.LiteralDefVal = baseProp.LiteralDefVal
                    Else
                        prop.DefaultValue = param.Default.ToString()
                        prop.LiteralDefVal = TypeOf param.Default.DescendantNodes?(0) Is LiteralExpressionSyntax
                    End If

                    If prop.DefaultValue = "" Then SetDefValue(prop)
                    prop.InheritanceModifier = baseProp.InheritanceModifier
                    properties(i) = prop
                    Exit Sub
                End If
            Next
        End If

        prop.IsReadOnly = prop.IsReadOnly Or DefaultPropInfo.IsReadOnly
        prop.IsKey = prop.IsKey Or DefaultPropInfo.IsKey

        prop.Type = If(param.AsClause?.ToString(), typeOfChar)

        If belongsToType <> "" AndAlso Not prop.Type.EndsWith(belongsToType) Then prop.Type = prop.Type & belongsToType

        prop.DefaultValue = param.Default?.ToString()

        If prop.DefaultValue = "" Then
            If prop.Type = "" Then
                prop.Type = "As Object"
            Else
                SetDefValue(prop)
            End If
        Else
            prop.LiteralDefVal = TypeOf param.Default.DescendantNodes?(0) Is LiteralExpressionSyntax
            If prop.Type = "" OrElse Not prop.LiteralDefVal Then
                Dim result = InferType(inheritance, methods, properties, "= Function() " & prop.DefaultValue.Substring(1))
                prop.LiteralDefVal = prop.LiteralDefVal OrElse result.IsConst
                If prop.Type = "" Then prop.Type = result.Type
            End If
        End If

        prop.Type = prop.Type.Substring(2).Trim()
        properties.Add(prop)
    End Sub

    Private Shared Sub SetDefValue(ByRef prop As PropertyInfo)
        Dim t = prop.Type.Substring(2).Trim()
        Select Case t.Trim("?"c).ToLower()
            Case "byte", "sbyte", "short", "ushort", "integer", "uinteger", "long", "ulong", "single", "double", "decimal"
                prop.LiteralDefVal = True
                prop.DefaultValue = "= 0"
            Case "boolean"
                prop.LiteralDefVal = True
                prop.DefaultValue = "= False"
            Case "char"
                prop.LiteralDefVal = True
                prop.DefaultValue = "= vbNullChar"
            Case "string"
                prop.LiteralDefVal = True
                prop.DefaultValue = "= """""
            Case "date", "datetime"
                prop.LiteralDefVal = True
                prop.DefaultValue = "= #1/1/0001#"
        End Select
        If t.EndsWith("?") Then prop.DefaultValue = "= Nothing"
    End Sub

    Public Shared Function WriteProperties(Properties As List(Of PropertyInfo)) As String
        Dim props As New StringBuilder
        For Each p In Properties
            If p.IsKey Then props.AppendLine("   <Key>")

            If p.DefaultValue <> "" Then
                props.AppendLine($"   <DefaultValue(""{If(p.LiteralDefVal, "1", "0")}{ p.DefaultValue.Replace("""", StringQuote)}"")>")
            End If

            If p.Attrs <> "" Then props.AppendLine("   " & p.Attrs)

            props.Append("   Public ")
            If p.InheritanceModifier <> "" Then props.Append(p.InheritanceModifier & " ")
            If p.IsReadOnly = True Then props.Append("ReadOnly ")
            props.Append($"Property [{p.Name}] As {p.Type}")
            props.AppendLine(vbCrLf)
        Next
        props.AppendLine()
        Return props.ToString()
    End Function

    Public Shared Sub WriteConstructor(Properties As List(Of PropertyInfo), record As StringBuilder)
        Dim params As New StringBuilder
        Dim body As New StringBuilder
        Dim addSep = False

        record.AppendLine("    Public Sub New(")

        For Each p In Properties
            If addSep Then
                params.Append("," & vbCrLf)
            Else
                addSep = True
            End If

            If p.LiteralDefVal Then
                params.Append($"                Optional [{p.camelCaseName}] As {p.Type} {p.DefaultValue}")
                body.AppendLine($"        Me.{p.Name} = [{p.camelCaseName}]")
            Else
                params.Append($"                Optional [{p.camelCaseName}] As [Optional](Of {p.Type}) = Nothing")
                body.AppendLine(
$"        If [{p.camelCaseName}].HasValue
            Me.{p.Name} = [{p.camelCaseName}].Value
        Else
            Me.{p.Name} {If(p.DefaultValue = "", "= Nothing", p.DefaultValue)}
        End If
")

            End If
        Next
        record.Append(params.ToString())
        record.AppendLine(vbCrLf & "            )")
        record.AppendLine()
        record.Append(body.ToString())
        record.AppendLine("    End Sub")
        record.AppendLine()
    End Sub

    Private Shared Function WriteMethods(Methods As List(Of String)) As String
        Dim sb As New StringBuilder
        For Each method In Methods
            sb.AppendLine(method)
            sb.AppendLine()
        Next
        Return sb.ToString
    End Function

    Private Shared Sub WriteTuplesOps(className As String, typeParams As String, Properties As List(Of PropertyInfo), record As StringBuilder)
        For n = 1 To Properties.Count - 1
            Dim body As New StringBuilder
            Dim addSep = False
            record.Append($"    Public Shared Widening Operator CType(anotherRecord As {className}{typeParams}) As (")
            For i = 0 To n
                Dim p = Properties(i)
                If addSep Then
                    record.Append(", ")
                    body.Append(", ")
                Else
                    addSep = True
                End If
                record.Append($"[{p.Name}] As {p.Type}")
                body.Append($"anotherRecord.{p.Name}")
            Next
            record.AppendLine(")")
            record.Append("        Return (")
            record.Append(body.ToString())
            record.AppendLine(")")
            record.AppendLine("    End Operator")
            record.AppendLine()

            record.Append($"    Public Shared Widening Operator CType(fromTuple As (")
            Dim methodType = "Operator"
            body.Clear()
            addSep = False

            For i = 0 To n
                Dim p = Properties(i)
                If addSep Then
                    record.Append(", ")
                    body.Append(", ")
                Else
                    addSep = True
                End If

                record.Append($"[{p.Name}] As {p.Type}")
                body.Append($"fromTuple.{p.Name}")
            Next
            record.AppendLine($")) As {className}{typeParams}")
            record.Append($"        Return new {className}{typeParams}(")
            record.Append(body.ToString)
            record.AppendLine(")")
            record.AppendLine("    End Operator")
            record.AppendLine()
        Next
    End Sub

    Private Shared Sub WriteEqualityOps(className As String, typeParams As String, record As StringBuilder)
        record.AppendLine(
$"    Public Shared Operator =(FirstRecord As {className}{typeParams}, secondRecord As {className}{typeParams}) As Boolean
        Return FirstRecord.Equals(secondRecord)
    End Operator

    Public Shared Operator <>(FirstRecord As {className}{typeParams}, secondRecord As {className}{typeParams}) As Boolean
        Return Not FirstRecord.Equals(secondRecord)
    End Operator")
        record.AppendLine()
    End Sub

    Public Shared Sub WriteEquals(className As String, typeParams As String, Properties As List(Of PropertyInfo), record As StringBuilder)
        Dim keys = From p In Properties
                   Where p.IsKey

        If keys.Any Then
            record.AppendLine(
 $"    Public Overrides Function Equals(anotherObject) As Boolean
            Dim anotherRecord = TryCast(anotherObject, {className}{typeParams})
            If anotherRecord Is Nothing Then Return False
            Return Equals(anotherRecord)
    End Function")
            record.AppendLine()

            record.AppendLine($"    Public Overloads Function Equals(anotherRecord As {className}{typeParams}) As Boolean")
            For Each p In keys
                record.AppendLine($"        If Not _{p.Name}.Equals(anotherRecord._{p.Name}) Then Return False")
            Next
            record.AppendLine("        Return True")
            record.AppendLine("    End Function")
        End If
        record.AppendLine()
    End Sub

    Private Shared Sub WriteToString(className As String, Properties As List(Of PropertyInfo), record As StringBuilder)
        record.AppendLine(
$"    Public Overrides Function ToString() As String
        Return ""{className}"" & "" {{ "" & RecordHelper.GetPropertyValuePairs(Me) & "" }}""
    End Function")

        record.AppendLine()
    End Sub

    Private Shared Sub WriteClone(className As String, typeParams As String, record As StringBuilder)
        record.AppendLine(
$"    Public Function Clone() As {className}{typeParams}
        Return Me.With()
    End Function")
        record.AppendLine()
    End Sub

    Private Shared Sub WriteWithProps(className As String, typeParams As String, Properties As List(Of PropertyInfo), record As StringBuilder)
        For Each p In Properties
            record.AppendLine($"    Public Function With{p.Name}([{p.camelCaseName}] As {p.Type}) As {className}{typeParams}")
            record.AppendLine($"        Return Me.With([{p.camelCaseName}]:=[{p.camelCaseName}])")
            record.AppendLine("    End Function")
            record.AppendLine()
        Next
    End Sub

    Public Shared Sub WriteWith(className As String, typeParams As String, Properties As List(Of PropertyInfo), record As StringBuilder)
        Dim params As New StringBuilder
        Dim body As New StringBuilder
        Dim addSep = False

        For Each p In Properties
            If addSep Then
                params.Append("," & vbCrLf)
                body.Append("," & vbCrLf)
            Else
                addSep = True
            End If

            If p.LiteralDefVal AndAlso p.Type.ToLower() <> "string" Then
                Dim type = If(p.Type.EndsWith("?"), $"[Optional](Of {p.Type})", p.Type & "?")
                params.Append($"                Optional [{p.camelCaseName}] As {type} = Nothing")
            Else
                params.Append($"                Optional [{p.camelCaseName}] As [Optional](Of {p.Type}) = Nothing")
            End If

            body.Append($"            If ([{p.camelCaseName}].HasValue, [{p.camelCaseName}].Value, Me.{p.Name})")
        Next

        record.AppendLine("    Public Function [With](")
        record.Append(params)
        record.AppendLine(vbCrLf & $"            ) As {className}{typeParams}")
        record.AppendLine()

        record.AppendLine($"        Return  New {className}{typeParams}(")
        record.AppendLine(body.ToString())
        record.AppendLine("        )")
        record.AppendLine("    End Function")
        record.AppendLine()
    End Sub

    Public Shared Sub AddInheritedPropertiesInfo(
                                     inheritance As String,
                                     properties As List(Of PropertyInfo),
                                     defaultPropInfo As PropertyInfo)

        Dim code = $"
{importsList}
Class Test_00000000000000
   {inheritance}
End Class
"

        Dim syntaxTree = SyntaxFactory.ParseSyntaxTree(code)
        Dim comp = CurrentCompilation.AddSyntaxTrees(syntaxTree)
        Dim sem = comp.GetSemanticModel(syntaxTree)
        Dim inhertsStatement = syntaxTree.GetRoot().DescendantNodes().OfType(Of InheritsStatementSyntax)().First

        Try
            Dim baseClass = sem.GetTypeInfo(inhertsStatement.ChildNodes(0)).Type
            For Each m In baseClass.GetMembers()
                Dim prop = TryCast(m, IPropertySymbol)
                If prop IsNot Nothing AndAlso prop.DeclaredAccessibility = Accessibility.Public Then
                    Dim attrs = prop.GetAttributes()
                    Dim defValue = ""
                    Dim isKey = defaultPropInfo.IsKey
                    For Each attr In attrs
                        If attr.AttributeClass.Name = "DefaultValueAttribute" Then
                            defValue = attr.ConstructorArguments(0).Value.ToString().Replace(StringQuote, """")
                        ElseIf attr.AttributeClass.Name = "Key" Then
                            isKey = True
                        End If
                    Next

                    Dim propInfo = New PropertyInfo() With {
                        .Name = prop.Name,
                        .Type = "As " & prop.Type.ToString(),
                        .LiteralDefVal = If(defValue = "", False, defValue(0) = "1"c),
                        .DefaultValue = If(defValue = "", "", defValue.Substring(1)),
                        .IsKey = isKey,
                        .IsReadOnly = prop.IsReadOnly OrElse defaultPropInfo.IsReadOnly,
                        .InheritanceModifier = If(prop.IsMustOverride OrElse prop.IsOverridable, "Overrides", "Shadows")
                    }

                    If propInfo.DefaultValue = "" Then SetDefValue(propInfo)
                    propInfo.Type = propInfo.Type.Substring(2).Trim()

                    properties.Add(propInfo)

                End If
            Next
        Catch
        End Try
    End Sub

    Private Shared Function InferType(
                                     inheritance As String,
                                     methods As List(Of String),
                                     properties As List(Of PropertyInfo),
                                     defaultValue As String
                         ) As (IsConst As Boolean, Type As String)

        Dim code = $"
{importsList}
Class Test_00000000000001
   {inheritance}
    Sub Foo
        Dim a {defaultValue}
    End Sub

{WriteProperties(properties)}
{WriteMethods(methods)}
End Class
"

        Dim syntaxTree = SyntaxFactory.ParseSyntaxTree(code)
        Dim comp = CurrentCompilation.AddSyntaxTrees(syntaxTree)
        Dim sem = comp.GetSemanticModel(syntaxTree)
        Dim variableDeclaration = syntaxTree.GetRoot().DescendantNodes().OfType(Of LocalDeclarationStatementSyntax)().First

        Dim isConst = False
        Try
            Dim valueSymbol = sem.GetSymbolInfo(CType(variableDeclaration.Declarators(0).Initializer.Value, SingleLineLambdaExpressionSyntax).Body).Symbol
            isConst = valueSymbol IsNot Nothing AndAlso (TryCast(valueSymbol, IFieldSymbol)?.IsConst OrElse TryCast(valueSymbol, ILocalSymbol)?.IsConst)
        Catch

        End Try

        Try
            Dim typeSymbol = CType(CType(sem.GetDeclaredSymbol(variableDeclaration.ChildNodes(0).ChildNodes(0)), ILocalSymbol).Type, INamedTypeSymbol).DelegateInvokeMethod.ReturnType
            If typeSymbol Is Nothing Then Return (isConst, "As Object")
            Dim typeName = typeSymbol.ToDisplayString()
            Return (isConst, "As " & If(typeName = "?" OrElse typeName.StartsWith("<anonymous type: "), "Object", typeName))
        Catch
        End Try

        Return (isConst, "As Object")

    End Function

    Private Shared Function GetPropertyInfo(attributeLists As SyntaxList(Of AttributeListSyntax)) As PropertyInfo
        Dim propInfo As New PropertyInfo
        Dim attrs As New StringBuilder

        For Each attrList In attributeLists
            Select Case attrList.Attributes(0).ToString().ToLower()
                Case "key"
                    propInfo.IsKey = True

                Case "readonly", "immutable"
                    propInfo.IsReadOnly = True

                Case "readonlykey", "immutablekey", "record"
                    propInfo.IsKey = True
                    propInfo.IsReadOnly = True
                Case Else
                    attrs.Append(attrList.ToString())
            End Select
        Next

        propInfo.Attrs = attrs.ToString()
        Return propInfo
    End Function

End Class


