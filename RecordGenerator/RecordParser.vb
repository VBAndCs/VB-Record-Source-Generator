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
    Public Shared RecordNamespace As String

    Private Shared TypeChars As New Dictionary(Of Char, String) From {
        {"%"c, "As Integer"},
        {"&"c, "As Long"},
        {"@"c, "As Decimal"},
        {"!"c, "As Single"},
        {"#"c, "As Double"},
        {"$"c, "As String"}
    }

    Private Shared DefaultNamespaces() As String = {
         "Microsoft.VisualBasic",
         "System",
         "System.LinQ",
         "System.Collections",
         "System.Collections.Generic"
    }

    Public Shared Sub Debug(code As String)
        CurrentCompilation = VisualBasicCompilation.Create("Debug")
        Generate(Nothing, code)
    End Sub


    Private Shared Comment As String

    Public Shared Sub Generate(context? As GeneratorExecutionContext, code As String)
        Dim st = 0
        importsList.Clear()

        Dim c = code.TrimStart({ChrW(10), ChrW(13), ChrW(9), " "c})
        If c.StartsWith("namespace", StringComparison.OrdinalIgnoreCase) Then
            Dim en = c.IndexOfAny({ChrW(10), ChrW(13)})
            If en = -1 Then en = c.Length - 1
            RecordNamespace = c.Substring(9, en - 8).Trim
            code = c.Substring(en + 1)
        Else
            RecordNamespace = ""
        End If

        code = ParseComment(code)

        For Each node In SyntaxFactory.ParseSyntaxTree(code).GetRoot.ChildNodes
            If node.Kind <> SyntaxKind.ImportsStatement Then Exit For
            st = node.SpanStart + node.Span.Length

            Dim namespaces = CType(node, ImportsStatementSyntax).ImportsClauses
            For Each importsClause In namespaces
                Dim ns = importsClause.ToString()
                Dim exists = (
                    From dns In DefaultNamespaces
                    Where dns.Equals(ns, StringComparison.OrdinalIgnoreCase)
                ).Any

                If Not exists Then importsList.AppendLine("Imports " & ns)
            Next
        Next

        If st > 0 Then
            Comment = SyntaxFactory.ParseLeadingTrivia(code, st).ToFullString()
            If Comment.Trim() = "" Then
                Comment = ""
                code = code.Substring(st)
            Else
                code = code.Substring(st + Comment.Length)
            End If
        End If

        Parse(context, Lower(code, False))
    End Sub

    Private Shared Function ParseComment(code As String) As String
        Comment = SyntaxFactory.ParseLeadingTrivia(code).ToFullString()
        If Comment.Trim() = "" Then
            Comment = ""
        Else
            code = code.Substring(Comment.Length)
        End If

        Return code
    End Function

    Public Shared Function Lower(code As String, isClassHeader As Boolean) As String
        Dim sb As New StringBuilder(code)
        Dim tokens = SyntaxFactory.ParseTokens(code).ToArray
        Dim token = ""
        Dim prevToken = tokens(tokens.Length - 1).Text.ToLower()

        For i = tokens.Length - 1 To 0 Step -1
            token = prevToken
            prevToken = If(i = 0, "", tokens(i - 1).Text.ToLower())

            Select Case token
                Case ">"
                    ' Lower: Fn(n) => n + 1 
                    ' To:       Function(n) n + 1
                    If prevToken = "=" Then
                        For n = i - 2 To 0 Step -1
                            If tokens(n).ToString().ToLower() = "fn" AndAlso tokens(n + 1).ToString() = "(" Then
                                Dim st = tokens(i - 1).SpanStart
                                sb.Remove(st, tokens(i).Span.End - st + 1)

                                st = tokens(n).SpanStart
                                sb.Remove(st, 2)
                                sb.Insert(st, "Function")
                                Exit For
                            End If
                        Next
                    End If

                Case "new"
                    ' Lower: As New Type
                    ' to:       = New Type
                    If prevToken = "as" Then
                        Dim kind = tokens(i + 1).Kind
                        If kind = SyntaxKind.IdentifierToken OrElse kind = SyntaxKind.IdentifierName Then
                            Dim st = tokens(i - 1).SpanStart
                            sb.Remove(st, 2)
                            sb.Insert(st, "=")
                        End If
                    End If

                Case "key"
                    Dim B4PrevToken = If(i < 2, "", tokens(i - 2).Text).ToLower()
                    If (prevToken = "readonly" OrElse prevToken = "immitable") AndAlso B4PrevToken <> "[" AndAlso B4PrevToken <> "<" Then
                        Dim st = If(i = 0, 0, tokens(i - 1).SpanStart)
                        sb.Remove(st, Math.Min(tokens(i).Span.End + 1, sb.Length) - st)
                        st = GetStartOfLineIfClassHeader(st, i, tokens)
                        sb.Insert(st, "<ReadOnlyKey>")
                        ' Skip ReadOnly
                        i -= 1
                        prevToken = B4PrevToken

                    ElseIf prevToken <> "[" AndAlso prevToken <> "<" Then
                        Dim st = tokens(i).SpanStart
                        sb.Remove(st, Math.Min(tokens(i).Span.End + 1, sb.Length) - st)
                        st = GetStartOfLineIfClassHeader(st, i, tokens)
                        sb.Insert(st, "<Key>")
                    End If

                Case "readonly", "immutable"
                    If prevToken <> "[" AndAlso prevToken <> "<" Then
                        Dim st = tokens(i).SpanStart
                        sb.Remove(st, Math.Min(tokens(i).Span.End + 1, sb.Length) - st)
                        st = GetStartOfLineIfClassHeader(st, i, tokens)
                        sb.Insert(st, "<ReadOnly>")
                    End If

                Case "readonlykey", "immutablekey"
                    If prevToken <> "[" AndAlso prevToken <> "<" Then
                        Dim st = tokens(i).SpanStart
                        sb.Remove(st, Math.Min(tokens(i).Span.End + 1, sb.Length) - st)
                        st = GetStartOfLineIfClassHeader(st, i, tokens)
                        sb.Insert(st, "<ReadOnlyKey>")
                    End If

                Case "record"
                    If isClassHeader AndAlso prevToken <> "[" AndAlso prevToken <> "<" Then
                        Dim st = tokens(i).SpanStart
                        sb.Remove(st, Math.Min(tokens(i).Span.End + 1, sb.Length) - st)
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
                If i < L AndAlso tokens(i + 1).Kind = SyntaxKind.OfKeyword Then ' Skip tokens untile reaching ")"
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
                    Dim lst = code.Substring(token.SpanStart)
                    members = SyntaxFactory.ParseParameterList(lst)
                Catch ex As Exception
                    Throw New Exception("This record doesn't have a valid memberlist:" & vbCrLf & definition & members?.ToString())
                End Try

                If Not members.ToString().EndsWith(")") Then
                    Throw New Exception("This record doesn't have a valid memberlist. Check For a missing comma:" & vbCrLf & definition & members.ToString())
                End If

                Dim pos = token.SpanStart + members.Span.Length

                ' Parse Inheritance and Implementation
                Dim inheritance = ""
                Dim implementaion = ""

                If pos < code.Length Then
                    ' Inherits Foo, Implements IFoo
                    ' or  Implements IFoo, Inherits Foo
                    Dim result = ParseInheritance(code.Substring(pos))
                    If result.Pos > -1 Then
                        inheritance = result.InheritsStatement
                        implementaion = result.ImplementsStatement
                        pos += result.Pos
                    End If
                End If

                GenerateRecord(context, inheritance, implementaion, definition, members)
                If pos < code.Length Then
                    code = ParseComment(code.Substring(pos))
                    Parse(context, code)
                End If
                Return
            End If

        Next
    End Sub

    Private Shared Function ParseInheritance(
                  code As String
               ) As (InheritsStatement$, ImplementsStatement$, Pos%)

        Dim inheritsStatement = ""
        Dim implementsStatement = ""
        Dim pos = -1
        Dim Offset = 0

LineAgain:
        Dim nodes = SyntaxFactory.ParseSyntaxTree(code).GetRoot().ChildNodes

        Dim node = nodes(0)
        If node IsNot Nothing Then
            If node.Kind = SyntaxKind.InheritsStatement Then
                inheritsStatement = node.ToString()
                pos = node.Span.End
                If inheritsStatement.Trim.EndsWith(",") Then
                    inheritsStatement = inheritsStatement.TrimEnd(" "c, ","c)
                    code = code.Substring(pos)
                    If Offset = 0 Then
                        Offset = pos
                        GoTo LineAgain
                    End If
                End If

            ElseIf node.Kind = SyntaxKind.ImplementsStatement Then
                pos = node.Span.End
                implementsStatement = node.ToString()
                If implementsStatement.Trim.EndsWith(",") Then
                    implementsStatement = implementsStatement.TrimEnd(" "c, ","c)
                    code = code.Substring(pos)
                    If Offset = 0 Then
                        Offset = pos
                        GoTo LineAgain
                    End If
                End If
            End If
        End If

        Return (inheritsStatement, implementsStatement, pos + Offset)
    End Function

    Private Shared Sub GenerateRecord(
                       context? As GeneratorExecutionContext,
                       inheritance As String,
                       implementaion As String,
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
        If className = "" Then
            Throw New Exception("This record doesn't have a name:" & vbCrLf & definition)
        End If

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

        If inheritance <> "" Then
            AddInheritedPropertiesInfo(inheritance, Properties, DefaultPropInfo)
        End If

        Dim basePropCount = Properties.Count

        Dim interfaceSymbol = GetInterfaceInfo(implementaion)

        For Each member As ParameterSyntax In paramList.ChildNodes
            Dim valueExpr = member.Default?.DescendantNodes?(0)

            If member.AsClause Is Nothing AndAlso TypeOf valueExpr Is LambdaExpressionSyntax Then
                LambdaToMethod(inheritance, Methods, Properties, member, valueExpr, interfaceSymbol, implementaion)
            Else
                AddPropertyInfo(inheritance, Methods, Properties, basePropCount, DefaultPropInfo, member)
            End If
        Next

        If implementaion <> "" Then
            ImplementProperties(Properties, InterfaceSymbol, implementaion)
        End If

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

        If RecordNamespace <> "" Then
            record.AppendLine("Namespace " & RecordNamespace)
        End If

        record.AppendLine()
        If Comment <> "" Then record.Append(Comment)

        record.AppendLine(definition)
        record.AppendLine("    " & inheritance)
        If implementaion <> "" Then record.AppendLine("    " & implementaion)

        record.AppendLine()
        If Properties.Count > 0 Then
            record.AppendLine(WriteProperties(Properties))
            WriteConstructor(Properties, record)
        End If

        record.AppendLine(WriteMethods(Methods))

        If Properties.Count > 0 Then
            WriteWith(className, typeParams, Properties, record)
            WriteWithProps(className, typeParams, Properties, record)
            WriteClone(className, typeParams, record)
            WriteToString(className, Properties, record)
            WriteEquals(className, typeParams, Properties, record, isClass)
            WriteGetHashCode(Properties, record)
            WriteEqualityOps(className, typeParams, record)
            WriteTuplesOps(className, typeParams, Properties, record)
        End If

        record.Append(If(isClass, "End Class", "End Structure"))

        If RecordNamespace <> "" Then
            record.AppendLine(vbCrLf)
            record.AppendLine("End Namespace")
        End If

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
                                     lanbdaExpr As LambdaExpressionSyntax,
                                     interfaceSymbol As ITypeSymbol,
                                     implementation As String)

        Dim header = lanbdaExpr.SubOrFunctionHeader
        Dim methodType = header.DeclarationKeyword.Text
        Dim isSub = methodType.ToLower() = "sub"
        Dim AsClause = ""
        If Not isSub Then
            AsClause = If(header.AsClause?.ToString(), InferType(inheritance, methods, properties, param.Default.ToString()).Type)
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

        Dim methodName = param.Identifier.ToString()
        Dim params = header.ParameterList
        Dim _implements = ""

        If implementation <> "" Then
            Dim methodSymbols = interfaceSymbol.GetMembers().
            OfType(Of IMethodSymbol)().
            Where(Function(m) m.CanBeReferencedByName AndAlso Not m.IsGenericMethod).
            ToList()

            Dim nodes = params.ChildNodes

            If methodSymbols.Count > 0 Then
                Dim matchingMethods =
                      From m In methodSymbols
                      Where methodName.ToLower = m.Name.ToLower AndAlso
                            (isSub = m.ReturnsVoid OrElse m.ReturnsVoid) AndAlso
                             nodes.Count = m.Parameters.Count

                If matchingMethods.Any Then
                    Dim n = nodes.Count - 1
                    Dim paramTypes(n) As String
                    Dim argList = ""

                    For i = 0 To n
                        Dim p = CType(nodes(i), ParameterSyntax)
                        Dim delLastChr = False
                        Dim t = p.AsClause?.ToString()

                        If t = "" Then
                            Dim paramName = p.ToString().TrimEnd("?"c)
                            Dim c = paramName(paramName.Length - 1)
                            If TypeChars.ContainsKey(c) Then
                                delLastChr = True
                                paramTypes(i) = TypeChars(c).Substring(3).ToLower()
                            Else
                                paramTypes(i) = "object"
                            End If
                        Else
                            paramTypes(i) = t.Substring(2).ToLower()
                        End If

                        Dim name = p.Identifier.ToString()
                        If delLastChr Then
                            argList &= name.Substring(0, name.Length - 1).TrimEnd("?"c) & ", "
                        Else
                            argList &= name.TrimEnd("?"c) & ", "
                        End If
                    Next

                    For Each m In matchingMethods
                        Dim ok = True
                        For i = 0 To n
                            If paramTypes(i) <> m.Parameters(i).Type.ToString().ToLower() Then
                                ok = False
                                Continue For
                            End If
                        Next

                        If ok Then
                            If m.ReturnsVoid AndAlso Not isSub Then
                                If argList <> "" Then argList = argList.Substring(0, argList.Length - 2)

                                methods.Add(
$"    Private Sub {interfaceSymbol.Name}_{methodName}{params} {implementation}.{m.Name}
        {methodName}({argList})
    End Sub")

                            Else
                                _implements = $" {implementation}.{m.Name}"
                            End If

                            Exit For
                        End If
                    Next
                End If

            End If
        End If


        methods.Add(
$"    Public {methodType} {methodName}{params} {AsClause}{_implements}
        {lambdaBody.Replace(vbCrLf, vbCrLf & "        ")}
    End {methodType}")

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
        Dim prop As New PropertyInfo
        If AccessAttr.Count > 0 Then prop = GetPropertyInfo(AccessAttr)

        Dim belongsToType = ""
        Dim id = param.Identifier
        prop.Name = id.Identifier.Text
        If prop.Name = "" Then Return

        ' Handle type chars
        Dim n = prop.Name.Length - 1
        Dim c = prop.Name(n)
        Dim typeChar As String = ""
        If TypeChars.ContainsKey(c) Then
            prop.Name = prop.Name.Substring(0, n)
            typeChar = TypeChars(c)
        End If

        prop.Name = prop.Name.Trim("["c, "]"c)

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

        prop.Type = If(param.AsClause?.ToString(), typeChar)
        If prop.Type <> "" AndAlso prop.Type.Trim().Length < 4 Then prop.Type = "As Object"
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
                prop.IsValueType = result.IsValueType
            End If
        End If

        prop.Type = prop.Type.Substring(2).Trim()
        properties.Add(prop)
    End Sub

    Private Shared Function IsValueType(type As String) As Boolean

        Dim code = $"
Class Test_00000000000002
    Sub Foo
        Dim a {type}
    End Sub
End Class
"

        Dim syntaxTree = SyntaxFactory.ParseSyntaxTree(code)
        Dim comp = CurrentCompilation.AddSyntaxTrees(syntaxTree)
        Dim sem = comp.GetSemanticModel(syntaxTree)
        Dim variableDeclaration = syntaxTree.GetRoot().DescendantNodes().OfType(Of LocalDeclarationStatementSyntax)().First

        Try
            Dim typeSymbol = CType(sem.GetDeclaredSymbol(variableDeclaration.Declarators(0).Names(0)), ILocalSymbol).Type
            Return typeSymbol.IsValueType
        Catch

        End Try

        Return False

    End Function

    Private Shared Sub SetDefValue(ByRef prop As PropertyInfo)
        Dim t = prop.Type.Substring(2).Trim()
        Select Case t.Trim("?"c).ToLower()
            Case "byte", "sbyte", "short", "ushort", "integer", "uinteger", "long", "ulong", "single", "double", "decimal"
                prop.LiteralDefVal = True
                prop.DefaultValue = "= 0"
                prop.IsValueType = True
            Case "boolean"
                prop.LiteralDefVal = True
                prop.DefaultValue = "= False"
                prop.IsValueType = True
            Case "char"
                prop.LiteralDefVal = True
                prop.DefaultValue = "= vbNullChar"
                prop.IsValueType = True
            Case "string"
                prop.LiteralDefVal = True
                prop.DefaultValue = "= """""
                prop.IsValueType = False
            Case "date", "datetime"
                prop.LiteralDefVal = True
                prop.DefaultValue = "= #1/1/0001#"
                prop.IsValueType = True
            Case Else
                If t.StartsWith("(") Then
                    prop.IsValueType = True
                Else
                    prop.IsValueType = IsValueType(prop.Type)
                End If
        End Select
        If t.EndsWith("?") Then prop.DefaultValue = "= Nothing"
    End Sub

    Public Shared Function WriteProperties(Properties As List(Of PropertyInfo)) As String
        Dim props As New StringBuilder
        For Each p In Properties
            If p.IsPrivateImplementation Then
                props.AppendLine(
$"   Private Property [{p.Name}] As {p.Type} {p.Implements}
        Get
            Return _{p.PublicPropName}
        End Get
        Set
            _{p.PublicPropName} = Value
        End Set
   End Property")

                props.AppendLine()
                Continue For
            End If

            If p.IsKey Then props.AppendLine("   <Key>")

            If p.DefaultValue <> "" Then
                props.AppendLine($"   <DefaultValue(""{If(p.LiteralDefVal, "1", "0")}{ p.DefaultValue.Replace("""", StringQuote)}"")>")
            End If

            If p.Attrs <> "" Then props.AppendLine("   " & p.Attrs)

            props.Append("   Public ")
            If p.InheritanceModifier <> "" Then props.Append(p.InheritanceModifier & " ")
            If p.IsReadOnly = True Then props.Append("ReadOnly ")
            props.Append($"Property [{p.Name}] As {p.Type}")
            If p.Implements <> "" Then props.Append(p.Implements)
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
            If p.IsPrivateImplementation Then Continue For

            If addSep Then
                params.Append("," & vbCrLf)
            Else
                addSep = True
            End If

            If p.LiteralDefVal Then
                params.Append($"                Optional [{p.camelCaseName}] As {p.Type} {p.DefaultValue}")
                body.AppendLine($"        Me.{p.Name} = [{p.camelCaseName}]")
            Else
                If p.IsValueType AndAlso Not p.Type.EndsWith("?") Then ' Can use Nullable
                    params.Append($"                Optional [{p.camelCaseName}] As {p.Type}? = Nothing")
                Else
                    params.Append($"                Optional [{p.camelCaseName}] As [Optional](Of {p.Type}) = Nothing")
                End If
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

    Private Shared Sub WriteTuplesOps(className As String, typeParams As String, allProperties As List(Of PropertyInfo), record As StringBuilder)
        Dim properties = From p In allProperties
                         Where Not p.IsPrivateImplementation

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

    Public Shared Sub WriteEquals(
                    className As String,
                    typeParams As String,
                    Properties As List(Of PropertyInfo),
                    record As StringBuilder,
                    isClass As Boolean
              )

        Dim keys = From p In Properties
                   Where p.IsKey

        If keys.Any Then
            record.AppendLine(
 $"    Public Overrides Function Equals(anotherObject As Object) As Boolean            
            If TypeOf anotherObject IsNot {className}{typeParams} Then Return False
            Dim anotherRecord = CType(anotherObject, {className}{typeParams})
            Return Equals(anotherRecord)
    End Function")
            record.AppendLine()

            record.AppendLine($"    Public Overloads Function Equals(anotherRecord As {className}{typeParams}) As Boolean")
            If isClass Then record.AppendLine($"        If anotherRecord.GetType IsNot Me.GetType() Then Return False")
            For Each p In keys
                record.AppendLine($"        If Not _{p.Name}.Equals(anotherRecord._{p.Name}) Then Return False")
            Next
            record.AppendLine("        Return True")
            record.AppendLine("    End Function")
        End If
        record.AppendLine()
    End Sub

    Public Shared Sub WriteGetHashCode(
                    Properties As List(Of PropertyInfo),
                    record As StringBuilder
              )

        Dim keys = From p In Properties
                   Where p.IsKey

        If keys.Any Then
            record.AppendLine($"    Public Overrides Function GetHashCode() As Integer")
            record.AppendLine($"        Dim hash As New HashCode()")
            For Each p In keys
                record.AppendLine($"        hash.Add(_{p.Name})")
            Next
            record.AppendLine("        Return hash.ToHashCode()")
            record.AppendLine("    End Function")
            record.AppendLine()
        End If
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
            If p.IsPrivateImplementation Then Continue For

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
            If p.IsPrivateImplementation Then Continue For

            If addSep Then
                params.Append("," & vbCrLf)
                body.Append("," & vbCrLf)
            Else
                addSep = True
            End If

            If p.IsValueType AndAlso Not p.Type.EndsWith("?") Then
                params.Append($"                Optional [{p.camelCaseName}] As {p.Type}? = Nothing")
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
Imports Microsoft.VisualBasic
Imports System
Imports System.LinQ
Imports System.Collections
Imports System.Collections.Generic
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
            For Each prop In baseClass.GetMembers()
                If TypeOf prop IsNot IPropertySymbol AndAlso TypeOf prop IsNot IFieldSymbol Then Continue For

                If prop.DeclaredAccessibility = Accessibility.Public Then
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
                        .Type = "As " & GetPropType(prop),
                        .IsField = TypeOf prop Is IFieldSymbol,
                        .LiteralDefVal = If(defValue = "", False, defValue(0) = "1"c),
                        .DefaultValue = If(defValue = "", "", defValue.Substring(1)),
                        .IsKey = isKey,
                        .IsReadOnly = IsReadOnly(prop) OrElse defaultPropInfo.IsReadOnly,
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

    Public Shared Function GetInterfaceInfo(implementation As String) As ITypeSymbol
        If implementation = "" Then Return Nothing

        Dim code = $"
Imports Microsoft.VisualBasic
Imports System
Imports System.LinQ
Imports System.Collections
Imports System.Collections.Generic
{importsList}
Class Test_00000000000000
   {implementation}
End Class
"

        Dim syntaxTree = SyntaxFactory.ParseSyntaxTree(code)
        Dim comp = CurrentCompilation.AddSyntaxTrees(syntaxTree)
        Dim sem = comp.GetSemanticModel(syntaxTree)
        Dim implementsStatement = syntaxTree.GetRoot().DescendantNodes().OfType(Of ImplementsStatementSyntax)().FirstOrDefault

        Try
            Return sem.GetTypeInfo(implementsStatement.ChildNodes(0)).Type
        Catch
        End Try
        Return Nothing
    End Function

    Private Shared Sub ImplementProperties(
                 properties As List(Of PropertyInfo),
                 interfaceSymbol As ITypeSymbol,
                 implementation As String)

        Dim propSymbols = interfaceSymbol.GetMembers().
            OfType(Of IPropertySymbol)().
            Where(Function(p) Not (p.IsIndexer OrElse p.Parameters.Count > 0)).
            ToList()

        If propSymbols.Count = 0 Then Return

        Dim prvProps As New List(Of PropertyInfo)

        For Each prop In properties
            Dim props = From p In propSymbols
                        Where prop.Name.ToLower = p.Name.ToLower AndAlso
                            prop.Type = p.Type.ToString()

            If props.Any Then
                Dim iProp = props(0)
                If prop.IsReadOnly AndAlso Not iProp.IsReadOnly Then
                    prvProps.Add(New PropertyInfo With {
                         .IsPrivateImplementation = True,
                         .PublicPropName = prop.Name,
                         .Name = $"{interfaceSymbol.Name}_{prop.Name}",
                         .Type = prop.Type,
                         .Implements = $" {implementation}.{iProp.Name}"
                    })
                Else
                    prop.Implements = $" {implementation}.{iProp.Name}"
                End If
            End If
        Next

        properties.AddRange(prvProps)
    End Sub

    Private Shared Function IsReadOnly(prop As ISymbol) As Boolean
        If TypeOf prop Is IPropertySymbol Then
            Return CType(prop, IPropertySymbol).IsReadOnly
        Else
            Return CType(prop, IFieldSymbol).IsReadOnly
        End If
    End Function

    Private Shared Function GetPropType(prop As ISymbol) As String
        If TypeOf prop Is IPropertySymbol Then
            Return CType(prop, IPropertySymbol).Type.ToString
        Else
            Return CType(prop, IFieldSymbol).Type.ToString
        End If
    End Function

    Private Shared Function InferType(
                                     inheritance As String,
                                     methods As List(Of String),
                                     properties As List(Of PropertyInfo),
                                     defaultValue As String
                         ) As (IsConst As Boolean, Type As String, IsValueType As Boolean)

        Dim code = $"
Imports Microsoft.VisualBasic
Imports System
Imports System.LinQ
Imports System.Collections
Imports System.Collections.Generic
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
            Dim body = CType(variableDeclaration.Declarators(0).Initializer.Value, SingleLineLambdaExpressionSyntax).Body
            Dim valueSymbol = sem.GetSymbolInfo(body).Symbol
            isConst = valueSymbol IsNot Nothing AndAlso (TryCast(valueSymbol, IFieldSymbol)?.IsConst OrElse TryCast(valueSymbol, ILocalSymbol)?.IsConst)
        Catch

        End Try

        Try
            Dim typeSymbol = CType(CType(sem.GetDeclaredSymbol(variableDeclaration.ChildNodes(0).ChildNodes(0)), ILocalSymbol).Type, INamedTypeSymbol).DelegateInvokeMethod.ReturnType
            If typeSymbol Is Nothing Then Return (isConst, "As Object", False)
            Dim typeName = typeSymbol.ToDisplayString()
            Return (
                            isConst,
                            "As " & If(typeName = "?" OrElse typeName.StartsWith("<anonymous type: "), "Object", typeName),
                            typeSymbol.IsValueType
                       )
        Catch
        End Try

        Return (isConst, "As Object", False)

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


