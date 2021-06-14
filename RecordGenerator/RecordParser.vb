' Created By, Mohammad Hamdy Ghanem, 
' Egypt, 2021

Imports System.Text
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Class RecordParser

    Public Shared Sub Debug(code As String)
        Parse(Nothing, code)
    End Sub

    Public Shared Sub Parse(context? As GeneratorExecutionContext, code As String)
        Dim tokens = SyntaxFactory.ParseTokens(code).ToArray
        For i = 0 To tokens.Length - 1
            Dim token = tokens(i)
            If token.Text = "(" Then
                If tokens(i + 1).Text.ToLower = "of" Then ' Skip tolens untile reaching "("
                    For j = i + 1 To tokens.Length - 1
                        token = tokens(j)
                        If token.Text = "(" Then Exit For
                    Next
                End If
                Dim definition = code.Substring(0, token.SpanStart)
                Dim members = SyntaxFactory.ParseParameterList(code.Substring(token.SpanStart))
                GenerateRecord(context, definition, members)
                Dim pos = token.SpanStart + members.Span.Length
                If pos < code.Length Then Parse(context, code.Substring(pos))
                Exit For
            End If
        Next
    End Sub

    Private Shared Sub GenerateRecord(context? As GeneratorExecutionContext, definition As String, paramList As ParameterListSyntax)
        Dim tokens = SyntaxFactory.ParseSyntaxTree(definition).GetRoot().ChildNodes(0).ChildNodes(0).ChildNodesAndTokens.ToArray
        Dim DefaultPropInfo As New PropertyInfo

        If tokens(0).Kind = SyntaxKind.AttributeList Then
            DefaultPropInfo = GetPropertyInfo(tokens(0))
            definition = definition.Substring(tokens(1).SpanStart)
        End If

        Dim isClass = (From token In tokens
                       Where token.Kind = SyntaxKind.ClassKeyword).Count > 0

        Dim className = (From token In tokens
                         Where token.Kind = SyntaxKind.IdentifierToken).First.ToString

        Dim typeParams = ""
        Dim lastToken = tokens(tokens.Length - 1)
        If lastToken.Kind = SyntaxKind.TypeParameterList Then
            typeParams = lastToken.ToString()
        End If

        Dim Properties As New List(Of PropertyInfo)
        Dim Methods As New List(Of String)

        For Each member As ParameterSyntax In paramList.ChildNodes
            Dim valueExpr = member.Default?.DescendantNodes?(0)

            If TypeOf valueExpr Is LambdaExpressionSyntax Then
                LambdaToMethod(Methods, member, valueExpr)
            Else
                AddPropertyInfo(DefaultPropInfo, Properties, member)
            End If
        Next

        ' ------------------------Generate the record class/struct ----------------------------

        Dim record As New System.Text.StringBuilder()

        record.AppendLine(definition)
        record.AppendLine()
        AddProperties(Properties, record)
        Dim params = AddConstructor(Properties, record)
        AddLambdaMethods(Methods, record)
        AddWith(className, typeParams, Properties, record, params)
        AddWithProps(className, typeParams, Properties, record)
        AddClone(className, typeParams, record)
        AddToString(className, Properties, record)
        AddEquals(className, typeParams, Properties, record)
        AddEqualityOps(className, typeParams, record)
        AddTuplesOps(className, typeParams, Properties, record)
        record.Append(If(isClass, "End Class", "End Structure"))

        If context.HasValue Then
            context.Value.AddSource(className & "Record", SourceText.From(record.ToString(), Encoding.UTF8))
        End If

    End Sub
    Private Shared Sub LambdaToMethod(Methods As List(Of String), param As ParameterSyntax, lanbdaExpr As LambdaExpressionSyntax)
        Dim Header = lanbdaExpr.SubOrFunctionHeader
        Dim MethodType = Header.DeclarationKeyword.Text
        Dim isSub = MethodType.ToLower() = "sub"
        Dim AsClause = ""
        If Not isSub Then
            AsClause = If(Header.AsClause, InferReturnType(param.Default.ToString()))
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

        Methods.Add(
$"    Public {MethodType} { param.Identifier}{Header.ParameterList} {AsClause}
        {lambdaBody}
    End {MethodType}")
    End Sub

    Private Shared Sub AddPropertyInfo(DefaultPropInfo As PropertyInfo, Properties As List(Of PropertyInfo), param As ParameterSyntax)
        Dim AccessAttr = param.AttributeLists
        Dim prop As PropertyInfo
        If AccessAttr.Count > 0 Then
            prop = GetPropertyInfo(AccessAttr(0))
        End If
        prop.IsReadOnly = prop.IsReadOnly Or DefaultPropInfo.IsReadOnly
        prop.IsKey = prop.IsKey Or DefaultPropInfo.IsKey

        prop.Name = param.Identifier.ToString()
        prop.Type = param.AsClause?.ToString()
        prop.DefaultValue = param.Default?.ToString()
        If prop.Type = "" Then
            prop.Type = If(prop.DefaultValue = "", "As Object", InferType(prop.DefaultValue))
        End If
        prop.Type = prop.Type.Substring(2).Trim
        Properties.Add(prop)
    End Sub

    Private Shared Sub AddProperties(Properties As List(Of PropertyInfo), record As StringBuilder)
        ' Properties
        For Each p In Properties
            record.Append("   Public ")
            If p.IsReadOnly = True Then record.Append("ReadOnly ")
            record.Append($"Property [{p.Name}] As {p.Type}")
            record.AppendLine()
        Next
        record.AppendLine()
    End Sub

    Private Shared Function AddConstructor(Properties As List(Of PropertyInfo), record As StringBuilder) As String
        Dim params As New StringBuilder
        Dim body As New StringBuilder
        Dim AddSep = False

        record.AppendLine("    Public Sub New(")
        AddSep = False

        For Each p In Properties
            If AddSep Then params.Append("," & vbCrLf & "")
            params.Append($"                Optional [{p.camelCaseName}] As [Optional](Of {p.Type}) = Nothing")
            AddSep = True
            body.AppendLine(
$"        If [{p.camelCaseName}].HasValue
            _{p.camelCaseName} = [{p.camelCaseName}].Value
        Else
            _{p.camelCaseName} {If(p.DefaultValue = "", "= Nothing", p.DefaultValue)}
        End If
")
        Next
        record.Append(params.ToString())
        record.AppendLine(vbCrLf & "            )")
        record.AppendLine()
        record.Append(body.ToString())
        record.AppendLine("    End Sub")
        record.AppendLine()
        Return params.ToString
    End Function

    Private Shared Sub AddLambdaMethods(Methods As List(Of String), record As StringBuilder)
        For Each method In Methods
            record.AppendLine(method)
            record.AppendLine()
        Next
    End Sub

    Private Shared Sub AddTuplesOps(className As String, typeParams As String, Properties As List(Of PropertyInfo), record As StringBuilder)
        For n = 1 To Properties.Count - 1
            Dim body As New StringBuilder
            Dim AddSep = False
            record.Append($"    Public Shared Widening Operator CType(anotherRecord As {className}{typeParams}) As (")
            For i = 0 To n
                Dim p = Properties(i)
                If AddSep Then
                    record.Append(", ")
                    body.Append(", ")
                End If
                record.Append($"{p.Name} As {p.Type}")
                body.Append($"anotherRecord.{p.Name}")
                AddSep = True
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
            AddSep = False

            For i = 0 To n
                Dim p = Properties(i)
                If AddSep Then
                    record.Append(", ")
                    body.Append(", ")
                End If
                record.Append($"{p.Name} As {p.Type}")
                body.Append($"fromTuple.{p.Name}")
                AddSep = True
            Next
            record.AppendLine($")) As {className}{typeParams}")
            record.Append($"        Return new {className}{typeParams}(")
            record.Append(body.ToString)
            record.AppendLine(")")
            record.AppendLine("    End Operator")
            record.AppendLine()
        Next
    End Sub

    Private Shared Sub AddEqualityOps(className As String, typeParams As String, record As StringBuilder)
        record.AppendLine(
$"    Public Shared Operator =(FirstRecord As {className}{typeParams}, secondRecord As {className}{typeParams}) As Boolean
        Return FirstRecord.Equals(secondRecord)
    End Operator

    Public Shared Operator <>(FirstRecord As {className}{typeParams}, secondRecord As {className}{typeParams}) As Boolean
        Return Not FirstRecord.Equals(secondRecord)
    End Operator")
        record.AppendLine()
    End Sub

    Private Shared Sub AddEquals(className As String, typeParams As String, Properties As List(Of PropertyInfo), record As StringBuilder)
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

    Private Shared Sub AddToString(className As String, Properties As List(Of PropertyInfo), record As StringBuilder)
        record.AppendLine(
$"    Public Overrides Function ToString() As String
        Dim stringBuilder As New System.Text.StringBuilder()
        stringBuilder.Append(""{className}"")
        stringBuilder.Append("" {{ "")")
        Dim AddSep = False
        For Each p In Properties
            Dim s = If(AddSep, ", ", "")
            record.AppendLine($"        stringBuilder.Append($""{s}{p.Name} = {{_{p.Name}}}"")")
            AddSep = True
        Next
        record.AppendLine(
"        stringBuilder.Append("" }"")
        Return stringBuilder.ToString()
    End Function")
    End Sub

    Private Shared Sub AddClone(className As String, typeParams As String, record As StringBuilder)
        record.AppendLine(
$"    Public Function Clone() As {className}{typeParams}
        Return Me.With()
    End Function")
        record.AppendLine()
    End Sub

    Private Shared Sub AddWithProps(className As String, typeParams As String, Properties As List(Of PropertyInfo), record As StringBuilder)
        For Each p In Properties
            record.AppendLine($"    Public Function With{p.Name}([{p.camelCaseName}] As {p.Type}) As {className}{typeParams}")
            record.AppendLine($"Return Me.With([{p.camelCaseName}]:=[{p.camelCaseName}])")
            record.AppendLine("    End Function")
            record.AppendLine()
        Next
    End Sub

    Private Shared Sub AddWith(className As String, typeParams As String, Properties As List(Of PropertyInfo), record As StringBuilder, params As String)
        record.AppendLine("    Public Function [With](")
        record.Append(params)
        record.AppendLine(vbCrLf & $"            ) As {className}{typeParams}")
        record.AppendLine()

        Dim body As New StringBuilder
        body.AppendLine($"        Dim newRecord As New {className}{typeParams}()")
        For Each p In Properties
            body.AppendLine(
$"        If [{p.camelCaseName}].HasValue
            newRecord._{p.Name} = [{p.camelCaseName}].Value
        Else
            newRecord._{p.Name} = Me._{p.Name}
        End If
")
        Next
        body.AppendLine("        Return  newRecord")
        record.Append(body.ToString())
        record.AppendLine("    End Function")
        record.AppendLine()
    End Sub

    Private Shared Function InferReturnType(lambda As String) As String
        Return "As Object"
    End Function

    Private Shared Function InferType(defaultValue As String) As String
        Dim code = $"
Class Test
    Sub Foo
        Dim a {defaultValue}
    End Sub
End Class
"
        Dim syntaxTree = SyntaxFactory.ParseSyntaxTree(code)
        Dim comp = VisualBasicCompilation.Create("Test", {syntaxTree})
        Dim sem = comp.GetSemanticModel(syntaxTree)
        Dim variableDeclarations = syntaxTree.GetRoot().DescendantNodes().OfType(Of LocalDeclarationStatementSyntax)()
        For Each variableDeclaration In variableDeclarations
            Dim typeSymbol = sem.GetOperation(variableDeclaration.Declarators(0).Initializer.Value).Type
            Dim typeName = typeSymbol.ToDisplayString()
            Return "As " & If(typeName = "?" OrElse typeName.StartsWith("<anonymous type: "), "Object", typeName)
        Next
        Return "As Object"
    End Function

    Private Shared Function GetPropertyInfo(attributeList As AttributeListSyntax) As PropertyInfo
        Dim propInfo As New PropertyInfo
        Select Case attributeList.Attributes(0).ToString().ToLower()
            Case "key"
                propInfo.IsKey = True

            Case "readonly"
                propInfo.IsReadOnly = True

            Case "readonlykey", "immitable", "record"
                propInfo.IsKey = True
                propInfo.IsReadOnly = True

        End Select

        Return propInfo
    End Function

End Class
