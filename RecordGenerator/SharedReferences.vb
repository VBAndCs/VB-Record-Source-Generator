' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Reflection
Imports System.Reflection.Metadata
Imports System.Reflection.PortableExecutable
Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Public Module SharedReferences
        Private ReadOnly s_frameworkDirectory As String = Directory.GetParent(GetType(Object).Assembly.Location).FullName
        Private ReadOnly s_referencePath As New List(Of String)
        Private ReadOnly s_visualBasicReferences As New List(Of MetadataReference)

        Private Sub AddReferences(L As List(Of String), FileNameWithPath As String)
            If L.Contains(FileNameWithPath) Then
                Exit Sub
            End If
            Dim hasMetadataOrIsAssembly As (HasMetadata As Boolean, IsAssembly As Boolean) = HasMetadataIsAssembly(FileNameWithPath)

            If Not hasMetadataOrIsAssembly.HasMetadata Then
                Exit Sub
            End If
            L.Add(FileNameWithPath)
            s_visualBasicReferences.Add(MetadataReference.CreateFromFile(FileNameWithPath))
        End Sub

        Private Sub BuildReferenceList(WindowsFormsLocation As String)
            If s_referencePath.Any Then
                Exit Sub
            End If
            ' CodeAnalysisReference
            Dim Location As String = GetType(Compilation).Assembly.Location
            AddReferences(s_referencePath, Location)

            'SystemReferences
            For Each DLL_Path As String In Directory.GetFiles(s_frameworkDirectory, "*.dll")
                If DLL_Path.EndsWith("System.EnterpriseServices.Wrapper.dll", StringComparison.Ordinal) Then
                    Continue For
                End If
                AddReferences(s_referencePath, DLL_Path)
            Next

            ' ComponentModelEditorBrowsable
            Location = GetType(ComponentModel.EditorBrowsableAttribute).GetAssemblyLocation
            AddReferences(s_referencePath, Location)

            ' SystemCore
            Location = GetType(Enumerable).Assembly.Location
            AddReferences(s_referencePath, Location)

            ' SystemXmlLinq
            Location = GetType(XElement).Assembly.Location
            AddReferences(s_referencePath, Location)

            ' VBRuntime
            Location = GetType(CompilerServices.StandardModuleAttribute).Assembly.Location
            s_visualBasicReferences.Add(MetadataReference.CreateFromFile(Location))

            ' Windows Forms
            If Not String.IsNullOrWhiteSpace(WindowsFormsLocation) Then
                AddReferences(s_referencePath, WindowsFormsLocation)
            End If

        End Sub

        Private Function HasMetadataIsAssembly(sourcePath As String) As (HasMetadata As Boolean, IsAssembly As Boolean)
            Using assemblyStream As New FileStream(sourcePath, FileMode.Open, FileAccess.Read, FileShare.Delete Or FileShare.Read)
                Try
                    Using peReader As New PEReader(assemblyStream, PEStreamOptions.LeaveOpen)
                        If peReader.HasMetadata Then
                            Dim reader As MetadataReader = peReader.GetMetadataReader()
                            Return (True, reader.IsAssembly)
                        End If
                    End Using
                Catch e1 As BadImageFormatException
                    ' not a PE
                End Try

                Return (False, False)
            End Using
        End Function

        <Extension>
        Friend Function GetAssemblyLocation(type As Type) As String
            Dim asm As Assembly = type.GetTypeInfo().Assembly
            Dim locationProperty As Reflection.PropertyInfo = asm.GetType().GetRuntimeProperties().Single(Function(p As Reflection.PropertyInfo) p.Name = "Location")
            Return CStr(locationProperty.GetValue(asm))
        End Function

        Public Function VisualBasicReferences(WindowsFormsLocation As String, Optional OptionalReference As IReadOnlyList(Of MetadataReference) = Nothing) As List(Of MetadataReference)
            Try
                If WindowsFormsLocation Is Nothing Then
                    WindowsFormsLocation = ""
                End If
                SyncLock s_referencePath
                    If Not s_visualBasicReferences.Any Then
                        BuildReferenceList(WindowsFormsLocation)
                    End If
                    If OptionalReference IsNot Nothing Then
                        ' Optional References
                        Dim tempList As New List(Of MetadataReference)
                        tempList.AddRange(s_visualBasicReferences)
                        tempList.AddRange(OptionalReference)
                        Return tempList
                    End If
                    Return s_visualBasicReferences
                End SyncLock
            Catch ex As OperationCanceledException
                Stop
            Catch ex As Exception
                Stop
                Throw
            End Try
            Return Nothing
        End Function

        Public Function GetReferences(namespaces As List(Of String)) As List(Of MetadataReference)
            Dim refs As New List(Of MetadataReference)
            For Each ns In namespaces
                refs.Add(MetadataReference.CreateFromFile(Assembly.Load(ns).Location))
            Next
            Return VisualBasicReferences(Assembly.Load("System").Location, refs)
        End Function
    End Module

