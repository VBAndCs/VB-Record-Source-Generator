![Untitled](https://user-images.githubusercontent.com/48354902/122991838-c8fcb000-d3a5-11eb-98de-46e853a21272.png)

# VB Record Source Generator v2.1
V2.1 fixex some bugs.
See what's new in V2.0 at the end of the file.

`Source Generators` is an amazing new feature added to VB.NET since VS.NET 16.9.
It allows you to write code to generate another code that is added to your project in compilation time. 
You can combine this feature with the Roslyn compiler powerful tools (like SyntaxFacyoty, SyntaxTree and SemanticModel) to parse, compile and analyze VB syntax and generate any additional code you want. 
As an application of these concepts, I created a syntax for VB Records (quite similar to C# records). 
It consists of a `Class/Structure declaration` followed by a `parameter list`, followed by an optional `Inherits statement`. 
You can also add Imports statements at top of the file. 
These all 4 parts are valid VB syntax parts, but I grouped them to compose the new record syntax. 
This allows me to use Roslyn to parse my syntax as if it is a formal VB syntax, which made my write the code generator easily.
These are three possible variations of the record syntax:

```VB.NET
Public Record NameValue(Name = "", Value = 0.0)
```

```VB.NET
Public ReadOnly Structure ROStruct(X$, Y%, Z@)
```

```VB.NET
Friend ReadOnly Class ROClass(A As Integer, B As Integer)
```

# To use the Record Generator:
1. Add the NuGet package to your project.
```
PM> Install-Package Visual-Basic-Record-Generator
```

2. Add one or more text files to your project, and change there extension to .rec.
3. Right-click the .rec file in the Solution Explorer and click Properties, then from the 'Build Action' dropdown list choose VB analyzer additional file and save the changes.
4. Write the one or more record syntax in each .rec file and save the changes. The generator will generate the record classes/structures immediately for you, and you can use them in your code as a part of your project. 

# Record Syntax:
Simply, it is a class definition followed by a parameter list. For example:
```VB.NET
<Record>
Public Class Person(
    ID = 0, 
    Name As String = "", 
    Address = (City := "", Street := "", No :=0)
)
```

Or just use the `Record` keyword for simplicity:
```VB.NET
Public Record Person(
    ID = 0, 
    Name As String = "", 
    Address = (City := "", Street := "", No :=0)
)
```

This is a C#-like Person record class, that has three properties (ID, Name and Address). You can use an As clause to define the property type, or you can just give it an initial value, and the generator will infer the type from it. And of course you can do both.
You can also define methods in the record and it will be converted to full body Subs or Functions. Look at this:
```VB.NET
Public Class Student(
    <ReadOnly>ClassRoom = 0,
    <Key>University As String,
    <ReadOnlyKey>Collage As String,
    Grades As double, 
    Print = Function() Name & Grades
) Inherits Person
```

Or just use keywords for simplicity:
```VB.NET
Public Class Student(
    ReadOnly ClassRoom = 0,
    Key University As String,
    ReadOnly Key Collage As String,
    Grades As double, 
    Print = Function() Name & Grades
) Inherits Person
```

The Student class inherits the Person class, and has a Print() method. 
Note how I use the  attributes/ keywords `Key`, `ReadOnly` and `ReadOnlyKey` (`ReadOnly Key`) to mark the properties. A key property is a property that will be examined when determining the equality of two records. 
In C# record all properties are keys, but in our vb record we have the option to define our keys. You have many options here:
1. Mark the whole class with the `<Record>` attribute (or use the Record keyword instead of the Class keyword) to tell the generator that it is an immutable class where all its properties are ReadOnly Keys.
2. Mark the whole class with the `ReadOnly` attr/keyword to make all properties readonly but not keys then mark some individual properties with `Key` attr/keyword. 
3. Mark the whole class with the `Key` attr/keyword to make all properties keys then mark some individual properties with `ReadOnly` attr/keyword.
4. Don't mark the class with any attr and use individual attrs/keywords to design your properties access as you need.

Note that the class attrs overrides property attrs.
So, you have all the options on the table, as you are not forces to generate immutable classes only, and not forced to use all properties as keys, but still can do both with one `Record` attr/keyword.

Note that you can still declare a delegate property, by explicitly using As Clause. This sample shows you many ways to declare delegate properties, while there is only one way to declare a method:
```VB
public Record TestDelegate(
    Delegate1 As Action, 
    Delegate2 As Func(Of Integer),
    Delegate3 As Action(Of String) = AddressOf MySub,
    Delegate4 As Func(Of Integer, Integer) = Function(x) x + 1,
    Method1 = Function( ) "This is a Function"
)
```

You can also use generic type parameters after the name of the class such as:
```VB.NET
Class Foo(Of T)(X As T, Y As T)
```

I wrote almost no code to get that working. It is the magic of Roslyn! You can look at the code at GitHub, and have fun.

# Using the generated records:
VB doesn't have init-Only properties, so, you have to use ReadOnly properties instead. You can initialize ReadOnly properties via the constructor, but you cant use the `With {}` initializer to set them. To deal with limitation, I made all the constructor params `Optional`, so, you can use named params to set any properties in any order:
`Dim std1 As new Student(name:="Adam", ID:="1")`
I also added a `With` method to allow you to modify some properties of a new copy of the record. It is like the cinstructor, with all params optional, so, it acts as the new with expression in C#:
`Dim std2 = std1.With(name:="Ali", iD:="2", university=:"Cairo university")`
I also added a `WithX` method to set each property individually:
`Dim std3 = std2.WithId(3).WithGrades(79)`
You can also use this `with chain` to initialize a new record like this:
```VB.NET
Dim Mohmmad = New Student( ).
    WithID(4).       
    WithName("Mohmmad").
    WithCollage("Engineering").
    WithUniversity("Cairo University")
```

But be carful that each WithX method in the chain crates a new record, so avoid using WithX chains inside loops and recursive functions.
And as you can design a recotrd with some mutable properties, you can use the `With {}` initializer on them, so you can have a mixture initialization using the constructor, With, WithX and with {} expression. For example, suppose you have these two records:
```VB.NET
Public Key Class Author(
    ReadOnly Key ID = 0, 
    ReadOnly Name = "",	
    Books As List(Of Book)
)

Public Class Book(
    ReadOnly Key ID%, 
    ReadOnly Name As String,	
    AuthorID As Integer
) 
```

You can crate an author with two books like this:
```VB.NET
Dim auth1 As New Author(iD:=1, name:="Author1") With {
     .Books = New List(Of Book) From {
           New Book(iD:=1, name:="Book1") With {.AuthorID = 1},
           New Book() With {.AuthorID = 1}.WithID(2).WithName("Book2")
     }
}
```

# Sending Nothing to optional params:
I allow you to set a default values for record properties, where I use the constructor to set them if the corresponding optional params are missing. I use this way because VB compiler refuses using any default value for the optional param unless it's a constant, and the only constant valid for objects is Nothing. So, I had to use Nothing as the default value of the param, and wrote an `If statement` to set the actual default value you provided instead of `Nothing`. 
The problem here is: what if you send the value `Nothing` to the parameter and really want to reset the property to Nothing? 
Unfortunately, this will not happen, because the If statement will replace nothing with the default value!
To solve this issue, I defined value type params as nullable, and defined nullable value types and ref typs params as `Optional(Of T)`. 
Both `Nullable(Of T)` and `Optional(Of T)` structures have `HasValue`and `Value` properties. So:
- When the optional param is missing, or you send the value `Nothing` to the param, the `HasValue` proprety will be false`, and I will set the default value (or copy the value from the current record if you are using the `With` method).
- If you want to set an ref type to Nothing, send `new [Optional](Of T)(Nothing)` to the param. In this case, `HasValue` will be True and `Value` will contain `Nothing`!
- You can pass any normal value directly to the param and it will be implicitly converted to `Nullable(Of T)` or `Optional(Of T)`, and will be implicitly concerted back to T when setting the property value.

In fact you don't have to worry about all these details, because it is used internally in the generated class. All that you want to know, is that when you need to send `Nothing` to a ref type, just send one of these alternatives to the param:
1. `new [Optional](Of T)(Nothing)`
2. `[Optional](Of T).Nothing`
3. `new [Nothing](Of T)`
Where T is the type of the property.
Note that this is not needed with value types, unless they are nullable (Like `Integer?`). I treat Nullable types as ref types because you may want to set them to Nothing.


# What's new in V2.0?
* Allow to declare a namespace for the records:
   Add the Namespace statement as the first statement in the file, without `End Statement`. 
   Ex: `Namespace MyApp.Test`

* Allow record to implement Interfaces.
  Add the Implements statement after the record property list. If you need to inherit a class to, add the two statements separated by `,` in any order:
  ```
  Record Obj(
      Dispose = Sub() Console.WriteLine("Disposed")
  ) Implements IDisposable
  ```

  Record properties and methods that match those of the interface are used to implement it in the generated class. I allowed functions to implement Subs, and readonly properties to implement read-write properties, but note that means that the property valuee can be chamge via the interface, dispit it is readonly.
  Note that record syntax doesn't allow to implement Indexers nor generic methods, but you can create a partial class to implement then as you do in other classes.

* Allow property-less records.   
* Allow `As new` expression in property definition:
   ` Record Test(Data As New List(Of T))
* Records now are aware of some default imported namespaces:
    like Sytem, System.Collections.Generic, and Microsoft.VisualBasic, so, you don't need to import them.
* Allow `Fn() =>` syntax for lambda expressions. Fn is a legacy vb keyword for Functions. Ex:
`Record Foo(Sum = Fn(a, b) => a + b)`
I see this syntax better than C# syntax and VB syntax for lambdas, so, I supported it in ZML before, and now in RecGen.


# To Do:
It will be helpful if .rec files have intellisense support, formatting, coloring, and syntax errors check.
