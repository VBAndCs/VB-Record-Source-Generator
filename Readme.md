# VB Record Source Generator
An amazing new feature called `Source Generators` has been added to VB.NET since VS.NET 16.9. It allows you to write code to generate another code that is added to your project in compilation time. 
You can combine this feature with the Roslyn compiler powerful tools (like SyntaxFacyoty, SyntaxTree and SemanticModel) to parse, compile and analyze VB syntax and generate any additional code you want. As an application of these concepts, I created a syntax for VB Records (quite similar to C# records). It consists of a `Class/Structure declaration` followed by a `parameter list`, followed by an optional `Inherits statement`. You can also add Imports statements at top of the file. 
These all 4 parts are valid VB syntax parts, but I grouped them to compose the new record syntax. This allows me to use Roslyn to parse my syntax as if it is a formal VB syntax, which made my write the code generator easily.

 #To use the Record Generator:
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

The Student class inherits the Person class, and has a Print() method.
Note how I use the  attributes Key, ReadOnly and ReadOnlyKey to mark the properties. A key property is a property that will be examined when determining the equality of two records. 
In C# record all properties are keys, but in our vb record we have the option to define our keys. You have many options here:
1. Mark the whole class with the `<Record>` or the `<Immutable>` attribute to tell the generator that it is an immutable record where all its properties are ReadOnly Keys.
2. Mark the whole class with the `<ReadOnly>` attr to make all properties readonly but not keys then mark some individual properties with `<Key>` attr. 
3. Mark the whole class with the `<Key>` attr to make all properties keys then mark some individual properties with `<ReadOnly>` attr.
4. Don't mark the class with any attr and design your properties access as you need.

Note that the class attr overrides the property attr.
So, you have all the options on the table, as you are not forces to generate immutable classes only, and not forced to use all properties as keys, but still can do both with one `<Record>` attr.

Note that I could use `Record`, `Key`, `ReadOnly` and `ReadOnlyKey`  as keywords (it is my syntax after all), but I choose to use a legal parts of VB syntax to make it easier for me to use Roslyn to parse the code. This also gave you the ability to use all valid vb expressions in the param list, and use all valid class modifiers and all kinds of attributes (but our `Record`, `Key` and `ReadOnl`y special attrs must appear before any other attrs.). 
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

And don't forget, you can design your recotrd to have some mutable properties, so, you can use the `With {}` initializer on them.

# Sending Nothing to optional params:
I allow you to set a default values for record properties, where I use the constructor to set them if the corresponding optional params are missing. I use this way because VB compiler refuses using any default value for the optional param unleaa it's a constant, and the only constant valid for objects is Nothing. So, I had to use Nothing as the default value of the param, and wrote an `If statement` to set the actual default value you provided instead of `Nothing`. The problem here is: what if you send the value `Nothing` to the parameter and really want to reset the property to Nothing? Unfortunately, this will not happen, because the If statement will replace nothing with the default value!
To solve this issue, I defined all params to be `Optional(Of T)`. This structure is like the nullable structure, with `HasValue` and `Value` properties. It works as follows:
- when the optional param is missing, or you send the value `Nothing` to the param, `HasValue = false`.
- when you send `new [Optional](Of T)(Nothing)` to the param, `HasValue = True` and `Value` contains `Nothing`!
- You can pass any normal value of type T directly to the param and it will be implicitly concerted to `Optional(Of T)`, and will be implicitly concerted back to T when setting the property value.

In fact you don't have to worry about all these details, because it is used internally in the generated class. All that you want to know, is that when you need to send `Nothing` to any object, all you have to do is send one of these alternatives:
1. `new [Optional](Of T)(Nothing)`
2. `[Optional](Of T).Nothing`
3. `new [Nothing](Of T)`
Where T is the type of the property.
Note that this is not needed with value types, as I use the Nullable structure instead. It is needed only with value-type params of the `With` methods, so I can set it's default vakue to Nothing (an actual nothing that will be restored in the nullable sturcture), as an indication of that param is missing, so I copy the value of the property from the current record (Me), to the new returned record.

# To Do:
It will be helpful if .rec files have intellisense support, formatting, coloring, and syntax errors check.