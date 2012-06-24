namespace MiniIoc

// Note: MiniIoc defaults to a general .Net facing library
// define FSHARP compilation symbol for an F# facing library

open System
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Reflection

#if FSHARP
type Message = string
exception TypeResolutionException of Message
#else
type TypeResolutionException(message) = inherit Exception(message)
#endif

#if FSHARP
type Lifetime = Singleton | Transient
#else
type Lifetime = Singleton = 0 | Transient = 1
[<AutoOpen; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Lifetime =
    let Singleton = Lifetime.Singleton
    let Transient = Lifetime.Transient
#endif

type AbstractType = Type
type ConcreteType = Type

type private Constructor = 
    | Reflected of ConcreteType 
    | Factory of (unit -> obj)

[<AutoOpen>]
module private Patterns =
    let (|FunType|_|) t =
        if FSharpType.IsFunction t then FSharpType.GetFunctionElements t |> Some
        else None
    let (|FuncType|_|) (t:Type) =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Func<_>> 
        then t.GetGenericArguments().[0] |> Some
        else None
    let (|SeqType|_|) (t:Type) =
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<IEnumerable<_>>
        then t.GetGenericArguments().[0] |> Some
        else None

module private Choice =
    let toOption = function Choice1Of2 x -> Some x | Choice2Of2 _ -> None

/// IoC Container
type Container () as container =
    let catalog = Dictionary<AbstractType, Constructor * Lifetime>()
    let singletons = Dictionary<ConcreteType,obj>()
    let rec tryResolve cs t =
        match catalog.TryGetValue t with
        | true, (Reflected u , lifetime) -> 
            tryObtain u (fun () -> tryReflect cs u) lifetime
        | true, (Factory f, lifetime) -> 
            tryObtain t (fun () -> f() |> Choice1Of2) lifetime
        | false, _ ->  
            tryObtain t (fun () -> tryReflect cs t) Singleton 
    and tryObtain t f lifetime =
        match singletons.TryGetValue t with
        | true, value -> Choice1Of2 value
        | false, _ ->
            let result = f()
            result |> function Choice1Of2 value -> store t value lifetime | Choice2Of2 _ -> ()
            result
    and store t value = function Lifetime.Singleton -> singletons.Add(t,value) | _ -> ()
    and tryReflect cs t =
        if cs |> List.exists ((=) t) 
        then Choice2Of2 "Cycle detected" 
        else tryConstructors (t::cs) t
    and tryConstructors cs t =
        let constructors =
            t.GetConstructors()
            |> Array.sortBy (fun c -> c.GetParameters().Length)
            |> Seq.map (tryConstructor cs)
        match constructors |> Seq.tryPick Choice.toOption with
        | Some value -> Choice1Of2 value
        | None -> constructorsError t constructors |> Choice2Of2
    and constructorsError t constructors =
        let constructors = constructors |> Seq.map (function Choice1Of2 _ -> "" | Choice2Of2 x -> x)
        "Failed to match constructor from:\r\n" + (constructors |> String.concat "\r\n")
    and tryConstructor cs ci =
        let ps = ci.GetParameters()
        let args = ps |> Array.map (fun p -> tryResolveArgument cs p.ParameterType)
        let args' = args |> Array.choose Choice.toOption
        if args'.Length = ps.Length then args' |> ci.Invoke |> Choice1Of2
        else constructorError ci.DeclaringType ps args |> Choice2Of2
    and constructorError t ps args =
        let ps = ps |> Seq.map (fun p -> p.Name + ":" + p.ParameterType.Name)
        let invalidArgs = args |> Seq.choose (function Choice2Of2 s -> Some s | Choice1Of2 _ -> None)
        t.Name + "(" + (String.concat "," ps) + ") -> " + (String.concat "\r\n" invalidArgs)
    and tryResolveArgument cs t =
        match t with
        | FunType(arg,result) when arg = typeof<unit> ->
            FSharpValue.MakeFunction(t,fun args -> container.Resolve(result)) |> Choice1Of2
        | FuncType result ->
            let mi = typeof<Container>.GetMethod("Resolve",[||]).MakeGenericMethod(result)
            Delegate.CreateDelegate(t, container, mi) |> box |> Choice1Of2
        | SeqType t ->
            getDerivedTypes cs t |> box |> Choice1Of2
        | t when t.IsPrimitive -> Choice2Of2 "Primitive arguments not supported"
        | t when t = typeof<string> -> Choice2Of2 "String arguments not supported"
        | t -> tryResolve cs t
    and getDerivedTypes cs t =
        let interfaces = catalog.Keys |> Seq.filter (fun x -> x.GetInterfaces() |> Seq.exists ((=) t))
        let subTypes = catalog.Keys |> Seq.filter (fun x -> x.IsSubclassOf t)
        let values =
            Seq.append interfaces subTypes 
            |> Seq.choose (tryResolve cs >> Choice.toOption) 
            |> Seq.toArray
        let result = Array.CreateInstance(t, values.Length)
        Array.Copy(values, result, values.Length)
        result
    /// Register sequence of abstract types against specified concrete type
    member container.Register(abstractTypes:AbstractType seq, concreteType:ConcreteType) =
        for t in abstractTypes do catalog.Add(t, (Reflected concreteType, Lifetime.Singleton))
    /// Register abstract type against specified type instance
    member container.Register<'TAbstract>(instance:'TAbstract) =
        catalog.Add(typeof<'TAbstract>, (Reflected typeof<'TAbstract>, Lifetime.Singleton))
        singletons.Add(typeof<'TAbstract>, instance)
    /// Register abstract type with fluent interface
    member container.Register<'TAbstract when 'TAbstract : not struct>() = 
        Registrar<'TAbstract>(container)
    /// Register abstract type against specified concrete type with given lifetime
    member container.Register<'TAbstract when 'TAbstract : not struct>
            (concreteType:ConcreteType, lifetime:Lifetime) =
        let abstractType = typeof<'TAbstract>
        if concreteType <> abstractType &&
           not (concreteType.IsSubclassOf(abstractType)) &&
           not (concreteType.GetInterfaces() |> Array.exists ((=) abstractType)) then
            invalidArg "concreteType" "Concrete type must implement abstract type"
        catalog.Add(abstractType, (Reflected concreteType, lifetime))
    /// Register abstract type against specified factory with given lifetime
#if FSHARP
    member container.Register<'TAbstract when 'TAbstract : not struct>
            (f:unit->'TAbstract, lifetime:Lifetime) = 
        catalog.Add(typeof<'TAbstract>, (Factory(f >> box), lifetime))
#else
    member container.Register<'TAbstract when 'TAbstract : not struct>
            (f:Func<'TAbstract>, lifetime:Lifetime) = 
        catalog.Add(typeof<'TAbstract>, (Factory(fun () -> f.Invoke() |> box), lifetime))
#endif 
    /// Resolve instance of specified abstract type
    member container.Resolve<'TAbstract when 'TAbstract : not struct>() =
        container.Resolve(typeof<'TAbstract>) :?> 'TAbstract
    /// Resolve instsance of specified abstract type
    member container.Resolve(abstractType:AbstractType) =
        match tryResolve [] abstractType with
        | Choice1Of2 value -> value
        | Choice2Of2 message -> TypeResolutionException(message) |> raise
    /// Remove instance reference from container
    member container.Release(instance:obj) =
        singletons |> Seq.filter (fun pair -> pair.Value = instance) |> Seq.toList
        |> List.iter (fun pair -> singletons.Remove(pair.Key) |> ignore)
and Registrar<'TAbstract when 'TAbstract : not struct> internal (container:Container) =
    /// Register as specified concrete type with given lifetime
    member this.As<'TConcrete when 'TConcrete : not struct> (lifetime) =
        container.Register<'TAbstract>(typeof<'TConcrete>, lifetime)