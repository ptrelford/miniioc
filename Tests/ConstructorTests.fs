namespace MiniIoc

open NUnit.Framework

[<TestFixture>]
module ``Constructor Tests`` =

    [<AbstractClass>]
    type AbstractType () = do ()

    type ConstructorWithValueTypeArg (arg:int) = inherit AbstractType()

    let [<Test>] ``resolving type with value type dependency in constructor should throw`` () =
        let container = Container()
        container.Register<AbstractType>(typeof<ConstructorWithValueTypeArg>, Singleton)
        Assert.Throws<TypeResolutionException>(fun () ->
            container.Resolve<AbstractType>() |> ignore
        ) |> ignore

    type ReferenceType() = do ()
    type ConstructorWithReferenceTypeArg (arg:ReferenceType) = inherit AbstractType()

    let [<Test>] ``resolving type with reference type dependency in constructor should inject reference`` () =
        let container = Container()
        container.Register<AbstractType>(typeof<ConstructorWithReferenceTypeArg>, Singleton)
        let instance = container.Resolve<AbstractType>()
        Assert.NotNull(instance)

    type ConstructorWithSelfReferenceArg (arg:AbstractType) = inherit AbstractType()

    let [<Test>] ``resolving type with self type dependency in constructor should fail`` () =
        let container = Container()
        container.Register<AbstractType>(typeof<ConstructorWithSelfReferenceArg>, Singleton)
        Assert.Throws<TypeResolutionException>(fun () ->
                container.Resolve<AbstractType>() |> ignore
        ) |> ignore

    type Cyclic(arg:ConstructorWithCyclicReferenceArg) = do ()
    and  ConstructorWithCyclicReferenceArg (arg:Cyclic) = do ()

    let [<Test>] ``resolving type with cyclic type dependency in constructor should fail`` () =
        let container = Container()
        container.Register<ConstructorWithCyclicReferenceArg>(typeof<ConstructorWithCyclicReferenceArg>, Singleton)
        Assert.Throws<TypeResolutionException>(fun () ->
                container.Resolve<AbstractType>() |> ignore
        ) |> ignore

    type ConstructorWithFunArg (arg:unit -> ReferenceType) = 
        inherit AbstractType()
        member this.Factory () = arg()

    let [<Test>] ``resolving type with fun type argument in constructor should inject factory`` () =
        let container = Container()
        container.Register<AbstractType>(typeof<ConstructorWithFunArg>, Singleton)
        let instance = container.Resolve<AbstractType>() :?> ConstructorWithFunArg
        let refValue = instance.Factory()
        Assert.NotNull(refValue)

    type SubType1 () = inherit AbstractType()
    type SubType2 () = inherit AbstractType()
    type ConstructorWithSeqArg (subTypes:AbstractType seq) =
        member this.SubTypes = subTypes

    let [<Test>] ``resolving type with seq type argument in constructor should inject sub types`` () =
        let container = Container()
        container.Register<SubType1>(typeof<SubType1>, Singleton)
        container.Register<SubType2>(typeof<SubType2>, Singleton)
        container.Register<ConstructorWithSeqArg>(typeof<ConstructorWithSeqArg>, Singleton)
        let instance = container.Resolve<ConstructorWithSeqArg>()
        let types = instance.SubTypes |> Seq.map (fun i -> i.GetType().Name) |> Set.ofSeq
        let types' = set [ typeof<SubType1>.Name; typeof<SubType2>.Name ]
        Assert.That((types = types'))

    type Marker = interface end
    type MarkedType1 () = interface Marker
    type MarkedType2 () = interface Marker
    type ConstructorWithInterfaceArg (markedTypes:Marker seq) =
        member this.MarkedTypes = markedTypes

    let [<Test>] ``resolving type with seq type argument in constructor should inject interfaces`` () =
        let container = Container()
        container.Register<MarkedType1>(typeof<MarkedType1>, Singleton)
        container.Register<MarkedType2>(typeof<MarkedType2>, Singleton)
        container.Register<ConstructorWithInterfaceArg>(typeof<ConstructorWithInterfaceArg>, Singleton)
        let instance = container.Resolve<ConstructorWithInterfaceArg>()
        let types = instance.MarkedTypes |> Seq.map (fun i -> i.GetType().Name) |> Set.ofSeq
        let types' = set [ typeof<MarkedType1>.Name; typeof<MarkedType2>.Name ]
        Assert.That((types = types'))

    do  ``resolving type with value type dependency in constructor should throw`` ()
        ``resolving type with reference type dependency in constructor should inject reference`` ()
        ``resolving type with self type dependency in constructor should fail`` ()
        ``resolving type with cyclic type dependency in constructor should fail`` ()
        ``resolving type with fun type argument in constructor should inject factory`` ()
        ``resolving type with seq type argument in constructor should inject sub types`` ()
        ``resolving type with seq type argument in constructor should inject interfaces`` ()
