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

    do  ``resolving type with value type dependency in constructor should throw`` ()
        ``resolving type with reference type dependency in constructor should inject reference`` ()
        ``resolving type with self type dependency in constructor should fail`` ()
        ``resolving type with cyclic type dependency in constructor should fail`` ()
        ``resolving type with fun type argument in constructor should inject factory`` ()