open MiniIoc

type ICalculate =
    abstract member Incr : int -> int

type Calculator () =
    interface ICalculate with
        member this.Incr(x:int) = x + 1

do
    let container = Container()
    container.Register<ICalculate>(typeof<Calculator>, Singleton)

    let calc = container.Resolve<ICalculate>()
    printfn "%d" (calc.Incr 1)

    container.Release(calc)

    System.Console.ReadLine() |> ignore