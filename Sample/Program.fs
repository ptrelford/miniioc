open MiniIoc

type ICalculate =
    abstract member Incr : int -> int

type Calculator () =
    interface ICalculate with
        member this.Incr(x:int) = x + 1
    
let container = Container()

do  container.Register<ICalculate>(typeof<Calculator>, Singleton)

let calc = container.Resolve<ICalculate>()
do  printfn "%d" (calc.Incr 1)

do  container.Release(calc)

do  System.Console.ReadLine() |> ignore