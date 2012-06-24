using MiniIoc;

namespace SampleCSharp
{
    class Program
    {
        static void Main(string[] args)
        {
            var container = new Container();
            container.Register<Program>(typeof(Program), Lifetime.Singleton);
            var x = container.Resolve<Program>();
            container.Release(x);
        }
    }
}
