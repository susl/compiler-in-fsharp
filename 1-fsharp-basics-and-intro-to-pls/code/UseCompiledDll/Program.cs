using System;
using System.Linq;
using Microsoft.FSharp.Collections;

namespace UseCompiledDll
{
    class Program
    {
        static void Main(string[] args)
        {
            var model1 = new global::Program.Model(42, new FSharpList<string>("vip", FSharpList<string>.Empty));
            Console.WriteLine(Rules.Complex(model1));
        }
    }
}
