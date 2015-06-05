using System;
using System.Linq;

namespace UseFromCs
{
    public class Model
    {
        public int Amount { get; set; }
        public bool HasTag(string tag) { return _tags.Contains(tag); }

        private readonly string[] _tags;

        public Model(int amount, string[] tags)
        {
            Amount = amount;
            _tags = tags;
        }
    }

    class Program
    {
        static void Main()
        {
            var rule = RuleCompiler.Compile<Model>("not(Amount < 10) and HasTag('vip') = true");
            var model = new Model(42, new[] { "vip" });
            var result = rule(model);
            Console.WriteLine(result);
        }
    }
}
