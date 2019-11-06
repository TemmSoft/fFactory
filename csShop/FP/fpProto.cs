using System;
using System.Collections.Generic;
using System.Text;

namespace csShop.FP
{
    public class fpProto
    {
        static void Proto1()
        {


            Func<int, int, int, int> sammm = (x, y, z) => x + y + z;
            var aa = sammm.Curry()(1);


            ////  var csList = new List<int> { 18, 2, 9, 3, 12, 9, 5, 17, 4, 13, 6, 11, 7 };
            // var csList = new List<int> { 1, 7, 2, 3, 9 };

            //  var fsList = ListModule.OfSeq<int>(csList); // get FSharpList<int>

            ////  var csRes = CSharpMaxProfits.MaxProfit3_New(csList); 
            //  var csRes = CSharpMaxProfits.MaxProfit3(csList);
            //  var fsRes = FSharpMaxProfits.maxProfit3(fsList);

            //  Console.WriteLine($"Max profits: C# - {csRes}, F# - {fsRes}");
            //  Console.ReadKey();
        }
    }
}
