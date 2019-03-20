using System;
using System.Collections.Generic;
using System.Linq;

namespace csShop
{
    public class CSharpMaxProfits
    {
        public static int MaxProfit3(List<int> l)
        {
            int StepBackward(int currPrice, int leftMaxDiff, Func<int, int, int, int> cont, int prevMaxValue, int prevRightMaxDiff, int currMax)
            {
                var newMaxValue = currPrice >= prevMaxValue ? currPrice : prevMaxValue;
                var newRightMaxDiff = prevMaxValue - currPrice > prevRightMaxDiff ? prevMaxValue - currPrice : prevRightMaxDiff;
                var newResult = leftMaxDiff + newRightMaxDiff > currMax ? leftMaxDiff + newRightMaxDiff : currMax;
                return cont(newMaxValue, newRightMaxDiff, newResult);
            }

            int StepForward(int prevMinValue, int prevLeftMaxDiff, Func<int, int, int, int> cont, List<int> prices)
            {
                switch (prices.Count)
                {
                    case 0: return 0; 
                    case 1: return cont(prices[0], 0, 0);
                    default:
                        var newMin = prices[0] < prevMinValue ? prices[0] : prevMinValue;
                        var newDif = prices[0] - prevMinValue > prevLeftMaxDiff ? prices[0] - prevMinValue : prevLeftMaxDiff;
                        return StepForward(newMin, newDif, (x, y, acc) => StepBackward(prices[0], newDif, cont, x, y, acc), prices.Skip(1).ToList());
                }
            }
            return StepForward(l[0], 0, (x, y, z) => z, l.Skip(1).ToList());
        }
        public static int MaxProfit3_WithListAccumulator(List<int> l)
        {
            List<(int, int)> StepBackward(int currPrice, int leftMaxDiff, Func<int, int, List<(int, int)>, List<(int, int)>> cont, int prevMaxValue, int prevRightMaxDiff, List<(int, int)> lst)
            {
                var newMaxValue = currPrice >= prevMaxValue ? currPrice : prevMaxValue;
                var newRightMaxDiff = prevMaxValue - currPrice > prevRightMaxDiff ? prevMaxValue - currPrice : prevRightMaxDiff;
                lst.Insert(0, (leftMaxDiff, newRightMaxDiff));
                return cont(newMaxValue, newRightMaxDiff, lst);
            }

            List<(int, int)> StepForward(int prevMinValue, int prevLeftMaxDiff, Func<int, int, List<(int, int)>, List<(int, int)>> cont, List<int> prices)
            {
                switch (prices.Count)
                {
                    case 0: return new List<(int, int)>(); //(0, 0, 
                    case 1: return cont(prices[0], 0, new List<(int, int)>()); //cont((prices[0], 0, new List<(int, int)>()));
                    default:
                        var newMin = prices[0] < prevMinValue ? prices[0] : prevMinValue;
                        var newDif = prices[0] - prevMinValue > prevLeftMaxDiff ? prices[0] - prevMinValue : prevLeftMaxDiff;
                        return StepForward(newMin, newDif, (x, y, acc) => StepBackward(prices[0], newDif, cont, x, y, acc), prices.Skip(1).ToList());
                }
            }
            var res = StepForward(l[0], 0, (x, y, z) => z, l.Skip(1).ToList());
            Console.Write($"C# pairs: \n[");
            res.ForEach(x => Console.Write($"{x}, "));
            Console.Write("]\n");
            return res.Select(x => x.Item1 > 0 || x.Item2 > 0 ? x.Item1 + x.Item2 : 0).Max();
        }
        public static int MaxProfit3_WithTupleAccumulator(List<int> l)
        {
            (int, int, List<(int,int)>) StepBackward(int currPrice, int leftMaxDiff, Func<(int, int, List<(int, int)>), (int, int, List<(int, int)>)> cont, (int prevMaxValue, int prevRightMaxDiff,  List<(int, int)> lst) acc)
            {
                var newMaxValue = currPrice >= acc.prevMaxValue ? currPrice : acc.prevMaxValue;
                var newRightMaxDiff = acc.prevMaxValue - currPrice > acc.prevRightMaxDiff ? acc.prevMaxValue - currPrice : acc.prevRightMaxDiff;
                acc.lst.Insert(0, (leftMaxDiff, newRightMaxDiff));
                return cont((newMaxValue, newRightMaxDiff, acc.lst));
            }

            (int, int, List<(int, int)>) StepForward(int prevMinValue, int prevLeftMaxDiff, Func<(int, int, List<(int, int)>), (int, int, List<(int, int)>)> cont, List<int> prices)
            {
                switch (prices.Count)
                {
                    case 0: return (0, 0, new List<(int, int)>());
                    case 1: return cont((prices[0], 0, new List<(int, int)>()));
                    default:
                        var newMin = prices[0] < prevMinValue ? prices[0] : prevMinValue;
                        var newDif = prices[0] - prevMinValue > prevLeftMaxDiff ? prices[0] - prevMinValue : prevLeftMaxDiff;
                        return StepForward(newMin, newDif, (acc) => StepBackward(prices[0], newDif, cont, acc), prices.Skip(1).ToList());
                }
            }
            var rAcc = StepForward(l[0], 0, x => x, l.Skip(1).ToList());
            Console.Write($"C# pairs: \n[");
            rAcc.Item3.ForEach(x => Console.Write($"{x}, "));
            Console.Write("]\n");
            return rAcc.Item3.Select(x => x.Item1 > 0 || x.Item2 > 0 ? x.Item1 + x.Item2 : 0).Max();
        }

    }
}


