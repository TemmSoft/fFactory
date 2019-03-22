using System;
using System.Collections.Generic;
using System.Text;

namespace csShop.Lang
{
    static public class Categories
    {
        static public Func<T1, Func<T2, TRes>> Curry<T1, T2, TRes>(this Func<T1, T2, TRes> func) => a => b => func(a, b);
        static public Func<T1, Func<T2, Func<T3, TRes>>> Curry<T1, T2, T3, TRes>(this Func<T1, T2, T3, TRes> func) => a => b => c => func(a, b, c);
        static public Func<T1, Func<T2, Func<T3, Func<T4, TRes>>>> Curry<T1, T2, T3, T4, TRes>(this Func<T1, T2, T3, T4, TRes> func) => a => b => c => d => func(a, b, c, d);
        static public Func<T1, Func<T2, Func<T3, Func<T4, Func<T5, TRes>>>>> Curry<T1, T2, T3, T4, T5, TRes>(this Func<T1, T2, T3, T4, T5, TRes> func) => a => b => c => d => e => func(a, b, c, d, e);
        static public Func<T1, Func<T2, Func<T3, Func<T4, Func<T5, Func<T6, TRes>>>>>> Curry<T1, T2, T3, T4, T5, T6, TRes>(this Func<T1, T2, T3, T4, T5, T6, TRes> func) => a => b => c => d => e => f => func(a, b, c, d, e, f);
        static public Func<T1, Func<T2, Func<T3, Func<T4, Func<T5, Func<T6, Func<T7, TRes>>>>>>> Curry<T1, T2, T3, T4, T5, T6, T7, TRes>(this Func<T1, T2, T3, T4, T5, T6, T7, TRes> func) => a => b => c => d => e => f => g => func(a, b, c, d, e, f, g);


    }
}
