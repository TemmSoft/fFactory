using System.Collections.Generic;

namespace csShop.Lang
{


    public class AType
    {
        public string str {  get; protected set; } = "goddam!!!";
    }

    public class SType: AType
    { 
        public string getStr => $"Eahh... This is a {str}!";
    }

    public class IType : AType
    {
        public int getInt() => int.Parse(str);
    }
    class Syntax
    {
        List<AType> lAT = new List<AType>() { new AType(), new AType() };
        List<SType> lST= new List<SType>() { new SType(), new SType() };
        List<IType> lIT = new List<IType>() { new IType(), new IType() };

        public void Variance()
        {

            IEnumerable<string> strings = new List<string>();
            // An object that is instantiated with a more derived type argument   
            // is assigned to an object instantiated with a less derived type argument.   
            // Assignment compatibility is preserved.   
            IEnumerable<object> objects = strings;

            IEnumerable<AType> stngs = new List<AType>();
            IEnumerable<SType> st = new List<AType>();
            IEnumerable<object> objs = strings;
          //  IEnumerable<string> strings = new List<string>();

            IList<AType> a = lST;
            IList<SType> s = lAT;
            IList<IType> i = ;

        }

    }
}
