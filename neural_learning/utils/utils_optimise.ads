--  Based on scikit-learn/sklearn/utils/optimise.py

with Optimise;

package Utils_Optimise is

   function Check_Optimize_Result (Result : Optimise.Optimise_Result;
                                   Max_Iter : Natural := 0) return Natural;

end Utils_Optimise;
