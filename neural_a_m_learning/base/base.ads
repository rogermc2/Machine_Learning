--  Based on scikit-learn/sklearn/_base.py

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Base is

   function Score (X : Float_Matrix; Y : Integer_Matrix) return Float;

end Base;
