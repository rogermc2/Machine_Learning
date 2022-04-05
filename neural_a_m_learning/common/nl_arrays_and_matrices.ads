
package NL_Arrays_And_Matrices is

   type Float_Array is array (Integer range <>) of Float;
   type Integer_Array is array (Integer range <>) of Integer;

   type Float_Matrix is array (Integer range <>, Integer range <>) of Float;
   type Integer_Matrix is array (Integer range <>, Integer range <>) of Integer;

    subtype Safe_Float is Float range Float'Range;

end NL_Arrays_And_Matrices;
